options(error=recover)
# R do vector to vector calculation, but has convention on how vectors
#	should be recycled or truncated.  For convinient, *apply() do special
#	map and reduce, in which: apply for array to vector,
#	{l,s,v}apply for vector to vector,
#	mapply for many vector to vector (and utilized by Vectorize()),
#	tapply for typed groups to array, rapply for recursive apply, and
#	eapply for environment apply.
#	outer() for vector x vector to array mapping, but need
#	vectorized FUN(); it actually takes care of the dimensions only.
#	package:Jmisc oapply() for 2 vectors to matrix, built on
#	expand.grid() and mapply.
# In addition, for a jagged (ragged) array R can
#	(1) have a list, and each sublist is the members of a group, which is
#	unconvinient (most math function not accepts list) but most flexible,
#	running with lapply() (also ref. package:rowr);
#	(2) have a vector (values) recording all members and another (index)
#	vector recording the grouping, running with tapply() (and by/aggregate);
#	(3) have a matrix where each column/row is for a group and short groups
#	are filled with placeholder like NA.
#	Representations of (1) and (2) are inter-convertable via stack/unstack.
#	Map and reduce for (2) seems handy and can be less flexible than
#	tapply(), since the grouping is continuous and is repeating,
#	naturally when members among groups are actually similar.
# attr(X, "sdim") is a named list, each name is prefixed with a 
#	margin (or exactly equal to one), and each value denotes the
#	repeating times of subdim residing in the coresponding dimension
#	(called superdim). 
#	More than 1 subdim reside in the same superdim is allowed.
#	This feature allows dividing a subdim further, organizing the subdims
#	into hierachy.  I need to clarify operation on sdim means whether
#	operating within every group independently (similar to apply(MARGIN))
#	or operating among groups, maintaining the contain of a group
#	(using apply() achieves this via apply() on the other margins, but
#	for sdim, there is no complement margin); similarly, should the length
#	of a sdim be the sizes of groups or the number of groups?
#	It seems the first convention is compatible with apply().
#	A still problem is that attributes in R are fragile, even indexing
#	will drop most attributes.
# package:lambda.tools allows block operations and two-dimensional map/fold,
#	but they are never generalized and smart.
#	package:tensorA is smart in the same manner but not generalized
#	(and `[.tensor` seems broken).
# Design:
#	* Naming convention: "simple array" - object that is.array() is TRUE;
#	  "generalized array" - object that is.garray() is TRUE;
#	  "array" - generalized array, especially in issued message;
#	* In this world, only 2 types of data are welcome: array and scalar.
#	* Most functions also work for simple array, with warnings.
#	* Attribute "sdim" of simple array are neglected (since no superdim).
#	* Attribute class="garray" is almost only for method dispatching.
#	  The validity of a generalized array in fact depends on the
#	  correctness of dimnames, which is tested by `is.garray()`.
# Vocabulary:
#	Context	code	message		help	
#	array	array	simple array	simple array
#	garray	garray	array		generalized array
#	sdim	sdim	sub-dimension	subdim
# CAVEAT:
#	* R's Native `%*%` multiples arrays (dim>2, and same lengths) as
#	  inner product of vectors, resulting a scalar.
#	* R seldom uses names(dim()) but save it.  However, some functions 
#	  discard it, like: t().  I will totally neglect it.
#	* Some R's Native functions (`%*%`/crossprod/apply) do not save values 
#	  of dimnames() if it is NULL evne thouth with names (margins).
#	* R's native `dim<-` should always go before `dimnames<-`. 
#	  Whenever `dim<-` is called, attribute `dimnames` gets removed.
#	  `dim.garray<-` tries to save dimnames as many as possible.

#' Generalized and smart array
#'
#' Creates or tests for generalized arrays.
#' They are generalized because they handle dim and sdim; and are smart
#'	because automatically match dimensions by margins (names of dimnames).
#'	Attribute sdim denotes the subdivision of dimensions or grouping of
#'	members of a dimension, for organizing a ragged array.
#' Implementation of garray is most similar to class table, i.e., use names of
#'	dimnames to store the margins
#' `is.garray` do simple validation, no check for validity of sdim because it
#'	is too expensive.  Operation of sdim by this package is always
#'	guaranteed valid.
#' @param dim  An integer vector giving the maximal indices in each dimension.
#' @param dimnames  A list (or it will be ignored) with for each dimension
#'	one component, either ‘NULL’ or a character vector.
#' @param margins  Override the names of dim and of dimnames.
garray <- function(X, dim=NULL, dimnames=NULL,
		margins=NULL, sdim=attr(X, "sdim", exact=TRUE)) {
	if (is.null(dim)) {
		X <- as.array(X) 
		d <- dim(X)	# array() keeps the names of dim.
		dn <- if (is.null(dimnames)) dimnames(X) else dimnames
	} else {
		d <- dim
		names(dim) <- NULL
		X <- array(X, dim, dimnames)
		dn <- dimnames(X)
	}
	if (is.null(dn)) dn <- vector("list", length(d))
	if (is.null(margins)) {
		margins <- if (!is.null(names(dn))) names(dn)
			else if (!is.null(names(d))) names(d)
			else stop("need margins")
	}
	names(dn) <- margins
	dimnames(X) <- dn
	class(X) <- "garray"
	sdim(X) <- sdim
	X
}
# garray() is not generic function, thus garray.array() will never be called
#	by dispatching.  It serves as a simple and faster version of garray(),
#	mainly for internal usage.
garray.array <- function(X, sdim) {
	class(X) <- "garray"
	stopifnot(is.garray.loose(X))
	sdim(X) <- sdim
	X
}
as.garray <- function(X, ...) UseMethod("as.garray")
as.garray.garray <- function(X, ...)
	if (is.garray(X)) X else stop("broken array")
as.garray.default <- function(X, ...) garray(X, ...)
# By definition and for S3 dispatching, class(.)='garray' is required, but
#	simple arrays with proper margins actually work correctly with most
#	functionalities of this package.  For the sake of conpatibility and
#	reducing warning message, I sometimes test array by is.garray.duck().
is.garray <- function(X) {
	n <- names(dimnames(X))
	is.array(X)&&!is.null(n)&&!anyNA(n)&&all(""!=n)&&inherits(X, "garray")
	#warning("a valid array but no class")
}
is.garray.duck <- function(X) {
	n <- names(dimnames(X))
	is.array(X)&&!is.null(n)&&!anyNA(n)&&all(""!=n)
}	# duck-typing validation without warning
as.data.frame.garray <- function(x, row.names=NULL, col.names=NULL, ...,
		stringsAsFactors=FALSE) {
	stopifnot(2==length(dim(x)))	# Make View(X) work for 2-D array.
	row.names <- if (is.null(row.names)) rownames(x)
	col.names <- if (is.null(col.names)) colnames(x)
	if (!is.null(spd <- .superdim(x))) {
		sd <- sdim(x)
		if (is.null(row.names)) {
			row.names <- do.call("paste", c(lapply(sd[names(
				spd[spd==margins(x)[1]])], function(reptimes) {
					rep.int(names(reptimes), reptimes)
				}), list(1:nrow(x)), sep="|"))
		}
		if (is.null(col.names)) {
			col.names <- do.call("paste", c(lapply(sd[names(
				spd[spd==margins(x)[2]])], function(reptimes) {
					rep.int(names(reptimes), reptimes)
				}), list(1:ncol(x)), sep="|"))
			colnames(x) <- col.names
		}	# as.data.frame.matrix() not has option col.names
	}
	as.data.frame.matrix(unclass(x), row.names=row.names, ...)
}
is.scalar <- function(X) { 1L==length(X) }
print.garray <- function(X, ...) {
	dn <- dimnames(X)
	if (1==length(dn)&&is.null(dn[[1]])) cat(names(dn), sep="\n")
	NextMethod("print")
}
#' a1 <- garray(1:27, c(A=3,B=9), sdim=list(A1=c(a=2,b=1),B1=c(a=3)))
#' a2 <- garray(1:27, c(A=3,B=9), sdim=list(A1=c(a=2,b=1),B1=c(a=4)))

#' The margins and dimensions of a generalized array object
#'
#' Margins means the names of dimnames of an array.
#'	`margins()` used to be `names()`, but I realized that even an array
#'	can have attribute `names`, which is by default retieved by `names()`.
#' `margins<-` and `remargins` are for renaming, but
#'	`margins<-` ignores the names of `value` while
#'	`remargins` according to the names of `value` renames the margins. Doing
#'	so, `remargins` may also keep sdim.  `margins<-` always removes sdim.
#'	For `remargins` the length of value can be shorter than that of the
#'	margins if the value has names.
margins <- function(X) {
	if (!is.garray(X)) stop("margins of invalid array may be incorrect")
	names(dimnames(X))
}
`margins<-` <- function(X, value) {
	if (!is.array(X)) {
		stop("set margins of non-array")
	} else {
		if (!inherits(X, "garray")) class(X) <- "garray"
	}	# try fix it.
	if (is.null(value)||anyNA(value)||any(""==value))
		stop("illegal margins")
	dn <- dimnames(X)
	if (is.null(dn)) dn <- vector("list", length(value))
	attr(X, "sdim") <- NULL
	names(dn) <- value
	dimnames(X) <- dn	# if value not match dim(X), stop here
	X
}
remargins <- function(X, value, warn=TRUE) {
	if (!is.array(X)) {
		stop("set margins of non-array")
	} else {
		if (!inherits(X, "garray")) class(X) <- "garray"
	}	# try fix it.
	if (is.null(value)||anyNA(value)||any(""==value))
		stop("illegal margins")
	dn <- dimnames(X)
	if (is.null(dn)) {
		dn <- vector("list", length(value))
	} else if (!is.null(nn <- names(value))) {
		sd <- sdim(X)
		if (!is.null(sd)) {
			spd <- .superdim(X)
			names(sd) <- vapply(names(sd), function(k) {
				if (spd[k]%in%nn) substr(k, 1, length(spd[k])
					) <- value[spd[k]]
				k
			}, "")
		}
		m <- names(dn)
		value <- value[m]
		m[!is.na(value)] <- value[!is.na(value)]
		value <- m
		attr(X, "sdim") <- sd
	} else {
		attr(X, "sdim") <- NULL
	}
	names(dn) <- value
	dimnames(X) <- dn
	X
}
`dim.garray` <- function(X) {
	d <- NextMethod("dim")
	names(d) <- margins(X)	# names of dim() is kept by dimnames(). 
	d	# dim(X) can have names, but I do not respect it.
}
`dim<-.garray` <- function(X, value) {
	n <- names(value)
	if (is.null(n)||anyNA(n)||any(""==n)) {
		warning("array assigned dim without names")
		class(X) <- NULL
		# Passing garray object into a function that not awares of
		#	garray and changes dim may issue the warning.  To avoid
		#	it, unclass() the object and then passing.
		NextMethod("dim<-")
	} else {
		dn <- vector("list", length(n))
		names(dn) <- n
		dn[n] <-dimnames(X)[n]	# save dimnames
		names(value) <- NULL	# keep names(attr(X,"dim")) NULL
		X <- NextMethod("dim<-")	# default dim() remove dimnames
		dimnames(X) <- dn
		X
	}
}

#' Indexing for the garray class
#'
#' Indexing along margins as usual `[.array`, and along subdim.
#' @param ...  In addition to the native styles accepted by `[`, can be
#'	1.1 - a matrix with column names,
#'	    the colnames(.) should be a permutation of margins of the array.
#'	    #TODO: a missing margin means select all along that margin
#'	#TODO: 1.2 - the colnames(.) can be from names of sdim, the return
#'	#	will be a list of arrays.
#'	2.0 - an unnamed list, where NULL means to select all.
#'	2.1 - a named list, where NULL means to select all;
#'	3.1 - arguments with names, where NULL and missing means to select all;
#'	These extensions make indexing 3 times slower than native indexing.
#'	Since it is hard to assign MissingArg in list(), at the moment
#'	MissingArg is only safe in native R subsettting style.
#'	Using NULL to select all like MissingArg is actually not consistent
#'	in semantic of other uses of NULL. As shown by what `c()` returns,
#'	NULL is a generalized form of `logical(0)`, `integer(0)`,
#'	and `character(0)`, all of which means to select none when indexing.
#'	So take care of NULL if indexing with variables.
#' @param drop  Whether indeces where 1==dim are removed.  Different from
#'	R's native `[`, a garray will become a garray or scalar, never a vector.
#' @param value  An array or a scalar.
`[.garray` <- function(..., drop=TRUE) {
	n <- margins(..1)
	arg <- match.call()[-c(1:2)]	# fast sys.call() not work for `[`(...)
	arg$drop <- NULL
	argn <- names(arg)
	exist2 <- !missing(..2)&&(is.null(argn)||""==argn[1])
	if (exist2&&is.matrix(..2)&&is.numeric(..2)&&
			1==length(arg)&&!is.null(colnames(..2))) {
		nn <- match(n, colnames(..2))
		if (anyNA(nn)||0L<anyDuplicated(nn)) {
			Z <- NextMethod("[", drop=FALSE)
		} else {
			Z <- .subset(..1, ..2[,nn])
		}
	} else if ((!is.null(argn)&&all(""!=argn))||
			(exist2&&is.list(..2)&&1==length(arg))) {
		pf <- parent.frame()
		idx <- .index_canonical(..1, lapply(arg, function(i)
			if (is.symbol(i)&&""==as.character(i)) NULL
			else eval(i, pf)))
		idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
		# double quote(), for do.call() and .subset().
		Z <- do.call(".subset", c(list(..1), idx, drop=FALSE))
	} else {
		Z <- NextMethod("[", drop=FALSE)
	}	# `[.array` not keep margins if elements of dimnames is NULL
	d <- dim(Z)
	if (length(n)==length(d)) {
		names(d) <- n
		spd <- .superdim(..1)
		sd <- sdim(..1)[names(spd)[spd%in%names(d)[d==dim(..1)&1L!=d]]]
		if (drop) d <- d[1L!=d]
		if (0L==length(d)) return(as.vector(Z))
		class(Z) <- "garray"
		dim(Z) <- d
		sdim(Z, warn=FALSE) <- sd
	}
	Z
}
`[<-.garray` <- function(..., value) {
	cl <- oldClass(..1)
	n <- margins(..1)
	arg <- match.call()[-c(1:2)]
	arg$value <- NULL
	argn <- names(arg)
	exist2 <- !missing(..2)&&(is.null(argn)||""==argn[1])
	matrix2 <- exist2&&is.matrix(..2)&&is.numeric(..2)&&1==length(arg)
	if (!matrix2&&is.garray(value)) {
		sd <- c(sdim(..1), sdim(value))
		d <- dim.garray(value)	# d is named
		if (0L<length(newname <- n[!n%in%names(d)])) {
			dd <- rep.int(1L, length(newname))
			names(dd) <- newname
			d <- c(d, dd)
			dim(value) <- d
		}
		value <- aperm(value, perm=n)
	}
	if (matrix2&&!is.null(colnames(..2))) {
		nn <- match(n, colnames(..2))
		if (anyNA(nn)||0L<anyDuplicated(nn)) {
			Z <- NextMethod("[<-")
		} else {
			Z <- `[<-`(unclass(..1), ..2[,nn], value=value)
			#NextMethod("[<-", ..1, ..2[,nn], value=value) not work
		}
	} else if ((!is.null(argn)&&all(""!=argn))||
			(exist2&&is.list(..2)&&1==length(arg))) {
		pf <- parent.frame()
		idx <- .index_canonical(..1, lapply(arg, function(i)
			if (is.symbol(i)&&""==as.character(i)) NULL
			else eval(i, pf)))
		idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
		if (is.garray(value)) value <- .rep.garray(value, dim(do.call(
			".subset", c(list(..1), idx, drop=FALSE))), sd)
		Z <- do.call("[<-", c(list(unclass(..1)), idx, list(value=
			value)))	# Need a fast method for dim(value[...])
		# Need subsetting sdim(Z)
	} else {
		if (is.garray(value)) value <- .rep.garray(value,
			dim(.subset(..., drop=FALSE)), sd)
		Z <- NextMethod("[<-")
	}
	#sdim(Z) <- sdim(..1)
	class(Z) <- cl
	Z
}
.index_canonical <- function(X, idx) {
# In the extension style of indexing, I use NULL to denote something like
#	MissingArg and avoid non-standard evaluation in this function.
	n <- margins(X)
	idxn <- names(idx)
	if (is.list(idx[[1]])&&(is.null(idxn)||""==idxn[1])) {
		stopifnot(1==length(idx))
		idx <- idx[[1]]
		if (is.null(names(idx))) return(idx) else idxn <- names(idx)
	}
	d <- dim(X)
	l <- vector("list", length(n))
	l[] <- list(NULL)
	names(l) <- n
	idx_not_sd <- idxn%in%n
	l[idxn[idx_not_sd]] <- idx[idx_not_sd]
	if (!all(idx_not_sd)) {
		spd <- .superdim(X)
		sd <- sdim(X)
		for (k in idxn[!idx_not_sd]) {
			reptimes <- sd[[k]]
			if (is.null(reptimes))
				stop(sprintf("no sdim named `%s`", k))
			offset <- cumsum(reptimes)-reptimes
			i <- idx[[k]]
			if (is.logical(i)) i <- seq_along(reptimes)[i]
			if (!is.null(i)) l[[spd[k]]] <-
				do.call("c", lapply(i,
				function(j) seq_len(reptimes[j])+offset[j]))
		}
	}
	names(l) <- NULL
	l
}
#' @examples
#' mm <- matrix(c(1:3,1), 2, 2, dimnames=list(NULL, c("B","A")))
#' a <- garray(1:27, c(A=3,B=9), sdim=list(A1=c(a=2,b=1),B1=c(a=3)))
#' b <- a[mm]
#' c1 <- a[B=1:2,A=NULL]
#' c2 <- a[B=1:2,A=]
#' c3 <- a[B=1:2]
#' c4 <- a[list(B=1:2)]
#' c5 <- a[list(B=1:2,A=NULL)]
#' c6 <- a[list(NULL,1:2)]
#' d1 <- a[,] ; d1[B=1:2,A=NULL]       <- c1*10
#' d2 <- a[,] ; d2[B=1:2,A=]           <- c1*10
#' d3 <- a[,] ; d3[B=1:2]              <- c1*10
#' d4 <- a[,] ; d4[list(B=1:2)]        <- c1*10
#' d5 <- a[,] ; d5[list(B=1:2,A=NULL)] <- c1*10
#' d6 <- a[,] ; d6[B=1:2,A=NULL] <- 1
#' d7 <- a[,] ; d7[mm] <- 1000
#' d8 <- a[,] ; d8[mm] <- 1:2*1000
#' e1 <- a[A1=1,drop=FALSE]
#' e2 <- a[A1="b",drop=FALSE]
#' e3 <- a[,] ; e3[A1="b"] <- e2*10
#' e4 <- a[A=c(TRUE,FALSE,FALSE),drop=FALSE]
#' e5 <- a[A=TRUE,drop=FALSE]
#' e6 <- a[B=c(TRUE,FALSE,FALSE),drop=FALSE]
#' e7 <- a[A1=TRUE,drop=FALSE]
#' e8 <- a[A1=c(TRUE,FALSE),drop=FALSE]


#' Subdim of an array
#'
#' Retireve or set the subdim of an array.
#'
#' Validation of subdim is expensive because its consistency with dim need
#'	checking.  Thus most of functions do not validate it. 
#'	Operations of subdim with functions discussed here are guaranteed to
#'	be always keeping the consistency.
#' @return  A non-empty list or NULL
sdim <- function(X) {
	if (!is.garray(X)) {
		warning("sdim of non-array cannot be retrieved")
		return(NULL)
	}
	attr(X, "sdim", exact=TRUE)
}
`sdim<-` <- function(X, warn=TRUE, value) {
	if (0L==length(value)) {
		attr(X, "sdim") <- NULL
		return(X)
	} else if (!is.list(value)) {
		warning("sdim should be a non-empty list with names") 
		return(X)
	} else if (!is.garray(X)) warning("assign sdim to non-array object")
	d <- dim.garray(X)
	spd <- .validate_sdim(d, value, warn=warn)
	if (0L<length(spd)) {
		nsd <- setNames(nm=names(spd))	# so lapply() return named list
		attr(X, "sdim") <- lapply(nsd, function(k) {
			reptimes <- as.integer(value[[k]])
			na <- names(value[[k]])
			ti <- d[spd[k]]%/%sum(reptimes)
			if (1L<ti) {
				reptimes <- rep.int(reptimes, ti)
				if (!is.null(na)) na <- make.unique(
					rep.int(na, ti))
			}
			names(reptimes) <- na
			reptimes
		})
		# If `sdim<-`() always save canonical reptimes, I do not need
		#	`rep(reptimes, length.out[k]/sum(sd[[k]]))` later.
	} else {
		attr(X, "sdim") <- NULL
	}
	X
}

#' Supressing or elevating subdims
#'
#' @param superdim  Named character.
#'	If a value is "" or NA, the name should be the name of some subdim. And
#'	if the reptimes of the subdim equal an integer, the subdim will be
#'	elevated into a margin; if the reptimes are not equal, stop.
#'	If a value is some margin, the name should be some other margin,
#'	the later will become a subdim (renamed to "value.name") residing in
#'	the former margin.
#TODO: resdim <- function(X, superdim) { }

#' Extract and validate superdim for compatible with subdim
#'
#' Extract is cheap, but validation is expensive.
.superdim <- function(X) {
	sd <- sdim(X)
	if (!is.list(sd) || 0L==length(sd)) return(character())
	sd[] <- names(sd)	# sd has names, which is respected by vapply().
	n <- margins(X)		# dim names (margins and superdim)
	vapply(sd, function(k) n[startsWith(k, n)], "")
	# If there are inconsistant superdim, vapply() will error.
}
.validate_sdim <- function(d, sd, warn=TRUE) {
	if (!is.list(sd) || 0L==length(sd)) return(character())
	n <- names(d)	# margins names (superdim), d can be dim of non garray
	m <- names(sd)	# subdim names
	stopifnot(is.numeric(d), !is.null(n), !anyNA(n), ""!=n,
		!is.null(m), !anyNA(m), ""!=m)
	starts <- outer(m, n, startsWith)
	nsuperdim <- rowSums(starts)
	if (warn&&any(1!=nsuperdim)) warning(sprintf(
		"sdim not prefixed with exactly 1 dim: %s (%s)",
		paste(m[1!=nsuperdim], collapse=", "), paste(n, collapse=", ")))
	spd <- vapply(seq_along(m), function(i) n[starts[i,]][1], "")
	names(spd) <- m
	idx <- vapply(names(spd), function(k) {
		if (is.na(spd[k])) return(FALSE)
		s <- sum(sd[[k]])
		if (0L==d[spd[k]]%%s) TRUE else {
			if (warn) warning(sprintf(
				"sdim %s (sum=%d) unmatch margin %s (dim=%d)",
				k, s, spd[k], d[spd[k]]))
			FALSE	# remove it
		} }, TRUE)
	spd[idx]
}

#' General array transposition
#'
#' Restore garray attributes that discarded by aperm.default().
#'	Cannot permute between subdim (means to promote subdim of equal length
#'	into regular dim and reduce dim into subdim), sorry.
#' @param perm  Desired margins after permutation, integer or character.
#aperm <- function (a, perm, ...) UseMethod("aperm")
aperm.garray <- function(X, perm=NULL) {
	if (all(perm==margins(X))||all(perm==seq_along(margins(X)))) return(X)
	Z <- aperm.default(a=X, perm=perm, resize=TRUE)
	class(Z) <- "garray"
	attr(Z, "sdim") <- attr(X, "sdim", exact=TRUE)	# no further validation
	Z
}

#' Combine generalized arrays
#'
#' Combine sdim correctly.
#' @param ...  arrays, or a list of several arrays.  Margins of
#'	these arrays should be the same.
#' @param along  The dimension along which to bind the arrays. 
#'	The arrays may have different lengths along that dimension,
#'	and are bind along it, with addition `sdim` indicating the composition
#'	of this dimension (creating a new subdim).  Some arrays may not have
#'	that margin, then the dimension of these arrays expand to 1.
#'	If assign a new margin is created,
#'	and will become last dimensions of output.
#'	In such case, all arrays should have the same dimension lengths.
#' @param margins  Resulting margins.  If includes a new margin, then
#'	`along` is neglected and the new margin setting `along`.
#'	If no new name, along the last `margins`.
abind <- function(..., margins=NULL, along=character()) {
	dots <- list(...)
	if (0L==length(dots)) return(NULL)
		else if (1L==length(dots) && is.list(dots[[1]]))
		dots <- dots[[1]]
	if (!all(vapply(dots, is.garray, TRUE))) stop("need one list of arrays")
	if (is.null(margins)) margins <- unique(c(margins(dots[[1]]), along))
	new <- !margins%in%margins(dots[[1]])
	stopifnot(2>sum(new))
	along <- if (0L==sum(new))  margins[length(margins)]
		else if (1L==sum(new)) margins[new]
		else stop("too many new margins")
	n <- c(margins[margins!=along], along)
	d <- dim(dots[[1]])
	d[along] <- 0
	d <- d[n]
	dn <- dimnames(dots[[1]])
	dn[[along]] <- character()
	dn <- dn[n]
	sd <- list()
	newsd <- integer()
	nam <- make.names(names(dots), unique=TRUE)
	for (i in seq_along(dots)) {
		new <- !n%in%margins(dots[[i]])
		di <- dim(dots[[i]])
		stopifnot(2>sum(new))
		if (1L<sum(new)) {
			stop(sprintf("too many new margins for dots[[%d]]", i))
		} else if (1L==sum(new)) {
			di[n[new]] <- 1
			dim(dots[[i]]) <- di
		}	# append with one more margin (length 1)
		dots[[i]] <- aperm(dots[[i]], n)
		if (any((dim(dots[[i]])!=dim(dots[[1]]))[-length(n)]))
			stop("too many dimensions not match")
		d[along] <- d[along]+dim(dots[[i]])[along]
		newsd[nam[i]] <- d[along]
		dn[[along]] <- c(dn[[along]], paste0(nam[i],
			dimnames(dots[[i]])[[along]]))
		sdi <- sdim(dots[[i]])
		for (k in names(sdi)) {
			if (along==k) warning(sprintf(
				"sdim named `%s` may be overrided", k))
			if (is.null(sd[[k]])) sd[[k]] <- sdi[[k]]
			else if (!identical(sd[[k]], sdi[[k]])) warning(sprintf(
				"discard divergence sdim %s for %d", k, i))
		}
	}	# used to be based on lapply(), but `<<-` is tricky.
	if (any(nam!=1)) sd[[along]] <- newsd	# User may rename it afterward
	aperm(garray(do.call("c", dots), d, dn, margins=n, sdim=sd), margins)
}

#' Simplified list to vector of some type (usually atomic) if possible
#' @param value1  A prototype of the return.
#' @return  An list (if impossible) or an simple array.
.simplify2array <- function(X, value1=X[[1]]) {
	if (!is.list(X)) stop("non-list need not simplifying")
	l <- unique.default(lengths(X))
	if (is.recursive(value1)) return(X)
	if (1!=length(l)||l!=length(value1)||is.null(value1)) {
		#any(vapply(X, typeof, "")!=typeof(value1))	# time consume
		warning("impossible to simplify the result to array")
		return(X)
	}
	if (is.array(value1)) {
		d1 <- dim(value1)
		dn1 <- dimnames(value1)
		n1 <- names(dn1)
		if (0L==length(n1)||anyNA(n1)||any(""==n1))
			warning("elements without graceful names")
	} else if (1L<l) {
		d1 <- length(value1) 
		dn1 <- list(VALUE1=names(value1))
	} else {
		d1 <- integer()
		dn1 <- NULL
	}	# if 0L==length(value1), dim(Z) will have 0 and 0==length(Z)
	Z <- do.call("c", X)
	dim(Z) <- c(d1, length(X))
	dimnames(Z) <- c(dn1, list(NULL))
	Z
}

#' @param length.out  The desired dimensions of the output. 
#'	Note that `length(length.out)==length(dim(X))` is mandatory.
#'	There are softwares that assume margins new to X to be 1 (like MATLAB
#'	and numpy for Python), and I once consider implement the feature,
#'	but I finally realize the feature is bug-prone and users do not need it.
#'	In circumstance the feature is useful (`amap` and `[<-`),
#'	I hard code it there.
#' @param sdim  Provide subdim for help matching dimensions of X and length.out.
#'	Usually `X` does not has such information.
#'	(X has dimensions smaller than length.out.  In function like `amap`,
#'	this information is carried by arrays with larger dimensions).
.rep.garray <- function(X, length.out, sdim=NULL) {
	#tail=utils::tail(which(1<dim(X)), 1)
	# Dimension after that will not be repeated, mainly
	#	because .mapply() will do repeating automatically and quickly. 
	#	Use NULL to disable detecting of tail.
	d <- dim(X)
	n <- names(d)
	if (setequal(names(length.out), n)) {
		length.out <- length.out[n]
	} else {
		stopifnot(is.null(names(length.out)))
		names(length.out) <- n
	}
	stopifnot(length(length.out)==length(d), length.out>=d, 0L<d)
	if (all(length.out==d)) return(X)
	spd <- if (is.null(sdim)) character(0)
		else .validate_sdim(length.out, sdim)
	if (0==length(spd)||all(d==1L|d==length.out)) {
		head <- which(d==length.out)
		perm <- c(head, seq_along(length.out)[-head])
		return(aperm(array(X, length.out[perm]), order(perm)))
	}	# sweep() does it in this manner, seems faster than .subset().
	idx <- lapply(seq_along(d), function(i) {
		if (d[i]==length.out[i]) NULL
		else if (1L==d[i]) rep.int(1L, length.out[i])
		else if (any(ii <- spd==n[i])) {
			j <- names(spd)[ii]
			jj <- which(length.out[i]==vapply(sdim[j], sum, 0L)&
				d[i]==vapply(sdim[j], length, 0L))
			if (0L==length(jj)) stop("sub-dimension match none")
			if (1<length(jj)) {
				warning("sub-dimension match many")
				jj <- jj[1]
			}
			reptimes <- sdim[j][[jj]]
			rep.int(seq_along(reptimes), reptimes)
		}
		else stop("dimension not match")
	})
	idx[d==length.out] <- list(quote(quote(expr=)))
	#idx[vapply(idx, is.null, TRUE)] <- list(quote(quote(expr=)))
	do.call(".subset", c(list(X), idx, drop=FALSE))
}	# mainly for internal usage, thus return only data, no dimnames.

#' Parallel summary, inspired by pmax() and pmin().
#'
#' Functions of Summary group are all, any, max, min, prod, range, and sum,
#'	which reduce a vector into a scalar (except range), thus the name
#'	of psummary().  Of course, other FUN can be passed-in,
#'	but functions like range() that returns a non-scalar vector result
#'	in unpredictable return.  For arguments of different
#'	size, pmin() and pmax() make fractional recycling and issue warning,
#'	but psummary() error since as.data.frame() do not fractionally recycle.
#' @param ...  Usually in the form `psummary(x, y, z, FUN=sum, na.rm=TRUE)`,
#'	alternaitvely `psummary(list(x, y, z), FUN=sum, na.rm=TRUE)`.
psummary <- function(...) UseMethod("psummary")
psummary.garray <- function(...) {
	dots <- list(...)
	ifun <- which(vapply(dots, is.function, TRUE))
	if (0L==length(ifun)) {
		args <- list()
		FUN <- sum
	} else {
		stopifnot(1<ifun[1])
		args <- dots[-(1:ifun[1])]
		FUN <- dots[[ifun[1]]]
		dots <- dots[1:(ifun[1]-1)]
	}
	if (1L==length(dots) && is.list(dots[[1]])) { dots <- dots[[1]] }
	stopifnot(vapply(dots, is.atomic, TRUE))
	do.call("amap", c(FUN, dots, args))
}
psummary.default <- function(...) {
	dots <- list(...)
	ifun <- which(vapply(dots, is.function, TRUE))
	if (0L==length(ifun)) {
		args <- list()
		FUN <- sum
	} else {
		stopifnot(1<ifun[1])
		args <- dots[-(1:ifun[1])]
		FUN <- dots[[ifun[1]]]
		dots <- dots[1:(ifun[1]-1)]
	}
	if (1L==length(dots) && is.list(dots[[1]])) { dots <- dots[[1]] }
	stopifnot(vapply(dots, is.atomic, TRUE))
	m <- as.matrix(as.data.frame(lapply(dots, as.vector)))
	# as.data.frame() do non-fractionally recycling
	s <- do.call("apply", c(list(m, 1, FUN), args))
	l <- vapply(dots, length, 0L)
	attributes(s) <- if (1L==which.max(l)) { attributes(dots[[1]]) }
	s
}

#' Read a complex table and return array in storagemode. 
#'
#' A complex table has
#'	several row and colume headers, some of which indicate the hierarchy
#'	of the dimensions (thus the returned array may have more dimensions than
#'	row and column), some of which are real row.names and
#'	col.names that will be turn into dimnames, and some of which are
#'	additional attributes.  Cells of non-header are all in a same format
#'	(like double).  The original header are preserved as attributes.
#'	The dimnames strictly save the layout of the matrix, thus row.names
#'	and col.names should be carefully chosen for matching the actual
#'	dimension.  Sometime aperm() is needed after this function.
#'	Sparse table, where dimnames is not complete (unbalanced) and needs
#'	non-fractionally recycling, is not supported.
#' @param header  hierarchy structure of the header, a list of length 2,
#'	assigning the index of row and col header,
#'	affecting parsing of the input table;
#' @param row.names,col.names  a list, whose elements can be
#'	a character vector assigning the name of one dimension directly, or
#'	a integer scalar which is the index ({icol,irow}) of {row,col}-header
#'	that will be extracted; since the row and the col headers can have
#'	hierarchy in the input table and the hierarchy will be organized into
#'	addisional dimensions, rownames indicate the names of dimensions that
#'	are from the row of the input table, while colnames, the col; in the
#'	input table, the header can be in two pattern: AAA and AOO;
#'	in the AAA pattern, the names are repeated for the cells that are in
#'	the same index in high dimension, while in the AOO pattern, the names
#'	appear once at the first and the other cells that are in the same index
#'	in high dimension are left blank; in the
#'	output array, {row,col}.names are not necessary dimnames[[1]] and [[2]];
#'	if all elements of the list are a integer scalar, then the list can
#'	also be coded as a integer vector (since they are the same for lapply);
#' @examples
#' express.prof <- Read.ComplexTable(
#'	"deleteome_responsive_mutants_ex_wt_var_controls.txt",
#'	header=list(1:2, 1:3), sep="\t", quote="\"",
#'	row.names=list(1), col.names=list(2, 1))
read.ctable <- function(file, header, sep="", quote="\"'",
		dec=".", row.names, col.names, comment.char="#",
		storagemode="double") {
	dat <- read.table(file, header=FALSE, sep=sep, colClasses="character",
		quote=quote, dec=dec, comment.char=comment.char)
	arr      <- unname(as.matrix(dat[-header[[1]],-header[[2]]]))
	storage.mode(arr) <- storagemode
	headrow  <- unname(as.matrix(dat[ header[[1]],-header[[2]]]))
	headcol  <- unname(as.matrix(dat[-header[[1]], header[[2]]]))
	headhead <- unname(as.matrix(dat[ header[[1]], header[[2]]]))
	dimnames <- c( lapply(row.names, function(i) {
		if (is.numeric(i) && 1L==length(i)) {
			i <- unique(headcol[,i])
			i <- i[""!=i]	# neglect empty rownames
		}
		return(i) }), lapply(col.names, function(i) {
		if (is.numeric(i) && 1L==length(i)) {
			i <- unique(headrow[i,])
			i <- i[""!=i]
		}
		return(i) }) )
	# Now arr is 2-dim, dimnames does not match dim(arr), but after runing
	#	the following line, dim(arr) is updated and dimnames(arr) gets
	#	discarded.  If run `dimnames(arr) <- dimnames` first, error.
	dim(arr) <- vapply(dimnames, length, 0)
	# Better than `arr <- array()` since it check for dimension match.
	dimnames(arr) <- dimnames
	attr(arr, "head") <- list(head=headhead, row=headrow, col=headcol)
	return(arr)
}

#' Mapping matching dimension of arrays with a function
#'
#' Generalized and smart mapply()/Map()/outer()/sweep() for data mapping.
#'	Matching is checked automatically.
#' @param FUN  Known vectorized function is recognized if passed in
#'	as character.  Other functions will be vectorized by `.mapply()`.
#' @param ...  Arrays with margins (names of dimnames) and maybe with sdim. 
#'	Orders of their margins can be different, but the extent along a
#'	margin is matched. Unmatched margins are broadcasting like outer(). 
#'	Scalar (length 1 vector) do not contribute margins and
#'	not broadcast here (they will broadcast by `.mapply()` later).
#' @param VECTORIZED  Whether FUN is vectorized will affect the behaviours.
#'	Some combination of FUN and VECTORIZED is not simply slowing down,
#'	but produces meaningless results or even stop (e.g., cumsum).
#'	TRUE - call `FUN` once with arrays being reorganize on dimensions;
#'	FALSE - call `FUN` many times (via `.mapply`), with each cell of arrays.
#' @param SAFE 
#'	FALSE - use unsafe but fast implementation, all inputs are repeated as
#'	few as possible, assuming that `FUN` recycles short vectors
#'	(like basic operators "+"). 
#'	TRUE - slower, all inputs are organized properly to have the same
#'	dimension and passed to FUN.  For VECTORIZED=FALSE, SAFE is neglected
#'	because `.mapply` recycle short vectors.
#' @return The dimensions is deduced from inputs.
amap<- function(FUN, ..., MoreArgs=NULL, SIMPLIFY=TRUE, VECTORIZED=NA) {
	dots <- lapply(list(...), function(X) if (is.scalar(X)) X
		else as.garray(X))
	if (is.na(VECTORIZED)) {
		VECTORIZED <- is.character(FUN)&&FUN%in%c(
			"abs", "sign", "sqrt","floor", "ceiling", "trunc",
			"round", "signif",
			"exp", "log", "expm1", "log1p",
			"cos", "sin", "tan","cospi", "sinpi", "tanpi",
			"acos", "asin", "atan",
			"cosh", "sinh", "tanh","acosh", "asinh", "atanh",
			"lgamma", "gamma", "digamma", "trigamma",
			"cumsum", "cumprod", "cummax", "cummin",
			"+", "-", "*", "/", "^", "%%",
			"&", "|", "!",
			"==", "!=", "<", "<=", ">=", ">",
			"Arg", "Conj", "Im", "Mod", "Re",
			"pmax", "pmin",
			"logsumexp")
		if (isTRUE(!VECTORIZED))
			warning("non-vectorized function is slow")
	}
	# Seldom run, because Ops is always called with VECTORIZED=TRUE when
	#	dispatching; and Math need not amap() at all, since they accept
	#	1 argument and preserve class=garray and sdim.
	n <- unique(do.call("c", lapply(dots, function(X)
		if (is.scalar(X)) character() else margins(X))))
	l <- length(n)
	ddat <- matrix(vapply(seq_along(dots), function(i) {
		if (is.scalar(dots[[i]])) return(rep.int(1L, l))
		d <- dim(dots[[i]])
		if (0L<length(newname <- n[!n%in%names(d)])) {
			dd <- rep.int(1L, length(newname))
			names(dd) <- newname
			d <- c(d, dd)
			dim(dots[[i]]) <<- d
		}
		d[n]
	}, integer(l)), l, length(dots), dimnames=list(n, names(dots)))
	#FIXME: May be feature of moving tail is good, need reviving.
	#if (1L<l) ddat <-	# margin len 1 moved to tail
	#	ddat[sort(rowSums(1==ddat), index.return=TRUE)$ix,,drop=FALSE]
	#n <- rownames(ddat)
	d <- vapply(n, function(k) max(ddat[k,], na.rm=TRUE), 0L,USE.NAMES=TRUE)
	dn <- lapply(n, function(k) {
		na <- character(0)
		for (i in seq_along(dots)) {
			nn <- dimnames(dots[[i]])[[k]]
			if (d[k]==length(nn)&&length(nn)>length(na)) na <- nn
			if (length(na)>1) break
		}
		na
	})
	names(dn) <- n
	sd <- do.call("c", lapply(dots, function(X)
		if (is.garray(X)) sdim(X) else NULL))
	sd <- sd[unique(names(sd))]
	dots <- lapply(dots, function(X) as.vector(if (is.scalar(X)) X
		else .rep.garray(aperm(X, perm=n), d, sd)))
	# aperm(X) consume less memory than .rep.garray(X)
	names(d) <- NULL
	FUN <- match.fun(FUN)
	# avoid "object 'FUN' of mode 'function' was not found",
	#	should call once here, not depend on match.fun() within
	#	mapply(), apply() or tapply().
	if (VECTORIZED) {
		X <- do.call(FUN, c(dots, MoreArgs))
		dim(X) <- d	#X <- aperm(., n)
		if (isTRUE(!SIMPLIFY))
			warning("alwayls return an array for vectorized fun")
		dimnames(X) <- dn
	} else {
		X <- .mapply(FUN, dots, MoreArgs)
		dim(X) <- d
		if (isTRUE(SIMPLIFY)) X <- .simplify2array(X)
		last <- length(dim(X))
		dnX <- dimnames(X)
		dim(X) <- c(dim(X)[-last], d)
		dimnames(X) <- c(dnX[-last], dn)
	}
	class(X) <- "garray"	# garray() is too expensive
	sdim(X) <- sd
	X
}
Ops.garray <- function(x,y) {	# Not include %*%
	FUN <- get(.Generic, envir=parent.frame(), mode="function")
	if (1L==nargs()) amap(FUN, x, VECTORIZED=TRUE)
		else amap(FUN, x, y, VECTORIZED=TRUE)
}
#' @examples
#' a <- garray(1:24, c(4,6,2), dimnames=list(X=1:4, Y=letters[1:6], Z=NULL),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' b <- garray(1:6/10,6,dimnames=list(Y=letters[1:6]))
#' c <- garray(1:4/100,c(X=4))
#' d <- garray(1:4/1000,c(Y=4))
#' e <- garray(1:2/1000,c(X=2))
#' f <- garray(0,c(Z=2))
#' g <- garray(0,c(ZZ=2))
#' m1 <- amap(psummary,c,a,b,      0.0001, VECTORIZED=FALSE)
#' m2 <- amap(sum,     c,a,b,      0.0001, VECTORIZED=FALSE)
#' m3 <-               c+a+b+      0.0001
#' n1 <- amap(sum,     c,a,b,d,    0.0001, VECTORIZED=FALSE)
#' n2 <- amap(sum,     c,a,b,e,    0.0001, VECTORIZED=FALSE)
#' n3 <- amap(sum,     c,a,b,e,f,  0.0001, VECTORIZED=FALSE)
#' p1 <- amap(sum,     c,a,b,e,f,g,0.0001, VECTORIZED=FALSE)
#' m1==m2
#' m2==m3
#' m2==aperm(m3, 3:1)

#' Generalized and smart apply()/Reduce()/tapply() for data folding.
#'
#' @param FUN  Usually a summary function (like `all` and `sum`).
#' @param X  A garray, with margins (names of dimnames) and maybe with sdim.
#' @param MARGIN  Some margins of X and names of sdim.
#'	MARGIN=character() means to reduce all margins (over no margin).
#'	In such case, areduce() is not needed actually.
#' @param SIMPLIFY  TRUE - simplifies the result list to vector of atomic
#'	if possible, and triggers warning and not simplifies if impossible;
#'	FALSE - not simplifies for non speed-up function, and issues warning
#'	(and have to simplify) for speed-up function; NA - simplifies but no
#'	warning if impossible.
#' @param SAFE  TRUE - use safe but slow implementation, in which data splited
#'	from the array are reorganized into small arrays (as are being subset
#'	by `[]`) and passed to FUN (other attributes are dropped, however);
#'	FALSE - faster, data are passed to FUN as dimension-less vectors.
#' @return A matrix (similar to return of apply() or tapply()), with the
#'	trailing margins the same as MARGIN, while the leading margins
#'	depend on FUN and SIMPLIFY.  If FUN returns a scalar or SIMPLIFY=FALSE,
#'	then no leading margins.  In MARGIN, subdim is replaced with superdims.
areduce <- function(FUN, X, MARGIN, ..., SIMPLIFY=TRUE, SAFE=FALSE) {
	stopifnot(is.garray(X), is.character(MARGIN))
	n <- margins(X)
	d <- dim(X)
	dn <- dimnames(X)
	sd <- sdim(X)
	na.rm <- ifelse(is.null(list(...)$na.rm), FALSE, list(...)$na.rm)
	if (all(MARGIN%in%n)) {
		n1 <- n[!n%in%MARGIN]
		dn1 <- dn[n1]
		X <- aperm(X, perm=c(n1, MARGIN))
		d1 <- prod(d[n1])
		d2 <- prod(d[MARGIN])	# prod(NULL)==1
		if (is.character(FUN)&&FUN%in%c("sum","mean")) {
			FUN <- c(sum=`.colSums`, mean=`.colMeans`)[[FUN]]
			# col{Sum,Mean}s are 3 times faster than row{Sum,Mean}s
			Z <- if (is.complex(X)) 
				FUN(Re(X), d1, d2, na.rm) +
				FUN(Im(X), d1, d2, na.rm)*1i
				else	# need not set dim(X) <- c(nrow, ncol)
				FUN(   X,  d1, d2, na.rm)
			if (isTRUE(!SIMPLIFY)||isTRUE(SAFE)) warning(
				"alwayls return an array for sum and mean")
			if (0L<length(MARGIN)) {
				dim(Z) <- d[MARGIN]
				dimnames(Z) <- dn[MARGIN]
			}
		} else {
			class(X) <- NULL
			dim(X) <- c(d1, d2)
			FUN <- match.fun(FUN)
			Z <- vector("list", d2)
			if (SAFE) {
				for (i in seq_along(Z)) {
					Z[[i]] <- forceAndCall(1, FUN,
						garray(X[,i], d[n1],
						dn1, sdim=sd), ...)
				}	# apply() internally use this loop
			} else {
				for (i in seq_along(Z)) {
					Z[[i]] <- forceAndCall(1, FUN,
						X[,i], ...)
				}
			}
			if (isTRUE(SIMPLIFY)) Z <- .simplify2array(Z)
			last <- length(dim(Z))
			dnZ <- dimnames(Z)
			if (0L<length(MARGIN)) {
				d <- c(dim(Z)[-last], d[MARGIN])
				dn <- c(dnZ[-last], dn[MARGIN])
			} else {
				d <- dim(Z)[-last]
				dn <- dnZ[-last]
			}
			if (0L==length(d)) {
				dim(Z) <- NULL
			} else {
				dim(Z) <- d
				dimnames(Z) <- dn
			}
		}
	} else {
		spd <- .superdim(X)
		if (!all(MARGIN%in%c(names(spd), n))) stop("foreign MARGIN")
		if (any(MARGIN%in%spd[MARGIN]))
			stop("some MARGINs are subdim residing in MARGINs")
		sdMARGIN <- MARGIN%in%names(spd)
		n0 <- spd[MARGIN[sdMARGIN]]	# along sdim (partial reduced)
		n2 <- MARGIN[!sdMARGIN]		# along dim
		n1 <- n[!n%in%c(n0, n2)]	# reduced dim
		X <- aperm(X, perm=c(n0,n1,n2))
		d0 <- prod(d[n0])
		d1 <- prod(d[n1])
		d2 <- prod(d[n2])
		class(X) <- NULL
		dim(X) <- c(d0*d1, d2)
		ngroup <- vector("list", sum(sdMARGIN))
		names(ngroup) <- spd[MARGIN[sdMARGIN]]
		group <- rep.int(1L, d0)
		extent <- 1L
		din <- cumprod(d[n0])	# inner dimension
		for (k in MARGIN[sdMARGIN]) {
			nn <- spd[k]
			reptimes <- sd[[k]]
			#storage.mode(reptimes) <- "integer"
			# Guarantee integer, or rowsum() will return all 0.
			na <- names(reptimes)
			ngroup[[nn]] <- if (is.null(na)) #||anyDuplicated(na)
				seq_along(reptimes) else na
			group <- group+extent*(rep.int(rep.int(seq_along(
				reptimes), reptimes*(din[nn]/d[nn])),
				d0/din[nn])-1L)
			extent <- extent*length(reptimes)
		}
		sd0 <- sd[MARGIN[sdMARGIN]]
		sd[MARGIN[sdMARGIN]] <- NULL	# subdim going to be reduced
		if (extent>.Machine$integer.max) stop(sprintf(
			"total number of levels > %d", .Machine$integer.max))
		MARGIN[sdMARGIN] <- n0
		if (identical("sum", FUN)) {	# Speed-up (10 times faster)
			ugroup <- seq_len(extent)
			Z <- .Internal(rowsum_matrix(X,
				rep.int(group, prod(d[n1])), ugroup,
				na.rm, as.character(ugroup)))
			# group should be integer; if double, result in 0.
			if (isTRUE(!SIMPLIFY))
				warning("alwayls return an array for sum")
			dim(Z) <- c(lengths(ngroup), d[n2])
			dimnames(Z) <- c(ngroup, dn[n2])
			Z <- aperm(Z, MARGIN)
		} else {
			# Implementations UNLIST(Z) - Faster
			Z <- vector("list", d2)
			if (SAFE) {
				nn <- spd[names(sd0)]
				names(sd0) <- nn
				lsd0 <- lengths(sd0)
				extent1 <- cumprod(lsd0)
				extent2 <- extent1/lsd0
				subdn <- function(dn, reptimes, i) {
					offset <- cumsum(reptimes)-reptimes
					dn[seq_len(reptimes[i])+offset[i]]
				}
				f <- function(i, ...) {
					idx <- (i-1L)%%extent1%/%extent2+1L
					n[n2] <- NULL
					FUN(aperm(garray(XX[[i]], c(.mapply(
						.subset, list(sd0, idx), NULL),
						d[n1]), c(.mapply(subdn,
						list(dn[nn], sd0, idx), 
						NULL), dn[n1]),
						c(nn, n1), sdim=sd), n), ...)
				}
				for (i in seq_len(d2)) {
					XX <- split(X, group)
					Z[[i]] <- lapply(seq_along(XX), f, ...)
				}
			} else {
				for (i in seq_len(d2)) {
					Z[[i]] <- lapply(split(X[,i], group),
						FUN, ...)
				}
			}
			Z <- do.call("c", Z)
			# Implementations REP(GROUP) - Slower
			#group <- rep.int(group, d2)+
			#	extent*rep(seq_len(d2), each=d0*d1)
			#Z <- lapply(split(X, group), FUN, ...)
			if (isTRUE(SIMPLIFY)) Z <- .simplify2array(Z)
			last <- length(dim(Z))
			dnZ <- dimnames(Z)
			dim(Z) <- c(dim(Z)[-last], lengths(ngroup), d[n2])
			dimnames(Z) <- c(dnZ[-last], ngroup, dn[n2])
			Z <- aperm(Z, c(names(dnZ)[-last], MARGIN))
		}
	}
	if (0L<length(MARGIN)) {
		class(Z) <- "garray"	# garray() is too expensive
		sdim(Z, warn=FALSE) <- sd
	}
	Z
}
#' @examples
#' a <- garray(matrix(1:24, 4, 6, dimnames=list(X=LETTERS[1:4], 
#' 	Y=letters[1:6])), sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' m1 <- areduce("sum", a, c("X"))
#' m2 <- areduce(`sum`, a, c("X"))
#' p1 <- areduce("sum", a, c("YY"))
#' p2 <- areduce(`sum`, a, c("YY"))
#' q1 <- areduce("sum", a, c("X","YY"))
#' q2 <- areduce(`sum`, a, c("X","YY"))
#' r1 <- areduce("sum", a, c("XX","YY"))
#' r2 <- areduce(`sum`, a, c("XX","YY"))
#' b <- garray(1:24, c(3,4,2), dimnames=list(X=LETTERS[1:3], 
#'      Y=letters[1:4],Z=NULL), sdim=list(XX=c(x1=2,x2=1), YY=c(y1=1,y2=1)))
#' s1 <- areduce("sum", b, c("XX","YY","Z"))
#' s2 <- areduce(`sum`, b, c("XX","YY","Z"))
#' t1 <- areduce(`identity`, b, c("XX","YY","Z"), SIMPLIFY=FALSE)
#' t2 <- areduce("c", b, c("XX","YY","Z"), SIMPLIFY=FALSE)	# not `c`
#' t3 <- areduce(`identity`, b, c("XX","YY"), SIMPLIFY=FALSE, SAFE=TRUE)
#' t4 <- areduce(`identity`, b, c("XX","YY"), SIMPLIFY=FALSE, SAFE=FALSE)

#' Generalized sweep() for data cleaning.
#'
#' @param STATS  Numeric array or function.
#' @param MARGIN  NULL - STATS is an array; character - STATS is a function,
#'	and by X being reduced along MARGIN, X is wiped.  Length 0 character
#'	vector means reducing along no margin, resulting in a scalar (in
#'	this case, for example, `areduce(sum, X)` is the same as `sum(X)`.
awipe <- function(X, FUN="-", STATS="mean", MARGIN=NULL,
		MoreArgs=NULL, ..., SIMPLIFY=TRUE, VECTORIZED=NA) {
	if (!is.null(MARGIN)) STATS <- areduce(STATS, X, MARGIN, ...)
	amap(FUN, X, STATS, MoreArgs=MoreArgs, SIMPLIFY=SIMPLIFY,
		VECTORIZED=VECTORIZED)
}
#' @examples
#' a <- garray(1:24, c(4,6), list(X=LETTERS[1:4], Y=letters[1:6]),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' m1 <- awipe(a, MARGIN="XX")
#' m2 <- awipe(a, `-`, mean, "XX")

#' Generalized array multiplication. 
#'
#' Default to Einstein summation convention, without explicitly subscripts.
#'
#' Margins shared by X and Y are parallelly mapped by FUN,
#'	and then reduced by SUM (inner product like `%*%`); 
#'	margins in BY and shared by X and Y are simply mapped by FUN
#'	but excluded from reducing (parallel product like `*`);
#'	other margins are extended repeatly (outer product like `%o%`).
#'	Shared margins not to be mapped have to be renamed (like outer product).
#' @param BY  margins excluded from summary by SUM.
#' @keywords garray, `%*%`
amult <- function(X, Y, FUN="*", SUM="sum", BY=NULL,
		MoreArgs=NULL, ..., SIMPLIFY=TRUE, VECTORIZED=TRUE) {
	nX <- margins(X)
	nY <- margins(Y)
	ni <- intersect(nX, nY)
	nx <- setdiff(nX, ni)
	ny <- setdiff(nY, ni)
	no <- union(nx, ny)
	dX <- dim(X)
	dY <- dim(Y)
	dnX <- dimnames(X)
	dnY <- dimnames(Y)
	sd <- c(sdim(X), sdim(Y))
	if (identical("*",FUN)&&identical("sum",SUM)&&is.null(BY)
			&&all(dX[ni]==dY[ni])) {
		if (length(nX)>2||length(nY)>2) {
			X <- aperm(X, perm=c(nx, ni))
			Y <- aperm(Y, perm=c(ni, ny))
			if (any(dX[ni]!=dY[ni])) {
				d <- pmax(dX[ni], dY[ni])
				X <- .rep.garray(X, c(dX[nx], d), sd)
				Y <- .rep.garray(Y, c(d, dY[ny]), sd)
			} else {
				d <- dX[ni]
			}
			attr(X, "dim") <- c(prod(dX[nx]), prod(d[ni]))
			attr(Y, "dim") <- c(prod(d[ni]), prod(dY[ny]))
			Z <- X%*%Y
		} else {
			FUN <- array(list(crossprod, `%*%`,
				function(X,Y) t(Y%*%X), tcrossprod),
				c(2,2))[nX==ni,nY==ni][[1]]
			Z <- FUN(X, Y)
		}
		dZ <- c(dX[nx],dY[ny])
		names(dZ) <- NULL
		dim(Z) <- dZ
		dimnames(Z) <- c(dnX[nx], dnY[ny])
		class(Z) <- "garray"
		sdim(Z, warn=FALSE) <- sd
		return(Z)
		# By definition, margins is important but not order.
		#	Sometimes seems saving commutative property of %*%.
	}	# Speed-up for special cases
	areduce(match.fun(SUM), amap(match.fun(FUN), X, Y, MoreArgs=MoreArgs,
		SIMPLIFY=SIMPLIFY, VECTORIZED=VECTORIZED), union(no, BY), ...)
}
`%X%` <- amult	# %*% is S4 generic but not S3 generic
#' @examples
#' a <- garray(1:24, c(4,6), list(X=LETTERS[1:4], Y=letters[1:6]),
#' 	sdim=list(XX=c(x1=3,x2=1), YY=c(y1=1,y2=2)))
#' b <- garray(1:20, c(Z=5, X=4))
#' c <- garray(1:120, c(X=4,Y=6,Z=5))
#' m1 <- amult(a, b)
#' m2 <- amult(a, b, `*`, sum)
#' m3 <- amult(b, a)
#' all.equal(m1, m2)
#' all.equal(m1, m3)
#' all.equal(m1, t(m3))
#' n1 <- amult(a, c, `*`, sum)
#' n2 <- a%X%c
#' all.equal(n1, n2)

`%+%` <- function(a, b) {
	force(a)
	force(b)
	function(...) a(b(...))
}
#' `+.function` not work since Ops dispatches on oldClass (attr("class"))
#'	and not on class ('function' is implicit class).
#' @example
#' lse <- log%+%sum%+%exp
#' lse(1:10)
#' logsumexp(1:10)	# actual logsumexp() is more sophistic
#' log(sum(exp(1:10)))
#' sum <- sd
#' lse(1:10)	# lse() is fixed at definition
#' log(sum(exp(1:10)))
#' (log%+%sum%+%exp)(1:10)	# now is (log%+%sd%+%exp)

#' Generalized array cross product 
#'
#' Generalizaion of `%*%`, dist() and cor(), allowing cross along dimensions
#'	of the same margins one by one.
#' In general, for 2 matrices to 1 mapping,
#'	`%*%` runs on row*column style ([2x4]%*%[4x3]=[2x3]),
#'	var/cov/cor/crossprod on column*column style (cor([4x2], [4x3])=[2x3]),
#'	dist2()/tcrossprod on row*row style (dist2([2x4], [3x4])=[2x3]).
#across <- function(X, Y, by, byX=by, byY=by, FUN) { }
