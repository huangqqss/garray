# Serial definition, and they will be redefined for parallel in .onAttach()
.LAPPLY <- function(X, FUN, ...) lapply(X, FUN, ...)
# Not simply `.LAPPLY <- lapply` since it introduces .Internal()
.MAPPLY <- function(FUN, dots, MoreArgs=NULL)
	.mapply(FUN, dots, MoreArgs)
.onAttach <- function(libname, pkgname) {
	if (0L==length(getOption("mc.cores"))||2L>getOption("mc.cores")[[1]]) {
		packageStartupMessage("Running without parallel")
	} else {
		packageStartupMessage(
			'Running with parallel, check options("mc.cores")')
		.LAPPLY <- function(X, FUN, ...) parallel::mclapply(X, FUN, ...)
		.MAPPLY <- function(FUN, dots, MoreArgs=NULL) {
			if (!length(dots)) return(list())
			l <- lengths(dots)
			n <- max(l)
			if (n && min(l)==0L) stop(
				"Zero-length inputs mixed with non-zero length")
			if (n<2L) {
				.mapply(FUN, dots, MoreArgs)
			} else {
				if (any(l!=n)) dots <- lapply(dots,
					function(x) rep(x, length.out=n))
				f <- function(idx) .mapply(FUN, lapply(dots,
					function(x) x[idx]), MoreArgs)
				do.call(c, parallel::mclapply(seq_len(n), f))
			}
		}	# Simplified from parallel::mcmapply()
	}
	invisible()
}
