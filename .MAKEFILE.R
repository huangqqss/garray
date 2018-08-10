#* Edit the help file skeletons in 'man', possibly combining help files
#  for multiple functions.
#* Edit the exports in 'NAMESPACE', and add necessary imports.
#* Put any C/C++/Fortran code in 'src'.
#* If you have compiled code, add a useDynLib() directive to
#  'NAMESPACE'.
#* Run R CMD build to build the package tarball.
#* Run R CMD check to check the package tarball.
#
#Read "Writing R Extensions" for more information.

setwd("~/Desktop/garray")
# Coding (with roxygen2)
#package.skeleton("garray", code_files="main.R")
# Write garray/DESCRIPTION file.
library(devtools)
document()
run_examples()
check()
build()` # produce '../garray_1.0.tar.gz'.
install()
