* Edit the help file skeletons in 'man', possibly combining help files
  for multiple functions.
* Edit the exports in 'NAMESPACE', and add necessary imports.
* Put any C/C++/Fortran code in 'src'.
* If you have compiled code, add a useDynLib() directive to
  'NAMESPACE'.
* Run R CMD build to build the package tarball.
* Run R CMD check to check the package tarball.

Read "Writing R Extensions" for more information.

1. Coding (with documentation embeded in comment) in file main.R.
2. Run `package.skeleton("garray", code_files="main.R")`.
3. Write garray/DESCRIPTION file.
4. Run `devtools::document()` in R at 'garray/'
5. Run `devtools::run_examples()` in R at 'garray/'
6. Run `devtools::build()` in R at 'garray/', produce '../garray_1.0.tar.gz'.
7. Run `devtools::check()` in R at 'garray/'.
8. Run `devtools::install()`.
