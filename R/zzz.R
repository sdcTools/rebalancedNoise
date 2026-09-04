.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion(pkgname)
  packageStartupMessage(glue::glue("Welcome to rebalancedNoise {v}."))
}
