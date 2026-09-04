.onAttach <- function(libname, pkgname) {
  v <- utils::packageVersion(pkgname)
  packageStartupMessage("Welcome to rebalancedNoise ", v, ".")
}
