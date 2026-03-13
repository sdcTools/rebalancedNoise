.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  packageStartupMessage(
    paste0("Welcome to rebalancedNoise v", version, " - Perturbation for Magnitude Tables")
  )
}
