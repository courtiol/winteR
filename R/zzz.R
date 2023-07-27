.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n winteR version ", utils::packageDescription("winteR")$Version," is now loaded",
    "\n Run `?winteR` to get started",
    "\n"
  )
}


#' Check optional dependencies
#'
#' This function checks that the dependencies not required to install the package but needed to run
#' the workflow are all met.
#'
#' @return noting
#' @export
#'
#' @examples
#' checkDependencies()
#'
checkDependencies <- function() {
  pkg_needed <- c("DHARMa", "spaMM", "cowplot", "cubelyr", "jagsUI", "pbmcapply", "showtext")
  for (pkg in pkg_needed) {
    if (!requireNamespace(pkg)) {
      stop(paste0("for full functionality, please install the package ", pkg, " using: install.packages(", pkg, ")\n then rerun checkDependencies() to make sure you have all other packages needed"))
      }
    }
  if (!requireNamespace("torpor")) {
    if (!requireNamespace("remotes")) {
      stop("please install the package torpor using: install.packages('remotes')\n then rerun checkDependencies()")
    }
      stop("please install the package torpor using: remotes::install_github('vullioud/torpor', build_vignettes = TRUE)\n then rerun checkDependencies() to make sure you have all other packages needed")
  }
  message("All packages seem to be installed! You can proceed.")
}
