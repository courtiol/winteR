.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n winteR version ", utils::packageDescription("winteR")$Version," is now loaded",
    "\n Run `?winteR` to get started",
    "\n"
  )
}
