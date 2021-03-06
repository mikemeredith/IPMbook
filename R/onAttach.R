

.onAttach <- function(libname, pkgname) {
  version <- try(packageVersion('IPMbook'), silent=TRUE)
  if(!inherits(version, "try-error"))
    packageStartupMessage("This is IPMbook ", version,
      ". For overview type ?IPMbook; for changes do news(p='IPMbook').")
}
