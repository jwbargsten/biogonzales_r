.onAttach <- function(libname, pkgname) {
  av <- gonz.analysis_version()
  gonz.log <- gonz.getlog()
  assign("gonz.log", gonz.log, envir = .GlobalEnv)
  gonz.log("info", paste0("invoked (", gonz.analysis_version(), ")", collapse=""))
}
