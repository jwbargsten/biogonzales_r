.onAttach <- function(libname, pkgname) {
  if(nchar(Sys.getenv("ANALYSIS_VERSION")) > 0 || file.exists("av")) {
    gonz.log <- gonz.getlog()
    assign("gonz.log", gonz.log, envir = .GlobalEnv)
    gonz.log("info", paste0("invoked (", gonz.analysis_version(), ")", collapse=""))
  }
}
