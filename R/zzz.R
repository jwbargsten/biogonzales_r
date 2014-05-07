.onAttach <- function(libname, pkgname) {
  if(file.exists("av")) {
    gonz.log <- gonz.getlog()
    assign("gonz.log", gonz.log, envir = .GlobalEnv)
    gonz.log("info", "invoked")
  }
}
