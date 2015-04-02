.log.print <- function(file, level, namespace, ...) {
  data <- unlist(lapply(list(...), function(x) { paste0(x, collapse="\n") }))
  cat(
      "[", format(Sys.time()), "] ",
      "[", toupper(level), "] ",
      ifelse(is.null(namespace), "", paste0(namespace, ": ", collapse="")),
      paste0(data, collapse="\n"), "\n",
      file=file,
      append=TRUE,
      sep=""
  )
}

gonz.createlog <- function(file=stderr(), level=c("debug", "info", "warn", "error", "fatal"), namespace=NULL, tee_stderr=FALSE) {
  level.report <- match.arg(level)
  LEVELS <- c("debug", "info", "warn", "error", "fatal")
  level.report.idx <- which(LEVELS == level.report)
  if(tee_stderr && file == stderr())
    tee_stderr <- FALSE

  function(level=c("debug", "info", "warn", "error", "fatal"), ...) {
    level <- match.arg(level)
    level.idx <- which(LEVELS == level)

    if( level.idx >= level.report.idx) {
      .log.print(file=file, level=level, namespace=namespace, ...)
      if(tee_stderr)
        .log.print(file=stderr(), level=level, namespace=namespace, ...)
      
    }
  }
}

gonz.getlog <- function() {
  ns <- grep("^--file=",commandArgs(), value=TRUE)
  if(length(ns) > 0) {
    ns <- sub("^--file=", "", ns)
    ns <- basename(ns)
  } else {
    ns <- "interactive"
  }
  av <- gonz.analysis_version()
  gonz.createlog(file=file.path(av, "gonz.log"), level="info", namespace=ns, tee_stderr=ns!="interactive")
}

.str2av <- function(data) {
  av <- gonz.analysis_version()
  paste0("  ", capture.output(str(data)), collapse="\n")
}
