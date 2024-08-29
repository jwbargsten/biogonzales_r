.log.fmt <- function(level, namespace, ...) {
  data <- unlist(lapply(list(...), function(x) { paste0(x, collapse="\n") }))

  paste0(
      "[", format(Sys.time()), "] ",
      "[", toupper(level), "] ",
      ifelse(is.null(namespace), "", paste0(namespace, ": ", collapse="")),
      paste0(data, collapse="\n")
  )
}

.log.print <- function(file, level, namespace, ...) {
  cat(.log.fmt(level, namespace, ...), "\n", file=file, append=TRUE, sep="")
}

gonz.createlog <- function(file=stderr(), level=c("debug", "info", "warn", "error", "fatal"), namespace=NULL, tee_stderr=FALSE) {
  level.report <- match.arg(level)
  LEVELS <- c("debug", "info", "warn", "error", "fatal")
  level.report.idx <- which(LEVELS == level.report)

  function(level=c("debug", "info", "warn", "error", "fatal"), ...) {
    level <- match.arg(level)
    level.idx <- which(LEVELS == level)

    if( level.idx >= level.report.idx) {
      .log.print(file=file, level=level, namespace=namespace, ...)
      if(namespace == "ipynb") {
        message(.log.fmt(level, namespace, ...), appendLF=FALSE)
      } else if(namespace == "interactive" && file != stderr()) {
        .log.print(file=stderr(), level=level, namespace=namespace, ...)
      }
    }
  }
}

gonz.getlog <- function() {
  ns <- grep("^--file=",commandArgs(), value=TRUE)
  if(length(ns) > 0) {
    ns <- sub("^--file=", "", ns)
    ns <- basename(ns)
  } else if(!is.null(getOption("jupyter.base_display_func"))) {
    ns <- "ipynb"
  } else {
    ns <- "interactive"
  }
  gonz.createlog(file=file.path(gonz.analysis_path(), "gonz.log"), level="info", namespace=ns)
}

