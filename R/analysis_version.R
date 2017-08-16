gonz.conf <- function(x=NULL, as.char=FALSE) {
  conf.file <- "gonz.conf.yml"
  conf.av.file <- paste(gonz.analysis_version(), "conf", "yml", collapse=".", sep=".")
  gonz.log <- gonz.getlog()
  conf <- list()

  if(file.exists(conf.file)) {
    conf <- gonz.read_conf(files=conf.file)
    gonz.log("info",paste0("reading >>", conf.file, "<<", collapse=" "))
  }

  if(file.exists(conf.av.file)) {
    conf.av <- gonz.read_conf(files=conf.av.file)
    gonz.log("info",paste0("reading >>", conf.av.file, "<<", collapse=" "))
    conf <- .merge.list.excl(conf,conf.av)
  }

  conf <- rapply(conf,
    function(x) {
      project_dir <- .find_root(getwd())

      x <- ifelse(grepl("^~[^/]*/", x), path.expand(x), x)

      x <- gsub("__av__", gonz.analysis_version(), x, fixed=TRUE)
      if(any(grepl("__path_to\\(([^)]+)\\)__", x) | grepl("__data__", x))) {
        if(is.null(project_dir)) {
          stop("could not find project dir")
        } else {
          gonz.log("info",paste0("project dir >>", project_dir, "<<", collapse=" "))
          x <- gsub("__path_to\\(([^)]+)\\)__", file.path(project_dir, "\\1"), x)
          x <- gsub("__data__", file.path(project_dir, "data"), x, fixed=TRUE)
        }
      }

      x
    },
    how="replace",
    classes="character"
  )

  if(is.null(x)) {
    gonz.log("info", "(conf) dump\n", .str2av(conf))
    return(conf)
  } else if(as.char) {
    dtmp <- as.character(conf[[x]])
    gonz.log("info", paste0("(conf) >", x, "<", collapse=" "), dtmp)
    return(dtmp)
  } else {
    gonz.log("info", paste0("(conf) >", x, "<", collapse=" "), .str2av(conf[[x]]))
    return(conf[[x]])
  }
}

gonz.analysis_version <- function() {
  av <- NULL
  av.env <- Sys.getenv("ANALYSIS_VERSION")
  if(nchar(av.env) > 0) {
    av <- av.env
  } else if(file.exists("av")) {
    av <- as.character(read.table("av", as.is=TRUE)[1,1])
  } else {
    warning("could not find the analysis version file")
    warning("using current dir as output dir")
    av <- "."
  }
  if(!file.exists(av))
    dir.create(av)
  av
}

gonz.nfi <- function(...) {
  f <- file.path(gonz.analysis_version(), ...)
  gonz.log <- gonz.getlog()
  gonz.log("info", paste0("(nfi) >", f, "<", collapse=" "))
  f
}

.file.path.collapse <- function(x) {
  paste0(x, collapse=.Platform$file.sep)
}

gonz.read_conf <- function(files) {
  candidates <- unlist(lapply(files, file.exists))
  if(is.null(candidates) || all(!candidates))
    stop("configuration file NOT found")
  filename <- files[which(candidates)[1]]

  if(is.na(filename))
    stop("no existing file supplied")

  if(grepl("\\.gz$", filename)) {
    if(grepl("\\.ya?ml\\.gz$", filename)) {
      raw <- paste(readLines(con <- gzcon(file(filename, "rb"))), collapse="\n")
      close(con)
      data <- yaml.load(raw)
    } else if(grepl("\\.json\\.gz$", filename)) {
      raw <- paste(readLines(con <- gzcon(file(filename, "rb"))), collapse="\n")
      close(con)
      data <- fromJSON(raw)
    } else {
        stop("could not find input/output file specs")
    }
  } else {
    if(grepl("\\.ya?ml$", filename)) {
      raw <- paste(readLines(con <- file(filename, "rb")), collapse="\n")
      close(con)
      data <- yaml.load(raw)
    } else if(grepl("\\.json$", filename)) {
      raw <- paste(readLines(con <- file(filename, "rb")), collapse="\n")
      close(con)
      data <- fromJSON(raw)
    } else {
        stop("could not find input/output file specs")
    }

  }
  rm(raw)
  if(is.null(data))
    data <- list()
  return(data)
}

## adds y to x. Overwrites names in x that are in y
.merge.list.excl <- function(x, y) {
  if (is.null(y)) {
    return(as.list(x))
  } else {
    y <- as.list(y)

    names.uniq.x <- ! names(x) %in% names(y)

    c(y, x[names.uniq.x])
  }
}

.find_root <- function(dir) {
  dir <- strsplit(dir, split="/")[[1]]
  root <- NULL

  while(length(dir) > 1) {
    if(file.exists(.file.path.collapse(c(dir, "analysis"))) && file.exists(.file.path.collapse(c(dir, "Makefile"))) && file.exists(.file.path.collapse(c(dir, ".git")))) {
      root <- dir
      break
    }
    dir <- head(dir,-1)
  }
  .file.path.collapse(root)
}

gonz.av <- gonz.analysis_version
gonz.c <- gonz.conf
gonz.config <- gonz.conf
