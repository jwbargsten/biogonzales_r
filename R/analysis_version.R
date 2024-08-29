.required_project_files <- c(".git", "docs", "analysis")
.required_av_files <- c("av")

.is.dir <- function(path) {
  file.info(path)[1,"isdir"]
}

.abs2rel <- function(p, base) {
  rel <- if(is.na(base)) p else sub(base, "", p, fixed=TRUE)
  sub("^/", "", rel)
}

.find_root <- function(start, req) {
  current <- normalizePath(start)
  if(!file.exists(current)) {
    return(NA)
  }
  if(!.is.dir(current)) {
    current <- dirname(current)
  }

  parent <- normalizePath(file.path(current, ".."))
  while(parent != current) {
    if(all(file.exists(file.path(current, req)))) {
      return(current)
    }
    current <- parent
    parent <- normalizePath(file.path(parent, ".."))
  }
  return(NA)
}

gonz.project_root <- function() .find_root(".", .required_project_files)
gonz.analysis_root <- function() {
  root <- .find_root(".", .required_av_files)
  if(is.na(root)) "." else root
}

gonz.analysis_version <- function() {
  av <- NULL
  av.env <- Sys.getenv("ANALYSIS_VERSION")
  av.src <- file.path(gonz.analysis_root(), "av")
  if(nchar(av.env) > 0) {
    av <- av.env
  } else if(file.exists(av.src)) {
    av <- as.character(read.table(av.src, as.is=TRUE)[1,1])
  } else {
    warning("could not find the analysis version file, using current dir as output dir")
    av <- "."
  }
  av
}

gonz.analysis_path <- function() {
  av.path <- file.path(gonz.analysis_root(), gonz.analysis_version())
  if(!file.exists(av.path))
    dir.create(av.path)
  av.path
}

.str2av <- function(data) {
  av <- gonz.analysis_version()
  paste0("  ", capture.output(str(data)), collapse="\n")
}

gonz.conf <- function(x=NULL, as.char=FALSE) {
  conf.file <- file.path(gonz.analysis_root(), "gonz.conf.yml")
  conf.av.file <- file.path(gonz.analysis_root(), paste(gonz.analysis_version(), "conf", "yml", collapse=".", sep="."))
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
      project_root <- gonz.project_root()

      x <- ifelse(grepl("^~[^/]*/", x), path.expand(x), x)

      x <- gsub("__av__", gonz.analysis_version(), x, fixed=TRUE)
      if(any(grepl("__path_to\\(([^)]+)\\)__", x) | grepl("__data__", x))) {
        if(is.na(project_root)) {
          stop("could not find project root")
        } else {
          gonz.log("info",paste0("project root >>", project_root, "<<", collapse=" "))
          x <- gsub("__path_to\\(([^)]+)\\)__", file.path(project_root, "\\1"), x)
          x <- gsub("__data__", file.path(project_root, "data"), x, fixed=TRUE)
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

gonz.nfi <- function(...) {
  project_root <- gonz.project_root()
  f <- file.path(gonz.analysis_path(), ...)
  gonz.log <- gonz.getlog()
  gonz.log("info", paste0("(nfi) >", .abs2rel(f, project_root), "<", collapse=" "))
  f
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


gonz.av <- gonz.analysis_version
gonz.av_path <- gonz.analysis_path
gonz.av_root <- gonz.analysis_root
gonz.c <- gonz.conf
gonz.config <- gonz.conf
