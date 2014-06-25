gonz.stamp <- function(txt, pwd = FALSE, time. = TRUE, grid=TRUE) {
  date.txt <- if (time.) 
      format(Sys.time())
  else format(Sys.time(), "%Y-%m-%d")

  if (pwd) 
      date.txt <- paste(getwd(), date.txt)

  if (!missing(txt)) 
      date.txt <- paste(txt, "   ", date.txt, sep = "")

  grid.text(date.txt,gp=gpar(col="grey", cex=0.7), x=unit(0.99,"npc"),y=unit(0.02,"npc"), just=c("right","bottom"))
}

#http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
                                       fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
                         capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
                      as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
      out <- head(out, n)
  out
}

gonz.lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

gonz.getopt <- function(..., opt=commandArgs(TRUE)) {
  opt_end <- which(opt == "--")[1]
  parsed_opt <- list()
  if(is.na(opt_end) && grepl("^--?\\w+", opt[1])[1]) {
    ## this script was called to use getopt functionality (first arg starts with "-")
    args <- c()
    parsed_opt <- getopt(..., opt=opt)
  } else if(is.na(opt_end) && !grepl("^--?\\w+", opt[1])[1]) {
    ## this script was called to use plain args
    args <- opt
  } else {
    ## this script was called to use plain args AND getopt functionality
    args <- tail(opt, -opt_end)
    opt <- head(opt, opt_end-1)
    parsed_opt <- getopt(..., opt=opt)
  }
  parsed_opt$args <-args
  parsed_opt
}


# STOLEN FROM THE RESHAPE PACKAGE, CREDITS GO TO HADLEY WICKHAM
# Merge all
# Merge together a series of data.frames
#
# Order of data frames should be from most complete to least complete 
#
# @arguments list of data frames to merge
# @seealso \code{\link{merge_recurse}}
# @keyword manip
gonz.merge_all <- function(dfs, ...) {
  if (length(dfs)==1) return(dfs[[1]])
  df <- gonz.merge_recurse(dfs, ...)
  df <- df[, match(names(dfs[[1]]), names(df))]
  df[do.call("order", df[, -ncol(df), drop=FALSE]), ,drop=FALSE]
}

# STOLEN FROM THE RESHAPE PACKAGE, CREDITS GO TO HADLEY WICKHAM
# Merge recursively
# Recursively merge data frames
#
# @arguments list of data frames to merge
# @seealso \code{\link{merge_all}}
# @keyword internal
gonz.merge_recurse <- function(dfs, ...) {
  if (length(dfs) == 2) {
    merge(dfs[[1]], dfs[[2]], all=TRUE, sort=FALSE, ...)
  } else {
    merge(dfs[[1]], Recall(dfs[-1]), all=TRUE, sort=FALSE, ...)
  }
}
