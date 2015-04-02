tictac <- function(x, ...) UseMethod("tictac")
checkpoint <- function(x, ...) UseMethod("checkpoint")

tictac.default <- function(...) {
  t <- list(times=c(), names=c())
  class(t) <- "tictac"
  t
}

##TODO
## print
## plot

start.tictac <- function(t) {
  t$times <- c(Sys.time())
  t$names <- c("start")
  t
}

checkpoint.tictac <- function(t,n) {
  t$times <- c(t$times, Sys.time())
  t$names <- c(t$names, n)
  cat( sprintf("%30s -> %-30s: %12.2f\n", tail(t$names, n=2)[1], n, diff(tail(t$times, n=2))),file=stderr())
  t
}
