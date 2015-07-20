log_trans_shift <- function(base = exp(1), shift=0) {
    trans <- function(x) {
          y <- log(x + shift, base)
    y[is.nan(y) | (is.infinite(y) & y < 0)] <- 0
        y
      }
  inv <- function(x) {
        y <- base ^ x - shift
      y[is.nan(y) | (is.infinite(y) & y < 0)] <- 0
          y
        }

    lbr <- function(x) {
          y <- log_breaks(base = base)(x+shift)
        y[is.nan(y) | (is.infinite(y) & y < 0)] <- 0
            y
          }
      trans_new(paste0("log-", format(base), "-", format(shift)), trans, inv,
            lbr, domain = c(1+1e-100, Inf))
}
log10s1_trans <- function() {
    log_trans_shift(10, 0)
}
