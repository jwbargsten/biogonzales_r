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
gg_annotate_boxplot_outliers <- function(g, data,
  label.var="label",
  value.var="value",
  group.var="group",
  ...) {
    gb <- ggplot_build(g)$data[[1]]

  lvls <- unique(levels(data[,group.var])[data[,group.var]])
  data <- data[is.finite(data[,value.var]),]
  outlier_labels <- gonz.merge_recurse(
    lapply(seq_along(lvls), function(i) {
      data.group <- data[data[,group.var]==lvls[i],] 
      if(length(gb$outliers[[i]]) == 0)
        return(NULL)
      
      data.frame( x=i, y=gb$outliers[[i]], label=data.group[data.group[,value.var] %in% gb$outliers[[i]], label.var])
  }))
  #browser()
  if(nrow(outlier_labels) > 0) {
    outlier_labels$pos <- ifelse(seq(1,nrow(outlier_labels)) %% 2 == 0, -0.1, 1.1)
    g <- g + annotate("text", x=outlier_labels$x, y=outlier_labels$y, label=outlier_labels$label, hjust=outlier_labels$pos, ...)
  }
  g
}
