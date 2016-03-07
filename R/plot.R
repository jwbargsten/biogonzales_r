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

theme_blank_jwb <- function() {
    theme_bw() %+replace% theme(
              #axis.line=element_blank(),
              #axis.line=element_segment(colour="grey50", size=0.3),
              #axis.line=element_blank(),
              axis.text.x=element_text(colour="black"),
              axis.text.y=element_text(colour="black"),
              #axis.ticks=element_blank(),
              #axis.title.x=element_blank(),
              #axis.title.y=element_blank(),
              legend.position="none",
              panel.background=element_blank(),
              panel.border=element_blank(),
              #panel.grid.major=element_blank(),
              panel.grid.major.x=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(),
              #plot.title=element_blank(),
              strip.background=element_blank(),
              strip.text.x=element_text(face="bold")
              #strip.text.y=element_blank()
              #axis.ticks.margin = unit(0.2, "lines")
              )
}
z <- gonz.merge_all(list(d.hsa, d.sce, d.ath))

graphics.off()
pdf(gonz.nfi("nw_degree_hist.pdf"), width=7, height=2)
  gp <- ggplot(z, aes(x=g.degree)) +
      geom_histogram(binwidth=0.2) +
      scale_x_log10() +
      xlab("degree") +
      ylab("no. of nodes") +
      scale_y_continuous(trans=log10s1_trans()) +
      theme_bw() + theme_blank_jwb() + facet_wrap(~ species) #+ 
  #gp <- ggplot(k, aes(x=g.degree)) + geom_histogram(binwidth=70) +   xlab(tag)
  gp
dev.off()

