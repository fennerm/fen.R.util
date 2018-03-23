#' @export
#' @import ggplot2
theme_shared <- function() {
    theme_bw(18) + theme(panel.grid.major.x = element_blank())
}


#' Box and whiskers plot with precomputed confidence intervals
#' @export
#' @import ggplot2
precomputed_boxplot <- function(
  df,
  xlab = NULL,
  ylab = NULL,
  xval = NULL,
  yscale = "unscaled",
  fill = NULL,
  limits = NULL,
  legend = TRUE) {

  p <- ggplot(df)
  p <- p + theme_shared()

  p <- p + geom_boxplot(aes_string(
      x = colnames(df)[1],
      ymin = "ci1",
      lower = "q25",
      middle = "value",
      upper = "q75",
      ymax = "ci2"
      ), stat = "identity", size = 0.6
    )

  if (!is.null(fill)) {
    p <- p + aes(fill = fill)
  }
  if (!legend) {
    p <- p + scale_fill_discrete(guide = FALSE)
  }

  p <- p + scale_x_discrete(labels = df[, 1])

  if (yscale == "log10") {
    if (!is.null(limits)) {
      p <- p + scale_y_log10(limits = limits)
    } else {
      p <- p + scale_y_log10()
    }
  } else {
    if (!is.null(limits)) {
      p <- p + ylim(limits)
    }
  }
  p <- p + labs(x = xlab, y = ylab)
  p
}
