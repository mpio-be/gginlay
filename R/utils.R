
#' @export
emptyGG <- function(x) UseMethod("emptyGG")

#' @export
emptyGG.default <- function(x, ...) {
  warning(paste(
    "emptyGG does not know how to handle object of class ",
    class(x)[length(class(x))]
  ))
}


#' @export
emptyGG.nativeRaster <- function(r, keep.ratio = TRUE) { 

  x = data.frame(
    y = seq(0, nrow(r), length.out = 10),
    x = seq(0, ncol(r), length.out = 10)
  )

  g = ggplot(x) +
    geom_line(aes(y = y, x = x), color = "red", linewidth = 0.2) +
    geom_line(aes(y = rev(y), x = x), color = "red", linewidth = 0.2) +
    xlab(NULL) +
    ylab(NULL) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_linedraw() +
  
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0), 
      panel.background = element_rect(fill='transparent'), 
      plot.background = element_rect(fill='transparent', color = NA), 
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )

  if (keep.ratio) {
    g = g + coord_fixed()
  }
  
  g

}