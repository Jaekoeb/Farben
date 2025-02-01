#' Plot Colors
#'
#' @method plot farben
#'
#' @description
#' This `farben` method for `plot` offers a quick visualization of colors.
#'
#' @param x Colors that should be plotted. Given as a `farben` object.
#' @param ... Additional arguments passed to plot.
#'
#' @return Plot containing all given colors, including their hex code.
#'
#' @export
plot.farben <- function(x, ...) {

  # Initiate an empty plot; note that we ignore extra arguments for simplicity
  plot(rep(0, length(x)+1), type = "n", axes = FALSE, xlab = "", ylab = "", ...)

  # Initiate counter
  i <- 0

  # Iteratively plot the rectangles (+ labels)
  for (col in x) {

    # Add a filled rectangle
    rect(xleft = 1 + i, ybottom = -10, xright = 2 + i, ytop = 10, col = col, border = NA)

    # Get RGB values and compute brightness
    rgb_vals <- col2rgb(col)
    brightness <- 0.299 * rgb_vals[1] + 0.587 * rgb_vals[2] + 0.114 * rgb_vals[3]

    # Set text color based on brightness
    text.col <- if (brightness < 60) "white" else "black"

    # Add the color label
    text(x = 1.5 + i, y = 0, labels = col, cex = 1.5, srt = 90, font = 3, col = text.col)

    # Increase counter
    i <- i + 1
  }
}
