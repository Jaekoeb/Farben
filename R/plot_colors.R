
#' Plot Colors
#'
#' @description
#' The function offers a quick visualization of colors. Can be used in combination
#' with color adjustment functions to find a set of colors that suits you best.
#'
#'
#' @param v.col Colors that should be plotted. Given as a vector of hex codes.
#'
#' @return Plot containing all given colors, including their hex code.
#'
#' @export
#'
#' @examples plot_colors(c("red", "blue", "yellow"))
#'

plot_colors = function(v.col){

  # Initiate an empty plot
  plot(rep(0,length(v.col)+1), type="n", axes=FALSE, xlab="", ylab="")

  # Initiate counter
  i = 0


  # Iteratively plot the rectangles (+ labels)
  for (col in v.col) {

    # Add a filled rectangle
    rect(xleft = 1+i, ybottom = -10, xright = 2+i, ytop = 10, col = col, border = NA)


    # Add color labels

    # RGB values of color
    rgb = col2rgb(col)

    # Compute the brightness of the color
    brightness = 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]

    # Change text color to white if brightness is below threshold
    if (brightness < 60) {text.col = "white"} else {text.col = "black"}

    text(x = 1.5+i, y = 0, paste0(col), cex = 1.5, srt = 90, font = 3, col = text.col)

    # Increase counter
    i = i+1
  }

}
