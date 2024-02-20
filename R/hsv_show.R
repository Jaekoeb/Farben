#' HSV Show
#'
#' @description
#' This function shows the HSV values for given colors.
#' HSV stands for hue, saturation, and value. This function is best used in combination
#' with the hsv_adjust function to tune a given color palette.
#'
#'
#' @param v.col A vector of colors given in hex code.
#' @param dec Number of decimals to be shown. Defaults to three.
#'
#' @return Returns a matrix with three rows and a column for each given color.
#' @export
#'
#' @examples hsv_show(c("red", "blue", "yellow"))
#'

hsv_show = function(v.col, dec = 3){

  # Change to RGB
  A = col2rgb(v.col)

  # Change to HSV
  A = rgb2hsv(A)

  # Change the column names
  colnames(A) = v.col

  # Round values
  A = round(A,dec)

  return(A)
}
