#' HSV adjust
#'
#' @description
#' This function is used to quickly adjust the HSV values of colors.
#'  HSV stands for hue, saturation, and value.
#'
#'
#' @param v.col A vector of colors given in hex code.
#' @param hue Hue adjustment, given as -100 to 100.
#' @param saturation Saturation adjustment, given as -100 to 100.
#' @param value Value adjustment, given as -100 to 100.
#'
#' @return Returns an adjusted vector of colors given in hex code.
#' @export
#'
#' @details
#' The adjustments are linear interpolations of the current hsv value and the maximum/minimum
#' values.
#'
#'
#' @examples hsv_adjust(c("red", "blue", "green"), hue = 10)
#' @examples hsv_adjust(c("red", "blue", "green"), saturation = -20, value = -10)

hsv_adjust = function(v.col, hue = NULL, saturation = NULL, value = NULL){

  # Change to RGB, keep the alpha values
  A = col2rgb(v.col, alpha = TRUE)

  # Change to HSV
  A[1:3,] = rgb2hsv(A[1:3,])
  A[4,] = A[4,]/255

  #--------------------------------------------------#
  # Check if hue is valid
  if (!is.null(hue)) {
    if (hue < -100 || hue > 100) {
      stop("Hue adjustment not valid")
    }
  }

  # Adjust hue
  if (!is.null(hue)) {
    if (hue > 0) {
      A[1,] = A[1,] + hue/100 * (1 - A[1,])
    } else {A[1,] = A[1,]*(1 + hue/100)}
  }
  #--------------------------------------------------#
  # Check if saturation is valid
  if (!is.null(saturation)) {
    if (saturation < -100 || saturation > 100) {
      stop("Saturation adjustment not valid")
    }
  }

  # Adjust saturation
  if (!is.null(saturation)) {
    if (saturation > 0) {
      A[2,] = A[2,] + saturation/100 * (1 - A[2,])
    } else {A[2,] = A[2,]*(1 + saturation/100)}
  }
  #--------------------------------------------------#
  # Check if value is valid
  if (!is.null(value)) {
    if (value < -100 || value > 100) {
      stop("Value adjustment not valid")
    }
  }

  # Adjust value
  if (!is.null(value)) {
    if (value > 0) {
      A[3,] = A[3,] + value/100 * (1 - A[3,])
    } else {A[3,] = A[3,]*(1 + value/100)}
  }
  #--------------------------------------------------#


  output = apply(A, 2, function(col){
    hsv( h = col[1], s = col[2], v = col[3], alpha = col[4]) }
  )

  return(output)

}
