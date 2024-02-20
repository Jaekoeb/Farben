#' Warmth Adjust
#'
#' @description
#' This function adjusts the "warmth" of a given set of colors. This is done by shifting the
#'  colors to red for warm and to blue for cold.
#'
#' @param v.col A vector of colors given in hex code.
#' @param adjust The adjustment, given from -100 to 100. Positive = warmer.
#'
#' @return Returns the adjusted colors
#' @export
#'
#' @examples warmth_adjust(c("green", "purple"), adjust = 30)

warmth_adjust = function(v.col, adjust){

  # Check if adjustment is valid
  if (adjust > 100 || adjust < -100) {stop("Adjustment not valid")}

  # Change colors to RGB
  A = col2rgb(v.col, alpha = TRUE)

  # Apply the adjustment
  B = apply(A, 2, function(col){
    if (adjust > 0) {
      col + c(adjust/100 * (255 - col[1]), 0, max(-adjust/100 * (255 - col[1]), -col[3]), 0)
    } else {
      col + c(adjust/100 * col[1], 0, min(-adjust/100 * col[1], 255 - col[3]), 0)
    }
  }
  )

  output = apply(B, 2, function(col){
    rgb( r = col[1], g = col[2], b = col[3], alpha = col[4], maxColorValue = 255) }
  )

  return(output)
}
