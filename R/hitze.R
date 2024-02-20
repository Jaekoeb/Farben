
#' Hitze Color Palette
#'
#' @description
#' This palette can be used for ordinal data.
#'
#' @param n Specifies the number of colors to return
#'
#' @return Returns a character vector of hex codes of length n.
#' @export
#'
#' @examples hitze(10)

hitze = function(n){
  colorRampPalette(
    c("#260a00", "#661100", "#991200", "#bd4408", "#e69500", "#ffbf00")
  )(n)
}
