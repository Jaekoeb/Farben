#' Good Bad Color Palette
#'
#' @description
#' This color palette can be used for ordinal data. It is of best use when the data has a notion of
#' better or worse.
#'
#'
#' @param n Specifies the number of colors to return
#'
#' @return Returns a character vector of hex codes of length n.
#' @export
#'
#' @examples goodbad(10)

goodbad = function(n){
  colorRampPalette(
    c("#aa1609", "#da1d0b", "#f2800d", "#f2cc0d", "#b8da0b", "#6ed00b", "#369108")
  )(n)
}
