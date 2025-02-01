
#' Good & Bad Colors
#'
#' @include constr.R
#'
#' @description A positive / negative color palette of 6 colors. Useful for continuous data.
#' Use in combination with `colorRampPalette` for custom number of colors.
#' @export goodbad
#'
#' @examples
#' # The colors themselves
#' goodbad
#'
#' # Using a custom number of colors
#' colorRampPalette(goodbad)(4)
#'
goodbad <- farben(c(
  "#aa1609",
  "#da1d0b",
  "#f2800d",
  "#f2cc0d",
  "#b8da0b",
  "#6ed00b",
  "#369108"
))
