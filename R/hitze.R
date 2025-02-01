
#' Hitze Colors
#'
#' @include constr.R
#'
#' @description A heat color palette of 6 colors. Useful for continuous data.
#' Use in combination with `colorRampPalette` for custom number of colors.
#' @export hitze
#'
#' @examples
#' # The colors themselves
#' hitze
#'
#' # Using a custom number of colors
#' colorRampPalette(hitze)(5)
#'
hitze <- farben(c(
  "#260a00",
  "#661100",
  "#991200",
  "#bd4408",
  "#e69500",
  "#ffbf00"
))

