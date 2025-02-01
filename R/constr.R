#' Constructor for `farben`
#'
#' The package `Farben` defines color (palettes) using S3 classes. This function creates a new instance of a class.
#'
#' @param colors A (named) vector of hex colors.
#'
#' @return An object of class `farben`
#' @export
#'
#' @examples
#' farben(c("#683F8C", "#0f618a"))
#'
farben <- function(colors){

  # Check if input is a character vector
  if (!is.character(colors)) {
    stop("Input must be a character vector.")
  }


  # Check if input contains NAs
  if (any(is.na(colors))) {
    stop("Input must not contains NAs.")
  }

  # Helper function to check if color is valid
  is_valid_color <- function(colors) {
    sapply(colors, function(col) {
      tryCatch({
        col2rgb(col)
        TRUE
      }, error = function(e) FALSE)
    })
  }

  # Check if input colors are valid
  if (!all(is_valid_color(colors))) {
    stop("Some values are not valid colors.")
  }

  structure(colors, class = c("farben", "character"))

}
