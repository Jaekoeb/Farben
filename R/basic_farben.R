#' Basic Colors
#'
#' @description
#' Color palette of five colors for categorical data.
#'
#' @param ... Specify which colors to return, by default returns all.
#'
#' @return Returns a character vector containing colors in hex code.
#' @export
#'
#' @examples basic_farben()
#' @examples basic_farben("strawberry", "grape")
#'

basic_farben = function(...){

  # Define Colors
  basic = c(
    `lime` = "#38a45e",
    `strawberry` = "#e72731",
    `mango` = "#f19d02",
    `blueberry` = "#0f618a",
    `grape` = "#683F8C"
  )

  cols = c(...)

  if (is.null(cols)) {
    return(basic)
  }

  basic[cols]

}
