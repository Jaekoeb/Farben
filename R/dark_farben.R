#' Dark Colors
#'
#' @description
#' Color palette of five colors for categorical data.
#'
#' @param ... Specify which colors to return, by default returns all.
#'
#' @return Returns a character vector containing colors in hex code.
#' @export
#'
#' @examples dark_farben()
#' @examples dark_farben("bvb", "stpauli")
#'

dark_farben = function(...){

  # Define Colors
  dark = c(
    `bayern` = "#701211",
    `werder` = "#20483b",
    `bvb` = "#ff7e00",
    `stpauli` = "#3B2317",
    `schalke` = "#0b3060"
    )

  cols = c(...)

  if (is.null(cols)) {
    return(dark)
  }

  dark[cols]

}
