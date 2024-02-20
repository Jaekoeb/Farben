
#' Plot Demo
#'
#' @description
#' This function can be used to quickly asses if a color palette is useful. It returns
#'  three different plots from the 'ChickWeight' data set.
#'
#'
#' @param v.col A vector of colors given in hex code.
#' @param include A character vector of which plots to include.
#'
#' @return Return 1-3 plots.
#' @export
#'
#' @examples plot_demo(v.col = c("red", "blue", "yellow"))

plot_demo = function(v.col, include = "all"){

  # Check if too many colors given:
  if (length(v.col) > 50) {stop("Too many colors given")}

  if (any(include %in% c("all", "line"))) {

    # Line Graph --------------------------------------------------------#

    # Find the max/min y value of the graph to scale axis
    ymax = with(ChickWeight, max(weight[Chick %in% seq_along(v.col)])) + 5
    ymin = with(ChickWeight, min(weight[Chick %in% seq_along(v.col)])) - 5

    # Initiate graph
    with(ChickWeight, plot(x = Time, y = weight, type = "n", xlab = "Time", ylab = "Weight", ylim = c(ymin, ymax)))


    # Add lines for all colors
    for (k in seq_along(v.col)) {
      with(ChickWeight, lines(x = Time[Chick == k], y = weight[Chick == k], col = v.col[k], lwd=3))
    }

    # Adding legend
    legend("topleft", legend = seq_along(v.col), fill = v.col, title = "Chick")

    #--------------------------------------------------------------------#
  }


  if (any(include %in% c("all", "scatter"))) {

    # Scatterplot -------------------------------------------------------#

    # Match categories to custom colors
    h = with(ChickWeight, match(Chick[Chick %in% seq_along(v.col)], unique(Chick)))

    cat.col = v.col[h]


    # We add some noise to the y-axis to create a fake variable "profit"
    with(ChickWeight, plot(x = weight[Chick %in% seq_along(v.col)],
                           y = weight[Chick %in% seq_along(v.col)] + rnorm(length(weight[Chick %in% seq_along(v.col)]), sd = 50),
                           col = cat.col,
                           xlab = "Weight",
                           ylab = "Profit",
                           pch = 16)
    )

    # Adding legend
    legend("topleft", legend = seq_along(v.col), fill = v.col, title = "Chick")

    #---------------------------------------------------------------------#
  }


  if (any(include %in% c("all", "bar"))) {

    # Stacked Bar Chart ------------------------------------------------#

    # Create data frame
    data = cbind(
      with(ChickWeight, weight[Time == 0 & Chick %in% seq_along(v.col)]),
      with(ChickWeight, weight[Time == 2 & Chick %in% seq_along(v.col)])
    )

    colnames(data) = c("0", "2")
    rownames(data) = paste0(seq_along(v.col))

    # Create stacked bar chart
    barplot(data, beside = TRUE, col = v.col, xlab = "Time", ylab = "Weight", ylim = c(0, 70))

    # Adding legend
    legend("topleft", legend = seq_along(v.col), fill = v.col, title = "Chick")

    #-------------------------------------------------------------------------#
  }

}
