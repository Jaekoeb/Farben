#' Plot Demo
#'
#' @description
#' This function can be used to quickly assess if a color palette is useful. It returns
#' three different plots from the 'ChickWeight' data set.
#'
#' @param color A farben object.
#' @param include A character vector of which plots to include. Options are `"line"`, `"scatter"`, `"bar"`,
#' or `"all"`.
#' @param bg The background color for the plot(s). Defaults to `"white"`.
#'
#' @return Produces 1–3 plots.
#' @export
#' @importFrom graphics barplot legend lines par points rect text
#' @importFrom stats rnorm
#'
#' @examples
#' col <- farben(c("red", "blue", "yellow"))
#' farben_demo(color = col)
#'
farben_demo <- function(color, include = "all", bg = "white") {
  # Check input class and number of colors
  if (!inherits(color, "farben")) {
    stop("Input must be of class 'farben'")
  }
  if (length(color) > 50) {
    stop("Too many colors given")
  }


  if (!(include %in% c("line", "scatter", "bar", "all"))) {
    stop("Invalid input for `include`")
  }

  # Helper: draw legend in the top left corner of the current plot region
  draw_legend <- function() {
    legend("topleft",
           legend = seq_along(color),
           fill = color,
           title = "Chick",
           horiz = TRUE,
           bty = "n")
  }

  ## 1. Line Graph Branch ##
  if (any(include %in% c("all", "line"))) {
    sel <- ChickWeight$Chick %in% seq_along(color)
    weights <- ChickWeight$weight[sel]
    ylim_range <- range(weights, na.rm = TRUE) + c(-5, 5)

    # Create empty plot and paint background
    plot(ChickWeight$Time, ChickWeight$weight, type = "n",
         xlab = "Time", ylab = "Weight", ylim = ylim_range)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = bg, border = NA)

    # Draw a line for each chick corresponding to a color
    for (k in seq_along(color)) {
      idx <- ChickWeight$Chick == k
      if (any(idx)) {
        lines(ChickWeight$Time[idx], ChickWeight$weight[idx],
              col = color[k], lwd = 3)
      }
    }
    draw_legend()
  }

  ## 2. Scatter Plot Branch ##
  if (any(include %in% c("all", "scatter"))) {
    sel <- ChickWeight$Chick %in% seq_along(color)
    x_vals <- ChickWeight$weight[sel]
    noise <- rnorm(length(x_vals), sd = 50)
    y_vals <- x_vals + noise
    chick_ids <- ChickWeight$Chick[sel]
    # Map chick IDs to the corresponding color
    cat_col <- color[match(chick_ids, seq_along(color))]

    # Create scatter plot and paint background
    plot(x_vals, y_vals, type = "n", xlab = "Weight", ylab = "Profit")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
         col = bg, border = NA)
    points(x_vals, y_vals, col = cat_col, pch = 16)
    draw_legend()
  }

  ## 3. Bar Plot Branch ##
  if (any(include %in% c("all", "bar"))) {
    idx0 <- ChickWeight$Time == 0 & ChickWeight$Chick %in% seq_along(color)
    idx2 <- ChickWeight$Time == 2 & ChickWeight$Chick %in% seq_along(color)

    # Create a matrix with weights at Time 0 and Time 2
    data_mat <- cbind("0" = ChickWeight$weight[idx0],
                      "2" = ChickWeight$weight[idx2])
    rownames(data_mat) <- seq_along(color)

    # Create barplot (the bg parameter is set via par() for this plot)
    opar <- par(bg = bg)
    barplot(data_mat, beside = TRUE, col = color,
            xlab = "Time", ylab = "Weight", ylim = c(0, 70))
    par(opar)
    draw_legend()
  }
}
