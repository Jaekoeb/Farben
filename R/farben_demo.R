#' Plot Demo
#'
#' @description
#' This function can be used to quickly assess if a color palette is useful. It returns
#' three different plots from the 'ChickWeight' data set.
#'
#' @param color A farben object (colors given as hex codes).
#' @param include A character vector of which plots to include. Options are `"line"`, `"scatter"`, `"bar"`,
#' or `"all"`.
#' @param bg The background color for the plot(s). Defaults to `"white"`.
#'
#' @return Produces 1–3 plots.
#' @export
#'
#' @examples
#' # Assuming farben() creates an object of class "farben"
#' col <- farben(c("red", "blue", "yellow"))
#' farben_demo(color = col)
farben_demo <- function(color, include = "all", bg = "white") {

  # Check that input is of class "farben"
  if (!inherits(color, "farben")) {
    stop("Input must be of class 'farben'")
  }

  # Prevent too many colors
  if (length(color) > 50) {
    stop("Too many colors given")
  }

  ### Helper: Draw legend in outer margin ###
  draw_legend <- function() {
    # Use the current plot's usr coordinates to horizontally center the legend;
    # the legend will be drawn in the outer margin area.
    usr <- par("usr")
    legend(x = mean(usr[1:2]),
           y = usr[4] + 0.5,   # offset by 0.5 (adjust as needed)
           legend = seq_along(color),
           fill = color,
           title = "Chick",
           horiz = TRUE,
           bty = "n",
           xpd = NA)
  }

  ##############################
  ## 1. Line Graph Branch     ##
  ##############################
  if (any(include %in% c("all", "line"))) {
    # Subset data for chicks that correspond to available colors
    sel <- ChickWeight$Chick %in% seq_along(color)
    ymax <- max(ChickWeight$weight[sel], na.rm = TRUE) + 5
    ymin <- min(ChickWeight$weight[sel], na.rm = TRUE) - 5

    # Set up standard inner margins and a modest outer margin at the top for the legend.
    old.mar <- par("mar")
    old.oma <- par("oma")
    par(mar = c(5, 4, 4, 2) + 0.1,  # inner margins (default)
        oma = c(0, 0, 1, 0))        # outer margin: 1 line at top
    # Initialize plot (axes drawn, but no data yet)
    with(ChickWeight,
         plot(Time, weight, type = "n", xlab = "Time", ylab = "Weight", ylim = c(ymin, ymax)))

    # Paint the plotting region background
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)

    # Draw lines for each chick/color (only for chicks 1, 2, ... length(color))
    for (k in seq_along(color)) {
      with(ChickWeight, {
        idx <- Chick == k
        if (any(idx)) {
          lines(Time[idx], weight[idx], col = color[k], lwd = 3)
        }
      })
    }

    # Draw legend in the outer margin (above the plot)
    draw_legend()

    # Restore original margins
    par(mar = old.mar, oma = old.oma)
  }

  ##############################
  ## 2. Scatter Plot Branch   ##
  ##############################
  if (any(include %in% c("all", "scatter"))) {
    old.mar <- par("mar")
    old.oma <- par("oma")
    par(mar = c(5, 4, 4, 2) + 0.1,
        oma = c(0, 0, 1, 0))

    # Subset data for chicks 1, 2, ... length(color)
    sel <- ChickWeight$Chick %in% seq_along(color)
    if (sum(sel) == 0) stop("No data available for the given color selection.")
    x_vals <- ChickWeight$weight[sel]
    noise <- rnorm(length(x_vals), sd = 50)
    y_vals <- ChickWeight$weight[sel] + noise

    # Map chick IDs (assumed numeric) to colors
    chick_ids <- ChickWeight$Chick[sel]
    # Ensure that indexing is safe:
    cat.col <- color[match(chick_ids, seq_along(color))]

    # Set up plot without plotting points
    plot(x_vals, y_vals, type = "n", xlab = "Weight", ylab = "Profit")

    # Paint the background in the plot region
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col = bg, border = NA)

    # Now add the points
    points(x_vals, y_vals, col = cat.col, pch = 16)

    # Draw legend in outer margin
    draw_legend()

    par(mar = old.mar, oma = old.oma)
  }

  ##############################
  ## 3. Bar Plot Branch       ##
  ##############################
  if (any(include %in% c("all", "bar"))) {
    old.mar <- par("mar")
    old.oma <- par("oma")
    par(mar = c(5, 4, 4, 2) + 0.1,
        oma = c(0, 0, 1, 0))

    # Select weights for Time 0 and Time 2 for chicks with IDs 1, 2, ... length(color)
    idx0 <- ChickWeight$Time == 0 & ChickWeight$Chick %in% seq_along(color)
    idx2 <- ChickWeight$Time == 2 & ChickWeight$Chick %in% seq_along(color)

    data_mat <- cbind(ChickWeight$weight[idx0],
                      ChickWeight$weight[idx2])
    colnames(data_mat) <- c("0", "2")
    rownames(data_mat) <- paste0(seq_along(color))

    # For barplot, set the device background temporarily
    old_bg <- par("bg")
    par(bg = bg)

    bp <- barplot(data_mat, beside = TRUE, col = color,
                  xlab = "Time", ylab = "Weight", ylim = c(0, 70))

    par(bg = old_bg)

    usr <- par("usr")
    draw_legend()

    par(mar = old.mar, oma = old.oma)
  }
}
