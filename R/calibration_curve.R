

#'Plot p-Value Calibration Curves
#'
#' The present function can be used to plot calibration curves for a set of p-Values
#' according to different Bayseian based metrics
#'
#' @param p_values A set of two numeric values between 0 and 1 defining the minimum
#' and maximum p-Values the calibration curve will be plotted for.
#' @param method a string defining the p-Value calibration method, select one of
#' the following options: Bayes Factor Bounds ("BFB"); False Positive Risk ("FPR");
#' Bayes Factor Bound probability vs False Positive Risk ("p_BFB vs FPR");
#' Probability of the Null Hypothesis ("p_H0"). By default BFB is selected
#' @param priors A numeric value between 0 and 1 defining the prior probability in
#' favour of the null hypothesis.
#' @param lwd Line width of the curve
#' @param col Colour of the line
#' @param add A boolean value that can be used to add curves to an already existing plot
#'
#' @seealso \code{\link{FPR}}
#' @examples
#'
#' # example 1
#'
#' calibration_curve(c(1, 0.0001))
#' calibration_curve(c(0.01, 0.0001), method = "BFB")
#'
#' # example 2
#'
#' calibration_curve(method = "FPR", lwd = 2)
#' calibration_curve(method = "FPR", priors = 0.7,
#'                   col = "red", lwd = 2, add = TRUE)
#' calibration_curve(method = "FPR", priors = 0.3,
#'                   col = "blue", lwd = 2, add = TRUE)
#' legend("topright", inset = 0.02,
#'        legend = c("0.7", "0.5", "0.3"), col = c("black", "red", "blue"),
#'        lty = 1, lwd = 2)
#'
#' # example 3
#'
#' par(mfrow = c(1, 3))
#' calibration_curve(method = "p_BFB vs FPR", priors = 0.7, lwd = 2)
#' calibration_curve(method = "p_BFB vs FPR", priors = 0.5, lwd = 2)
#' calibration_curve(method = "p_BFB vs FPR", priors = 0.3, lwd = 2)
#' par(mfrow = c(1, 1))
#'
#' # example 4
#'
#' calibration_curve(method = "p_H0", priors = 0.7, lwd = 2, col = "black")
#' calibration_curve(method = "p_H0", priors = 0.5, lwd = 2, col = "red", add = TRUE)
#' calibration_curve(method = "p_H0", priors = 0.3, lwd = 2, col = "blue", add = TRUE)
#' legend("topright", inset = 0.02,
#'        legend = c("0.7", "0.5", "0.3"), col = c("black", "red", "blue"),
#'        lty = 1, lwd = 2)
#'
#' @export

calibration_curve <- function(
  p_values = c(1, 0.00001),
  method = c("BFB", "FPR", "p_BFB vs FPR", "p_H0"),
  priors = NULL,
  lwd = 1,
  col = "black",
  add = FALSE
) {

  check_colours <- function(col_list) {
    sapply(col_list, function (values) {
      tryCatch(is.matrix(col2rgb(values)),
               error = function(e) FALSE)
    })
  }

  bfb_cal_curve <- function(p){
    bayes_factor_bound <- (1 / (-exp(1) * p * log(p)))
    return(bayes_factor_bound)
  }

  fpr_cal_curve <- function(p, priors) {
    pH = priors / (1 - priors)
    return(1 / (1 + (pH * bfb_cal_curve(p))))
  }

  p_bfb_cal_curve <- function(p, priors){
    pH = priors / (1 - priors)
    FPR_value = fpr_cal_curve(p, priors = 0.5)
    value = (1 - FPR_value) / (1 - FPR_value + (pH * FPR_value))
    return(value)
  }

  p_H0_cal_curve <- function(p, priors = 0.5){

    if(p <= 0.3681) {

      value = fpr_cal_curve(p, priors = priors)

    } else {

      value = 1 - fpr_cal_curve(p, priors = 1 - priors)

    }

    return(value)

  }; p_H0_cal_curve<-Vectorize(p_H0_cal_curve)

  if (length(p_values) == 2) {

    if (!is.numeric(p_values[1]) | !is.numeric(p_values[2]))  {
      stop("p-Values must be numeric values")
    }
    if(p_values[1] < 0 | p_values[2] < 0 | p_values[1] > 1 | p_values[2] > 1) {
      stop("p-Values must be between 0 and 1")
    }

  } else {

    stop("Invalid Upper and Lower p-Value limits provided")

  }

  if (is.null(priors)) {
    priors = 0.5
  } else {
    if (!is.numeric(priors)) {
      stop("Priors must be numeric")
    }

    if (priors < 0 | priors > 1) {
      stop("Priors must be between 0 and 1")
    }
  }

  if (!is.numeric(lwd)) {
    stop("Line width must be a numeric value")
  } else {
    if (lwd <= 0) {
      stop("Invalid line width provided")
    }
  }

  if (length(col) == 1) {
    if (col != "black") {
      colour_bool <- check_colours(col)
      if (FALSE %in% colour_bool) {
        stop("Invalid colour provided")
      }
    }
  } else {
    colour_bool <- check_colours(col)
    if (FALSE %in% colour_bool) {
      stop("Invalid colour provided")
    }
  }

  if (!is.logical(add)) {
    stop("add must be a boolean value")
  } else {
    if (add != FALSE) {
      active_window <- try(points(1,2), silent = TRUE)
      if (class(active_window) == "try-error") {
        stop("add can only be set to TRUE if a plotting device is already active")
      }
    }
  }

  plot_method <- match.arg(method)

  par(mar = c(5.1, 5, 4.1, 2.))

  p_values <- p_values[order(p_values, decreasing = TRUE)]

  x_axis_labels <- seq(p_values[1], p_values[2], length.out = 5)
  for (tick in 1:length(x_axis_labels)) {
    if (x_axis_labels[tick] > 0.1) {
      x_axis_labels[tick] <- round(x_axis_labels[tick], 1)
    } else {
      if (x_axis_labels[tick] > 0.01) {
        x_axis_labels[tick] <- round(x_axis_labels[tick], 2)
      } else if (x_axis_labels[tick] > 0.001) {
        x_axis_labels[tick] <- round(x_axis_labels[tick], 3)
      } else if (x_axis_labels[tick] > 0.0001) {
        x_axis_labels[tick] <- round(x_axis_labels[tick], 4)
      } else {
        x_axis_labels[tick] <- round(x_axis_labels[tick], 5)
      }
    }
  }

  if (add == FALSE) {
    if (plot_method == "BFB") {

      plot(
        function(x) {bfb_cal_curve(x)},
        xlim = rev(c(p_values[2], p_values[1])),
        ylim = c(0, 60),
        lwd = lwd,
        col = col,
        xlab = "", ylab = "", xaxt = "none", yaxt = "none"
      )
      mtext(side = 1, line = 3, "p-value", cex = 1.5, font = 2)
      mtext(side = 2, line = 3, "BFB", cex = 1.5, font = 2)
      axis(1, seq(p_values[1], p_values[2], length.out = 5),
           labels = x_axis_labels,
           font = 1, cex.axis = 1.25)
      axis(2, seq(0, 60, 20), font = 1, cex.axis = 1.25)

    } else if (plot_method == "FPR") {

      plot(
        function(x) {fpr_cal_curve(x, priors = priors)},
        xlim = rev(c(p_values[2], p_values[1])),
        ylim = c(0, 1),
        lwd = lwd,
        col = col,
        xlab = "", ylab = "", xaxt = "none", yaxt = "none"
      )
      mtext(side = 1, line = 3, "p-value", cex = 1.5, font = 2)
      mtext(side = 2, line = 3, "FPR", cex = 1.5, font = 2)
      axis(1, seq(p_values[1], p_values[2], length.out = 5),
           labels = x_axis_labels,
           font = 1, cex.axis = 1.25)
      axis(2, seq(0, 1, 0.2), font = 1, cex.axis = 1.25)

    } else if (plot_method == "p_BFB vs FPR") {

      if (length(col) < 2) {
        if (col == "black") {
          col = c("black", "red")
        } else {
          stop("Please provide two colours for the p_BFB vs FPR plot")
        }
      }

      plot(
        function(x) {fpr_cal_curve(x, priors = priors)},
        xlim = rev(c(p_values[2], p_values[1])),
        ylim = c(0, 1),
        lwd = lwd,
        col = col[1],
        xlab = "", ylab = "", xaxt = "none", yaxt = "none"
      )
      plot(
        function(x) {p_bfb_cal_curve(x, priors = priors)},
        from = p_values[2],
        to = p_values[1],
        add = TRUE,
        lwd = lwd,
        col = col[2]
      )
      mtext(side = 1, line = 3, "p-value", cex = 1.5, font = 2)
      axis(1, seq(p_values[1], p_values[2], length.out = 5),
           labels = x_axis_labels,
           font = 1, cex.axis = 1.25)
      axis(2, seq(0, 1, 0.2), font = 1, cex.axis = 1.25)

      title(expression("FPR    " * phantom("p_BFB")), col.main = col[1])
      title(expression(phantom("FPR    ") * "p_BFB"), col.main = col[2])

    } else if (plot_method == "p_H0") {

      plot(
        function(x) {p_H0_cal_curve(x, priors = priors)},
        xlim = rev(c(p_values[2], p_values[1])),
        ylim = c(0, 1),
        lwd = lwd,
        col = col,
        xlab = "", ylab = "", xaxt = "none", yaxt = "none"
      )
      mtext(side = 1, line = 3, "p-value", cex = 1.5, font = 2)
      mtext(side = 2, line = 3, "Probability of H0", cex = 1.5, font = 2)
      axis(1, seq(p_values[1], p_values[2], length.out = 5),
           labels = x_axis_labels,
           font = 1, cex.axis = 1.25)
      axis(2, seq(0, 1, 0.2), font = 1, cex.axis = 1.25)

    } else {
      par(mar = c(5.1, 4.1, 4.1, 2.1))
      stop("Error")
    }
  } else {
    if (plot_method == "BFB") {

      plot(
        function(x) {bfb_cal_curve(x)},
        from = p_values[2],
        to = p_values[1],
        lwd = lwd,
        col = col,
        add = TRUE
      )

    } else if (plot_method == "FPR") {

      plot(
        function(x) {fpr_cal_curve(x, priors = priors)},
        from = p_values[2],
        to = p_values[1],
        lwd = lwd,
        col = col,
        add = TRUE
      )

    } else if (plot_method == "p_BFB vs FPR") {

      stop("p_BFB vs FPR has no add == TRUE option")

    } else if (plot_method == "p_H0") {

      plot(
        function(x) {p_H0_cal_curve(x, priors = priors)},
        from = p_values[2],
        to = p_values[1],
        lwd = lwd,
        col = col,
        add = TRUE
      )

    } else {
      par(mar = c(5.1, 4.1, 4.1, 2.1))
      stop("Error")
    }
  }

  par(mar = c(5.1, 4.1, 4.1, 2.1))

}
