
#' Interquantile Range and Confidence Intervals
#'
#' The present function is used to calculate asymmetric measurements of
#' sample dispersal using the Interquantile range.
#'
#' @param x A vector of numeric values
#' @param q A set of two numeric values between 0 and 1 that define the confidence
#' intervals the user wishes to calculate. If only one value is provided then this
#' function simply returns the specified quantile. Default intervals are set to 5
#' and 95\%
#'
#' @section Bibliography:
#'
#' Hohle, J. and Hohle, M. (2009) Accuracy Assessment of Digital Elevation Models by
#' Means of Robust Statistical Methods, ISPRS Journal of Photogrammetry and Remote Sensing.
#' 64(4):398-406
#'
#' @return Either the lower and upper quantile confidence intervals or a specified quantile
#'
#' @examples
#'
#' trial_data <- rnorm(100)
#' quantile_CI(trial_data)
#'
#' @export

quantile_CI <- function(x, q = c(0.025, 0.975)) {

  if (!is.numeric(x)) {
    stop("Quantile values can only be calculated on univariate numeric distributions")
  }

  if (is.matrix(x)) {
    if (dim(x)[2] != 1) {
      stop("Quantile values can only be calculated on univariate numeric distributions")
    }
  }

  if (length(q) == 2) {

    if (!is.numeric(q[1]) | !is.numeric(q[2]))  {
      stop("Quantiles must be numeric")
    }
    if(q[1] < 0 | q[2] < 0 | q[1] > 1 | q[2] > 1) {
      stop("Quantile values must be between 0 and 1")
    }

    upper_CI <- sort(x)[ceiling(q[2] * length(x))]
    lower_CI <- sort(x)[ceiling(q[1] * length(x))]

    return(
      c(lower_CI, upper_CI)
    )

  } else if (length(q) == 1) {

    x_prima = sort(x)[ceiling(q * length(x))]

    return(x_prima)

  } else {
    stop("Invalid number of quantiles provided")
  }

}
