
#' Biweight Midvariance and the Square Root of the Biweight Midvariance
#'
#' The present function is used to calculate the robust Biweight Midvariance of
#' a vector of values.
#' The Square Root of the Biweight Midvariance can also be calculated.
#'
#' @param x A vector of numeric values
#' @param sqrt_bwmv A boolean value indicating whether the Square Root of the
#' Biweight Midvariance should be calculaed. The default is set to TRUE.
#'
#' @section Bibliography:
#'
#' Rodriguez-Martin, M. et al. (2019) Validation of Portable Mobile Mapping System
#' for Inspection Tasks
#' In Thermal and Fluid Mechanical Facilities. Remote Sensing. 11(19):2205-2219
#'
#' Nocerino, E. et al. (2017)
#' Investigation of Indoor and Outdoor Performance of Two Portable Mobile Mapping
#' Systems. Proceedings Volume 10332, Videometrics, Range Imaging, and Applications XIV
#' DOI: 10.1117/12.2270761
#'
#' Courtenay, L.A. et al. (2020) Obtaining New Resolutions in Carnivore Tooth Pit Morphological
#' Analyses: A Methodological Update for Digital Taphonomy. PLoS ONE. 15(10):e0240328
#'
#' Courtenay, L.A. et al. (2021) Hyperspectral Imaging and Robust Statistics in Non-Melanoma
#' Skin Cancer Analysis, Biomedical Optics Express. 12(8):5107-5127
#'
#' @examples
#'
#' trial_data <- rnorm(100)
#' biweight_midvariance(trial_data)
#'
#' @export

biweight_midvariance <- function(x, sqrt_bwmv = TRUE) {

  if (!is.numeric(x)) {
    stop("MAD or NMAD values can only be calculated on univariate numeric distributions")
  }

  if (is.matrix(x)) {
    if (dim(x)[2] != 1) {
      stop("MAD or NMAD values can only be calculated on univariate numeric distributions")
    }
  }

  if (!is.logical(sqrt_bwmv)) {
    stop("the nmad parameter only accepts boolean values")
  }

  n <- dim(as.matrix(x))[1]

  nmad <- median_absolute_deviation(x, nmad = TRUE)

  U <- (x - median(x)) / (9 * qnorm(0.75) * nmad)
  a <- ifelse(U <= -1 | U >= 1, 0, 1)

  nx <- sqrt(n) * sqrt(
    sum((a * ((x - median(x))^2)) * ((1 - U^2)^4))
  )
  dx <- abs(sum(a * (1 - U^2) * (1 - 5 * U^2)))
  value <- (nx / dx)^2

  if (sqrt_bwmv == TRUE) {
    return(sqrt(value))
  } else {
    return(value)
  }

}
