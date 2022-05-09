
#' (Normalised) Median Absolute Deviation
#'
#' The present function is used to calculate the Median Absolute Deviation of a vector
#' of values. By default, this function calculates the normalised version.
#'
#' @param x A vector of numeric values
#' @param nmad A boolean value indicating whether the normalised MAD value
#' should be computed. The default is set to TRUE.
#' @param constant A numeric value defining the value which to normalise MAD values
#' with
#'
#' @section Bibliography:
#'
#' Hohle, J. and Hohle, M. (2009) Accuracy Assessment of Digital Elevation Models by
#' Means of Robust Statistical Methods, ISPRS Journal of Photogrammetry and Remote Sensing.
#' 64(4):398-406
#'
#' Rodriguez-Martin, M. et al. (2019) Validation of Portable Mobile Mapping System for Inspection Tasks
#' In Thermal and Fluid Mechanical Facilities. Remote Sensing. 11(19):2205-2219
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
#' median_absolute_deviation(trial_data)
#'
#' @export

median_absolute_deviation <- function(x, nmad = TRUE, constant = 1.4826) {

  if (!is.numeric(x)) {
    stop("MAD or NMAD values can only be calculated on univariate numeric distributions")
  }

  if (is.matrix(x)) {
    if (dim(x)[2] != 1) {
      stop("MAD or NMAD values can only be calculated on univariate numeric distributions")
    }
  }

  if (!is.logical(nmad)) {
    stop("the nmad parameter only accepts boolean values")
  }

  if (!is.numeric(constant)) {
    stop("The constant to be used for NMAD must be numeric")
  }

  n <- length(x)
  x_bar <- median(x)

  mad_prima <- median(abs(x - x_bar))

  if (nmad == TRUE) {
    if (constant == 1.4826) {
      return(mad_prima * 1.4826)
    } else {
      return(mad_prima * constant)
    }
  } else {
    return(mad_prima)
  }

}
