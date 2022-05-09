
#' Bayes Factor Bound
#'
#' The present function is used to calculate the upper Bayes Factor Bound
#' of a reported p-Value according to Benjamin and Berger (2019)
#'
#' @param p A numeric p-Value between 0 and 1
#'
#' @section Details:
#' The BFB is generally reported as the odds (at most) against the Null
#' Hypothesis.
#'
#' @return The Bayes Factor Bound odds for the corresponding p-Value
#'
#' @section Bibliography:
#'
#' Sellke, T.; Bayarri, M.J.; Berger, J.O. (2001) Calibration of
#' p values for testing precise null hypotheses. The American
#' Statistician. 55(1):62-71
#'
#' Held, L.; Ott, M. (2018) On p-Values and Bayes Factors. Annual Review of
#' Statistics and its Applications. 5(6):1-27
#'
#' Benjamin, D.J.; Berger, J.O. (2019) Three recommendations for Improving
#' the Use of p-Values. The American Statistician. 73(Sup1):186-191.
#' DOI: 10.1080/00031305.2018.1543135
#'
#' @examples
#'
#' BFB(0.05) # check the BFB value for the traditional p = 0.05
#' BFB(0.003) # check the BFB value for the recommended p = 0.003
#'
#' @export

BFB <- function(p){

  if (!is.numeric(p)) {
    stop("The p-Value must be numeric")
  }

  if (p < 0 | p > 1) {
    stop("p-Values must be between 0 and 1")
  }

  bayes_factor_bound <- (1 / (-exp(1) * p * log(p)))

  return(bayes_factor_bound)

}
