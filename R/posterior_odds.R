
#' Posterior Odds of the Bayes Factor Bound
#'
#' The present function is used to calculate the posterior odds of the Alternative
#' Hypothesis agains the Null Hypothesis
#'
#' @param p A numeric p-Value between 0 and 1
#' @param priors A numeric value between 0 and 1 defining the prior probability in
#' favour of the null hypothesis. 0.5 is the default prior probability, considering the
#' recommendation of Colquhoun (2017, 2019)
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
#' Colquhoun, D. (2017) The Reproducibility of Research and the Misinterpretation of p-Values.
#' Royal Society of Open Sciences. 4:171085. DOI: 10.1098/rsos.171085
#'
#' Held, L.; Ott, M. (2018) On p-Values and Bayes Factors. Annual Review of
#' Statistics and its Applications. 5(6):1-27
#'
#' Benjamin, D.J.; Berger, J.O. (2019) Three recommendations for Improving
#' the Use of p-Values. The American Statistician. 73(Sup1):186-191.
#' DOI: 10.1080/00031305.2018.1543135
#'
#' Colquhoun, D. (2019) The False Positive Risk: A Proposal Concerning What to do
#' about p-Values. The American Statistician, 73(Sup1):192-201.
#' DOI: 10.1080/00031305.2018.1529622
#'
#' @examples
#'
#' posterior_odds(0.05)
#' posterior_odds(0.003)
#'
#' # check different priors
#'
#' posterior_odds(0.05, priors = 0.7)
#' posterior_odds(0.05, priors = 0.3)
#'
#' @export

posterior_odds <- function(p, priors = 0.5){

  if (!is.numeric(p)) {
    stop("The p-Value must be numeric")
  }

  if (p < 0 | p > 1) {
    stop("p-Values must be between 0 and 1")
  }

  if (!is.numeric(priors)) {
    stop("Priors must be numeric")
  }

  if (priors < 0 | priors > 1) {
    stop("Priors must be between 0 and 1")
  }

  return(priors * BFB(p))

}
