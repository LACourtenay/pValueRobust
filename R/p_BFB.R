
#' Upper Bound on the Posterior Probability of the Alternative Hypothesis
#'
#' The present function is used to calculate the posterior
#' probability of the Alternative Hypothesis according to the
#' upper Bayes Factor Bound of a reported p-Value according to
#' Benjamin and Berger (2019)
#'
#' @param p A numeric p-Value between 0 and 1
#' @param priors A numeric value between 0 and 1 defining the prior probability in
#' favour of the null hypothesis. 0.5 is the default prior probability, considering the
#' recommendation of Colquhoun (2017, 2019)
#'
#' @section Details:
#' This is a probability value corresponding to the upper bound of
#' the posterior probability distribution in favour of the alternative
#' hypothesis, in other words the BFB value represented as a probability
#' value, as opposed to odds.
#'
#' @return The Probability value corresponding to the upper bound of
#' the posterior probability of the Alternative Hypothesis.
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
#' @seealso \code{\link{BFB}}
#' @examples
#'
#' p_BFB(0.05)
#' p_BFB(0.003)
#'
#' @export

p_BFB <- function(p, priors = 0.5){

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


  if (priors == 0.5) {

    value = BFB(p) / (1 + BFB(p))

  } else {

    pH = priors / (1 - priors)
    FPR_value = FPR(p, priors = 0.5)
    value = (1 - FPR_value) / (1 - FPR_value + (pH * FPR_value))

  }

  return(value)
}
