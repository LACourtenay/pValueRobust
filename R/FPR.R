
#' False Positive Risk
#'
#' The present function is used to calculate the False Positive Risk, otherwise
#' known as the probability that an observation is a Type I statistical error, as
#' defined by Colquhoun (2017, 2019)
#'
#' @param p A numeric p-Value between 0 and 1
#' @param priors A numeric value between 0 and 1 defining the prior probability in
#' favour of the null hypothesis. 0.5 is the default prior probability, considering the
#' recommendation of Colquhoun (2017, 2019)
#'
#' @return Probability that a given p-Value is a Type I Statistical Error when
#' concluding the alternative hypothesis to be true.
#'
#' @section Note:
#'
#' The present function implements the Sellke-Berger approach for the calculation of
#' the likelihood ratio of the null hypothesis against the alternative hypothesis.
#'
#' It is worth noting that values above p = 0.3681 begin to favour the null hypothesis
#' as opposed to the alternative hypothesis. See the Supplementary Materials of
#' Courtenay et al. (2021b).
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
#' Colquhoun, D. (2019) The False Positive Risk: A Proposal Concerning What to do
#' about p-Values. The American Statistician, 73(Sup1):192-201.
#' DOI: 10.1080/00031305.2018.1529622
#'
#' Benjamin, D.J.; Berger, J.O. (2019) Three recommendations for Improving
#' the Use of p-Values. The American Statistician. 73(Sup1):186-191.
#' DOI: 10.1080/00031305.2018.1543135
#'
#' Courtenay et al. (2021a) Hyperspectral Imaging and Robust Statistics in Non-Melanoma
#' Skin Cancer Analysis, Biomedical Optics Express. 12(8):5107-5127
#'
#' Courtenay et al. (2021b) Developments in Data Science Solutions for Carnivore
#' Tooth Pit Classification: Supplementary Materials, Appendix 3. Scientific Reports.
#'
#' @examples
#'
#' FPR(0.05)
#' FPR(0.003)
#'
#' @export

FPR <- function(p, priors = 0.5){

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

  pH = priors / (1 - priors)

  return(1 / (1 + (pH * BFB(p))))

}
