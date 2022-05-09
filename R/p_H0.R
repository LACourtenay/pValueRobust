
#' Probability of the Null Hypothesis
#'
#' The present function is used to calculate the probability of the null
#' hypothesis, performing the False Positive Risk calculation for values
#' below p = 0.3681, and the inverse of this function for values above p = 0.3681.
#' This can thus be framed as the probability of error when concluding the
#' alternative hypothesis to be true
#'
#' @param p A numeric p-Value between 0 and 1
#' @param priors A numeric value between 0 and 1 defining the prior probability in
#' favour of the null hypothesis. 0.5 is the default prior probability, considering the
#' recommendation of Colquhoun (2017, 2019)
#'
#' @return The Probability that the observer is making a mistake when concluding the
#' alternative hypothesis
#'
#' @section Bibliography:
#'
#' Colquhoun, D. (2017) The Reproducibility of Research and the Misinterpretation of p-Values.
#' Royal Society of Open Sciences. 4:171085. DOI: 10.1098/rsos.171085
#'
#' Colquhoun, D. (2019) The False Positive Risk: A Proposal Concerning What to do
#' about p-Values. The American Statistician, 73(Sup1):192-201.
#' DOI: 10.1080/00031305.2018.1529622
#'
#' Courtenay et al. (2021a) Hyperspectral Imaging and Robust Statistics in Non-Melanoma
#' Skin Cancer Analysis, Biomedical Optics Express. 12(8):5107-5127
#'
#' Courtenay et al. (2021b) Developments in Data Science Solutions for Carnivore
#' Tooth Pit Classification: Supplementary Materials, Appendix 3. Scientific Reports.
#'
#' @seealso \code{\link{FPR}}
#' @examples
#'
#' p_H0(0.05)
#' p_H0(0.003)
#'
#' @export

p_H0 <- function(p, priors = 0.5){

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

  if(p <= 0.3681) {

    value = FPR(p, priors = priors)

  } else {

    value = 1 - FPR(p, priors = 1 - priors)

  }

  return(value)

}; p_H0<-Vectorize(p_H0)
