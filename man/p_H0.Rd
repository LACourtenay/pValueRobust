% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_H0.R
\name{p_H0}
\alias{p_H0}
\title{Probability of the Null Hypothesis}
\usage{
p_H0(p, priors = 0.5)
}
\arguments{
\item{p}{A numeric p-Value between 0 and 1}

\item{priors}{A numeric value between 0 and 1 defining the prior probability in
favour of the null hypothesis. 0.5 is the default prior probability, considering the
recommendation of Colquhoun (2017, 2019)}
}
\value{
The Probability that the observer is making a mistake when concluding the
alternative hypothesis
}
\description{
The present function is used to calculate the probability of the null
hypothesis, performing the False Positive Risk calculation for values
below p = 0.3681, and the inverse of this function for values above p = 0.3681.
This can thus be framed as the probability of error when concluding the
alternative hypothesis to be true
}
\section{Bibliography}{


Colquhoun, D. (2017) The Reproducibility of Research and the Misinterpretation of p-Values.
Royal Society of Open Sciences. 4:171085. DOI: 10.1098/rsos.171085

Colquhoun, D. (2019) The False Positive Risk: A Proposal Concerning What to do
about p-Values. The American Statistician, 73(Sup1):192-201.
DOI: 10.1080/00031305.2018.1529622

Courtenay et al. (2021a) Hyperspectral Imaging and Robust Statistics in Non-Melanoma
Skin Cancer Analysis, Biomedical Optics Express. 12(8):5107-5127

Courtenay et al. (2021b) Developments in Data Science Solutions for Carnivore
Tooth Pit Classification: Supplementary Materials, Appendix 3. Scientific Reports.
}

\examples{

p_H0(0.05)
p_H0(0.003)

}
\seealso{
\code{\link{FPR}}
}
