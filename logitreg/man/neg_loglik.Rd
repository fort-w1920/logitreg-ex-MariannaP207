% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation-helpers-logitreg.R
\name{neg_loglik}
\alias{neg_loglik}
\title{Computation of negative log-likelihood for logistic regression}
\usage{
neg_loglik(response, design, coefs)
}
\arguments{
\item{response}{vector of responses}

\item{design}{design matrix for the model}

\item{coefs}{start values for coefficients}
}
\value{
negative log-likelihood for given data and coefficients
}
\description{
Compute negative log-likelihood of data based on the logistic distribution
}
