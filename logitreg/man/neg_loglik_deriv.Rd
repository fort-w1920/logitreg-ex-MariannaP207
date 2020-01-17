% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation-logit-reg.R
\name{neg_loglik_deriv}
\alias{neg_loglik_deriv}
\title{Computation of the negative gradient of the log-likelihood}
\usage{
neg_loglik_deriv(coefs, design, response)
}
\arguments{
\item{coefs}{numeric vector for initial values of coefficients with which
the log-likelihood must be optimized}

\item{design}{design matrix of independent variables}

\item{response}{numeric vector of a dependent variable}
}
\value{
negative gradient values for given coefficients
}
\description{
Compute the first negative derivative / gradient of the log-likelihood
for given data and coefficients based on the logistic distribution
}