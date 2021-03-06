% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logitreg-helper-methods.R
\name{plot.logitreg}
\alias{plot.logitreg}
\title{method \code{plot} for class \code{logitreg}}
\usage{
\method{plot}{logitreg}(x, y, ...)
}
\arguments{
\item{x}{object of class \code{logitreg}.}

\item{y}{optional if \code{x} has an appropriate structure.}

\item{...}{further parameters, such as graphical parameters (see \link{par}).}
}
\value{
a plot with a ROC-curve for the \code{logitreg} model
}
\description{
Plots a \href{https://en.wikipedia.org/wiki/Receiver_operating_characteristic}{ROC-curve}
for the \link{logitreg} object.
}
