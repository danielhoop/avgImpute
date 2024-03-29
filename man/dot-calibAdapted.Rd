% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avgImpute.R
\name{.calibAdapted}
\alias{.calibAdapted}
\title{calibration Method from package 'sampling'.}
\usage{
.calibAdapted(
  Xs,
  d,
  total,
  q = rep(1, length(d)),
  method = c("linear", "raking", "truncated", "logit"),
  bounds = c(low = 0, upp = 10),
  description = FALSE,
  max_iter = 500
)
}
\arguments{
\item{Xs}{matrix of calibration variables.}

\item{d}{vector of initial weights.}

\item{total}{vector of population totals.}

\item{q}{vector of positive values accounting for heteroscedasticity; the variation of the g-weights is reduced for small values of q.}

\item{method}{calibration method (linear, raking, logit, truncated).}

\item{bounds}{vector of bounds for the g-weights used in the truncated and logit methods; 
'low' is the smallest value and 'upp' is the largest value.}

\item{description}{if description=TRUE, summary of initial and final weights are printed, 
and their boxplots and histograms are drawn; by default, its value is FALSE.}

\item{max_iter}{maximum number of iterations in the Newton's method.}
}
\description{
Slightly adapted to work with variables of which the population \code{total} is 0.
}
\seealso{
\code{\link[sampling:calib]{sampling::calib}}
}
\author{
Alina Matei, adapted by Daniel Hoop
}
\keyword{internal}
