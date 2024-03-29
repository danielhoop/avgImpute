% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avgImpute.R
\name{distToWeight}
\alias{distToWeight}
\title{Convert distances to weights}
\usage{
distToWeight(dist, ...)
}
\arguments{
\item{dist}{A matrix containing the pairwise distances between the observations.}

\item{...}{Not used but needed such that the same parameters can be passed into different functions such as \code{\link{createLookupWithMeanMethod}} and \code{\link{distToWeight}}.}
}
\value{
A matrix with equal dimensions like \code{dist}, containing the weights.
}
\description{
Package default function to convert distances to weights. These weights are then used to calculate a weighted average of all similar observations, which is subsequently used for the imputation.
You may implement your own function and use it instead.
}
\author{
Daniel Hoop
}
