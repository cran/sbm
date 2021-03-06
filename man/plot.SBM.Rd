% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R6Class-SBM.R
\name{plot.SBM}
\alias{plot.SBM}
\title{SBM Plot}
\usage{
\method{plot}{SBM}(x, type = c("data", "expected"), ordered = TRUE, ...)
}
\arguments{
\item{x}{a object inheriting from class SBM}

\item{type}{character for the type of plot: either 'data' (true connection) or 'expected' (fitted connection). Default to 'data'.}

\item{ordered}{logical: should the rows and columns be reoredered according to the clustering? Default to \code{TRUE}.}

\item{...}{additional parameters for S3 compatibility. Not used}
}
\value{
a ggplot2 object
}
\description{
basic matrix plot method for SBM object
}
\details{
Basic matrix plot method for SBM object
}
