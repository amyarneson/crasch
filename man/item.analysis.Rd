% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item.R
\name{item.analysis}
\alias{item.analysis}
\title{Creates item analysis tables.}
\usage{
item.analysis(results, writeout = FALSE, fileSuffix = NULL)
}
\arguments{
\item{results}{The output from a run of \code{craschR}. (link?)}

\item{writeout}{A logical indicated whether the estimate objects should be
written to your working directory as CSVs.  If \code{TRUE}, two files will
be produced, one beginning \code{item-byitem} and the other
\code{item-bystep} with any specified \code{fileSuffix}.}

\item{fileSuffix}{A character string that will be affixed to the end of each
file name (if \code{writeout = TRUE}). Use this if you are conducting
multiple analyses in the same working directory and do not wish for your
existing files to be overwritten.}
}
\value{
A list with the following entries:
  \item{byItem}{A matrix with classical statistics and descriptives by item.}
  \item{byStep}{A matrix with classical statistics and descriptives by step.}
}
\description{
Creates item analysis tables.
}
