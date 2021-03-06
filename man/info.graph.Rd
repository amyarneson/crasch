% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/personItem.R
\name{info.graph}
\alias{info.graph}
\title{Plots the standard error of measurement or test information function.}
\usage{
info.graph(results, dim = NULL, type = "SEM", completeOnly = TRUE,
  palette = "BASS", thetaGrid = seq(-6, 6, length.out = 100),
  writeout = FALSE, imageType = "pdf", fileSuffix = NULL)
}
\arguments{
\item{results}{The output from a run of \code{craschR}. (link?)}

\item{dim}{Specify which dimension(s) to create graphic/tables for.  If
\code{NULL}, output and graphics for each dimension will be produced.}

\item{type}{\code{c("SEM", "TIC", "IIC")}}

\item{completeOnly}{A logical indicating whether to graph points for
respondents who received a score on all items or everyone, no matter what.}

\item{palette}{A character string indicating the color scheme.  Can be
"BASS", any RColorBrewer palette, or a vector containing 2 colors (the
first for the curve and the second for the points).}

\item{thetaGrid}{A vector containing the range of thetas for the function
graph.}

\item{writeout}{A logical indicated whether the graph should be written to
your working directory as your indicated \code{imageType}.  If \code{TRUE},
the file name will begin \code{SEM} or \code{TIC} and will include an index
(if more than one graph is produced) and the \code{fileSuffix} if provided.}

\item{imageType}{A character string indicating the format for graphics (if
\code{writeout = TRUE}). Supported types:
\code{c("pdf","svg","jpeg","bmp","tiff","png")}.}

\item{fileSuffix}{A character string that will be affixed to the end of each
file name (if \code{writeout = TRUE}). Use this if you are conducting
multiple analyses in the same working directory and do not wish for your
existing files to be overwritten.}
}
\value{
A plot of the specified test information curve.  (Note: if you want
  category information curves, you can use the TAM function
  \code{IRT.informationCurves()} directly.)
}
\description{
Plots the standard error of measurement or test information function.
}
