% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decTag.R
\name{decTag}
\alias{decTag}
\title{Tag Decoder}
\usage{
decTag(fSeq, fDec, dNumerator = 100)
}
\arguments{
\item{fSeq}{Numerical vector}

\item{fDec}{Position to decode}

\item{dNumerator}{Digit position}
}
\value{
Numerical vector of decoded values
}
\description{
\emph{decTag} decodes a numeric vector given a specified position \emph{fDec} by digits as \emph{dNumerator}
}
\examples{
x <- c(10.34, 435.5002, 436.7879)
decTag(x, 0.01)
}
