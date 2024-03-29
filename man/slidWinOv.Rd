% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slidWinOv.R
\name{slidWinOv}
\alias{slidWinOv}
\title{Window Slider with Overlaps}
\usage{
slidWinOv(fSeq, dBinSize = 500, dBinOverlap = 10)
}
\arguments{
\item{fSeq}{Numerical vector}

\item{dBinSize}{Bin size. \strong{Default = 500}}

\item{dBinOverlap}{Bin overlap. \strong{Default = 10}}
}
\value{
Numerical vector
}
\description{
Takes in numerical vector as \emph{'fSeq'} and outputs their positions assuming \emph{'dBinSize'} and \emph{'dBinOverlap'}
}
\section{Warning}{

\emph{'slidWinOv'} is only useful for single numbers, otherwise use \emph{'slidWinTov'}
\emph{'slidWinOv'} uses \emph{'slidWin'} to determine the window sliding
}

\examples{
slidWinOv(37, 15, 5)
}
\seealso{
\code{\link{slidWin}}
}
