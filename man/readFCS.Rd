% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chinookmodelfunctions.R
\name{readFCS}
\alias{readFCS}
\title{(Calibration Performance) Import FCS Files}
\usage{
readFCS(filepath, first.stockline = 3, stocks.key.df = NULL,
  startingyear = 1, finalyear = 9999)
}
\arguments{
\item{filepath}{A character vector of length one. The name of the FCS file to
import.}

\item{first.stockline}{An integer, row number in FCS file where first stock
metadata commences. See details.}

\item{stocks.key.df}{A data frame. These data are stored in the package. If
the argument is left with its default value (NULL), then the data are
loaded from the package. The user can supply a new data frame with updated
stocks, so long as the updated data frame has the same structure as found
in \code{\link{stocks.key}}.}

\item{startingyear}{An integer.}

\item{finalyear}{An integer. The final (4 digit) year to be included from all series
imported. Default is 9999, meaning all years before 9999.}
}
\value{
A list, with two elements. The first element is a data frame of all
  the data fro all the stocks transposed into a long format. The second
  element is a list comprising as many lists as stocks in the FCS file. All
  the data associated with each stock can be found in each sub-sub list.
}
\description{
Import one or more FCS files into a list.
}
\details{
Need details.
}
\examples{
\dontrun{
### read in FCS files ###
fcs.files <- list.files(data.pathname, pattern = "\\\\.FCS")
filename <- fcs.files[grep("OCN", x = fcs.files)]
filepath <- paste(data.pathname, filename, sep='/')
fcs.list <- readFCS(filepath)
}
}
