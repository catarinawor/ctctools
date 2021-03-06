% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catch_functions.R
\name{importCatch.jon}
\alias{importCatch.jon}
\title{Import catch csv file made by Jonathan Carey.}
\usage{
importCatch.jon(filename, startingyear = 1, finalyear = 9999,
  calibration.name.new = "new", calibration.name.old = "old",
  commonstocks = FALSE, fishery.names.exclude = NA)
}
\arguments{
\item{filename}{A character. The name of the csv file for import.}

\item{startingyear}{An integer. The default is 1, which means the series will start with the ealiest year.}

\item{finalyear}{An integer. The default is 9999, which means the series will end with the most recent year.}

\item{calibration.name.new}{A character. The default name is "new".}

\item{calibration.name.old}{A character. The default name is "old".}

\item{commonstocks}{A logical. The default is FALSE.}

\item{fishery.names.exclude}{A character vector of fishery names to be excluded from the data.}
}
\value{
A list of three data frames. The data frame named \code{data.combined} comprises the fishery specific catches, reshaped to long format. The data frame named \code{total} is the recalculated totals after exclusion of named fisheries. The data frame \code{data.original} is the unmodified initial import.
}
\description{
Import catch csv file made by Jonathan Carey.
}
