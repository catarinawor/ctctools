#' @title Jurisdiction data for each stock
#'
#' @description A dataset containing the stocks and their associated jurisdictions, including
#' province or state.
#'
#' @format A data frame with 58 rows and 5 variables:
#' \describe{
#'   \item{stock}{three letter stock identifier}
#'   \item{stock.long}{the full stock name}
#'   \item{jurisdiction}{two letter identifier of province or state}
#'   \item{jurisdiction.long}{full name of province or state, in lower case}
#'   \item{stock.country}{two letter identifier country, but include 'TB' for transboundary}
#'    ... }
#' @source \url{http://www.psc.org/}
#' @examples
#' data(jurisdiction)
#' #or
#' data("jurisdiction")
#' str(jurisdiction)
"jurisdiction"


#' @title Data defining each fishery.
#'
#' @description  A dataset containing the fisheries and associated meta data.
#'
#' @format A data frame with 78 rows and 9 variables:
#' \describe{
#'   \item{fishery.index}{The integer number of each fishery}
#'   \item{gear}{Troll = "T", net = "N", sport = "S", nonfishery = "X"}
#'   \item{fishery.type}{Preterminal = "P", terminal = "T"}
#'   \item{fishery.country}{"US" or "CA"}
#'   \item{fishery.name}{A slightly longer text string desribing fishery}
#'   \item{fishery.region}{}
#'   \item{fishery.psc}{}
#'   \item{aabm}{"seak" or "nbc" or "wcvi"}
#'   \item{baseperiodfishery.name}{A selection of the names found in "48FisheryName.txt". These define the fisheries in the *.stk files.}
#'    ... }
#' @source \url{http://www.psc.org/}
#' @examples
#' data(fishery.def)
#' #or
#' data("fishery.def")
#' str(fishery.def)
"fishery.def"



#' @title Data defining each stock.
#'
#' @description A dataset containing the stock number and three letter definition.
#'
#' @format A data frame with 78 rows and 9 variables:
#' \describe{
#'   \item{StockNumber}{The integer number of each stock.}
#'   \item{Stock}{The three letter term for each stock.}
#'   \item{Description}{The full name for each stock.}
#'    ... }
#' @source \url{http://www.psc.org/}
#' @examples
#' data(stocks.key)
#' #or
#' data("stocks.key")
#' str(stocks.key)
"stocks.key"
