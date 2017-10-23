#' @title Create a new stocks.key data frame from *.STF files.
#'
#' @param stocfile.names A character vector of *.STF files.
#' @param overwriteRDA A logical, with default FALSE. If set to TRUE the current
#'   stock_key.rda will be overwritten.
#'
#' @return A data frame of the stocks found in the *.STF files. If \code{overwriteRDA} is TRUE then the stocks_key.rda file is overwritten.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "."
#' stocfile.names <- list.files(path = path, pattern = "*.STF$", full.names = TRUE)
#' stocks.key <- build_stocks.key(stocfile.names)
#' }
build_stocks.key <- function(stocfile.names, overwriteRDA=FALSE){
	data.list <- lapply(stocfile.names, FUN=function(x) readStockData(x))
	data.list.stocks <- lapply(data.list, "[[", "stocks.df")
	stocks.key <- do.call("rbind", data.list.stocks)
	stocks.key <- unique(stocks.key)
	colnames(stocks.key) <- c("StockNumber", "Description", "Stock", "start.age")

	#this next line results in the rda file being recreated:
	if(overwriteRDA) devtools::use_data(stocks.key, overwrite = TRUE)

	cat("\n A new stocks.key file has been made")
	return(stocks.key)
}#END build_stocks.key


#' @title Create new jurisdiction data frame.
#'
#' @param stocfile.names A character vector of *.STF files.
#'
#' @details This combines all stocks found in the stocfiles listed along with
#'   the currently available jurisdiction data frame. The user can then see what
#'   stocks are missing from the jurisdiction data frame and update the country
#'   and jurisdiction columns for those new stocks.
#'
#' @return A data frame comprising unique row for each stock in the *.STF files
#'   and those found in the jurisdiction.rda file.
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "."
#' stocfile.names <- list.files(path = path, pattern = "*.STF$", full.names = TRUE)
#' jurisdiction <- build_jurisdiction(stocfile.names)
#' View(jurisdiction)
#' jurisdiction[is.na(jurisdiction$stock.country),]
#' }
build_jurisdiction <- function(stocfile.names){
	data.list <- lapply(stocfile.names, FUN=function(x) readStockData(x))
	data.list.stocks <- lapply(data.list, "[[", "stocks.df")
	stocks.key <- do.call("rbind", data.list.stocks)
	stocks.key <- unique(stocks.key)
	colnames(stocks.key) <- c("StockNumber", "Description", "Stock", "start.age")
	data("jurisdiction")

	dat.tmp <- merge(jurisdiction, stocks.key[,"Stock",drop=FALSE], by.x= "stock", by.y= "Stock", all=TRUE)
	return(dat.tmp)
}#END build_jurisdiction



