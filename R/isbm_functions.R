


#' @title (ISBM/CYER) Build a mapping table to define a fishery group's country.
#'
#' @param mort.df A data frame. The element named \code{data.mort.long}, which
#'   is in the output from \code{\link{readMortalityDist}}.
#' @param fisheries.list A list containing one or more lists. Each sub-list will
#'   have the elements \code{country} and \code{fishery.term}. The
#'   \code{fishery.term} is a character string to search for in the column
#'   \code{mort.df$fisherygroup}.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' data.path <- "../data/totalmortalityDistribution"
#' mort.filenames <- list.files(data.path, pattern = ".csv$")
#' mort.filepaths <- paste(data.path, mort.filenames, sep="/")
#' mort.list <- readMortalityDist(mort.filepaths)
#' mort.df <- mort.list$data.mort.long
#' buildCMZfishery(mort.df=mort.df)
#' }
buildCMZfishery <- function(mort.df, fishery.list = list(list(country="canada", fishery.term=c("nbc", "cbc", "wcvi", "bc", "canada")), list(country="us", fishery.term=c("seak", "falcon", "wac", "puget", "us")))){

	dat.tmp <- unique(mort.df[,c("fishery.psc", "fisherygroup", "gear")])
	dat.tmp$country <- NA

	for(i in fishery.list){
		pattern.vec <- paste(tolower(i$fishery.term), collapse = "|")
		fisheries.ind <- grep(pattern.vec, tolower(dat.tmp$fisherygroup))
		dat.tmp$country[fisheries.ind] <- i$country
	}

	dat.nocountry <- dat.tmp[is.na(dat.tmp$country),]
	if(nrow(dat.nocountry)>0){
		cat("Some fishery groupings still lack a defined country.\nThey are listed here:\n")
	  print(dat.nocountry)
	}
	rownames(dat.tmp) <- NULL
	return(dat.tmp)
}#END buildCMZfishery





#' @title Calculate errors of ISBM and CYER
#'
#' @param dat A data frame. Typically the output from \code{\link{calcCYER}} or
#'   \code{\link{combinePTdata}}.
#' @param postseason An integer vector. Defines what postseason periods to
#'   evaluate. Default is 0:3, which equates to postseason years 1:4.
#' @param minyear An integer of length one. The initial year in the time series
#'   for evaluation.
#' @param maxyear An integer of length one. The final year in the time series
#'   for evaluation.
#' @param estimationyear An integer of length one. The final year in the time
#'   series.
#' @param pm A character vector. Names of the performance measures to calculate.
#'   Default tests are \code{mpe} and \code{mape}. Look to
#'   \code{\link{PBSperformance::PMs}} for details.
#'
#' @return A list of two data frames. One data frame holds the annual error
#'   estimates, the other data frame has the performance measure statistic.
#' @export
#'
#' @examples
#' \dontrun{
#' mort.list <- readMortalityDist("QUI2000_2-6_CMZ.csv")
#' mort.df <- mort.list$data.mort.long
#' mort.df$year <- mort.df$CatchYear
#' fishery.map <- buildCMZfishery(mort.df = mort.df)
#' dat.cyer <- calcCYER(mort.df = mort.df, fishery.map=fishery.map)
#' dat.cyer$index <- dat.cyer$mortality.percent/100
#' results.cyer <- calcIndexError(dat = dat.cyer)
#' }
calcIndexError <- function(dat, postseason=0:3, minyear=2000, maxyear=2013, estimationyear=2016, pm=c("mpe", "mape"), standardize=FALSE,...){

	res <- lapply(postseason, FUN = function(postseason.ind, dat){

		dat.retro <- dat[dat$evaluationyear==dat$year+postseason.ind & dat$year >= minyear & dat$year <= maxyear,]
		dat.retro$postseason <- postseason.ind+1
		dat.current <- dat[dat$evaluationyear==estimationyear & dat$year<=maxyear,]
		colnames(dat.current)[colnames(dat.current)=="index"] <- "index.current"
		dat.anal <- merge(dat.retro, dat.current[,c('year', "agerange", "stock", "index.current")], by=c('year', "agerange", "stock"))
	}, dat)

	res.comb <- do.call("rbind", res)

####the following is a way to manage for missing files. This makes sure there are no gaps by year.
	res.comb$key <- paste(res.comb$year, res.comb$agerange, res.comb$stock, res.comb$evaluationyear, res.comb$postseason, sep="_")

	res.comb.options <- expand.grid(year=unique(res.comb$year), agerange=unique(res.comb$agerange), stock=unique(res.comb$stock), postseason=unique(res.comb$postseason))
	res.comb.options$evaluationyear <- res.comb.options$year+res.comb.options$postseason-1

	res.comb.options$key <- paste(res.comb.options$year, res.comb.options$agerange, res.comb.options$stock, res.comb.options$evaluationyear, res.comb.options$postseason, sep="_")

	res.comb.options <- res.comb.options[res.comb.options$year<=res.comb.options$evaluationyear,]
	missing.colnames <- colnames(res.comb)[! colnames(res.comb) %in%	colnames(res.comb.options)]
	df <- data.frame(matrix(vector(), nrow(res.comb.options), length(missing.colnames),
	                       dimnames=list(c(), missing.colnames)),
	                stringsAsFactors=F)

	res.comb.options <- data.frame(res.comb.options, df)

	#check for missing index:
	res.comb.options.missing <- res.comb.options[! res.comb.options$key %in% res.comb$key,]
	res.comb <- rbind(res.comb, res.comb.options.missing)
####END of appending missing years


	res.actual <-  with(res.comb, by(res.comb, list(stock, agerange, postseason), FUN = function(res.comb.subset){

	  #standardize the data if requested:
  	if(standardize){
  	  res.comb.subset$index <- calcZscore(res.comb.subset$index)
  	  res.comb.subset$index.current <- calcZscore(res.comb.subset$index.current)
    }

		pm.res <- lapply(pm, FUN = function(pm.ind, res.comb.subset){
			do.call(pm.ind, list(expect =  res.comb.subset$index, obs = res.comb.subset$index.current, layman=TRUE))
		}, res.comb.subset)

		return(list(stock=unique(res.comb.subset$stock), agerange=unique(res.comb.subset$agerange), postseason=unique(res.comb.subset$postseason),year=res.comb.subset$year, pm=pm.res, res.comb.subset=res.comb.subset))
	}
	))

  if(standardize) {
    #the standardized data are in res.actual so move them to res.comb
    res.actual.tmp <- lapply(res.actual,"[[", "res.comb.subset")
    res.comb <- do.call("rbind", res.actual.tmp)
  }

	res.long.list <- lapply(res.actual, function(res.actual.ind){
		lapply(res.actual.ind$pm, function(pm.ind, res.actual.ind){

			pm.type <- names(pm.ind)[2]

			errors.df <- data.frame(stock=res.actual.ind$stock, agerange=res.actual.ind$agerange, postseason=res.actual.ind$postseason,  pm.type=pm.type, year=res.actual.ind$year, errors=pm.ind$errors, stringsAsFactors = FALSE)

			pm.stats.df <- data.frame(stock=res.actual.ind$stock, agerange=res.actual.ind$agerange, postseason=res.actual.ind$postseason,  pm.type=pm.type,  pm.stat=pm.ind[[2]], stringsAsFactors = FALSE)
			return(list(errors.df=errors.df, pm.stats.df=pm.stats.df))
		}, res.actual.ind)
	})

  #extract errors:
	errors.list <- lapply(res.long.list, FUN=function(x) {
				lapply(x, "[[","errors.df")
	})
	errors.list <- lapply(errors.list, function(x) do.call("rbind", x))
	errors.df.long <- do.call("rbind", errors.list)

	errors.df.long$col.name <- paste(errors.df.long$postseason, errors.df.long$pm.type, sep="_")

	#now get pm stats:
	pm.stats.list <- lapply(res.long.list, FUN=function(x) {
		lapply(x, "[[","pm.stats.df")
	})
	pm.stats.list <- lapply(pm.stats.list, function(x) do.call("rbind", x))
	pm.stats.df.long <- do.call("rbind", pm.stats.list)

	pm.stats.df.long$col.name <- paste(pm.stats.df.long$postseason, pm.stats.df.long$pm.type, sep="_")

	return(list(res.comb=res.comb, errors.df.long=errors.df.long, pm.stats.long=pm.stats.df.long))

}#END calcIndexError




#' (ISBM/CYER) Calculate CYER and its index.
#'
#' @param mort.df  A data frame. The element named \code{data.mort.long}, which
#'   is in the output from \code{\link{readMortalityDist}}.
#' @param fishery.map A data frame. Output from \code{\link{buildCMZfishery}}.
#'
#' @return A data frame with the estimates of catch year exploitation rate and
#'   index.
#' @export
#'
#' @examples
#' \dontrun{
#' data.path <- "../data/totalmortalityDistribution/qui_2-6"
#' mort.filenames <- list.files(data.path, pattern = ".csv$")
#' mort.filepaths <- paste(data.path, mort.filenames, sep="/")
#' mort.list <- readMortalityDist(mort.filepaths)
#' mort.df <- mort.list$data.mort.long
#' mort.df$year <- mort.df$CatchYear
#' fishery.map <- buildCMZfishery(mort.df = mort.df)
#' calcCYER(mort.df, fishery.map)
#' }
calcCYER <- function(mort.df, fishery.map, country=c("canada", "us")){
	country <- match.arg(country)

	mort.df <- merge(mort.df, fishery.map[, c("fishery.psc", "fisherygroup", 'gear', "country")], by=c("fishery.psc", "fisherygroup", 'gear'))
	mort.df <- mort.df[mort.df$MortType=="TM",]

	cyer.df <- aggregate(mortality.percent~year+stock+agerange+evaluationyear, data=mort.df[mort.df$fishery.psc %in% c("ISBM", "Terminal") & tolower(mort.df$country)==tolower(country),], FUN = function(x){
		if(all(is.na(x))) {
			#if a complete year of TM data are NAs, this makes sure a result is still returned
			NA
		}else{
			sum(x, na.rm = TRUE)
		}
		}

		, drop=TRUE, na.action = na.pass)

	return(cyer.df)

}#END calcCYER



#' @title Calculate the Catch year exploitation rate index (CYER index).
#'
#' @param cyer.df A data frame. Output from \code{\link{calcCYER}}
#'
#' @return A data frame with same structure as the argument \code{cyer.df} with
#'   the addition of a column: \code{cyer.index}.
#' @export
#'
#' @examples
#' \dontrun{
#' calcCYERindex(cyer.df)
#' }
calcCYERindex <- function(cyer.df){
stop("function incomplete")

	cyer.df <- aggregate(mortality.percent~year+stock+agerange+evaluationyear, data=mort.df[mort.df$fishery.psc %in% c("ISBM", "Terminal") & mort.df$country=="ca",], FUN = "sum")

	cyer.bp.df <- aggregate(mortality.percent~stock+agerange+evaluationyear, data=cyer.df[cyer.df$year>=1979 & cyer.df$year<=1982,], FUN = "mean")
	colnames(cyer.bp.df)[colnames(cyer.bp.df)=="mortality.percent"] <- "bp.mean"

	cyer.df <- merge(cyer.df, cyer.bp.df, by=c("stock", "agerange", "evaluationyear"))
	cyer.df$cyer.index <- cyer.df$mortality.percent/cyer.df$bp.mean
	return(cyer.df)

}#END calcCYERindex



#' @title Clean *cmz.csv files for easy importing.
#'
#' @param filenames  A character vector of length one or more. The names of the
#'   *CMZ.csv files.
#'
#' @return Nothing returned. The files listed in filenames are overwritten with
#'   the cleaned CMZ files
#' @export
#'
#' @examples
#' \dontrun{
#'data.path <- '../data/'
#'mort.filenames <- list.files(data.path, pattern = '.csv$')
#'mort.filepaths <- paste(data.path, mort.filenames, sep='/')
#'cleanCMZ(mort.filepaths)
#' }
cleanCMZ <- function(filenames){
	invisible(lapply(filenames, function(filename.ind){

		dat.tmp <- readLines(filename.ind)
		notelines.ind <- grep(pattern = "note:", x = tolower(dat.tmp))
		if(length(notelines.ind)>0) dat.tmp <- dat.tmp[-notelines.ind]

		dat.tmp <- gsub(pattern = "\"",replacement = "", dat.tmp)

		write(x = dat.tmp, file = filename.ind)
	}))
}#END cleanCMZ



#' @title (ISBM/CYER) Combine the data from multiple .pt files.
#'
#' @param pt.list A list. The output from \code{\link{readPT}}.
#'
#' @return A list of three data frames named \code{data.isbm.long},
#'   \code{data.isbm.ages}, and  \code{data.bper.long}. Each data frame is in
#'   long format and has the same strucutre as the long format data frames
#'   produced by \code{\link{readPT}}, but each data frame includes a
#'   \code{cwtstock} column as all stocks are combined into the same data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' data.path <- "../data/isbm"
#' pt.filenames <- list.files(data.path, pattern = ".PT$")
#' pt.filepaths <- paste(data.path, pt.filenames, sep="/")
#' pt.list <- readPT(pt.filepaths)
#' pt.list.long <- combinePTdata(pt.list)
#' names(pt.list.long)
#' }
combinePTdata <- function(pt.list){
	data.isbm.long <- lapply(pt.list, "[[", "data.isbm.long")
	for(i in 1:length(data.isbm.long)){
		data.isbm.long[[i]]$cwtstock <- names(data.isbm.long)[i]
	}
	data.isbm.long <- do.call("rbind", data.isbm.long)

	data.isbm.ages <- lapply(pt.list, "[[", "data.isbm.ages")
	for(i in 1:length(data.isbm.ages)){
		data.isbm.ages[[i]]$cwtstock <- names(data.isbm.ages)[i]
	}
	data.isbm.ages <- do.call("rbind", data.isbm.ages)

	data.bper.long <- lapply(pt.list, "[[", "data.bper.long")
	for(i in 1:length(data.bper.long)){
		data.bper.long[[i]]$cwtstock <- names(data.bper.long)[i]
	}
	data.bper.long <- do.call("rbind", data.bper.long)

	return(list(data.isbm.long=data.isbm.long, data.isbm.ages=data.isbm.ages, data.bper.long=data.bper.long))
}#END combinePTdata




#' @title (ISBM/CYER) Import motality distribution CMZ (*.csv) files.
#'
#' @param filenames A character vector of length one or more. The names of the
#'   *CMZ.csv files.
#'
#' @return A list comprising two data frames. The first data frame
#'   (\code{data.mort.wide}) has the same structure as found in the csv file.
#'   The second data frame (\code{data.mort.long}) is reshaped to a long format
#'   and the fishery column names are parsed into additional columns.
#' @export
#'
#' @examples
#' \dontrun{
#' mort.list <- readMortalityDist(mort.filepaths)
#' mort.df <- mort.list$data.mort.long
#' View(mort.list[mort.df$fishery.psc=="ISBM",])
#' }
readMortalityDist <- function(filenames, ...){

	mort.list <- lapply(filenames, FUN = function(filename.x, agerange=NA){

			data.mort <- read.csv(filename.x, stringsAsFactors = FALSE)
			data.mort <- subset(data.mort, select = -X)
			data.mort.long <- reshape(data.mort, dir="long", varying=list(4:ncol(data.mort)), timevar = "cmz.column",  v.names = "mortality.percent")
			data.mort.long$cmz.column <-  attributes(data.mort.long)$reshapeLong$varying[[1]][data.mort.long$cmz.column]
			data.mort.long <- subset(data.mort.long, select = -id)
			data.mort.long$fishery.psc <- substr(data.mort.long$cmz.column, 1, unlist(regexpr(pattern = "\\.", data.mort.long$cmz.column))-1)

			underscore.ind <- gregexpr("_", data.mort.long$cmz.column)
			underscore.ind <- do.call(rbind, underscore.ind)
			data.mort.long$fisherygroup <- substr(data.mort.long$cmz.column, underscore.ind[,1]+1, underscore.ind[,2]-1)
			#data.mort.long$aabm <- substr(data.mort.long$cmz.column, underscore.ind[,1]+1, underscore.ind[,2]-1)
			#data.mort.long$aabm[data.mort.long$fishery.psc!="AABM"] <- NA

			#data.mort.long$isbm <- substr(data.mort.long$cmz.column, underscore.ind[,1]+1, underscore.ind[,2]-1)
			#data.mort.long$isbm[data.mort.long$fishery.psc!="ISBM"] <- NA

			data.mort.long$gear <- substr(data.mort.long$cmz.column, nchar(data.mort.long$cmz.column), nchar(data.mort.long$cmz.column))

			#if agerange isn't supplied in the argument then try to find it in filename

			if(!is.na(agerange)){
			  data.mort.long$agerange <- agerange
			} else {
			  #extract the filename as it might include a path:
			  foreslash.ind <- max(1,1+unlist(gregexpr(pattern = "/", filename.x)))
			  filename.tmp <- substring(filename.x, foreslash.ind, nchar(filename.x))

			  #perferred pattern to search for:
			  search.ind <- regexpr(pattern = "_[2-6]-[2-6]", filename.tmp)
  			if(search.ind>0){
  			   data.mort.long$agerange <- substr(filename.tmp, search.ind+1, search.ind+3)
  			}else{
  			  #second pattern to search for:
  			  search.ind <- regexpr(pattern = "[2-6]-[2-6]", filename.tmp)
  			  if(search.ind>0){
  			    data.mort.long$agerange <- substr(filename.tmp, search.ind, search.ind+2)
  			  }else{
  			    data.mort.long$agerange <- NA
  			  }
			}
			}

			data.mort.long$evaluationyear <- max(data.mort.long$CatchYear, na.rm = TRUE)

			return(list(data.mort.wide=data.mort, data.mort.long=data.mort.long))
	}, ...)#END lapply

	names(mort.list) <- filenames

	data.mort.long.list <- lapply(mort.list, "[[", "data.mort.long")
	data.mort.long <- do.call('rbind', data.mort.long.list)

	return(list(data.mort.long=data.mort.long, data.mort.byfile=mort.list))

}#END readMortalityDist




#' @title (ISBM/CYER) Import one or more ISBM index (*.pt) files.
#'
#' @param filenames A character vector. The names of the *.pt files for import.
#'
#' @return A list comprising elements that are also lists, one for each .pt file
#'   imported. Each sub list comprises six elements. Their names are:
#'   \code{metadata}, \code{data.isbm}, \code{data.isbm.long},
#'   \code{data.isbm.ages}, \code{data.bper}, \code{data.bper.long}. The element
#'   \code{metadata} is a list of seven named vectors that include all the same
#'   information at the top of each .pt file. The element \code{data.isbm} is a
#'   data frame of the ISBM index data in the same format as found in the .pt
#'   file. The element \code{data.isbm.long} is a data frame and comprises the
#'   same ISBM index data reshaped into the more useful 'long' format. The age
#'   data are exluded from \code{data.isbm.long} but can be found in
#'   \code{data.isbm.ages}, which is a data frame in long format. The element
#'   \code{data.bper} is data frame of the base perior exploitation rate data
#'   (BPER), in the same format as found in the .pt file. The element
#'   \code{data.bper.long} is the same BPER data but in long format.
#' @export
#'
#' @examples
#' \dontrun{
#' data.path <- "../data/isbm"
#' pt.filenames <- list.files(data.path, pattern = ".PT$")
#' pt.filepaths <- paste(data.path, pt.filenames, sep="/")
#' pt.list <- readPT(pt.filepaths)
#' }
readPT <- function(filenames){
	pt.list <- lapply(filenames, FUN = function(filename.x){

			dat.tmp <- readLines(filename.x)
			data.rowindex <- vector(mode = "integer", length = 0)
			data.rowindex[1] <- grep(pattern = "^ YR",dat.tmp )
			data.rowindex[2] <- grep(pattern = "^BASE PERIOD EXPLOIT RATES",dat.tmp )
			data.meta <- dat.tmp[1:(data.rowindex[1]-1)]
			data.meta <- data.meta[data.meta !=""]
			data.metaindex <- grep(pattern = ":",data.meta)

			data.meta.list <- lapply(data.metaindex, FUN = function(x, data.meta){

				colon.ind <- regexpr(pattern = ":", data.meta[x])
				element.name <- substr(x = data.meta[x], start = 1, stop = colon.ind-1 )
				element.value <- trimws(substr(x = data.meta[x], start = colon.ind+1, nchar(data.meta[x]) ))
				results <- element.value
				names(results) <- element.name
				return(results)
			}, data.meta)
			names(data.meta.list) <- trimws(unlist(lapply(data.meta.list, FUN = names)))

			comment.index <- (1:length(data.meta))[! (1:length(data.meta)) %in% data.metaindex]
			comment.value <- data.meta[comment.index]
			comment.value <- paste(trimws(comment.value), collapse = " ")
			data.meta.list$comment <- comment.value

			underscore.ind <- max(unlist(gregexpr(pattern = "_", filename.x)))
			data.meta.list$agerange <- substr(filename.x, underscore.ind+1, underscore.ind+3)

			#check if there are US data:
			usdata.bol <- grepl("US", toupper(dat.tmp[data.rowindex[1]]))
			if(usdata.bol){
				widths <- diff(c(0, 4, 7, 10, 13, 16, 23, 37))
				colnames.vec <- c("canada", "us")
			}else{
				widths <- diff(c(0, 4, 7, 10, 13, 16, 23))
				colnames.vec <- c("canada")
			}

			data.isbm <- read.fwf(filename.x, header = FALSE, widths= widths, skip = data.rowindex[1], nrows = data.rowindex[2]-2-data.rowindex[1], stringsAsFactors=FALSE)
			colnames(data.isbm) <- c("year", paste0("age.index", 1:4), colnames.vec)

			#exclude the age columns:

			agecol.ind <- grep(pattern = "age.index",colnames(data.isbm))
			data.isbm.tmp <- data.isbm[,c(! 1:ncol(data.isbm) %in% agecol.ind)]
			data.isbm.long <- reshape(data.isbm.tmp, dir="long", varying = list(2:ncol(data.isbm.tmp)), timevar = "country", v.names = "isbm.index")
			data.isbm.long$country <- colnames.vec[data.isbm.long$country]
			data.isbm.long$evaluationyear <- max(data.isbm.long$year)
			data.isbm.long$agerange <- data.meta.list$agerange
			data.isbm.long <- subset(data.isbm.long, select = -id)

			data.isbm.ages <- reshape(data.isbm[,c(1, agecol.ind)], dir="long", varying = agecol.ind)
			data.isbm.ages <- data.isbm.ages[!is.na(data.isbm.ages$age), c("year", "age")]
			data.isbm.ages <- data.isbm.ages[order(data.isbm.ages$year, data.isbm.ages$age),]
			data.isbm.ages$evaluationyear <- max(data.isbm.ages$year)
			data.isbm.ages$agerange <- data.meta.list$agerange

			data.meta.list$evaluationyear <- max(data.isbm.ages$year)

			data.bper <- read.fwf(filename.x, header=FALSE, skip=data.rowindex[2], widths= diff(c(0, 3, 29, 36, 45, 54, 63)), stringsAsFactors=FALSE)
			colnames(data.bper) <- c("fishery.index", "fishery.name", paste0("bper", 1:4))
			data.bper$fishery.name <- trimws(data.bper$fishery.name)

			data.bper.long <- reshape(data.bper, dir="long", varying = list(3:ncol(data.bper)), timevar = "bper.index",  v.names = "bper")
			data.bper.long <- data.bper.long[, c("fishery.index", "fishery.name", "bper.index", "bper")]

			return(list(metadata=data.meta.list, data.isbm=data.isbm, data.isbm.long=data.isbm.long, data.isbm.ages=data.isbm.ages, data.bper=data.bper, data.bper.long=data.bper.long))
	})#END lapply

	names(pt.list) <- unlist(lapply(lapply(pt.list, "[[", "metadata"), "[[", "CWT STOCK"))
	return(pt.list)

}#END readPT



#' @title (ISBM/CYER) Import one or more ISBM index files, in step11 csv format.
#'
#' @param filenames A character vector. The names of the *step11*.csv files for
#'   import.
#' @param agerange A character vector of length one. This overrides the age
#'   range value declared in the file name. e.g. "2-6"
#'
#' @return A list comprising elements that are also lists, one for each .csv
#'   file imported. Each sub list comprises six elements. Their names are:
#'   \code{metadata}, \code{data.isbm}, \code{data.isbm.long},
#'   \code{data.isbm.ages}, \code{data.bper}, \code{data.bper.long}. The element
#'   \code{metadata} is a list of seven named vectors that include all the same
#'   information at the top of each .pt file. The element \code{data.isbm} is a
#'   data frame of the ISBM index data in the same format as found in the .pt
#'   file. The element \code{data.isbm.long} is a data frame and comprises the
#'   same ISBM index data reshaped into the more useful 'long' format. The age
#'   data are exluded from \code{data.isbm.long} but can be found in
#'   \code{data.isbm.ages}, which is a data frame in long format. The element
#'   \code{data.bper} is data frame of the base perior exploitation rate data
#'   (BPER), in the same format as found in the .pt file. The element
#'   \code{data.bper.long} is the same BPER data but in long format.
#' @export
#'
#' @examples
#' \dontrun{
#' data.path <- "../data/isbm"
#' pt.filenames <- list.files(data.path, pattern = "*step11*.csv$")
#' pt.filepaths <- paste(data.path, pt.filenames, sep="/")
#' pt.list <- readPTfromstep11csv(pt.filepaths)
#' }
readPTfromstep11csv <- function(filenames, agerange=NA, country=c("canada", "us")){
  pt.list <- lapply(filenames, FUN = function(filename.x){


    #extract the filename as it might include a path:
    foreslash.ind <- max(1,1+unlist(gregexpr(pattern = "/", filename.x)))
    filename.tmp <- substring(filename.x, foreslash.ind, nchar(filename.x))

    #if agerange isn't supplied in the argument then try to find it in filename:
    if(!is.na(agerange)){
      data.mort.long$agerange <- agerange
    } else {
      #perferred pattern to search for:
      search.ind <- regexpr(pattern = "_[2-6]-[2-6]", filename.tmp)
      if(search.ind>0){
        agerange <- substr(filename.tmp, search.ind+1, search.ind+3)
      }else{
        #second pattern to search for:
        search.ind <- regexpr(pattern = "[2-6]-[2-6]", filename.tmp)
        if(search.ind>0){
          agerange <- substr(filename.tmp, search.ind, search.ind+2)
        }else{
          agerange <- NA
        }
      }
    }#END agerange
    data.meta.list <- vector(mode = "list")
    data.meta.list$"CWT STOCK" <- substr(filename.tmp, 1,3)
    data.meta.list$agerange <- agerange
    data.meta.list$evaluationyear <- NA

    data.isbm <- read.csv(filename.x, header = TRUE,  skip = 1, stringsAsFactors=FALSE)
    if(country=="canada"){
      data.isbm.long <- data.isbm[,c("YR", "Canada_ISBM")]
      data.isbm.long$isbm.index <- data.isbm.long$Canada_ISBM
    }else if(country=="us"){
      data.isbm.long <- data.isbm[,c("YR", "US_ISBM")]
      data.isbm.long$isbm.index <- data.isbm.long$US_ISBM
      
    }  

    data.isbm.long$country <- country
    
    data.isbm.long$year <- data.isbm.long$YR+1900
    data.isbm.long$evaluationyear <- max(data.isbm.long$year, na.rm = TRUE)
    data.meta.list$evaluationyear <- max(data.isbm.long$year, na.rm = TRUE)

    data.isbm.long$agerange <- data.meta.list$agerange

    data.isbm.long <- data.isbm.long[,c("year", "country", "isbm.index", "evaluationyear", "agerange")]

    return(list(metadata=data.meta.list, data.isbm=data.isbm, data.isbm.long=data.isbm.long, data.isbm.ages=NULL, data.bper=NULL, data.bper.long=NULL))
  })#END lapply

  names(pt.list) <- unlist(lapply(lapply(pt.list, "[[", "metadata"), "[[", "CWT STOCK"))
  return(pt.list)

}#END readPTfromstep11csv



#' @title (ISBM/CYER) Build, save, and open an R script to help compare ISBM &
#'   CYER indices.
#'
#' @description This creates and opens a script named "isbm_script.R". This is a
#'   template for the user to work with when doing comparison of ISBM and CYER
#'   indices. This is intended to help the user understand the work flow and due
#'   to file path differences, is unlikely to work as is. Some object values
#'   will need updating (for example the data path).
#'
#' @return Opens an R script that includes the functions to compare indices.
#'
#' @export
#'
#' @examples
#' writeScriptISBM()
writeScriptISBM <- function(){

	script.str <- c("

####### SETUP #######
rm(list=ls())

###### COMMENTS ########

#the data are rounded to 3 decimal places before calculations as that's what was
#done in the spreadsheed version by Antonio.

###### PACKAGES ########

#devtools::install_github('MichaelFolkes/ctctools')

require(ctctools)
require(PBSperformance)
####### DATA #######

country <- 'canada'

#ISBM data:

data.path <- '../data/qui'
pt.filenames <- list.files(data.path, pattern = '.PT$')
pt.filepaths <- paste(data.path, pt.filenames, sep='/')
pt.list <- readPT(pt.filepaths)
pt.list.long <- combinePTdata(pt.list)

dat.pt <- pt.list.long$data.isbm.long
dat.pt <- dat.pt[dat.pt$country==country,]
dat.pt <- pt.list.long$data.isbm.long[pt.list.long$data.isbm.long$country==country,]
dat.pt$stock <- dat.pt$cwtstock
dat.pt$index <- dat.pt$isbm.index
dat.pt$index <- round(dat.pt$index,3)

# Mortality Distribution data:

# data.path <- '../data/totalmortalityDistribution/qui_modified'
mort.filenames <- list.files(data.path, pattern = '.csv$')
mort.filepaths <- paste(data.path, mort.filenames, sep='/')
cleanCMZ(mort.filepaths)
mort.list <- readMortalityDist(mort.filepaths)
mort.df <- mort.list$data.mort.long
mort.df$year <- mort.df$CatchYear

####### MAIN #######

fishery.map <- buildCMZfishery(mort.df = mort.df)
#the csv mortality distribution files don't contain the cyer values, so calc:
dat.cyer <- calcCYER(mort.df = mort.df, fishery.map=fishery.map, country=country)
dat.cyer$index <- dat.cyer$mortality.percent/100
dat.cyer$index <- round(dat.cyer$index,3)

#results based on non-standardized data:
results.cyer <- calcIndexError(dat = dat.cyer, pm = c('mpe', 'mape'))
results.cyer$errors.df.long$index.type <- 'cyer'
results.pt <- calcIndexError(dat = dat.pt, pm = c('mpe', 'mape'), standardize = FALSE)
results.pt$errors.df.long$index.type <- 'isbm'
results.combined.actual <- rbind(results.cyer$errors.df.long, results.pt$errors.df.long)
results.combined.actual$data.type <- 'actual'

results.pt$res.comb$index.type <- 'isbm'
results.pt$res.comb$data.type <- 'actual'
results.cyer$res.comb$index.type <- 'cyer'
results.cyer$res.comb$data.type <- 'actual'
colnames.common <- colnames(results.pt$res.comb)[colnames(results.pt$res.comb) %in% colnames(results.cyer$res.comb)]
dat.combined.actual <- rbind(results.pt$res.comb[,colnames.common], results.cyer$res.comb[,colnames.common])


#results based on standardized data:
results.cyer <- calcIndexError(dat = dat.cyer, pm = c('mse', 'mae', 'mre'), standardize = TRUE)
results.cyer$errors.df.long$index.type <- 'cyer'
results.pt <- calcIndexError(dat = dat.pt, pm = c('mse', 'mae', 'mre'), standardize = TRUE)
results.pt$errors.df.long$index.type <- 'isbm'
results.combined.standardized <- rbind(results.cyer$errors.df.long, results.pt$errors.df.long)
results.combined.standardized$data.type <- 'standardized'

results.combined <- rbind(results.combined.actual, results.combined.standardized)

results.pt$res.comb$index.type <- 'isbm'
results.pt$res.comb$data.type <- 'standardized'
results.cyer$res.comb$index.type <- 'cyer'
results.cyer$res.comb$data.type <- 'standardized'
colnames.common <- colnames(results.pt$res.comb)[colnames(results.pt$res.comb) %in% colnames(results.cyer$res.comb)]
dat.combined.standardized <- rbind(results.pt$res.comb[,colnames.common], results.cyer$res.comb[,colnames.common])


#summary stats:
grandstats <- aggregate(errors~stock+agerange+year+pm.type+index.type+data.type, data=results.combined, sum)

grandstats <- grandstats[order(grandstats$data.type, grandstats$index.type, grandstats$pm.type, grandstats$agerange),]

grandstats.means <- aggregate(errors~stock+agerange+pm.type+index.type+data.type, data=grandstats, mean)
#floating point issue with the means - if value is less than 10^-15 then make it zero.
grandstats.means$errors[grandstats.means$errors<10^-15] <- 0

#### WRITE RESULTS:

#write the grand stats results to files, grouped by stock and age grouping:
with(grandstats, by(grandstats, list(stock, agerange, index.type), FUN=function( grandstats.sub){
grandstats.sub$col.name <- paste(grandstats.sub$data.type, grandstats.sub$pm.type, sep='_')

table.wide <- reshape(grandstats.sub[,c('year', 'pm.type', 'data.type', 'errors', 'col.name')], direction = 'wide', idvar = 'year',  timevar = 'col.name', drop = c('pm.type', 'data.type') )

col.rename.ind <- grep(pattern = 'errors', colnames(table.wide))
colnames(table.wide)[col.rename.ind] <- sub( pattern =  'errors.', replacement='', colnames(table.wide)[col.rename.ind])
filename <- paste('grandstats_3decimals', unique(grandstats.sub$stock), unique(grandstats.sub$agerange), unique(grandstats.sub$index.type), '.csv', sep='_')
write.csv(
table.wide[,c('year', 'actual_mpe', 'actual_mape', 'standardized_mse', 'standardized_mae','standardized_mre')], file = filename, row.names = FALSE
)
}))

#append the means to the bottom of each csv:
with(grandstats.means, by(grandstats.means, list(stock, agerange, index.type), FUN=function( grandstats.means.sub){

grandstats.means.sub$col.name <- paste(grandstats.means.sub$data.type, grandstats.means.sub$pm.type, sep='_')
grandstats.means.sub$year <- 'Average'
table.wide <- reshape(grandstats.means.sub[,c('year', 'errors', 'col.name')], direction = 'wide', idvar = 'year', timevar = 'col.name')
col.rename.ind <- grep(pattern = 'errors', colnames(table.wide))
colnames(table.wide)[col.rename.ind] <- sub( pattern =  'errors.', replacement='', colnames(table.wide)[col.rename.ind])
filename <- paste('grandstats_3decimals', unique(grandstats.means.sub$stock), unique(grandstats.means.sub$agerange), unique(grandstats.means.sub$index.type), '.csv', sep='_')

write.table(
table.wide[,c('year', 'actual_mpe', 'actual_mape', 'standardized_mse', 'standardized_mae','standardized_mre')], file = filename, row.names = FALSE, append = TRUE, sep=',', col.names = FALSE
)
}))


##### SAVE DATA ####
dat.cyer$index.type <- 'cyer'
dat.pt$index.type <- 'isbm'
colnames.common <- colnames(dat.cyer)[colnames(dat.cyer) %in% colnames(dat.pt)]
dat.combined <- rbind(dat.cyer[,colnames.common], dat.pt[,colnames.common])
dat.combined <- dat.combined[order(dat.combined$stock, dat.combined$agerange, dat.combined$index.type, dat.combined$year),]

dat.combined2 <- rbind(dat.combined.actual, dat.combined.standardized)

save(dat.combined, dat.combined2, results.combined, grandstats, grandstats.means, file = 'isbm_cyer_analysis.Rdata')

##### LATTICE PLOTS ####

require(lattice)

###time series:
with(dat.combined2, by(dat.combined2, list( agerange, stock), FUN = function(dat.combined2.sub){

filename <- paste('timeseries', 'currentEstimate', 'stock', unique(dat.combined2.sub$stock), 'agerange', unique(dat.combined2.sub$agerange),  '.png', sep = '_')

png(filename =filename, height = 3, width = 8, units = 'in', res = 600 )
print(xyplot(index.current~year|data.type, groups=index.type, data=dat.combined2.sub[ dat.combined2.sub$year>=2000 & dat.combined2.sub$year<=2013 & dat.combined2.sub$postseason==1,],  auto.key = TRUE,  scales=list(relation='free', alternating=FALSE), ylab = 'Index value', xlab = 'Calendar Year', as.table=TRUE, type='b', cex=0.5 ))
dev.off()
}
))

###index plots:
with(dat.combined2, by(dat.combined2, list(postseason, agerange, stock), FUN = function(dat.combined2.sub){

filename <- paste('retroIndex_vs_currentIndex', 'stock', unique(dat.combined2.sub$stock), 'agerange', unique(dat.combined2.sub$agerange), 'postseason', unique(dat.combined2.sub$postseason), '.png', sep = '_')

png(filename =filename, height = 4, width = 8, units = 'in', res = 600 )
print(xyplot(index~index.current|factor(index.type, levels = c('isbm', 'cyer'), labels = toupper(c('isbm', 'cyer'))), data=dat.combined2.sub, as.table=TRUE,
scales=list(relation='free', alternating=FALSE),
xlab = 'Current Index', ylab = 'Retrospective Index',
main = paste(unique(dat.combined2.sub$stock), ', Age range: ', unique(dat.combined2.sub$agerange), ', Postseason: ', unique(dat.combined2.sub$postseason), sep=''),
panel = function(x,y){
panel.abline(a=0,b=1, col='grey')
panel.points(x,y)
}))
dev.off()

}))


###errors plots:
pm.rename <- data.frame(pm.type=c('mpe', 'mape', 'mse', 'mae',  'mre'), pm.error.name=c('Percent Error',  'Absolute Percent Error', 'Squared Error', 'Absolute Error',  'Raw Error'), stringsAsFactors = FALSE)
results.combined <- merge(results.combined, pm.rename, by='pm.type')
results.combined <- results.combined[order(results.combined$pm.type, results.combined$stock, results.combined$agerange, results.combined$postseason, results.combined$year),]

with(results.combined, by(results.combined, list(postseason, agerange, stock, data.type), FUN = function(results.combined.sub){

filename <- paste('errortimeseries', 'stock', unique(results.combined.sub$stock), 'agerange', unique(results.combined.sub$agerange), 'postseason', unique(results.combined.sub$postseason),  'data.type', unique(results.combined.sub$data.type),'.png', sep = '_')
panel.count <- length(unique(results.combined.sub$pm.type))

png(filename =filename, height = 3*panel.count, width = 8, units = 'in', res = 600 )
print(xyplot(errors~year|pm.error.name, groups=index.type, data=results.combined.sub, as.table=TRUE, type='b', auto.key = TRUE,
layout=c(1, panel.count),
scales=list(relation='free', alternating=FALSE, y=list(rot=45)),
xlab = 'Calendar Year', ylab = 'Error Value',
main = paste('Data: ', unique(results.combined.sub$data.type), ', Stock: ',  unique(results.combined.sub$stock), ', Age range: ', unique(results.combined.sub$agerange), ', Postseason: ', unique(results.combined.sub$postseason), sep='')
))
dev.off()

}))


###errors plots, grouped by PM type:

with(results.combined, by(results.combined, list(pm.type, agerange, stock, data.type), FUN = function(results.combined.sub){

filename <- paste('errortimeseries_groupedByPM', 'stock', unique(results.combined.sub$stock), 'agerange', unique(results.combined.sub$agerange), 'data.type', unique(results.combined.sub$data.type),'pm.type', unique(results.combined.sub$pm.type),  '.png', sep = '_')
panel.count <- 4 # length(unique(results.combined.sub$pm.type))

png(filename =filename, height = 2*panel.count, width = 8, units = 'in', res = 600 )
print(xyplot(errors~year|factor(paste0('Postseason: ', postseason)), groups=index.type, data=results.combined.sub, as.table=TRUE, type='b', auto.key = TRUE,
layout=c(1, panel.count),
scales=list( alternating=FALSE, y=list(rot=45)),
xlab = 'Calendar Year', ylab = 'Error Value',
main = paste('Data: ', unique(results.combined.sub$data.type), ', Stock: ',  unique(results.combined.sub$stock), ', Age range: ', unique(results.combined.sub$agerange), ', PM: ', unique(results.combined.sub$pm.error.name), sep='')
))
dev.off()

}))

####### END #######
")
	write(script.str, file="isbm_script.R")
	file.edit("isbm_script.R" )
}#END writeScriptISBM


