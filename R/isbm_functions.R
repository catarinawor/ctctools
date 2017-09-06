


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
buildCMZfishery <- function(mort.df, fishery.list = list(list(country="ca", fishery.term=c("nbc", "cbc", "wcvi", "bc", "canada")), list(country="us", fishery.term=c("seak", "falcon", "wac", "puget", "us")))){

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
#' dat.cyer$index <- dat.cyer$mortality.prop/100
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



	res.actual <-  with(res.comb, by(res.comb, list(stock, agerange, postseason), FUN = function(res.comb.subset){

	  #standardize the data if requested:
  	if(standardize){
  	  res.comb.subset$index <- calcZscore(res.comb.subset$index)
  	  res.comb.subset$index.current <- calcZscore(res.comb.subset$index.current)
    }


		pm.res <- lapply(pm, FUN = function(pm.ind, res.comb.subset){
		  browser()
			do.call(pm.ind, list(expect =  res.comb.subset$index, obs = res.comb.subset$index.current, layman=TRUE))
		}, res.comb.subset)
		#res.mpe <- mpe(expect =  x$index, obs = x$index.current, layman=TRUE)
		#res.mape <- mape(expect =  x$index, obs = x$index.current)
		return(list(stock=unique(res.comb.subset$stock), agerange=unique(res.comb.subset$agerange), postseason=unique(res.comb.subset$postseason),year=res.comb.subset$year, pm=pm.res))
	}
	))

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

	return(list(errors.df.long=errors.df.long, pm.stats.long=pm.stats.df.long))

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
calcCYER <- function(mort.df, fishery.map){

	mort.df <- merge(mort.df, fishery.map[, c("fisherygroup", 'gear', "country")], by=c("fisherygroup", 'gear'))
	mort.df <- mort.df[mort.df$MortType=="TM",]


	cyer.df <- aggregate(mortality.prop~year+stock+agerange+evaluationyear, data=mort.df[mort.df$fishery.psc %in% c("ISBM", "Terminal") & mort.df$country=="ca",], FUN = "sum")

	cyer.bp.df <- aggregate(mortality.prop~stock+agerange+evaluationyear, data=cyer.df[cyer.df$year>=1979 & cyer.df$year<=1982,], FUN = "mean")
	colnames(cyer.bp.df)[colnames(cyer.bp.df)=="mortality.prop"] <- "bp.mean"

	cyer.df <- merge(cyer.df, cyer.bp.df, by=c("stock", "agerange", "evaluationyear"))
	cyer.df$cyer.index <- cyer.df$mortality.prop/cyer.df$bp.mean
	return(cyer.df)

}#END calcCYER


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
#'   csv files.
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
			data.mort.long <- reshape(data.mort, dir="long", varying=list(4:ncol(data.mort)), timevar = "cmz.column",  v.names = "mortality.prop")
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

####### DATA #######

#ISBM data:

data.path <- '../data/isbm/qui_2-6'
pt.filenames <- list.files(data.path, pattern = '.PT$')
pt.filepaths <- paste(data.path, pt.filenames, sep='/')
pt.list <- readPT(pt.filepaths)
pt.list.long <- combinePTdata(pt.list)
dat.pt <- pt.list.long$data.isbm.long
dat.pt$stock <- dat.pt$cwtstock
dat.pt$index <- dat.pt$isbm.index

names(pt.list.long$data.isbm.long)
View(pt.list.long$data.isbm.long)

# Mortality Distribution:

data.path <- '../data/totalmortalityDistribution/qui_2-6'
mort.filenames <- list.files(data.path, pattern = '.csv$')
mort.filepaths <- paste(data.path, mort.filenames, sep='/')
mort.list <- readMortalityDist(mort.filepaths)
mort.df <- mort.list$data.mort.long
mort.df$year <- mort.df$CatchYear
names(mort.df)

####### MAIN #######

#the fishery map is the means of mapping fishery groupings to a country:
fishery.map <- buildCMZfishery(mort.df = mort.df)
View(fishery.map)

dat.cyer <- calcCYER(mort.df = mort.df, fishery.map=fishery.map)
dat.cyer$index <- dat.cyer$mortality.prop/100


results.cyer <- calcIndexError(dat = dat.cyer, pm = c('mpe', 'mape'))
results.cyer$errors.df.long$index <- 'cyer'
results.pt <- calcIndexError(dat = dat.pt, pm = c('mpe', 'mape'))
results.pt$errors.df.long$index <- 'isbm'

results.combined <- rbind(results.cyer$errors.df.long, results.pt$errors.df.long)



filepath <- paste(data.path, 'qui2000_modified_cyerindex.csv', sep='/')
write.csv(x = cyer.df, file = filepath, row.names = FALSE)
View(cyer.df)



#simple lattice plot:

require(lattice)
dat.pt <- pt.list.long$data.isbm.long
names(dat.pt)
dat.pt$stock <- dat.pt$cwtstock
dat.pt$cwtstock.fac <- as.factor(dat.pt$cwtstock)
dat.pt$country.fac <- as.factor(dat.pt$country)
dat.pt$evaluationyear.fac <- as.factor(dat.pt$evaluationyear)
xyplot(isbm.index~year|country.fac, groups=evaluationyear.fac, data=dat.pt, type='b', scales=list(alternating=FALSE), auto.key = TRUE)

####### END #######
")
	write(script.str, file="isbm_script.R")
	file.edit("isbm_script.R" )
}#END writeScriptISBM


