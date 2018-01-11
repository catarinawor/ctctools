
.addsums <- function( data.combined){
  # (Calibration Performance) Sum ages by stock & brood year.
  # @param data.combined A dataframe. Output of \code{\link{importFCSCCC}}.
  # @return The same data frame given in the argument, but with brood year sums
  #   appended.
  #
  #get sums by brood year:
  data.forsum <- data.combined[data.combined$data.type %in% c("escapement", "terminalrun")
                               & data.combined$agemetagroup=="age.structure"
                               & data.combined$brood.complete==TRUE & data.combined$age>2,]

  fcs.sum <- aggregate(value.fcs~stock+brood.year+data.type+calibration,
                       data = data.forsum, sum, na.rm=TRUE)
  fcs.sum$agemetagroup <- "age.structure.sum"
  fcs.sum$agegroup <- "brood.sum"

  ccc.sum <- aggregate(value.ccc~stock+brood.year+data.type+calibration,
                       data = data.forsum, sum, na.rm=TRUE)
  ccc.sum$agemetagroup <- "age.structure.sum"
  ccc.sum$agegroup <- "brood.sum"

  sums.merged <- merge(fcs.sum,ccc.sum,
                       by=colnames(fcs.sum)[colnames(fcs.sum) != "value.fcs"], all=TRUE)
  #add column names into sums.merged to match data.combined:
  new.colnames <- colnames(data.combined)[! colnames(data.combined) %in% colnames(sums.merged)]
  sums.merged[,new.colnames] <- NA

  #there can't be a return year shown for the summed broods:
  sums.merged$year <- sums.merged$brood.year
  return(sums.merged)
}#END .addsums



#' @title (Calibration Performance) Automatic Creation Of argument List
#'
#' @description Constructs list of arguments to be utilized by numerous
#'   functions within ctctools.
#'
#' @param stock.names A vector of the three letter stock name acronyms, or "all"
#'   for all stocks.
#' @param commonstocks A Boolean. Whether to compare results based only on
#'   common stocks across calibrations. If only running one model keep as FALSE.
#'   The default is FALSE
#' @param stockmap.pathname A string. The path to the stockmap file.
#' @param data.path.vec A string. The path to the ccc and fcs files.
#' @param stocks.key.pathname.vec  A string. The path to the stock key file.
#' @param age.structure A Boolean. Include analyses based on age structure.
#' @param totalabundance A Boolean. Include analyses for stocks having only
#'   total abundance data. year totals?
#' @param data.type A string of length one or two. The values can be
#'   "escapement" and "terminalrun".
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#'
#' @return A list, to be used as the argument to many functions.
#' @export
#'
#' @examples
#' \dontrun{
#' commonstocks <- FALSE
#' stock.names <- 'all'
#' stockmap.pathname <- "../data/Old-New_Stock_Mapping_20160916.csv"
#' data.path.vec <- c("../data/2016_CI_test")
#' stocks.key.pathname.vec <- c("../data/calibration2017/CLB17bb/stocks.oldnames.csv")
#' # this can be "brood.year" or "return.year":
#' grouping.year <- "return.year"
#' age.structure <- TRUE
#' totalabundance <- TRUE
#' data.type <- c("escapement", "terminalrun")
#' samplesize.min <- 10
#' results.path <- "../data/calibration2017"
#' ranking.method <- c('ordinal', "interpolated")
#' doPlots <- TRUE  # TRUE OR FALSE. ALL CAPS
#' savepng <-  TRUE  # TRUE OR FALSE. ALL CAPS
#' model.list <- buildmodel.list(commonstocks = commonstocks, stockmap.pathname = stockmap.pathname, data.path.vec = data.path.vec, stocks.key.pathname.vec = stocks.key.pathname.vec, grouping.year = grouping.year, age.structure = age.structure, totalabundance =totalabundance, data.type=data.type, results.path = results.path, stock.names = stock.names, groupingby=c( "agegroup"), ranking.method = ranking.method)
#' }
#' @examples
#' \dontrun{
#' model.list <- buildmodel.list(commonstocks = TRUE, stockmap.pathname = stockmap.pathname, data.path.vec = data.path.vec, stocks.key.pathname.vec = stocks.key.pathname.vec, grouping.year = grouping.year, age.structure = age.structure, totalabundance =totalabundance, data.type=data.type, results.path = results.path, stock.names = stock.names, groupingby=c( 'agegroup'), ranking = ranking)

#' }
buildmodel.list <- function(stock.names="all", commonstocks=FALSE, stockmap.pathname, data.path.vec, stocks.key.pathname.vec, startingyear=NA, finalyear=NULL, grouping.year, age.structure, totalabundance, data.type=c("escapement", "terminalrun"), results.path=NA, groupingby=c('stock', 'agegroup'), ranking.method=c('ordinal', 'interpolated')){
  stocks.key.pathname.vec <- ctctools:::.expand_args(stocks.key.pathname.vec,data.path.vec)[[1]]
  groupingby <- match.arg(groupingby)

   models <- vector("list", length(data.path.vec))
   names(models) <- paste0("group", 1:length(data.path.vec))
   for(i in 1:length(models)){
     models[[i]] <-
       list(fcs=list(path=data.path.vec[i], filename=NA),
            ccc=list(path=data.path.vec[i], filename=NA),
            stock.names=stock.names,
            stocks.key.pathname=stocks.key.pathname.vec[i],
        #    grouping.year=grouping.year,
            age.structure=age.structure, totalabundance=totalabundance,
            data.type=data.type
       )
   }
   results.path <- ifelse(is.na(results.path) | results.path=="NA" | results.path=="", paste(getwd(), "results", sep="/"), paste(results.path, "results", sep="/"))
  model.list <- list(commonstocks=commonstocks, stockmap.pathname=stockmap.pathname, results.path=results.path, startingyear=startingyear, finalyear=finalyear, grouping.year=grouping.year, groupingby=groupingby, ranking.method=ranking.method, models=models)

}#END buildmodel.list



#' @title (Calibration Performance) Frequency counts by MPE bin ranges.
#'
#'
#' @param metrics.arg A data frame. Output of \code{\link{calcPMs}}.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#' @param mpe.range.vec A vector
#' @param ... Optional arguments.
#'
#'
#' @return A csv file for each value in the argument \code{mpe.range.vec}.
#' @export
#'
#' @examples
#' \dontrun{
#' calcMPEfreq(metrics, results.path = model.list$results.path, mpe.range.vec = c('pos', 'neg', 'abs'))
#' }
#'
calcMPEfreq <- function(metrics.arg, results.path=".", mpe.range.vec=c('abs', 'neg', 'pos'), ...){
  .makeDir(results.path)
  args <- list(...)
  mpe.range.vec <- tolower(mpe.range.vec)
  groupingby <- args$groupingby


  #the code loops through the options on whether or not to take abs value:
  for(mpe.range.i in mpe.range.vec){

   transform.mutiplier <- ifelse(mpe.range.i=="neg",-1,1)
  #binning:

  br.vec <- c(seq(0,1,by=0.1),1e6 ) *transform.mutiplier
  br <- data.frame(lower=numeric(length(br.vec)-1))
  if(mpe.range.i=='neg'){
    br$lower <- br.vec[-1]
    br$upper <-br.vec[-length(br.vec)]
    br$ranges <- paste0(br$lower, " <= mpe < ", br$upper)
    br$ranges[1] <- paste0(br$lower[1], " <= mpe <= ", br$upper[1])
    br$ranges[length(br$ranges)] <- "mpe < -1"
  }else{
    br$lower <- br.vec[-length(br.vec)]
    br$upper <- br.vec[-1]
    br$ranges <- paste0(br$lower, " < mpe <= ", br$upper)
    br$ranges[1] <- paste0(br$lower[1], " <= mpe <= ", br$upper[1])
    br$ranges[length(br$ranges)] <- "1 < mpe"
  }#END if(trans)


  if(mpe.range.i=="abs"){
    metrics <- metrics.arg
    metrics$metrics.wide$mpe <- abs(metrics.arg$metrics.wide$mpe)
  }else if(mpe.range.i=="neg"){
    metrics <- metrics.arg
    metrics$metrics.wide <- metrics.arg$metrics.wide[metrics.arg$metrics.wide$mpe<=0,]
  }else if(mpe.range.i=="pos"){
    metrics <- metrics.arg
    metrics$metrics.wide <- metrics.arg$metrics.wide[metrics.arg$metrics.wide$mpe>=0,]
  }#END if(mpe.range.i=="abs"){

  for(agemetagroup in unique(metrics$metrics.wide$agemetagroup)){
    metrics.temp <- metrics$metrics.wide[metrics$metrics.wide$agemetagroup==agemetagroup & metrics$metrics.wide$agegroup %in% c('age.3', 'age.4', 'age.5', "totalabundance", "brood.sum"),]

    freq.mpe <- by(metrics.temp, list( metrics.temp$calibration), FUN = function(x){

      x$stockage <- paste(x$stock, ifelse(x$agegroup=="totalabundance","",paste0("-",x$agegroup)), sep="")
      #table(cut(x$mpe,breaks = br.vec))

      #be warned that hist() sorts the breaks from smallest to largest in the ouput.
      # if evaluating negative data my breaks go from 0 to -1
      # right=FALSE for negative ranges
      right <- mpe.range.i !="neg"
      freq <- hist(x$mpe, breaks=br.vec, include.lowest=TRUE,right = right, plot=FALSE)
      freq <- data.frame(lower=freq$breaks[-length(freq$breaks)], freq.counts=freq$counts)
      freq$freq.proportions <- round(freq$freq.counts/sum(freq$freq.counts, na.rm = TRUE),3)
      temp.results <- merge(br, freq, by='lower')
      #the following abs() is needed to always go from 0 to 1 or 0 to -1:
      temp.results <- temp.results[order(abs(temp.results$lower)),]

      temp.results$stockage <- NA
      temp.results$stockage <- apply(temp.results,1,FUN = function(temp.results.sub,x){

        upper <- as.numeric(temp.results.sub['upper'])
        lower <- as.numeric(temp.results.sub['lower'])
        stockage.vec <- x$stockage[x$mpe>=lower & x$mpe<upper]
        stockage.str <- paste(stockage.vec, collapse = "; ")
        return(stockage.str)
      }, x)

     return(temp.results[,c('ranges', 'freq.counts', "freq.proportions", 'stockage')])
    })

    data.type <- paste0(unique(metrics.temp$data.type), collapse = "+")

    filename <- paste("Table6_MPEfrequencies", data.type, agemetagroup, mpe.range.i, ".txt", sep="_")

    calibrations <- unique(metrics$metrics.long$calibration[metrics$metrics.long$agemetagroup==agemetagroup ])
    stock.vec <- unique(metrics$metrics.long$stock[metrics$metrics.long$agemetagroup==agemetagroup & metrics$metrics.long$calibration %in% calibrations ] )

    cat(c(paste0(stock.vec, collapse = "+"), "\n"),file = paste( results.path, filename, sep="/" ))
    options(width = 300)
    capture.output(print( freq.mpe), file=paste( results.path, filename, sep="/")  , append = TRUE)
    options(width = 60)

  }
  }#END for(mpe.range.i in transform.vec){

}#END calcMPEfreq



#' @title (Calibration Performance) Group Performance Measures
#'
#' @description A wrapper to calculate multiple performance measures.
#'
#' @param data.combined A dataframe. Output of \code{\link{importFCSCCC}}.
#' @param datasubset A character vector defining the values from data.type to be selected.
#' @param pm A list of performance measures to calculate.
#' @param writecsv A Boolean confirming if output should also be written to a csv file.
#'
#' @details This is a convenient wrapper that estimates one or more performance
#' measures for the data supplied. This function is somewhat specialized for use
#' on CTC calibration model comparisons (most often against values in the *.fcs files.
#' The output from the \code{\link{mergeFCSCCC}} function is what normally would be used for
#' the argument \code{data.combined}.
#' This function expects \code{data.combined} to include columns with names:
#' \code{calibration, stock, data.type, agegroup, value.fcs, value.ccc}.
#' The latter two columns are, respectively, the data that will be used in the arguments:
#' \code{expect} and \code{obs} of the various performance metric functions.
#'
#' @return The function returns a dataframe comprising the performance metrics requested,
#' grouped by \code{calibration, stock, data.type, and agegroup}.
#' @export
#'
#' @examples
#' metrics <- calcPMs(data.combined, pm = list(PBSperformance::mpe,  PBSperformance::mape), writecsv = TRUE, samplesize.min = samplesize.min, results.path = model.list$results.path)
#'
calcPMs <- function(data.combined, datasubset=  c('escapement', 'terminalrun'),
										pm =list(PBSperformance::mre, PBSperformance::mae, PBSperformance::mpe, PBSperformance::mape, PBSperformance::rmse),
										samplesize.min=10,
										writecsv=TRUE,results.path=".",... ){

	data.combined.sub <- data.combined[data.combined$data.type %in% datasubset,]

	results <- with(data.combined.sub, by(data.combined.sub, list(calibration, stock, data.type, agegroup), FUN= function(x){

		if(sum(complete.cases(x[,c('value.ccc','value.fcs')]))<samplesize.min){
			#if number of years is too small just replacing all data with NA is easier
			# as it proceeds with calculationa and desired output structure.
			x[,c('value.ccc','value.fcs')] <- NA
		}
		expect = x[,'value.ccc']
		obs = x[,'value.fcs']
		pm.list <- lapply(pm, function(x,...) {

			eval(x)(expect , obs,... )},...)

		pm.df <- data.frame(lapply(pm.list, "[[",2))
		colnames(pm.df) <- lapply((lapply(pm.list, "[",2)),names)

		data.frame(stock= x[1,'stock'], agemetagroup=x[1,'agemetagroup'], agegroup=x[1,'agegroup'], data.type=x[1,'data.type'], calibration= x[1,'calibration'], pm.df, stringsAsFactors = FALSE )
	}))

	performance.metrics <- do.call(rbind, results)
	metrics.wide <- performance.metrics[complete.cases(performance.metrics),]

	pm.ind <- seq(ncol(metrics.wide), by=-1, len=length(pm))
	metrics.long <- reshape(metrics.wide, dir='long', varying = list(pm.ind), timevar = 'pm.type',times = colnames(metrics.wide)[pm.ind],  v.names= 'value')
	metrics.long <- metrics.long[,-which(colnames(metrics.long)=="id")]
	metrics <- list(metrics.wide=metrics.wide, metrics.long=metrics.long)

	if(writecsv) {
		.makeDir(results.path)
		#filename <- paste("Table2", unique(metrics.wide$agemetagroup ) , "metics.csv", sep="_")
		filename <- paste("Table2", "metics.csv", sep="_")
		write.csv(x = metrics$metrics.wide, file=paste(results.path, filename, sep="/"), row.names = FALSE, quote = FALSE)
	}

	return(metrics)
}#END calcPMs



#' @title (Calibration Performance) Rank Calculation
#'
#' @description Calculate rank of data.
#'
#' @param dat A dataframe
#' @param columnToRank A vector of integers defining what columns to rank
#'   (independently)
#' @param rank.method A character defining the ranking method to apply. The
#'   method can be either 'ordinal' or 'interpolated'.
#' @param abs.value A Boolean vector with length equal to the length of the
#'   columnToRank argument. This indicates whether or not to take the absolute
#'   value of the data before the ranking. Default value is FALSE.
#'
#' @return A dataframe comprising all columns of the argument: \code{dat} and
#'   one additional column of ranks for each column column declared in
#'   \code{columnToRank}. The name of each rank column is a combination of the
#'   data column name and ".rank".
#' @export
#'
#' @examples
#' \dontrun{
#' dat.test <- data.frame(id=1:20, values=rnorm(20))
#' calcRanks(dat.test,  columnToRank=2, abs.value = TRUE )
#' }
calcRanks <- function(dat, columnToRank, rank.method=c('ordinal', 'interpolated'), abs.value=FALSE){

  # make sure abs.value has same length as columnToRank:
  abs.value <- .expand_args(abs.value, columnToRank)[[1]]

  # dat must be a data.frame
  rank.method <- match.arg(rank.method)

  if(class(columnToRank)=='character'){
    column.ind <- which(colnames(dat) %in% columnToRank)
  } else {
    #columnToRank is a vector of indices
    column.ind <- columnToRank
  }

  for(i in column.ind){
    dat.tmp <- dat[,i]
    #option to take absolute value:
    if(abs.value[which(column.ind==i)]) dat.tmp <-  abs(dat[,i])

    if(rank.method=="ordinal"){
      dat[,paste(colnames(dat)[i], ".rank",sep='')] <- as.integer(rank(dat.tmp))
    } else if(rank.method=="interpolated") {
      rank.perpmunit <- length(dat.tmp)/(max(dat.tmp)-min(dat.tmp))
      dat[,paste(colnames(dat)[i], ".rank",sep='')] <- dat.tmp*rank.perpmunit -min(dat.tmp)*rank.perpmunit        } #if(rank.method)
  }#for(i in column.in)
  return(dat)
}#END calcRanks


#' @title (Calibration Performance) Import FCS and CCC files
#'
#' @param data.path.vec A vector of path names that point to data folders. This
#'   argument is only needed if model.list is not included. And the code is
#'   evolving such that it's best to include model.list. Eventually this
#'   argument may be removed from the function.
#' @param model.list A list. Output of the buildmodel.list function.
#'
#' @description Based on the criteria defined in the model.list, this function
#'   imports a single FCS file and one or more CCC files.
#'
#' @return A data frame in long format, comprising both the FCS and CCC data.
#' @export
#'
#' @examples
#' \dontrun{
#' data.combined <- importFCSCCC(model.list = model.list)
#' }
importFCSCCC <- function(data.path.vec=NA, model.list=NULL,...){

  .import.vec <- function(data.pathname){

     ### read in CCC files ###
     filename <- list.files(data.pathname, pattern = "CCC")
     filepath <- paste(data.pathname,  filename, sep='/')
     ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE)

     ### read in FCS files ###
     fcs.files <- list.files(data.pathname, pattern = "\\.FCS")
     filename <- fcs.files[grep("OCN", x = fcs.files)]
     filepath <- paste(data.pathname, filename, sep='/')
     fcs.list <- readFCS(filepath, startingyear=model.list$startingyear, finalyear = model.list$finalyear)


     ### merge the two file types into a common dataframe
     data.combined <- mergeFCSCCC(ccc.list = ccc.list, fcs.list = fcs.list, stocks.names = stocks.names )
     return(data.combined)
   }#END .import.vec

  .import.list <- function(model.sublist){

    if(exists("stocks.key.pathname", where=model.sublist)){
      stocks.key <- readStockKey( model.sublist$stocks.key.pathname, sep=",")
    }

    finalyear <- model.list$finalyear
    startingyear <- model.list$startingyear


    ### read in CCC files ###

    data.pathname <- model.sublist$ccc$path

    if(exists("filename", where=model.sublist$ccc) & any(!is.na(model.sublist$ccc$filename))){
      filename <- model.sublist$ccc$filename
    }else{
      filename <- list.files(data.pathname, pattern = "CCC")
    }

    filepath <- paste(data.pathname,  filename, sep='/')

    if(exists("stocks.key.pathname", where=model.sublist)){

      ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE, startingyear=startingyear, finalyear = finalyear, stocks.key=stocks.key)
    }else{
      ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE, startingyear=startingyear, finalyear = finalyear)
    }

    ### read in FCS files ###
    data.pathname <- model.sublist$fcs$path
    if(exists("filename", where=model.sublist$fcs) & any(!is.na(model.sublist$fcs$filename))){
      filename <- model.sublist$fcs$filename
    }else{

      filename <- list.files(data.pathname, pattern = "\\.FCS")
      #fcs.files <- list.files(data.pathname, pattern = "\\.FCS")
      #filename <- fcs.files[grep("OCN", x = fcs.files)]
    }

    filepath <- paste(data.pathname, filename, sep='/')
    fcs.list <- readFCS(filepath, stocks.key = stocks.key , startingyear=startingyear, finalyear =finalyear)

    ### merge the two file types into a common dataframe
    data.combined <- mergeFCSCCC(ccc.list, fcs.list, stocks.names =model.sublist$stock.names  )

    data.combined$agemetagroup <- "age.structure"
    data.combined$agemetagroup[data.combined$agegroup == "totalabundance"] <- 'no.age.structure'

    age.structure <- ifelse(exists("age.structure", where=model.sublist), model.sublist$age.structure, TRUE)
    totalabundance <- ifelse(exists("totalabundance", where=model.sublist), model.sublist$totalabundance, TRUE)

    data.combined$age <- NA
    data.combined$age[data.combined$agemetagroup=="age.structure"] <- as.integer(substr(data.combined$agegroup[data.combined$agemetagroup=="age.structure"] ,5,5))
    data.combined$return.year <- data.combined$year
    data.combined$brood.year <- data.combined$return.year - data.combined$age

    data.combined$year <- data.combined[,model.list$grouping.year]

    #define complete brood
    data.ageclass <- data.combined[data.combined$agemetagroup=="age.structure" & data.combined$data.type=="escapement" & data.combined$age>2,]

    brood.complete <- with(data.ageclass, by(data.ageclass, list(data.ageclass$stock, data.ageclass$brood.year), FUN=function(x) length(unique(x$age))==3  ))
    brood.complete <-  do.call('cbind',list(brood.complete))
    brood.complete <- data.frame(stock=row.names(brood.complete), brood.complete, stringsAsFactors = FALSE)
    brood.complete <- reshape(brood.complete, dir='long', varying = list(2:ncol(brood.complete)),idvar = 'stock', times = colnames(brood.complete)[-1])

    brood.complete <- data.frame(stock=brood.complete$stock, brood.year=as.integer(substr(brood.complete$time,2,5)), brood.complete=brood.complete[,3], stringsAsFactors = FALSE)

    data.combined <- merge(data.combined, brood.complete, by=c('stock', 'brood.year'), all = TRUE)


    #option to subset by data.type:
    if(exists("data.type", where=model.sublist)){
      data.combined <- data.combined[data.combined$data.type %in% model.sublist$data.type,]
    }

    age.selection <- c(if(age.structure){ "age.structure"}, if(totalabundance){"no.age.structure"})

    data.combined <- data.combined[data.combined$agemetagroup %in% age.selection,]

    #rename the stocks to their old stock name equivalent:
    data.combined <- merge(data.combined, stocks.key[,c("acronym.search", "acronym.replace")], by.x = "stock", by.y = "acronym.search", all.x = TRUE)
    data.combined$stock <- data.combined$acronym.replace
    data.combined <- subset(data.combined, select = -acronym.replace)

    #the age.structure column is not needed, I think...
    data.combined <- subset(data.combined, select=-age.structure)

    return(data.combined)
  }#END .import.list




  if(is.null(model.list)){
    #use data.path
    data.combined.list <- lapply(data.path.vec, .import.vec)
  }else{
    data.combined.list <- lapply(model.list$models, .import.list)
  }

  #data.combined.list <- c(data.combined.list, make.row.names = FALSE)
  data.combined.df <- do.call(rbind,args = data.combined.list)


  if(!is.null(model.list)){
    commonstocks <- ifelse(exists("commonstocks", where=model.list), model.list$commonstocks, FALSE)
    if(commonstocks){
      #if commonstocks is TRUE, then merge function will result in data that only includes stocks that are found in both old and new models.
        if(exists("stockmap.pathname", where=model.list) & !is.na(model.list$stockmap.pathname)){
          stockmap.pathname <- model.list$stockmap.pathname
        } else {
          stop(cat("\n\n !!!!!!!!!!!!\n Import function stopped before completion. \n Need path and filename for file that links new and old stocks.\n (aka stockmap.pathname)\n"))
        }

      stockmap <- readStockMap(stockmap.pathname)

      #stockmap <- getlinkID(stockmap)
      stockmap <- stockmap[tolower(stockmap$Equivalency)=="yes",c("acronym.old", "acronym.replace")]

      data.combined.df <- merge(data.combined.df, stockmap, by.x="stock", by.y = "acronym.replace", all.x = FALSE)

    }else{
      #not limiting to common stocks
      #so keep all stocks
    }#END if(commonstocks){

     #all.x <- ifelse(commonstocks, FALSE, TRUE)

  }#END if(!is.null(model.list)){

  if(model.list$grouping.year=="brood.year") data.combined.df <- .addsums( data.combined = data.combined.df)


  return(data.combined.df)


}#END importFCSCCC



#' @title (Calibration Performance) Import and combine tables of differences
#'   from csv files.
#'
#' @param ranking.method A character defining the ranking method to apply. The
#'   method can be either 'ordinal' or 'interpolated', or both.
#' @param pm.type.vec A character vector of performance measures to evaluate.
#' @param data.type See Details
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#'
#' @return A data frame in long format, that is combination of all csv files.
#' @export
#'
#' @examples
#' \dontrun{
#' # this relies on the csv files that are written by writeTableOfDifferences() above
#' tableofdifferences <- importTableOfDifferences(ranking.method = model.list$ranking.method,
#'  pm.type.vec =  c('mape', 'mpe'),
#'  data.type = model.list$models$group1$data.type,
#'  results.path = model.list$results.path)
#' }
#'
importTableOfDifferences <- function(ranking.method, pm.type.vec=c('mpe', 'mape'),  data.type ="escapement+terminalrun", results.path = "."){

  dat.df <- expand.grid(pm.type=pm.type.vec, rank.method=ranking.method)
  data.type <- paste0(data.type, collapse = "+")

  td <- apply(dat.df, 1, function(x, data.type, results.path){
    filename <- paste("TableOfDifferences", "table3", x['rank.method'], "ranking.method", x['pm.type'], data.type, ".csv", sep="_")

    td <- read.csv(paste(results.path, filename, sep="/"), stringsAsFactors = FALSE)
    td$pm.type <- x['pm.type']
    td$rank.method <- x['rank.method']
    colnames(td)[colnames(td)==x['pm.type']] <- "value"
    return(td)
  }, data.type, results.path)

  td <- do.call('rbind', td)
  return(td)
}#END importTableOfDifferences



#' @title (Calibration Performance) Merge FCS And CCC Data
#'
#' @description Merge FCS and CCC data
#'
#' @param ccc.list A list, see details
#' @param fcs.list A list, see details
#'
#' @details The function arguments are output from the functions
#'   \code{\link{readCCC}} and \code{\link{readFCS}}, respectively. This
#'   function is called within \code{\link{importFCSCCC}} and the user is unlikely
#'   to use it on its own.
#'
#' @return A dataframe is produced.
#' @export
#'
#' @examples
#' \dontrun{
#' data.combined <- mergeFCSCCC(ccc.list = ccc.list, fcs.list = fcs.list)
#' }
mergeFCSCCC <- function(ccc.list, fcs.list, stocks.names='all'){

  ccc.list <- lapply(ccc.list,FUN = function(x){
    x$data.long$calibration <- x$calibration
    return(x)
    })
  ccc.allcalib <- do.call(what = "rbind", lapply(ccc.list, "[[",5) )

  fcs.stocks <- unique(fcs.list$data.long$stock)

  fcs.stocks.combined <- lapply(fcs.stocks[nchar(fcs.stocks)>3], FUN = substring,c(1,4), c(3,6) )
  if(length(fcs.stocks.combined)>0){

    # this generates new dataframes that are sums of combined stocks:
    ccc.summed <- lapply(fcs.stocks.combined, FUN= function(x, ccc.allcalib){
      temp.summed <- aggregate(value~year+agegroup+data.type+calibration, FUN= sum, data = ccc.allcalib[ccc.allcalib$stock %in% x & ccc.allcalib$data.type %in% c('escapement', 'terminalrun'),])
      temp.summed$stock <- paste(x, collapse = "" )
      return(temp.summed)
      }, ccc.allcalib)

    ccc.summed <- do.call("rbind", ccc.summed)
    ccc.summed$stocknumber <- NA
    #re-sort so the dataframes match:
    ccc.summed <- ccc.summed[,colnames(ccc.allcalib)]
    ccc.allcalib <- rbind(ccc.allcalib, ccc.summed)
  }#END if(length(fcs.stocks.combined)>0){


  data.combined <- merge(ccc.allcalib , fcs.list$data.long, by=c('stock', 'year', 'agegroup', 'data.type'), all=TRUE)

  colnames(data.combined)[colnames(data.combined) == 'value.x'] <- 'value.ccc'
  colnames(data.combined)[colnames(data.combined) == 'value.y'] <- 'value.fcs'

  data.combined <- data.combined[order(data.combined$stock, data.combined$year, data.combined$agegroup),]
  #subset to only selected stocks:

  if(any(!is.na(stocks.names)) &  all(toupper(stocks.names) != "ALL")) {
    if( nrow(data.combined[data.combined$stock %in% toupper(stocks.names),])==0 ) {
      stop("Stock name doesn't match to any stocks in data.")
    }
    data.combined <- data.combined[data.combined$stock %in% toupper(stocks.names),]
  }

  #Remove source stocks that contribute to combined stocks.
  # eg. remove RBH & RBT
  combined.stocks <- unique(data.combined$stock[nchar(data.combined$stock)>3])
  combined.stocks.source <-  unlist(lapply(combined.stocks,substring,c(1,4), c(3,6)))
  #double negative required to exclude the source stocks:
  data.combined <- data.combined[!(data.combined$stock %in% combined.stocks.source),]

  return(data.combined)
}#END mergeFCSCCC



#' @title (Calibration Performance) Model Comparison Plots
#'
#' @param data.combined A dataframe. Output of \code{\link{importFCSCCC}}.
#' @param agegroup.n An integer, equates to the number of plots per page. See
#'   detail
#' @param savepng A Boolean confirming to also save a PNG file of each plot.
#' @param ...
#'
#' @description 1:1 plots for comparing FCS and CCC data by stock and age.
#'
#' @details Option to produce PNG files. Relies on the package: \code{lattice}.
#'   Argument \code{data.combined} Argument \code{agegroun.n} determines the
#'   number of lattice panels per page. The value would typically range 1--4
#'   based on the number of age groups being evaluated. Considering the unique
#'   values of \code{agegroup} are c('age.3', 'age.4', 'age.5',
#'   'totalabundance').
#'

#'
#' @return One lattice plot per unique combination of stock and calibration model,
#' with option to produce the same in the form of a PNG file.
#' @export
#'
#' @examples
#' \dontrun{
#' plotFCSCCC(data.combined)
#' }
plotFCSCCC <- function(data.combined, savepng=FALSE, results.path = ".", point.col.df, ...){

  data.combined <- data.combined[order(data.combined$calibration, data.combined$agegroup, data.combined$year),]

  #this calculate the axes range by stock, by age, across all calibration models:
  data.combined <- with(data.combined,by(data.combined,list(stock, agegroup ), FUN= function(x){
    x$range.lower <- min(range(c(x$value.ccc, x$value.fcs), na.rm = TRUE))
    x$range.upper <- max(range(c(x$value.ccc, x$value.fcs), na.rm = TRUE))
    return(x)
  }))
  data.combined <- do.call("rbind", data.combined)

  #data.combined$calibration.username <- substr(data.combined$calibration,1, nchar(data.combined$calibration)-4 )

  stock.name <- unique(data.combined$stock)
  years.interval <- .getBrokeninterval(sort(unique(data.combined$year)))
  col.val <- length(unique(data.combined$agegroup))
  row.val <- length(unique(data.combined$calibration))
  par.strip.text.cex <- ifelse(row.val==1, 0.5,1)

  key.list <- list( space='top',
                  text = list(paste(point.col.df$year.start,point.col.df$year.end , sep="-")),
                  points = list( col = point.col.df$point.col, pch=16,cex=0.75)
                )

  xyplot.eg <- lattice::xyplot(value.fcs/1e3~value.ccc/1e3|as.factor(agegroup)+as.factor(calibration), data=data.combined, as.table=TRUE,
                               key=key.list,
                               main = paste0(stock.name, " (", years.interval,")" ),
                               scales=list(relation='free', cex=1),
                               aspect=1, layout= c(col.val,row.val),
                              # par.strip.text=list(cex=par.strip.text.cex),
                               prepanel = function(x, y, subscripts) {

                                 # if(sum(complete.cases(x,y)) > samplesize.min){
                                 #      rr <-   range(cbind(x,y), na.rm=TRUE)
                                 #     }else{
                                 #       #insufficient sample size, no plotting of data, get axes range from all data
                                 #        rr <-  range(data.combined$value.fcs/1e3, data.combined$value.ccc/1e3, na.rm = TRUE)
                                 #     }
                                rr <- range(data.combined$range.lower[subscripts], data.combined$range.upper[subscripts], na.rm = TRUE)/1e3

                                 list(xlim = rr, ylim= rr)
                               },
                               panel=function(x,y, subscripts,...){

                                 if(sum(complete.cases(x,y)) > samplesize.min) {
                                   col <- data.combined$point.col[subscripts]
                                   lattice::panel.xyplot(x, y, type='n',...)
                                   lattice::panel.abline(a=0, b=1)
                                   lattice::panel.points(x,y, pch=16,cex=0.75, col=col)
                                 }
                               },...)

  xyplot.eg <- update(xyplot.eg, par.settings = list(fontsize = list(text = 10, points = 4)))

  if(savepng){
    filename <- paste(unique(data.combined$agemetagroup ),  unique(data.combined$stock), unique(data.combined$data.type), ".png", sep="_")
    .makeDir(results.path)

    png(file= paste(results.path, filename, sep="/"), wid=col.val*3, height=row.val*3, units="in", res=600)
    print(xyplot.eg)
    dev.off()
  }else{
    print(xyplot.eg)
  }
}#END plotFCSCCC

#' @title (Calibration Performance) Lattice plot to compare PMs across
#'   calibration models
#'
#' @param tableofdifferences A data frame, Output of
#'   \code{\link{importTableOfDifferences}}
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#' @param savepng A Boolean. Save output to a png file. Default is FALSE.
#'
#' @return Lattice plot of the data found in the "Table of differences" files.
#' @export
#'
#' @examples
#' \dontrun{
#' plotPM(tableofdifferences = tableofdifferences,
#'  results.path = model.list$results.pat,savepng=TRUE )
#' }
plotPM <- function(tableofdifferences, results.path = ".", savepng=FALSE){

  layout.vec <- c(length(unique(tableofdifferences$groupingvar)),nrow(unique(cbind(tableofdifferences$pm.type, tableofdifferences$rank.method ))), 1)

  xyplot.eg <- lattice::xyplot(value~as.factor(calibration)|groupingvar+rank.method+pm.type, data=tableofdifferences, as.table=TRUE, scales=list(alternating=FALSE, x=list(rot=45)), layout=layout.vec, xlab="Calibration")

  if(savepng){
    .makeDir(results.path)
    filename <- paste("plotPMaverage_byage",data.type, ".png", sep="_")
    png(file= paste(results.path, filename, sep="/"), wid=8, height=11, units="in", res=600)
    print(xyplot.eg)
    dev.off()
  }else{
    print(xyplot.eg)
  }

}#END plotPM


plotPM.old <- function(ranking.method, pm.type.vec=c('mpe', 'mape'),  data.type ="escapement+terminalrun", results.path){

  plotting.df <- expand.grid(pm.type=pm.type.vec, rank.method=ranking.method)
  data.type <- paste0(data.type, collapse = "+")

  .makeDir(results.path)

  td <- apply(plotting.df,1, function(x, data.type, results.path){
      filename <- paste("TableOfDifferences", "table3", x['rank.method'], "ranking.method", x['pm.type'], data.type, ".csv", sep="_")
    td <- read.csv(paste(results.path, filename, sep="/"))
    td$pm.type <- x['pm.type']
    td$rank.method <- x['rank.method']
    colnames(td)[colnames(td)==x['pm.type']] <- "value"
    return(td)
  }, data.type, results.path)

  td <- do.call('rbind', td)
  layout.vec <- c(length(unique(td$groupingvar)),nrow(unique(cbind(td$pm.type, td$rank.method ))), 1)
  xyplot.eg <- lattice::xyplot(value~as.factor(calibration)|groupingvar+rank.method+pm.type, data=td, as.table=TRUE, scales=list(x=list(rot=45)), layout=layout.vec)

  filename <- paste("plotPMaverage_byage",data.type, ".png", sep="_")
  png(file= paste(results.path, filename, sep="/"), wid=8, height=11, units="in", res=600)
  print(xyplot.eg)
  dev.off()

}#END plotPM

#' plotFCSvsCCC
#'
#' @param data.combined A dataframe. Output of \code{\link{importFCSCCC}}.
#' @param samplesize.min The number of years required per stock & age structure
#'   type. Default is 10.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#' @param point.col.df A data frame
#' @param ... Arguments to pass to internal function \code{\link{plotFCSCCC}}.
#'
#' @return Plots sent to graphics window or PNG files. See
#'   \code{\link{plotFCSCCC}} for PNG creation.
#' @export
#'
#' @examples
#' \dontrun{
#' # The NA value in year.end prompts the code to get the latest year available:
#' point.col.df <-  data.frame(year.start=c(1979,1985,1999,2009),
#'                             year.end=c(1984,1998,2008,NA),
#'                             point.col=c('black', 'blue', 'green', 'red'),
#'                             stringsAsFactors = FALSE)
#'
#' plotFCSvsCCC(data.combined,samplesize.min, results.path = model.list$results.path,
#'  point.col.df=point.col.df)
#'  }
plotFCSvsCCC <- function(data.combined, samplesize.min=10, results.path = ".",
                         point.col.df= data.frame(year.start=c(1979,1985,1999,2009), year.end=c(1984,1998,2008,NA), point.col=c('black', "yellow", "red", "green"), stringsAsFactors = FALSE) ,
                         ...){
  data.combined.sub <- data.combined[data.combined$data.type %in% c('escapement', 'terminalrun') & data.combined$agegroup %in% c('age.3', 'age.4', 'age.5', 'totalabundance', "brood.sum"), ]

  # this adds point colours:
  # if no max year is set, then grab from data:
  if(is.na(point.col.df$year.end[length(point.col.df$year.end)])) point.col.df$year.end[length(point.col.df$year.end)] <- max(data.combined.sub$year, na.rm=TRUE)

  data.combined.sub <- apply(point.col.df,1, FUN = function(x, data.combined.sub){
    data.combined.sub$point.col[data.combined.sub$year>=x['year.start'] & data.combined.sub$year<=x['year.end']] <- x['point.col']
    return(data.combined.sub[data.combined.sub$year>=x['year.start'] & data.combined.sub$year<=x['year.end'],])
  }, data.combined.sub)

    data.combined.sub <- do.call('rbind', data.combined.sub)

  with(data.combined.sub, by(data.combined.sub, list(agemetagroup,  stock, data.type), FUN=function(x){
    # this is tested twice as this sum is across all age classes
    # I use this test to prevent calls of plotFCSCCC() when there is no non-age.structured data
    comp.case.count <- c(with(x, by(x, list(agegroup, calibration), FUN=function(x2){
      sum(complete.cases(x2[,c('value.ccc', 'value.fcs')]))
    })))

 # all data sets within a stock must have minimum size
    if(any(comp.case.count > samplesize.min)) {
      x <- x[complete.cases(x[,c('value.ccc', 'value.fcs')]),]

      plotFCSCCC(x,  xlab= paste0("Modelled ",unique(x$data.type), " (1000s)"), ylab= paste0("Observed ",unique(x$data.type), " (1000s)"), savepng = savepng, results.path = results.path, point.col.df ,... )
    }#END if
  }),results.path)

}#END plotFCSvsCCC



.plotFCSvsCCC.old <- function(data.combined, samplesize.min, results.path = "."){
  data.combined.sub <- data.combined[data.combined$data.type %in% c('escapement', 'terminalrun') & data.combined$agegroup %in% c('age.3', 'age.4', 'age.5', 'totalabundance'), ]

  with(data.combined.sub, by(data.combined.sub, list(agemetagroup, calibration, stock, data.type), FUN=function(x){
    # this is tested twice as this sum is across all age classes
    # I use this test to prevent calls of plotFCSCCC() when there is no non-age.structured data
    if(sum(complete.cases(x[,c('value.ccc', 'value.fcs')])) > samplesize.min) {
      x <- x[complete.cases(x[,c('value.ccc', 'value.fcs')]),]

      plotFCSCCC(x, agegroup.n = length(unique(x$agegroup)), xlab= paste0("CCC ",unique(x$data.type), " (1000s)"), ylab= paste0("FCS ",unique(x$data.type), " (1000s)"), savepng = savepng, results.path = results.path )
    }#END if
  }),results.path)

}#END .plotFCSvsCCC.old



#' @title (Calibration Performance) Import CCC Files
#'
#' @description Import one or more CCC files into a list.
#'
#' @details Need details.
#'
#' @param filepath A character vector of the CCC files to be imported. See details
#' @param data.types See details
#' @param stocks.key See details
#' @param startingyear An integer.
#' @param finalyear An integer. The final (4 digit) year to be included from all series
#'   imported. Default is 9999, meaning all years before 9999.
#'
#' @return A list, with one element per CCC (calibration) file.
#' @export
#'
#' @examples
#' \dontrun{
#' ### read in CCC files ###
#' filename <- list.files(data.pathname, pattern = "CCC")
#' filepath <- paste(data.pathname,  filename, sep='/')
#' ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE)
#' }
readCCC <- function(filepath, data.types = c("AEQ", 'cohort', 'termrun', "escape"), stocks.key=NA, startingyear=1, finalyear=9999){
	if(is.na(startingyear)) startingyear <- 1
	if(is.null(finalyear)) finalyear <- 9999

  #have to read in header and data separately as the number of comma delims differs
   #if(is.na(stocks.key)) stocks.key <- stocks.key.hidden
   #get calibration number from filename
  final.slash <- rev(gregexpr("\\/", filepath)[[1]])[1]
  dot.index <- rev(gregexpr("\\.", filepath)[[1]])[1]
  calib.num <- substr(filepath,final.slash+1, dot.index-1)
  calib.num <- substr(calib.num, 1, nchar(calib.num)-4)

  #A more proper vectorized version (in case x is longer than 1):
  # sapply(gregexpr("\\.", x), function(x) rev(x)[1])

  ccc <- read.csv(filepath, skip=1, header=FALSE, stringsAsFactors = FALSE)

  ccc.colnames <- read.table(filepath, sep=',', nrows=1, stringsAsFactors=FALSE)

  ccc.colnames <- unlist(ccc.colnames)
  #get rid of white space in variable names
  ccc.colnames <- gsub(" ", "", ccc.colnames)

  #put in two dummy colnames:
  ccc.colnames <- c(ccc.colnames[1:10],"empty01", ccc.colnames[11:length(ccc.colnames)], 'empty02')
  colnames(ccc) <- tolower(ccc.colnames)
  #remove empty columns
  ccc <- ccc[, -c(grep("empty", colnames(ccc)))]


  #remove final two years (now turned off)
  #final.twoyears <- tail(sort(unique(ccc$year)),2)
  #ccc <- ccc[!ccc$year %in% final.twoyears,]

  #now limit to user's choice of year range:
  ccc <- ccc[ccc$year >= startingyear & ccc$year <= finalyear,]


  data.types <- tolower(data.types)
  #data will go into a list
  ccc.list <- list()

  for(data.type in data.types){
    col.ind <- grep(data.type, colnames(ccc))

    ccc.list[[data.type]] <- reshape(ccc, dir='long', idvar=c('year', 'stock'), drop =colnames(ccc)[-c(1,2,col.ind)]  , varying = list(col.ind), timevar = 'agegroup',times = paste0('age.',2:5),  v.names= 'value')

    ccc.list[[data.type]]['data.type'] <- data.type
  }#END for

  # am no longer merging. doing rbind instead
  #merge.all <- function(x, y) {  merge(x, y, all=TRUE, by=c('stock', 'year','agegroup'))}
  #ccc.list[['data.long']] <- Reduce(merge.all, ccc.list)

  ccc.list[['data.long']] <- do.call('rbind', ccc.list)


  #fix column names
  colnames(ccc.list[['data.long']])[which(colnames(ccc.list[['data.long']])=="stock")] <- 'stocknumber'
  ccc.list[['data.long']] <- merge(ccc.list[['data.long']],stocks.key[,c('stocknumber', 'stock')], by='stocknumber')
  ccc.list[['data.long']] <- ccc.list[['data.long']][,c('stock',colnames(ccc.list[['data.long']])[-ncol(ccc.list[['data.long']])])]

  #calculate sum of ages 3:5, by year for termrun and escape (separately)
  temp.df <- ccc.list[['data.long']]
  temp.df <- temp.df[temp.df$data.type %in% c('termrun', 'escape'),]
  temp.df <- temp.df[temp.df$agegroup != "age.2",]

  data.summed.age <- aggregate(value~stock+stocknumber+year+data.type, data = temp.df, FUN = sum,  na.rm=TRUE )
  data.summed.age$agegroup <- "totalabundance"

  #rbind this summed result with the age specific data:
  # first, reorganize columns so they match
  data.summed.age <- data.summed.age[,colnames(ccc.list[['data.long']])]
  ccc.list[['data.long']] <- rbind(ccc.list[['data.long']], data.summed.age)

  #change values in data.type field so they match fcs file:
  ccc.list[['data.long']][ccc.list[['data.long']]['data.type']=='escape', 'data.type'] <- 'escapement'
  ccc.list[['data.long']][ccc.list[['data.long']]['data.type']=='termrun', 'data.type'] <- 'terminalrun'

  ccc.list$calibration <- calib.num
  #make a list within a list so it can be given a name=calibration number
  ccc.list <- list(ccc.list)
  names(ccc.list) <- calib.num

  return(ccc.list)
  #return(list(c(calib.num, ccc.list)))

}#END readCCC


#' @title (Calibration Performance) Import FCS Files
#'
#' @description Import one or more FCS files into a list.
#'
#' @details Need details.
#'
#' @param filepath A character vector of length one. The name of the FCS file to
#'   import.
#' @param first.stockline An integer, row number in FCS file where first stock
#'   metadata commences. See details.
#' @param stocks.key.df A data frame. These data are stored in the package. If
#'   the argument is left with its default value (NULL), then the data are
#'   loaded from the package. The user can supply a new data frame with updated
#'   stocks, so long as the updated data frame has the same structure as found
#'   in \code{\link{stocks.key}}.
#' @param startingyear An integer.
#' @param finalyear An integer. The final (4 digit) year to be included from all series
#'   imported. Default is 9999, meaning all years before 9999.
#'
#' @return A list, with two elements. The first element is a data frame of all
#'   the data fro all the stocks transposed into a long format. The second
#'   element is a list comprising as many lists as stocks in the FCS file. All
#'   the data associated with each stock can be found in each sub-sub list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ### read in FCS files ###
#' fcs.files <- list.files(data.pathname, pattern = "\\.FCS")
#' filename <- fcs.files[grep("OCN", x = fcs.files)]
#' filepath <- paste(data.pathname, filename, sep='/')
#' fcs.list <- readFCS(filepath)
#' }
readFCS <- function(filepath, first.stockline=3, stocks.key.df=NULL, startingyear=1, finalyear=9999){
  #first.stockline is the row where the first stock begins (ie accounts for
  #header rows not associated with stocks

  #stock.key is a translation table between the 3 letter stock name stock number
	if(is.null(stocks.key.df)) stocks.key.df <- stocks.key

	if(is.na(startingyear)) startingyear <- 1
	if(is.null(finalyear)) finalyear <- 9999

  fcs.vec <- readLines(filepath)

  fcs.vec <- trimws(fcs.vec)

  #first row of file defines the start year of each data series, so this value is how we define our search for the starting row of each series:
  year.first <- strsplit(fcs.vec[1],"," )[[1]][1]

  #remove initial unneeded rows
  fcs.vec <-  fcs.vec[first.stockline:length(fcs.vec)]

  #these are the index/row numbers for the start of each data series:
  year.first.ind <- grep(pattern = paste0("^", year.first), fcs.vec)

  ind.ranges <- data.frame(year.first.ind=year.first.ind, year.last.ind=c(year.first.ind[-1],length(fcs.vec)))

  ind.ranges$year.last.ind <- apply(ind.ranges,1, FUN=function(x, fcs.vec){
    temp1 <- strsplit(fcs.vec[x[1]:(x[2]-1)], ",")
    #the row starting with a stock name gets converted to NA, so one befor it is
    #the end of the prior series:

    next.meta.ind <- min(which(is.na(as.numeric(unlist(lapply(temp1,"[[",1))))))

    #this -2 below isn't the way of removing the final year, it's different.
    #look to line 1059 for year removal
    year.last.ind <- x[1] + next.meta.ind-2


    #This fixes last row:
    if(is.infinite(year.last.ind)) year.last.ind <- length(fcs.vec)
    return(year.last.ind)
  }, fcs.vec)


  #this defines how many rows of metadata exist for each stock:
  ind.ranges$number.metarows <- (ind.ranges$year.first.ind-1)- c(0, ind.ranges$year.last.ind[-length(ind.ranges$year.last.ind)])

  fcs.list <-
    apply(ind.ranges, 1, FUN =function(x, fcs.vec){

      data.str <- fcs.vec[x['year.first.ind']:x['year.last.ind']]
      #parse string into columns of data
      data.list <- strsplit(data.str, ",")

      #leave out final two years (now turned off)
      #data.list <- data.list[-c(length(data.list):(length(data.list)-1))]

      # deals with data that have inconsistent number of columns
      n <-   max(sapply(data.list, length))
      data <- do.call(rbind, lapply(data.list, `[`, seq_len(n)))

      mode(data) <- 'integer'

      #In years without age structure (col 2=0) move the summed value to 3rd col
      # in year with age stucture:
      #!!!have now turned off summing for age structure years:
      #!!! place sum in 3rd col and keep age strcture values
      #need to transpose results
      data <- t(apply(data,1,FUN=function(x){
        if(x[2]==0) {
          x <- c(x[1:2], x[4], NA, NA, NA)
        }else if(x[2]==1){
          #x <- c(x[1:2], sum(x[3:length(x)], na.rm=TRUE), x[3:length(x)] )
          x <- c(x[1:2], NA, x[3:length(x)] )
        }
        return(x)
      }))

      #In years without age structure (col 2=0) and when count=0 then reset values to NA
      data <- t(apply(data,1,FUN=function(x){
        # the age total column has been moved to the third position
        #putting summed age first give flexibility for additional ages

        if(x[2]==0 & x[3]==0) {x[3] <- NA}
        return(x)}))

      #If there are any years without age structure and have data (non-zero) then sum all years with age structure to an annual total
      #!!! NOT CURRENTLY IMPLEMENTED.
      #if(any( apply(data[data[,2]==0,3:ncol(data), drop=FALSE],1,sum,na.rm=TRUE) >0)){data <- cbind(data[,1:2], apply(data[,3:ncol(data)], 1, sum, na.rm=TRUE))
        #am reseting second column as the data are now summed:
        #data[,2] <- 0}

      #make year column an actual year...
      data[,1] <- data[,1]+1900

      meta.df <- as.data.frame(matrix(fcs.vec[x['year.first.ind']-(x['number.metarows']:1)], nrow=1), stringsAsFactors = FALSE )
      colnames(meta.df) <- paste0('meta', 1:x['number.metarows'])
      meta.list <-  apply(meta.df, 1, strsplit, ",")
      meta.list <- lapply(meta.list[[1]],FUN = trimws)

      #make data a data.frame
      data <- data.frame(data)

      return(c(list(metadata=meta.list), list(data.str=data.str, data=data)))
    },
    fcs.vec)#END apply

  #put stock name as a column into each data table
  fcs.list <- lapply(fcs.list, FUN = function(x){

    colnames(x[[3]])[1:2] <- c('year', 'age.structure')
    colnames(x[[3]])[3] <- "totalabundance"
    colnames(x[[3]])[4:ncol(x[[3]])] <- paste0("age.", (4:(ncol(x[[3]])))-1)


    stock.acronym <- x[[1]]$meta1[1]


    meta.first.level <- unlist(lapply(x[1]$metadata,"[[",1))[-1] #excluce stock name

    #test if any of the metadata (after stock) is also a stock name.
    # this implies data is for combined stocks

    secondstock.bol <- meta.first.level %in% stocks.key$stock

    if(any(secondstock.bol)){

      second.stock <- meta.first.level[secondstock.bol]
    }else{
      second.stock <- NULL
      }


    x[[3]]$stock <- paste0(stock.acronym, second.stock )

    #defining the data types
    if(x[[1]]$meta3[1]==1){
      data.type <- 'escapement'
    }else if(x[[1]]$meta3[1] %in% c(2,5)){
      data.type <- 'terminalrun'
    }else{
      data.type <- 'undefined'
    }#END if
    x[[3]]$data.type <- data.type

    #rearrange dataframe columns to put stock in first and keep data in final columns
    x[[3]] <- x[[3]][,c('stock', 'year', "data.type", 'age.structure', 'totalabundance', colnames(x[[3]])[4:(ncol(x[[3]])-2)]) ]

    #build a long table of each stock
    x$data.long <- reshape(x[[3]], dir='long',idvar=c('year', 'stock', 'data.type'), varying = list(5:ncol(x[[3]])), timevar = 'agegroup', times= colnames(x[[3]][5:ncol(x[[3]])]) , v.names='value' )
    x$data.long$agegroup <- factor(x$data.long$agegroup,  levels = colnames(x[[3]][5:ncol(x[[3]])])  )
    return(x)
  } )#END lapply


  #naming each sublist with the stock 3 letter name, found in the metadata:
  names(fcs.list) <-  unlist(lapply(fcs.list, function(list.sub){ list.sub$metadata$meta1[[1]] }))

  #build long table that combines stocks:
  fcs.datatables <- lapply(fcs.list, `[[`, 4)
  data.long <- do.call(rbind, fcs.datatables)

  #agreed with Antonio that if age.5=0 then revise to 1 for both data.types
  data.long$value[data.long$agegroup=="age.5" & data.long$value==0] <- 1

  #clear row names of both objects:
  rownames(data.long) <- NULL

  for(i in fcs.list){
  	rownames(i["data"]) <- NULL
  	rownames(i["data.long"]) <- NULL
  }


  #subset all data to be in the year range argument:
  data.long <- data.long[data.long$year>=startingyear & data.long$year<=finalyear,]

    for(i in 1:length(fcs.list)){
     fcs.list[[i]]$data <- fcs.list[[i]]$data[fcs.list[[i]]$data$year>=startingyear & fcs.list[[i]]$data$year<=finalyear,]
     fcs.list[[i]]$data.long <- fcs.list[[i]]$data.long[fcs.list[[i]]$data.long$year>=startingyear & fcs.list[[i]]$data.long$year<=finalyear,]
    }


  return(list(data.long=data.long, stocks=fcs.list))
}#END readFCS


readStockKey <- function(pathname=NA,...){
  if(is.na(pathname)){
    filename <- "stocks.txt"
    data.path <- "."
    data.subpath <- NULL
    pathname <- paste(data.path, data.subpath,  filename, sep='/')
   }

  stocks <- read.delim(pathname, stringsAsFactors = FALSE,...)

  #colnames(stocks) <- tolower(colnames(stocks))
  colnames(stocks) <-c("stocknumber", "stock", "description", "acronym.search", "acronym.replace")
  return(stocks)
}#END readStockKey

readStockMap <- function(pathname=NA){
  if(is.na(pathname)){
    filename <- "Old-New_Stock_Mapping.csv"
    data.path <- "."
    data.subpath <- NULL
    pathname <- paste(data.path, data.subpath,  filename, sep='/')
  }

  stockmap <- read.csv(pathname, stringsAsFactors = FALSE)
  stockmap$linkID <- ifelse(tolower(stockmap$new)=="no", stockmap$stocknumber,NA)
  stockmap$acronym.old[nchar(stockmap$acronym.old)==0] <- NA
  return(stockmap)
}#END readStockMap


#' @title (Calibration Performance) Performance Measure Summation
#'
#' @description Cumulative sums of PM values for model ranking. This may not
#'   longer be in use?
#'
#' @param dat A dataframe, see details.
#'
#' @return A dataframe of the summed PM values (across ages), grouped by
#'   calibration model. The column \code{allages} is the sum of RMSE values
#'   across stocks, for all \code{agegroup}'s excludeing "totalabundance". The
#'   column \code{totalabundance} is the sum of RMSE values across stocks, for
#'   only the "totalabundance" agegroup.
#'
#' @details The arguement \code{dat} is generally the outpt from
#'   \code{\link{calcPMs}}.
#' @export
#'
#' @examples
#' \dontrun{
#' sumPMs(dat)
#' }
sumPMs <- function(dat){

 #
 #  if(any(dat$agegroup != "totalabundance")){
 #  sums.bymodel.byage <- aggregate(value~calibration + agegroup + pm.type, FUN = sum, data = dat[dat$agegroup != "totalabundance",], na.rm=TRUE)
 #
 #  sums.allages <- aggregate(value~calibration + pm.type, FUN = sum, data = dat[dat$agegroup != "totalabundance",], na.rm=TRUE)
 #  sums.allages$agegroup <- "allages"
 #  }# if(any(dat$agegroup !=
 #
 # if(any(dat$agegroup == "totalabundance")){
 #  totalabundance <- aggregate(value~calibration + pm.type, FUN = sum, data = dat[dat$agegroup == "totalabundance",],  na.rm=TRUE)
 #  totalabundance$agegroup <- "totalabundance"
 #  sums.allages <- rbind(sums.allages, totalabundance)
 # }# if(any(dat$agegroup ==





  sums.bymodel.byage <- aggregate(value~calibration+agemetagroup+agegroup+pm.type, FUN=sum, data=dat, na.rm=TRUE)
  sums.allages <-  aggregate(value~calibration+agemetagroup+pm.type, FUN=sum, data=dat, na.rm=TRUE)

  sums.allages$agegroup <- "summed" #t "totalabundance"
  sums.allages$agegroup[sums.allages$agemetagroup=="age.structure"] <- 'summed'

  sums.allages <- sums.allages[,colnames(sums.bymodel.byage)]
  results <- rbind(sums.allages,sums.bymodel.byage )

  results$pm.type <- paste0(results$pm.type, ".arithmetic")
  results$value <- round(results$value)
  return(results)

}#END sumPMs



#' @title (Calibration Performance) Tabulate Ranking
#'
#' @description Tabulate Ranking Of calibation models. Used internally by other
#'   functions.
#'
#' @details This function calls \code{\link{calcRanks}} and organizes the ouput
#'   for comparing performance metric specific ranking of calibration models.
#'   The argument \code{metrics} is a list and is to be taken from the output of
#'   \code{\link{calcPMs}}.
#'
#' @param metrics A \code{list} represented by the output of
#'   \code{\link{calcPMs}}.
#' @param ... Further arguments. Currently limited to \code{rank.method}, which
#'   is sent to \code{\link{calcRanks}}.
#'
#' @return A list, with length equal to the number of ranking methods chosen.
#' @export
#'
#' @examples
#' \dontrun{
#' ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
#' }
tabulateMetrics <- function(metrics, groupingby, ...){
  rank.method <- list(...)
  rank.results <- list()

  #the variable for statistics is whatever groupingby variable is not:
  pooling.var <- c('stock', 'agegroup')[c('stock', 'agegroup') != groupingby]
  metrics.df <- metrics$metrics.long

  pm.vec <- sort(unique(metrics.df$pm.type))
  metrics.df$groupingvar <- unlist(metrics.df[groupingby])
  #groupingvar can be either age or stock
  byPM.bygroupingvar.bymodel.mean <- aggregate(value~pm.type+agemetagroup+groupingvar+calibration, data = metrics.df, FUN = mean, na.rm=TRUE)

  byPM.bygroupingvar.bymodel.rank <-  with(byPM.bygroupingvar.bymodel.mean, by(byPM.bygroupingvar.bymodel.mean, list(byPM.bygroupingvar.bymodel.mean$pm.type,  byPM.bygroupingvar.bymodel.mean$agemetagroup, byPM.bygroupingvar.bymodel.mean$groupingvar), FUN=function(x){

    value.ind <- which(colnames(x)=='value')
    #rank method is passed via the ...
    calcRanks(dat = x,  value.ind, abs.value = TRUE, ... )}))

  byPM.bygroupingvar.bymodel.rank <- do.call('rbind',byPM.bygroupingvar.bymodel.rank)


  #ranaming so vars in prep for rbind:
  byPM.bygroupingvar.bymodel.rank <- subset(byPM.bygroupingvar.bymodel.rank, select = -value)
  colnames(byPM.bygroupingvar.bymodel.rank)[colnames(byPM.bygroupingvar.bymodel.rank)=='value.rank'] <- "value"

  metrics.df$pooling.var <- metrics.df[,pooling.var]
  byPM.bygroupingvar.bymodel.mean$pooling.var <- "Average"
  byPM.bygroupingvar.bymodel.rank$pooling.var <- "Rank"

  metrics.df <- metrics.df[,colnames(byPM.bygroupingvar.bymodel.mean)]
  table3.temp <- rbind(metrics.df, byPM.bygroupingvar.bymodel.mean)
  table3.temp <- rbind(table3.temp, byPM.bygroupingvar.bymodel.rank)


  rank.results$table3 <-  table3.temp

  rank.results$table3.statistics <- table3.temp[table3.temp$pooling.var=='Rank' | table3.temp$pooling.var=='Average',]

  table4.temp <- table3.temp[table3.temp$pooling.var=='Rank',]

  rank.byPM.bygroupingvar.bymodel.mean.sums <- aggregate(value~calibration+agemetagroup+pm.type, data=table4.temp, FUN=sum)
  rank.byPM.bygroupingvar.bymodel.mean.sums$groupingvar <- "Summed.Rank"

  table4.temp <- table4.temp[,colnames(rank.byPM.bygroupingvar.bymodel.mean.sums)]
  table4.temp <- rbind(table4.temp, rank.byPM.bygroupingvar.bymodel.mean.sums)

  rank.results$table4 <-  table4.temp

  return(rank.results)
}#END tabulateMetrics




#' @title (Calibration Performance) Build, save, and open an R script to help
#'   run calibration model performance analysis.
#'
#' @description This creates and opens a script named "CalibrationTester.R".
#'   This is a template for the user to work with when doing performance
#'   analysis of the calibration models. This is intended to help the user
#'   understand the work flow. However, due to file path differences, the script
#'   may not work as is. Some object values will need updating (for example the
#'   paths).
#'
#' @return Opens an R script that includes the template of functions to evaluate
#'   CCC performance.
#' @export
#'
#' @examples
#' writeScriptCalibrationTester()
writeScriptCalibrationTester <- function(){

  date.creation <- Sys.time()

script.str <- c("
# !!!! look for USER INPUT ZONE below.!!!!


####### SETUP #######
rm(list=ls())
###### COMMENTS ########
script.name <- 'CalibrationTester'
if(!exists('script.metadata')) script.metadata <- list()

script.metadata[[script.name]] <- list(
fileName = paste0(script.name,'.R'),
author= 'Michael Folkes',",
paste0("date.creation=", "'",as.character(date.creation),"',"),
"date.edit= NA,
comments = NA
)# END list

##########################################################################
## USER INPUT ZONE ##

# NOTE, the pound symbol '#' is a comment line, which R ignores.

####### FINAL YEAR ###
#  year range included in the fcs import:
startingyear <- 1
finalyear <- 2015


####### GROUPING YEAR ###
# if choosing 'brood.year' then programs will only return results for summed brood years
# this can be 'brood.year' or 'return.year':
grouping.year <- 'brood.year'


# regardless of stocks chosen, user can limit evaluation to stocks that
# are common to all model runs (old stocks vs new stocks)
# if only running one model, the following line should be FALSE:
commonstocks <- TRUE


####### DEFINE DATA PATH AND OUTPUT FOLDER ###
#Get file path string for input models

modelgroup.1 <-  tcltk::tclvalue( tcltk::tkchooseDirectory(title='Choose directory of first set of models.'))
modelgroup.2 <-  tcltk::tclvalue( tcltk::tkchooseDirectory(title='Choose directory of second set of models.'))
data.path.vec <- c(modelgroup.1, modelgroup.2)
data.path.vec <- data.path.vec[data.path.vec != '']

#location for files that has stock number and acronym to allow merging of CCC and FCS
stockkey.1 <- choose.files(default = paste(getwd(), '../data', sep='/'), caption = 'Select stock key files.', multi = FALSE, filters = cbind('Comma delimited (*.csv)', '*.csv'))
stockkey.2 <- choose.files(default = paste(getwd(), '../data', sep='/'), caption = 'Select stock key files.', multi = FALSE, filters = cbind('Comma delimited (*.csv)', '*.csv'))
stocks.key.pathname.vec <- c(stockkey.1, stockkey.2)

# This is the path for where to find the file to map new and old stocks.
# User will only be prompted when: commonstock=TRUE.
#stockmap.pathname <- '../data/Old-New_Stock_Mapping.csv'
stockmap.pathname <- ifelse(commonstocks, choose.files(default = paste(getwd(), '../data' , sep='/'), caption = 'Select file for mapping new and old stocks', multi = FALSE, filters = cbind('Comma delimited (*.csv)', '*.csv'), index = nrow(Filters)),NA )

# location to write all text file results and graphs:
#results.path <- '../data/2016_CI_test/results'
results.path <-  tcltk::tclvalue( tcltk::tkchooseDirectory(title= 'Choose directory for creating results folder.'))

####### STOCK SELECTION ###

# To subset by specific stocks, choose three letter stock names here.
# example choices (currently showing only old stock names):

# 'AKS','BON','CWF','CWS','FRE','FRL','GSH','GSQ','GST','LRW','LYF','MCB','NKF','NKS','NTH','ORC','PSF','PSFPSY','PSN','PSY','RBH','RBHRBT','RBT','SKG','SNO','SPR','STL','SUM','URB','WCH','WCN','WSH'

# Upper or lower case,
# stocks acronyms are separated by commas, and each acromym, is in its own quotes (single or double, but matching quote type),
# together, they are placed between the single set of parentheses: c()

# eg:
#stocks.names <- c('aks', 'bon')
#stocks.names <- c('wch')
stock.names <- 'all'




####### AGE STRUTURE ###
# include stocks with age structure
# TRUE or FALSE must be upper case
age.structure <- TRUE
# include stocks lacking age structure
totalabundance <- TRUE

####### DATA TYPE ###
# escapement, terminalrun
data.type <- c('escapement', 'terminalrun')


####### MINIMUM TOLERATED DATA SIZE ###
#number of years required per stock & age structure type
samplesize.min <- 10

####### PERFORMANCE MEASURES & RANKING ###
# chose one or both ranking methods. Ouput will specify what method was chosen.
ranking <- c('ordinal', 'interpolated')
#ranking <- 'ordinal'

####### MPE STATISTICS ###
# Output MPE data to table 6:
# TRUE or FALSE must be upper case
results.mpe.bol <- TRUE

####### PLOT SELECTION ###
# create FCS vs CCC scatter plots (grouped by calibration model and by age)
doPlots <- TRUE  # TRUE OR FALSE. ALL CAPS

# create png files of plots, if FALSE, plots are ouput to screen (likely dozens):
savepng <-  TRUE  # TRUE OR FALSE. ALL CAPS

## END OF USER INPUT ZONE ##
##########################################################################
# All the following code must be run to produce tables of results:

### Build the model.list object that holds all information for import and other functions:
model.list <- buildmodel.list(commonstocks = commonstocks, stockmap.pathname = stockmap.pathname, data.path.vec = data.path.vec, stocks.key.pathname.vec = stocks.key.pathname.vec, startingyear=startingyear, finalyear=finalyear, grouping.year = grouping.year, age.structure = age.structure, totalabundance =totalabundance, data.type=data.type, results.path = results.path, stock.names = stock.names, groupingby=c( 'agegroup'), ranking = ranking)


### Import data ###
data.combined <- importFCSCCC(model.list = model.list)


##########################################################################
### Table 1 export ###
writeCalibrationTable1(data.combined, results.path = model.list$results.path)

##########################################################################
### Performance tables ###
### Table 2 export and create metrics list object ###
#layman=TRUE is passed to the mpe function. read the help file on mpe for details.
metrics <- calcPMs(data.combined, pm = list(PBSperformance::mpe,  PBSperformance::mape), writecsv = TRUE, samplesize.min = samplesize.min, results.path = model.list$results.path, layman=TRUE)

### Table 3 export ###
# this creates the text files of tabulated ranks
# writeCalibrationTable3 can handle multiple ranking methods:
writeCalibrationTable3(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby)

### Non-parametric model comparison ###
# specify with argument 'tabletype' if grouping data to level of table3 (i.e. age specific)
# or level of table4 (ages pooled)
# the user choice is included in filename to allow differentiation
writeTableOfDifferences(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby, tabletype = 'table3')

writeTableOfDifferences(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby, tabletype = 'table4')


### this relies on the csv files that are written by writeTableOfDifferences() above
tableofdifferences <- importTableOfDifferences(
  ranking.method = model.list$ranking.method,
  pm.type.vec =  c('mape', 'mpe'),
  data.type = model.list$models$group1$data.type,
  results.path = model.list$results.path)

plotPM(tableofdifferences = tableofdifferences , results.path = model.list$results.path, savepng=TRUE )

### Table 4 export ###
# this creates the text files of tabulated ranks
# writeCalibrationTable4 can handle multiple ranking methods:
writeCalibrationTable4(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby)

### Table 5 export ###
#should we expect results if doing brood year age sums?
writeCalibrationTable5(metrics, results.path= model.list$results.path, groupingby=model.list$groupingby)

### TABLE 6 MPE FREQUENCIES ###
# the argument 'mpe.range.vec' selects what mpe values to tabulate,
# whether only positive, only negative, abs value of all, or all three options.
# these choices each create a unique file. the filename includes the term chosen.
if(results.mpe.bol) calcMPEfreq(metrics, results.path = model.list$results.path, groupingby=model.list$groupingby, mpe.range.vec = c('pos', 'neg', 'abs'))

#another example:
if(results.mpe.bol) calcMPEfreq(metrics, results.path = model.list$results.path, groupingby=model.list$groupingby, mpe.range.vec = 'pos')


### Plot FCS vs CCC ###
# The NA value in year.end prompts the code to get the latest year available:
point.col.df <-  data.frame(year.start=c(1979,1985,1999,2009),
year.end=c(1984,1998,2008,NA),
point.col=c('black', 'blue', 'green', 'red'),
stringsAsFactors = FALSE)

if(doPlots) plotFCSvsCCC(data.combined,samplesize.min, results.path = model.list$results.path, point.col.df=point.col.df)

####### END #######

")
  write(script.str, file="CalibrationTester.R")
  file.edit("CalibrationTester.R" )
}#END writeScriptCalibrationTester




#' @title (Calibration Performance) Build, save, and open an R script to
#'   simplify creation of \code{model.list} object.
#'
#' @description This creates and opens a script named "ModelListBuilder.R". This
#'   is a template for the user to work with when doing performance analysis of
#'   the calibration models. This is intended to help the user automatically
#'   build the \code{model.list} object, which is used as an argument to many
#'   functions. Some object values will need updating (for example the paths).
#'
#' @return Opens an R script.
#' @export
#'
#' @examples
#' writeModelListBuilder()
writeModelListBuilder <- function(){

  date.creation <- Sys.time()

  script.str <- c("
####### SETUP #######
rm(list=ls())
###### COMMENTS ########
script.name <- 'ModelListBuilder'
if(!exists('script.metadata')) script.metadata <- list()

script.metadata[[script.name]] <- list(
fileName = paste0(script.name,'.R'),
author= 'Michael Folkes',",
paste0("date.creation=", "'",as.character(date.creation),"',"),
"date.edit= NA,
comments = NA
)# END list


commonstocks <- FALSE
stock.names <- 'all'
#stock.names <- c('urb','sum', 'spr', 'mcb')

stockmap.pathname <- '../data/Old-New_Stock_Mapping_20160916.csv'
#stockmap.pathname <- NA

data.path.vec <- c('../data/calibration2017/CLB17bb', '../data/calibration2017/CLB17cc')
#data.path.vec <- c('../data/2016_CI_test')

stocks.key.pathname.vec <- c('../data/calibration2017/CLB17bb/stocks.oldnames.csv', '../data/calibration2017/CLB17cc/stocks.oldnames.csv')

# this can be 'brood.year' or 'return.year':
grouping.year <- 'return.year'

age.structure <- TRUE
totalabundance <- TRUE
data.type <- c('escapement', 'terminalrun')

samplesize.min <- 10
results.path <- '../data/calibration2017'

ranking <- c('ordinal', 'interpolated')

doPlots <- TRUE  # TRUE OR FALSE. ALL CAPS

savepng <-  TRUE  # TRUE OR FALSE. ALL CAPS

model.list <- buildmodel.list(commonstocks = commonstocks, stockmap.pathname = stockmap.pathname, data.path.vec = data.path.vec, stocks.key.pathname.vec = stocks.key.pathname.vec, grouping.year = grouping.year, age.structure = age.structure, totalabundance =totalabundance, data.type=data.type, results.path = results.path, stock.names = stock.names, groupingby=c( 'agegroup'), ranking = ranking)


####### END #######

")
write(script.str, file="ModelListBuilder.R")
  file.edit("ModelListBuilder.R" )
}#END writeModelListBuilder




#' @title (Calibration Performance) Write Table1 To csv
#'
#' @description Write table 1 as a csv, by stock, calibration, & data.type.
#'
#' @param data.combined A dataframe. Output of \code{\link{importFCSCCC}}.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#'
#' @return Writes a csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' writeCalibrationTable1(data.combined, results.path = model.list$results.path)
#' }
writeCalibrationTable1 <- function(data.combined, results.path="."){
  .makeDir(results.path)
  data.combined <- data.combined[data.combined$agegroup %in% c("age.3", "age.4", "age.5", "totalabundance","brood.sum"),]

  data.combined$age.structure2[data.combined$agegroup %in% c("totalabundance", "brood.sum")]  <- FALSE
  data.combined$age.structure2[data.combined$agegroup %in% c("age.3", "age.4", "age.5")] <- TRUE

  with(data.combined, by(data.combined, list(age.structure2), FUN=function(x){
      data.combined.sub <- x[x$data.type %in% c("escapement","terminalrun"),]

  data.combined.sub <- data.combined.sub[,c('stock','year',	'agegroup', "age.structure2", 'calibration', 'data.type',	'value.ccc', 'value.fcs')]
  data.combined.sub <- data.combined.sub[order(data.combined.sub$stock, data.combined.sub$year,	data.combined.sub$agegroup, data.combined.sub$age.structure2, data.combined.sub$calibration),]
  age.structure.val <- ifelse(any(data.combined.sub$age.structure2),"age.structure","no.age.structure")



  write.csv(data.combined.sub[complete.cases(data.combined.sub),], file = paste(results.path, paste("Table1.long", age.structure.val, ".csv", sep="_"), sep="/" ), row.names = FALSE)

  with(data.combined.sub, by(data.combined.sub, list(stock, calibration, data.type), FUN = function(x){

    dat.ccc.wide <- reshape(x[,c('year', 'agegroup', 'value.ccc')], dir='wide', idvar = 'year', timevar = 'agegroup', v.names = 'value.ccc')
    dat.fcs.wide <- reshape(x[,c('year', 'agegroup', 'value.fcs')], dir='wide', idvar = 'year', timevar = 'agegroup', v.names = 'value.fcs')
    table1 <- merge(dat.ccc.wide, dat.fcs.wide, by='year')
    colnames(table1)[grep("value", colnames(table1))] <- substr(x = colnames(table1)[grep("value", colnames(table1))], 7, nchar(colnames(table1)[grep("value", colnames(table1))]))

    if(any(complete.cases(table1[,2:ncol(table1)]))) {

      write.csv(table1, file = paste(results.path, paste("Table1", x$stock[1], x$calibration[1], age.structure.val, x$data.type[1], ".csv", sep="_"), sep="/" ), row.names = FALSE)
    }

  }) )
 }))
}#END writeCalibrationTable1



#' @title (Calibration Performance) Write Table3
#'
#' @description Write table 3 as a text file.
#'
#' @param metrics A \code{list} represented by the output of
#'   \code{\link{calcPMs}}.
#' @param ranking.method A character defining the ranking method to apply. The
#'   method can be either 'ordinal' or 'interpolated', or both.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#'
#' @return Writes a csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' ### Table 3 export ###
#' # this creates the text files of tabulated ranks
#' # writeCalibrationTable3 can handle multiple ranking methods:
#' writeCalibrationTable3(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby)
#' }
writeCalibrationTable3 <- function(metrics, ranking.method, results.path=".",...){
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)
  args <- list(...)

  lapply(ranking.method, FUN=function(x, metrics, results.path, groupingby){

    rank.method <- x
    #the interim step of "table 3" is done in tabulateMetrics()

    ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
    table3.long <- ranks.list$table3

    with(table3.long, by(table3.long, list(agemetagroup,groupingvar, pm.type),FUN = function(x){

      x2 <- reshape(x, dir='wide', drop=c('pm.type', 'agemetagroup', 'groupingvar'), idvar = 'calibration', timevar = 'pooling.var')
      x2 <- setNames(x2, c('calibration', unique(x$pooling.var)))
      x2 <- x2[order(x2$calibration),]

      data.type <- paste0(unique(metrics$metrics.long$data.type), collapse = "+")

      filename <- paste("Table3", unique(x$agemetagroup), unique(x$pm.type), "GroupingBy",groupingby, rank.method, "ranking.method", unique(x$groupingvar), data.type, ".csv", sep="_")
      write.csv(x2, file = paste(results.path, filename, sep="/"), quote = FALSE, row.names = FALSE)
    }))


  }#END lapply(ranking.method
  , metrics, results.path, args$groupingby)

  options(warn = oldw)

}#END writeCalibrationTable3



#' @title (Calibration Performance) Write Table4
#'
#' @description Write table 4 as a text file.
#'
#' @param metrics A \code{list} represented by the output of
#'   \code{\link{calcPMs}}.
#' @param ranking.method A character defining the ranking method to apply. The
#'   method can be either 'ordinal' or 'interpolated', or both.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#'
#' @return Writes a csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' ### Table 4 export ###
#' # this creates the text files of tabulated ranks
#' # writeCalibrationTable4 can handle multiple ranking methods:
#' writeCalibrationTable4(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby)
#' }
writeCalibrationTable4 <- function(metrics, ranking.method, results.path,...){
  args <- list(...)
  groupingby <- args$groupingby
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)

  for(rank.method in ranking.method){
    ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
    table4 <- ranks.list$table4

    for(age.structure in c("age.structure", "no.age.structure")){
      table4.sub <- table4[table4$agemetagroup==age.structure,]

      for(pm.type in unique(table4.sub$pm.type)){
        table4.sub.bypm <- table4.sub[table4.sub$pm.type==pm.type,]
        table4.sub.bypm.wide <- reshape(table4.sub.bypm, dir='wide',idvar = 'calibration', timevar = 'groupingvar',drop=c('pm.type', 'agemetagroup'))
        table4.sub.bypm.wide <- setNames(table4.sub.bypm.wide, c("Calibration", unique(table4.sub.bypm$groupingvar)))


        if(which(unique(table4.sub$pm.type) ==pm.type)==1) {
          #if the first table then allow for appending stock names (and write over file)
          append.stocks <- TRUE
          append.file <- FALSE
        }

        data.type <- paste0(unique(metrics$metrics.long$data.type), collapse = "+")
        filename <- paste("Table4", unique(table4.sub$agemetagroup),  "GroupingBy", groupingby, rank.method, "ranking.method", data.type, ".csv", sep="_")

          if(append.stocks) {
            csv.count <- ncol(table4.sub.bypm.wide)-1
            stock.vec <- unique(metrics$metrics.long$stock[metrics$metrics.long$agemetagroup==age.structure] )
            cat(c(paste0(stock.vec , collapse = "+"),rep(",", csv.count), "\n"),file = paste( results.path, filename, sep="/" ), append = append.file)
            append.stocks <- FALSE
            append.file <- TRUE
          }
          cat(c(pm.type ,rep(",", csv.count), "\n"), file = paste( results.path, filename, sep="/" ), append = append.file )

          write.table( table4.sub.bypm.wide, file = paste( results.path, filename, sep="/" ), sep = ",", quote = FALSE, append = TRUE, row.names =FALSE)
          cat("\n", file = paste( results.path, filename, sep="/" ),  append = TRUE)


      }#END for(pm.type
    }
  }#END for(rank.method in ranking.method){

  # lapply(ranking.method, FUN=function(x, metrics, results.path, groupingby){
  #
  #   rank.method <- x
  #   #the interim step of "table 3" is done in tabulateMetrics()
  #   ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
  #   table4 <- ranks.list$table4
  #
  #   lapply(c("age.structure", "no.age.structure"),FUN=function(x, table4){
  #     age.structure <- x
  #     browser()
  #     table4.sub <- table4[table4$agemetagroup==age.structure,]
  #
  #     data.type <- paste0(unique(metrics$metrics.long$data.type), collapse = "+")
  #     filename <- paste("Table4", unique(table4.sub$agemetagroup), unique(table4.sub$pm.type) , "GroupingBy", groupingby, rank.method, "ranking.method", data.type, ".csv", sep="_")
  #
  #     append.bol <- FALSE
  #     append.stocks <- TRUE
  #     for(i in 1:length(ranks.list)){
  #
  #       if(attributes(ranks.list$table4[[i]])$agemetagroup==age.structure){
  #         csv.count <- ncol(ranks.list$table4[[i]])-1
  #         if(append.stocks) {
  #
  #           cat(c(paste0(attributes(ranks.list$table4[[i]])$stock.vec , collapse = "+"),rep(",", csv.count), "\n"),file = paste( results.path, filename, sep="/" ), append = append.bol)
  #           append.bol <- TRUE
  #           append.stocks <- FALSE
  #         }
  #         cat(c(attributes(ranks.list$table4[[i]])$df.name,rep(",", csv.count), "\n"), file = paste( results.path, filename, sep="/" ), append = append.bol )
  #
  #         write.table( ranks.list$table4[[i]], file = paste( results.path, filename, sep="/" ), sep = ",", quote = FALSE, append = TRUE, row.names =FALSE)
  #         cat("\n", file = paste( results.path, filename, sep="/" ),  append = TRUE)
  #         }
  #     }
  #  }, table4)
  # }#END lapply(ranking.method
  # , metrics, results.path, args$groupingby)

  options(warn = oldw)
}#END writeCalibrationTable4



#' @title (Calibration Performance)
#'
#' @param metrics A \code{list} represented by the output of
#'   \code{\link{calcPMs}}.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#' @param ...
#'
#' @return Writes a csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' ### Table 5 export ###
#' writeCalibrationTable5(metrics, results.path= model.list$results.path, groupingby=model.list$groupingby)
#' }
writeCalibrationTable5 <- function(metrics, ranking.method, results.path,...){
  args <- list(...)

  metrics.long <- metrics$metrics.long[metrics$metrics.long$pm.type=="mpe",]
  metrics.long$groupingvar <- unlist(metrics.long[args$groupingby])
  mpe.results <- aggregate(value~calibration+agemetagroup+groupingvar, data=metrics.long, FUN=mean, na.rm=TRUE)

  for(agemetagroup in unique(mpe.results$agemetagroup)){

    data.type <- paste0(unique(metrics.long$data.type), collapse = "+")
    filename <- paste("Table5_mpe.results", data.type, agemetagroup , ".csv", sep="_")
    calibrations <- unique(metrics.long$calibration[metrics.long$agemetagroup==agemetagroup ])
    stock.vec <- unique(metrics.long$stock[metrics.long$agemetagroup==agemetagroup & metrics.long$calibration %in% calibrations ] )
    csv.count <- ncol(mpe.results[mpe.results$agemetagroup==agemetagroup,])-1
    cat(c(paste0(stock.vec, collapse = "+"),rep(",", csv.count), "\n"),file = paste( results.path, filename, sep="/" ))

    oldw <- getOption("warn")
    #turn off warnings as it warns about column names being appended to a file that already exists
    options(warn = -1)
    write.table(mpe.results[mpe.results$agemetagroup==agemetagroup,], file = paste(results.path, filename, sep="/" ), sep=",", row.names = FALSE, quote = FALSE, append=TRUE)
    options(warn = oldw)
  }

}#END writeCalibrationTable5



#' @title (Calibration Performance)
#'
#' @param metrics A \code{list} represented by the output of
#'   \code{\link{calcPMs}}.
#' @param ranking.method A character defining the ranking method to apply. The
#'   method can be either 'ordinal' or 'interpolated', or both.
#' @param results.path A character vector of length one. The absolute path or
#'   path relative to the working directory where a new directory named
#'   "results" will be written (if not already present). All output files will
#'   be written here. Default is the current working directory.
#' @param tabletype A character vector of having value of either 'table3' or
#'   'table4'.
#' @param ... Currently just the 'groupingby' argument value to be passed to
#'   \code{\link{tabulateMetrics}}. See that function for details.
#'
#' @return Writes a csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' ### Non-parametric model comparison ###
#' # specify with argument 'tabletype' if grouping data to level of table3 (i.e. age specific)
#' # or level of table4 (ages pooled)
#' # the user choice is included in filename to allow differentiation
#' writeTableOfDifferences(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby, tabletype = 'table3')
#' writeTableOfDifferences(metrics, ranking, results.path = model.list$results.path, groupingby=model.list$groupingby, tabletype = 'table4')
#' }
writeTableOfDifferences <- function(metrics, ranking.method, results.path, tabletype=c('table3', 'table4'), ...){
  args <- list(...)
  groupingby <- args$groupingby
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)

  lapply(ranking.method, FUN=function(x, metrics, results.path, groupingby, tabletype){

    rank.method <- x

    ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
    table.average <- ranks.list$table3.statistics

    table.average <- table.average[order(table.average$pooling.var, table.average$pm.type, table.average$agemetagroup, table.average$groupingvar, table.average$calibration),]

    table.average.temp <- table.average[table.average$pooling.var=="Average",]

    if(tabletype=="table3"){

      table.ranks.temp <- table.average[table.average$pooling.var=="Rank",]
      colnames(table.ranks.temp)[which(colnames(table.ranks.temp)=="value")] <- "rank"

    }else if(tabletype=='table4'){

      table.average <- aggregate(value~pm.type+agemetagroup+calibration, data=table.average[table.average$pooling.var=="Average",], sum)
      table.average$groupingvar <- 1
      table.average$pooling.var <- "Average"
      table.average.temp <- table.average[table.average$pooling.var=="Average",]

      table.ranks.temp <- with(table.average, by(table.average, list(pm.type,agemetagroup), FUN=function(x){
        calcRanks(x , columnToRank = 'value', rank.method=rank.method, abs.value=TRUE )
      }))
      table.ranks.temp <- do.call('rbind', table.ranks.temp)
      colnames(table.ranks.temp)[which(colnames(table.ranks.temp)=="value.rank")] <- "rank"

    }#END if(tabletype=="table3"){


    table.bestmodel <- table.ranks.temp[table.ranks.temp$rank==min(table.ranks.temp$rank),]
    colnames(table.bestmodel)[which(colnames(table.bestmodel)=="calibration")] <- "calibration.best"


    table.diffs <- aggregate(value~pm.type+agemetagroup+groupingvar, data = table.average[table.average$pooling.var=="Average",], FUN = function(x){abs(x)- min(abs(x)) })

    table.diffs <- data.frame(table.diffs[,1:3], as.data.frame(table.diffs[,4:ncol(table.diffs)]))
    colnames(table.diffs) <- c(colnames(table.diffs)[1:3], unique(table.average$calibration))
    table.diffs.long <- reshape(table.diffs, dir='long', idvar=c('pm.type', 'agemetagroup', 'groupingvar'), varying = list(4:ncol(table.diffs)), timevar = 'calibration',times = unique(table.average$calibration),  v.names= 'value')
    rownames(table.diffs.long) <- NULL
    #table.diffs.long <- table.diffs.long[order(table.diffs.long$pm.type, table.diffs.long$agemetagroup, table.diffs.long$groupingvar, table.diffs.long$calibration),]

    table.diffs.long$significance <- ifelse(table.diffs.long$value<0.05, "NSD", ifelse(table.diffs.long$value>=0.05 & table.diffs.long$value<0.1,"*", ifelse(table.diffs.long$value>=0.1 & table.diffs.long$value<0.15,"**","***" )))

    table.diffs.long <- table.diffs.long[,c('pm.type', 'agemetagroup', "groupingvar", "calibration", "significance")]

    table.diffs.long <- merge(table.diffs.long, table.bestmodel[,c("pm.type", "agemetagroup", "groupingvar", "calibration.best")], by= c("pm.type", "agemetagroup", "groupingvar"))
    table.diffs.long$calibration.compare <- paste(table.diffs.long$calibration.best, table.diffs.long$calibration, sep="-")

    #next twp lines empty calibration.compare and significance cell if comparing same model to itself:
    table.diffs.long$calibration.compare[table.diffs.long$calibration.best== table.diffs.long$calibration] <- ""
    table.diffs.long$significance[table.diffs.long$calibration.best== table.diffs.long$calibration] <- ""



    table.diffs.long <- merge(table.diffs.long, table.ranks.temp[, c("pm.type", "agemetagroup", "groupingvar", 'calibration', 'rank')], by= c("pm.type", "agemetagroup", "groupingvar", 'calibration'))

    table.diffs.long <- merge(table.diffs.long, table.average.temp[, c("pm.type", "agemetagroup", "groupingvar", 'calibration', 'value')], by= c("pm.type", "agemetagroup", "groupingvar", 'calibration'))

    with(table.diffs.long, by(table.diffs.long, list(pm.type),FUN = function(x){

      pm.type <- unique(x$pm.type)
      x <- data.frame(x[,c("agemetagroup", "groupingvar", "calibration", "value", 'rank', 'calibration.compare', "significance")])
      colnames(x)[which(colnames(x)=="value")] <- pm.type
      x <- x[order(x$agemetagroup, x$groupingvar, x$rank),]
      data.type <- paste0(unique(metrics$metrics.long$data.type), collapse = "+")
      filename <- paste("TableOfDifferences",tabletype, rank.method, "ranking.method", pm.type, data.type, ".csv", sep="_")
      write.csv(x, file = paste(results.path, filename, sep="/"), quote = FALSE, row.names = FALSE)
    }))


  }#END lapply(ranking.method
  , metrics, results.path, args$groupingby, tabletype)

  options(warn = oldw)


}#END writeTableOfDifferences


