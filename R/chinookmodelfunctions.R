#' Title
#'
#' @param model.list
#' @param data.combined
#'
#' @return
#' @export
#'
#' @examples
addsums <- function( data.combined){
  #get sums by brood year:
  data.forsum <- data.combined[data.combined$data.type %in% c("escapement", "terminalrun") & data.combined$agemetagroup=="age.structure" & data.combined$brood.complete==TRUE & data.combined$age>2,]


  fcs.sum <- aggregate(value.fcs~stock+brood.year+data.type+calibration, data = data.forsum, sum, na.rm=TRUE)
  fcs.sum$agemetagroup <- "age.structure.sum"
  fcs.sum$agegroup <- "brood.sum"

  ccc.sum <- aggregate(value.ccc~stock+brood.year+data.type+calibration, data = data.forsum, sum, na.rm=TRUE)
  ccc.sum$agemetagroup <- "age.structure.sum"
  ccc.sum$agegroup <- "brood.sum"

  sums.merged <- merge(fcs.sum,ccc.sum, by=colnames(fcs.sum)[colnames(fcs.sum) != "value.fcs"], all=TRUE)
  #add column names into sums.merged to match data.combined:
  new.colnames <- colnames(data.combined)[! colnames(data.combined) %in% colnames(sums.merged)]
  sums.merged[,new.colnames] <- NA

  #data.combined <- rbind(data.combined, sums.merged)

  #there can't be a return year shown for the summed broods:
  sums.merged$year <- sums.merged$brood.year
  return(sums.merged)
}#END addsums

#' Automatic Creation Of Input List
#'
#' @description Constructs list of arguements to be utilized by numerous functions within PBSctc.
#'
#' @param stock.names A vector of the three letter stock name acronyms, or "all" for all stocks.
#' @param commonstocks
#' @param stockmap.pathname
#' @param data.path.vec
#' @param stocks.key.pathname.vec
#' @param age.structure
#' @param totalabundance
#' @param data.type
#' @param results.path
#'
#' @return
#' @export
#'
#' @examples
buildmodel.list <- function(stock.names="all", commonstocks=FALSE, stockmap.pathname, data.path.vec, stocks.key.pathname.vec, grouping.year, age.structure, totalabundance, data.type=c("escapement", "terminalrun"), results.path=NA, groupingby=c('stock', 'agegroup'), ranking=c('ordinal', 'interpolated')){
  stocks.key.pathname.vec <- .expand_args(stocks.key.pathname.vec,data.path.vec)[[1]]
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
  model.list <- list(commonstocks=commonstocks, stockmap.pathname=stockmap.pathname, results.path=results.path, grouping.year=grouping.year, groupingby=groupingby, ranking=ranking, models=models)

}#END buildmodel.list


#' calcMPEfreq
#'
#' @return
#' @export
#'
#' @examples
calcMPEfreq <- function(metrics.arg, results.path=".",mpe.range.vec=c('abs', 'neg', 'pos'), ...){
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



#' Group Performance Measures
#'
#' @description A wrapper to calculate multiple performance measures.
#'
#' @param data.combined A dataframe.
#' @param datasubset A character vector defining the values from data.type to be selected.
#' @param pm A list of performance measures to calculate.
#' @param writecsv A Boolean confirming if output should also be written to a csv file.
#'
#' @details This is a convenient wrapper that estimates one or more performance
#' measures for the data supplied. This function is somewhat specialized for use
#' on CTC calibration model comparisons (most often against values in the *.fcs files.
#' The output from the \code{mergedata} function is what normally would be used for
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
calcPMs <- function(data.combined, datasubset=  c('escapement', 'terminalrun'),
                    pm =list(PBSperformance::mre, PBSperformance::mae, PBSperformance::mpe, PBSperformance::mape, PBSperformance::rmse),
                    samplesize.min=10,
                    writecsv=TRUE,results.path="." ){

  data.combined.sub <- data.combined[data.combined$data.type %in% datasubset,]

  results <- with(data.combined.sub, by(data.combined.sub, list(calibration, stock, data.type, agegroup), FUN= function(x){
    #browser()
    if(sum(complete.cases(x[,c('value.ccc','value.fcs')]))<samplesize.min){
      #if number of years is too small just replacing all data with NA is easier
      # as it proceeds with calculationa and desired output structure.
      x[,c('value.ccc','value.fcs')] <- NA
    }
    expect = x[,'value.ccc']
    obs = x[,'value.fcs']
    pm.list <- lapply(pm, function(x) eval(x)(expect , obs ))
    pm.df <- data.frame(lapply(pm.list, "[[",2))
    colnames(pm.df) <- lapply((lapply(pm.list, "[",2)),names)

    data.frame(stock= x[1,'stock'], agemetagroup=x[1,'agemetagroup'], agegroup=x[1,'agegroup'], data.type=x[1,'data.type'], calibration= x[1,'calibration'], pm.df, stringsAsFactors = FALSE )
  }))
#browser()
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

#' Rank Calculation
#'
#' @description Calculate rank of data.
#'
#' @param dat A dataframe
#' @param columnToRank A vector of integers defining what columns to rank (independently)
#' @param rank.method A character defining the ranking method to apply.
#'
#' @return A dataframe comprising all columns of the argument: \code{dat} and one
#' additional column of ranks for each column column declared in \code{columnToRank}.
#' The name of each rank column is a combination of the data column name and ".rank".
#' @export
#'
#' @examples
calcRanks <- function(dat,columnToRank, rank.method=c('ordinal', 'interpolated'), abs.value=FALSE){

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

.DTK.plot <- function (x = "DTK.test output", ...)
{
  args <- list(...)

  out = x[[2]]
  a = x[[1]]
  n = nrow(out)
  mai.tmp <- par('mai')
  par(mai=c(mai.tmp[1],args$left.mai, mai.tmp[3:4]))
  #par(mgp=c(15,1,0))

  plot(c(max(out[, 3]), min(out[, 2])), c(1, n), type = "n",
       xlab = "Mean Difference", ylab = "",
       yaxt = "n")
  axis(2, at = seq(1,n), labels = rownames(out)[n:1], las=1)
  title(main = paste(paste(((1 - a) * 100), "%", sep = ""),
                     "Confidence Intervals", "\n",
        "Red intervals are significant and black non-significant"))
  for (i in 1:n) {
    i.rev <- rev(1:n)[i]
    lines(x = c(-1e+100, 1e+100), y = c(i.rev, i.rev), col = "gray",
          lty = 2)
    if (out[i, 2] > 0 || out[i, 3] < 0) {
      lines(out[i, 2:3], y = c(i.rev, i.rev), col = "red", lwd = 2)
      points(x = out[i, 1], y = i.rev, col = "red")
    }
    else {
      lines(out[i, 2:3], y = c(i.rev, i.rev))
      points(x = out[i, 1], y = i.rev)
    }
  }
}#END .DTKplot


.makeDir <- function(path){
   if(!dir.exists(path)) dir.create(path)
}#END .makeDir

.expand_args <- function(...){
  dots <- list(...)
  max_length <- max(sapply(dots, length))
  lapply(dots, rep, length.out = max_length)
}#END .expand_args

.getBrokeninterval <- function(x){
  result <- c(1,diff(x))
  df <- data.frame(x,result)
  df$res2 <- df$result>1
  df$res2[1] <- TRUE
  df$res3 <- cumsum(df$res2)
  df
  df2 <- aggregate(x~res3, data=df, FUN = function(x2){

    range.temp <- range(x2)
    range.str <- ifelse(diff(range.temp)==0,range.temp[1],paste(range.temp,collapse = "-"))
    return(range.str)
  }
  )
  return(paste(df2$x,collapse = ","))
}#END .getBrokeninterval

getlinkID <- function(stockmap){

  tempID <- rbind(setNames(stockmap[,c("acronym.old", "linkID")], c("acronym.replace", "linkID")), stockmap[,c("acronym.replace", "linkID")])
  tempID <- tempID[complete.cases(tempID),]
  tempID <- unique(tempID)
  #colnames(tempID) <- c("acronym", "linkID")
  return(tempID)
}#END getlinkID


importData <- function(data.path.vec=NA, model.list=NULL){

  .import.vec <- function(data.pathname){

     ### read in CCC files ###
     #data.pathname <- paste0(data.path, "/", data.subpath)
     filename <- list.files(data.pathname, pattern = "CCC")
     filepath <- paste(data.pathname,  filename, sep='/')
     ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE)

     ### read in FCS files ###
     fcs.files <- list.files(data.pathname, pattern = "\\.FCS")
     filename <- fcs.files[grep("OCN", x = fcs.files)]
     filepath <- paste(data.pathname, filename, sep='/')
     fcs.list <- readFCS(filepath)


     ### merge the two file types into a common dataframe
     data.combined <- mergeData(ccc.list, fcs.list, stocks.names )
     return(data.combined)
   }#END .import.vec

  .import.list <- function(model.sublist){

    if(exists("stocks.key.pathname", where=model.sublist)){
      stocks.key <- readStockKey( model.sublist$stocks.key.pathname, sep=",")
    }


    ### read in CCC files ###

    data.pathname <- model.sublist$ccc$path

    if(exists("filename", where=model.sublist$ccc) & any(!is.na(model.sublist$ccc$filename))){
      filename <- model.sublist$ccc$filename
    }else{
      filename <- list.files(data.pathname, pattern = "CCC")
    }

    filepath <- paste(data.pathname,  filename, sep='/')

    if(exists("stocks.key.pathname", where=model.sublist)){

      ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE,stocks.key=stocks.key)
    }else{
      ccc.list <- sapply(filepath, readCCC, USE.NAMES = FALSE)
    }

    ### read in FCS files ###
    data.pathname <- model.sublist$fcs$path
    if(exists("filename", where=model.sublist$fcs) & any(!is.na(model.sublist$fcs$filename))){
      filename <- model.sublist$fcs$filename
    }else{
      fcs.files <- list.files(data.pathname, pattern = "\\.FCS")
      filename <- fcs.files[grep("OCN", x = fcs.files)]
    }

    filepath <- paste(data.pathname, filename, sep='/')
    fcs.list <- readFCS(filepath, stocks.key = stocks.key )


    ### merge the two file types into a common dataframe
    data.combined <- mergeData(ccc.list, fcs.list, stocks.names =model.sublist$stock.names  )

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
    brood.complete <- reshape(brood.complete, dir='long', varying = list(2:ncol(brood.complete)),idvar = 'stock', , times = colnames(brood.complete)[-1])
    brood.complete <- data.frame(stock=brood.complete$stock, brood.year=as.integer(substr(brood.complete$time,2,5)), brood.complete=brood.complete$X1974, stringsAsFactors = FALSE)

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

  if(model.list$grouping.year=="brood.year") data.combined.df <- addsums( data.combined = data.combined.df)


  return(data.combined.df)


}#END importData

#' Merge FCS And CCC Data
#'
#' @description Merge FCS and CCC data
#'
#' @param ccc.list A list, see details
#' @param fcs.list A list, see details
#'
#' @details The function arguments are output from the functions \code{readCCC}
#' and \code{readFCS}, respectively.
#'
#' @return A dataframe is produced.
#' @export
#'
#' @examples
mergeData <- function(ccc.list, fcs.list, stocks.names='all'){

  ccc.list <- lapply(ccc.list,FUN = function(x){
    x$data.long$calibration <- x$calibration
    return(x)
    })
  ccc.allcalib <- do.call(what = "rbind", lapply(ccc.list, "[[",5) )

  fcs.stocks <- unique(fcs.list$data.long$stock)
  fcs.stocks.combined <- lapply(fcs.stocks[nchar(fcs.stocks)>3], FUN = substring,c(1,4), c(3,6) )

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
#browser()
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
}#END mergeData

#' Model Comparison Plots
#'
#' @description 1:1 plots for comparing FCS and CCC data by stock and age.
#'
#' @details Option to produce PNG files. Relies on the package: \code{lattice}.
#' Argument \code{data.combined}
#' Argument \code{agegroun.n} determines the number of lattice panels per page.
#' The value would typically range 1--4 based on the number of age groups being evaluated.
#' Considering the unique values of \code{agegroup} are c('age.3', 'age.4', 'age.5', 'totalabundance').
#'
#' @param data.combined A dataframe, see details
#' @param agegroup.n An integer, equates to the number of plots per page. See detail
#' @param savepng A Boolean confirming to also save a PNG file of each plot.
#' @param ...
#'
#'
#' @return One lattice plot per unique combination of stock and calibration model,
#' with option to produce the same in the form of a PNG file.
#' @export
#'
#' @examples
plotCompare <- function(data.combined, savepng=FALSE, results.path = ".", point.col.df, ...){

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
}#END plotCompare

#' Model Comparison Plots
#'
#' @description 1:1 plots for comparing FCS and CCC data by stock and age.
#'
#' @details Option to produce PNG files. Relies on the package: \code{lattice}.
#' Argument \code{data.combined}
#' Argument \code{agegroun.n} determines the number of lattice panels per page.
#' The value would typically range 1--4 based on the number of age groups being evaluated.
#' Considering the unique values of \code{agegroup} are c('age.3', 'age.4', 'age.5', 'totalabundance').
#'
#' @param data.combined A dataframe, see details
#' @param agegroup.n An integer, equates to the number of plots per page. See detail
#' @param savepng A Boolean confirming to also save a PNG file of each plot.
#' @param ...
#'
#'
#' @return One lattice plot per unique combination of stock and calibration model,
#' with option to produce the same in the form of a PNG file.
#' @export
#'
#' @examples
.plotCompare.old <- function(data.combined, agegroup.n, savepng=FALSE, results.path = ".", ...){
  stock.name <- unique(data.combined$stock)
  years.range <- range(data.combined$year)
  xyplot.eg <- lattice::xyplot(value.fcs/1e3~value.ccc/1e3|as.factor(agegroup), data=data.combined, as.table=TRUE,
                      main = paste0(stock.name, " (", years.range[1],"-",years.range[2],")" ),
                      scales=list(relation="free", cex=0.5),
                      aspect=1, layout= c(1, agegroup.n),
                      prepanel = function(x, y, subscripts) {
                        rr<- range(cbind(x,y), na.rm=TRUE)

                        list(xlim = rr, ylim= rr)
                      },
                      panel=function(x,y, subscripts,...){

                        if(length(x) > samplesize.min) {
                          lattice::panel.xyplot(x, y, type='n',...)
                          lattice::panel.abline(a=0, b=1)
                          lattice::panel.points(x,y, pch=1,cex=0.5, col='black')
                        }
                      },...)

  if(savepng){
    filename <- paste(unique(data.combined$agemetagroup ),  unique(data.combined$stock), unique(data.combined$data.type), unique(data.combined$calibration), ".png", sep="_")
  .makeDir(results.path)
  png(file= paste(results.path, filename, sep="/"), wid=8, height=8, units="in", res=600)
  print(xyplot.eg)
  dev.off()
  }else{
    print(xyplot.eg)
  }
}#END .plotCompare.old

plotPM <- function(ranking, pm.type.vec=c('mpe', 'mape'),  data.type ="escapement+terminalrun", results.path){
  require(lattice)
  plotting.df <- expand.grid(pm.type=pm.type.vec, rank.method=ranking)
  data.type <- paste0(data.type, collapse = "+")

  .makeDir(results.path)

  td <- apply(plotting.df,1, function(x, data.type, results.path){
      filename <- paste("TableOfDifferences", "table3", x['rank.method'], "ranking", x['pm.type'], data.type, ".csv", sep="_")
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

plotFCSvsCCC <- function(data.combined, samplesize.min, results.path = ".",
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
    # I use this test to prevent calls of plotCompare() when there is no non-age.structured data
    comp.case.count <- c(with(x, by(x, list(agegroup, calibration), FUN=function(x2){
      sum(complete.cases(x2[,c('value.ccc', 'value.fcs')]))
    })))

 # all data sets within a stock must have minimum size
    if(any(comp.case.count > samplesize.min)) {
      x <- x[complete.cases(x[,c('value.ccc', 'value.fcs')]),]

      plotCompare(x,  xlab= paste0("Modelled ",unique(x$data.type), " (1000s)"), ylab= paste0("Observed ",unique(x$data.type), " (1000s)"), savepng = savepng, results.path = results.path, point.col.df ,... )
    }#END if
  }),results.path)

}#END plotFCSvsCCC



.plotFCSvsCCC.old <- function(data.combined, samplesize.min, results.path = "."){
  data.combined.sub <- data.combined[data.combined$data.type %in% c('escapement', 'terminalrun') & data.combined$agegroup %in% c('age.3', 'age.4', 'age.5', 'totalabundance'), ]

  with(data.combined.sub, by(data.combined.sub, list(agemetagroup, calibration, stock, data.type), FUN=function(x){
    # this is tested twice as this sum is across all age classes
    # I use this test to prevent calls of plotCompare() when there is no non-age.structured data
    if(sum(complete.cases(x[,c('value.ccc', 'value.fcs')])) > samplesize.min) {
      x <- x[complete.cases(x[,c('value.ccc', 'value.fcs')]),]

      plotCompare(x, agegroup.n = length(unique(x$agegroup)), xlab= paste0("CCC ",unique(x$data.type), " (1000s)"), ylab= paste0("FCS ",unique(x$data.type), " (1000s)"), savepng = savepng, results.path = results.path )
    }#END if
  }),results.path)

}#END .plotFCSvsCCC.old



#' Import CCC Files
#'
#' @description Import one or more CCC files into a list.
#'
#' @details Need details.
#'
#' @param filepath A character vector of the CCC files to be imported. See details
#' @param data.types See details
#' @param stocks.key See details
#'
#' @return A list, with one element per CCC (calibration) file.
#' @export
#'
#' @examples
readCCC <- function(filepath, data.types = c("AEQ", 'cohort', 'termrun', "escape"), stocks.key=NA){
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


  #remove final two years
  final.twoyears <- tail(sort(unique(ccc$year)),2)
  ccc <- ccc[!ccc$year %in% final.twoyears,]


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


#' Import FCS Files
#'
#' @description Import one or more FCS files into a list.
#'
#' @details Need details.
#'
#' @inheritParams readCCC
#' @param first.stockline An integer, row number in FCS file where first stock
#' metadata commences. See details.
#' @param stocks.key See details
#'
#' @return A list, with one element per CCC (calibration) file.
#' @export
#'
#' @examples
readFCS <- function(filepath, first.stockline=3, stocks.key=NA){
  #first.stockline is the row where the first stock begins (ie accounts for header rows not associated with stocks

 # if(is.na(stocks.key)) stocks.key <- stocks.key.hidden

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
    next.meta.ind <- min(which(is.na(as.numeric(unlist(lapply(temp1,"[[",1))))))
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

      #leave out final two years
      data.list <- data.list[-c(length(data.list):(length(data.list)-1))]
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
      #browser()
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


  #build long table that combines stocks:
  fcs.datatables <- lapply(fcs.list, `[[`, 4)
  data.long <- do.call(rbind, fcs.datatables)

  #agreed with Antonio that if age.5=0 then revise to 1 for both data.types
  data.long$value[data.long$agegroup=="age.5" & data.long$value==0] <- 1

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


#' Performance Measure Summation
#'
#' Cumulative sums of PM values for model ranking.
#'
#' @param dat A dataframe, see details.
#'
#' @return A dataframe of the summed PM values (across ages), grouped by calibration model.
#' The column \code{allages} is the sum of RMSE values across stocks, for all \code{agegroup}'s
#' excludeing "totalabundance". The column \code{totalabundance} is the sum of RMSE
#' values across stocks, for only the "totalabundance" agegroup.
#'
#' @details The arguement \code{dat} is generally the outpt from \code{calcPMs}.
#' @export
#'
#' @examples
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

#' Tabulate Ranking
#'
#' @description Tabulate Ranking Of Calibation Models
#'
#' @details This function calls \code{calcRanks} and organizes the ouput for
#' comparing performance metric specific ranking of calibration models.
#' The argument \code{metrics} is a list and is to be taken from the output of
#' \code{calcPMs}.
#'
#' @param metrics A \code{list}. See Details
#' @param ... Further arguments. Currently limited to \code{rank.method}, which
#' is sent to \code{calcRanks}.
#'
#' @return A list, with length equal to the number of ranking methods
#' chosen.
#' @export
#'
#' @examples
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



#' Tabulate Ranking
#'
#' @description Tabulate Ranking Of Calibation Models
#'
#' @details This function calls \code{calcRanks} and organizes the ouput for
#' comparing performance metric specific ranking of calibration models.
#' The argument \code{metrics} is a list and is to be taken from the output of
#' \code{calcPMs}.
#'
#' @param metrics A \code{list}. See Details
#' @param ... Further arguments. Currently limited to \code{rank.method}, which
#' is sent to \code{calcRanks}.
#'
#' @return A list, with length equal to the number of ranking methods
#' chosen.
#' @export
#'
#' @examples
.tabulateMetricsOLD <- function(metrics, ...){
  rank.method <- list(...)
  rank.results <- list(...)

  pm.vec <- sort(unique(metrics$metrics.long$pm.type))

  metrics$metrics.long <- metrics$metrics.long[metrics$metrics.long$pm.type != "mpe",]

  rank.byPM.bystock.byage <-  by(metrics$metrics.long, list(metrics$metrics.long$pm.type, metrics$metrics.long$stock, metrics$metrics.long$agemetagroup, metrics$metrics.long$agegroup), FUN=function(x){
    value.ind <- which(colnames(x)=='value')
    calcRanks(dat = x,  value.ind, ... )})

  rank.byPM.bystock.byage <- do.call('rbind', rank.byPM.bystock.byage)

  rank.byPM.bystock.byage.mean <- aggregate(value.rank~calibration+agemetagroup+agegroup+pm.type, data = rank.byPM.bystock.byage, mean, na.rm=TRUE)

  new.colnames <- colnames(rank.byPM.bystock.byage)[!(colnames(rank.byPM.bystock.byage) %in% colnames(rank.byPM.bystock.byage.mean))]

  #need a temp df as the original is used later
  temp.mean <- rank.byPM.bystock.byage.mean
  temp.mean[,new.colnames] <- NA
  temp.mean$stock <- 'Average'
  rank.results$table3 <-  rbind(rank.byPM.bystock.byage, temp.mean)

  rank.byPM.bystock.byage.mean.sums <- aggregate(value.rank~calibration+agemetagroup+pm.type, data=rank.byPM.bystock.byage.mean, FUN=sum)

  #colnames(rank.byPM.bystock.byage.mean.sums)[colnames(rank.byPM.bystock.byage.mean.sums)=='age.metagroup'] <- 'agegroup'
#HERE
  rank.byPM.bystock.byage.mean.sums$agegroup <- "summed" # "totalabundance"
  rank.byPM.bystock.byage.mean.sums$agegroup[rank.byPM.bystock.byage.mean.sums$agemetagroup=="age.structure"] <- "summed"
  rank.byPM.bystock.byage.mean.sums <- rank.byPM.bystock.byage.mean.sums[,colnames(rank.byPM.bystock.byage.mean)]


  #ranks.combined <- rbind(subset(rank.byPM.bystock.byage.mean,select= -c(age.metagroup)), rank.byPM.bystock.byage.mean.sums)
  ranks.combined <- rbind(rank.byPM.bystock.byage.mean, rank.byPM.bystock.byage.mean.sums)
  ranks.combined$pm.type <- paste(ranks.combined$pm.type, rank.method, sep=".")

  colnames(ranks.combined)[colnames(ranks.combined)=='value.rank'] <- 'value'
  ranks.combined$value <- round(ranks.combined$value, 3)

  # if(any(pm.vec=="rmse")){
  #   rmse.sums <-    sumPMs(metrics$metrics.long[metrics$metrics.long$pm.type=='rmse',])
  #   ranks.combined <- rbind(ranks.combined, rmse.sums)
  # }


 # ranks.combined <-  ranks.combined[ranks.combined$agegroup != "totalabundance",]
  ranks.combined <- ranks.combined[order(ranks.combined$pm.type, ranks.combined$calibration, ranks.combined$agemetagroup, ranks.combined$agegroup ),]

  rank.results$table4 <- by(ranks.combined, list(ranks.combined$agemetagroup, ranks.combined$pm.type), FUN = function(x){
   x2 <- reshape(x, dir='wide', drop=c('pm.type', 'agemetagroup'), idvar = 'calibration', timevar = 'agegroup')
   df.name <- paste(x[1,'agemetagroup'] , x[1,'pm.type'], sep="_" )

   stock.vec <- unique(metrics$metrics.long$stock[ metrics$metrics.long$calibration  %in% x2$calibration & metrics$metrics.long$agemetagroup== x[1,'agemetagroup']])
   attributes(x2) <- c(attributes(x2),list( agemetagroup=x[1,'agemetagroup'], pm.type=x[1,'pm.type'], df.name=df.name, stock.vec=stock.vec ))
   attr(x2, 'row.names') <- 1:length(attr(x2, 'row.names'))
   return(x2)
  })


  df.names <- unlist(lapply(rank.results$table4, FUN = function(x){attributes(x)$df.name }))
  names(rank.results$table4) <- df.names

  #next line removes "value" from each column name (clean up)
  rank.results$table4 <- lapply(rank.results$table4, FUN = function(x){
    colnames(x)[grep("value", colnames(x))] <- substr(colnames(x)[grep("value", colnames(x))],7,  nchar(colnames(x)[grep("value", colnames(x))]))

    return(x)
  })

  # if(any(pm.vec=="rmse")){
  #   order.ind <- factor(c(paste(pm.vec, rank.method, sep="."), "rmse.arithmetic" ), levels= c(paste(pm.vec, rank.method, sep="."), "rmse.arithmetic" ) )
  # }else{
  #   order.ind <- factor(paste(pm.vec, rank.method, sep="."), levels= paste(pm.vec, rank.method, sep=".") )
  # rank.results <- rank.results[order(levels(order.ind ))]
  # }
  #rmse.sums.long <- reshape(rmse.sums, dir='long', varying= list(3:4), timevar = 'age.metagroup', times = colnames(rmse.sums)[3:4],  v.names= 'value')
  #rmse.ranks <- calcRanks(rmse.sums,seq(ncol(rmse.sums), by=-1, len=2))

  return(rank.results)
}#END .tabulateMetricsOLD


testDTK <- function(data, results.path){
  #data is a data frame with same structure as metrics$metrics.long

  with(data,by(data, list(agemetagroup, agegroup, pm.type), FUN = function(x){
    x$calibration.fac <- factor(x$calibration, levels=sort(unique(x$calibration)))

    DTK.result <- DTK.test(x=x$value, f=x$calibration.fac)

    data.type <- paste0(unique(x$data.type), collapse = "+")
    filename.prefix <- paste("DTK",unique(x$agemetagroup), unique(x$agegroup), data.type, unique(x$pm.type), sep="_")

    write.csv(DTK.result[[2]], file = paste(results.path, paste0(filename.prefix,".csv"), sep="/"))

    png(filename = paste(results.path, paste0(filename.prefix,".png"), sep="/"), width = 8,height = 8, units = "in", res = 600)
    .DTK.plot(DTK.result, left.mai=3)
    dev.off()

  }))

}#END testDTK


#' Write Table1 To csv
#'
#' @description Write table 1 as a csv, by stock, calibration, & data.type.
#'
#' @param data.combined
#'
#' @return
#' @export
#'
#' @examples
writeTable1 <- function(data.combined, results.path="."){
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
}#END writeTable1

#' Write Table3
#'
#' @description Write table 3 as a text file.
#'
#' @param metrics
#' @param ranking
#' @param results.path
#'
#' @return
#' @export
#'
#' @examples
writeTable3 <- function(metrics, ranking, results.path,...){
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)
  args <- list(...)

  lapply(ranking, FUN=function(x, metrics, results.path, groupingby){

    rank.method <- x
    #the interim step of "table 3" is done in tabulateMetrics()

    ranks.list <- tabulateMetrics(metrics = metrics, groupingby = groupingby, rank.method)
    table3.long <- ranks.list$table3

    with(table3.long, by(table3.long, list(agemetagroup,groupingvar, pm.type),FUN = function(x){

      x2 <- reshape(x, dir='wide', drop=c('pm.type', 'agemetagroup', 'groupingvar'), idvar = 'calibration', timevar = 'pooling.var')
      x2 <- setNames(x2, c('calibration', unique(x$pooling.var)))
      x2 <- x2[order(x2$calibration),]

      data.type <- paste0(unique(metrics$metrics.long$data.type), collapse = "+")
      filename <- paste("Table3", unique(x$agemetagroup), unique(x$pm.type), "GroupingBy",groupingby, rank.method, "ranking", unique(x$groupingvar), data.type, ".csv", sep="_")
      write.csv(x2, file = paste(results.path, filename, sep="/"), quote = FALSE, row.names = FALSE)
    }))


  }#END lapply(ranking
  , metrics, results.path, args$groupingby)

  options(warn = oldw)

}#END writeTable3

#' Write Table4
#'
#' @description Write table 4 as a text file.
#'
#' @param metrics
#' @param ranking
#' @param results.path
#'
#' @return
#' @export
#'
#' @examples
writeTable4 <- function(metrics, ranking, results.path,...){
  args <- list(...)
  groupingby <- args$groupingby
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)

  for(rank.method in ranking){
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
        filename <- paste("Table4", unique(table4.sub$agemetagroup),  "GroupingBy", groupingby, rank.method, "ranking", data.type, ".csv", sep="_")

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
  }#END for(rank.method in ranking){

  # lapply(ranking, FUN=function(x, metrics, results.path, groupingby){
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
  #     filename <- paste("Table4", unique(table4.sub$agemetagroup), unique(table4.sub$pm.type) , "GroupingBy", groupingby, rank.method, "ranking", data.type, ".csv", sep="_")
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
  # }#END lapply(ranking
  # , metrics, results.path, args$groupingby)

  options(warn = oldw)
}#END writeTable4


writeTable5 <- function(metrics, results.path,...){
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

}#END writeTable5

writeTableOfDifferences <- function(metrics, ranking, results.path, tabletype=c('table3', 'table4'), ...){
  args <- list(...)
  groupingby <- args$groupingby
  .makeDir(results.path)
  oldw <- getOption("warn")
  options(warn = -1)

  lapply(ranking, FUN=function(x, metrics, results.path, groupingby, tabletype){

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
      filename <- paste("TableOfDifferences",tabletype, rank.method, "ranking", pm.type, data.type, ".csv", sep="_")
      write.csv(x, file = paste(results.path, filename, sep="/"), quote = FALSE, row.names = FALSE)
    }))


  }#END lapply(ranking
  , metrics, results.path, args$groupingby, tabletype)

  options(warn = oldw)


}#END writeTableOfDifferences



#' Write Table4
#'
#' @description Write table 4 as a text file.
#'
#' @param metrics
#' @param ranking
#' @param results.path
#'
#' @return
#' @export
#'
#' @examples
.writeTable4.old <- function(metrics, ranking, results.path){
  .makeDir(results.path)
  ranks.results <- lapply(ranking, FUN=function(x, metrics, results.path){

    rank.method <- x
    ranks.results <- tabulateMetrics(metrics = metrics, rank.method)

    lapply(c("age.structure", "no.age.structure"),FUN=function(x, ranks.results){
      data.type <- x
      temp.results <- list()
       for(i in 1:length(ranks.results)){
           if(attributes(ranks.results[[i]])$agemetagroup==data.type){
             temp.results[[attributes(ranks.results[[i]])$df.name]] <- ranks.results[[i]]}
       }

        filename <- paste("Table4", data.type, rank.method,"ranks", ".txt", sep="_")


       .fn.print <- function(temp.results){
          for(i in 1:length(temp.results)){
           print(attributes(temp.results[[i]])$df.name)
           print(temp.results[[i]], row.names=F)
           cat("\n")
          }
         }
       cat(capture.output(fn.print(temp.results), file=paste( results.path, filename, sep="/" ) ))

  }, ranks.results)
  }#END lapply(ranking
  , metrics, results.path)

  #return(ranks.results)
  }#END writeTable4.old
