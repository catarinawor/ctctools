#' (HRJ) Check for matching B & C file names in the vector of file names.
#'
#' @param filepath A character vector, which can have length greater than one.
#'   Each element of the vector is the path and filename for each HRJ file to be
#'   read in.
#'
#' @description This tests for that every file named in the filepath, whether
#'   'b' or 'c' data, the alternate data ('c' or 'b') by stock, has also been
#'   named.
#'
#' @return No object is returned. If files are included in the vector but lack a
#'   matching alternate data, then those are printed to the console.
#' @export
#' @return A message to the console listing the stocks that lacked matching B or
#'   C file.
#'
#' @examples
#' checkMissingfiles(filepath)
checkMissingfiles <- function(filepath){
  if(!is.list(filepath)) {
    filename.string <- list("c1.hrj", 'b1.hrj')
    filepath <- lapply(filename.string, function(x){as.list(filepath[grep(x, tolower(filepath))])})
    names(filepath) <- sapply(filename.string, function(x)substr(x,1,1))
  }

  filepath2 <- lapply(filepath, function(x) list(filepath=unlist(x)))
  filepath2 <- lapply(filepath2, function(x){

    slash.ind <-  gregexpr("/",x$filepath)
    slash.final <- unique(unlist(lapply(slash.ind, max)))
    stock <- substr(x$filepath, slash.final+1, slash.final+3)
    filename <- substr(x$filepath, slash.final+1, unique(nchar(x$filepath)))
    return(list(filepath=unlist(x), stock=stock, filename=filename))
  }
  )

  indices <- 1:length(filepath2)
  for(i in 1:length(filepath2)){
    missing.file <- filepath2[[i]]$stock[!filepath2[[i]]$stock %in% filepath2[[indices[indices !=i]]]$stock]
    result <- paste0("The following stock(s) are found in ", names(filepath2[i]) , " files but missing in ", names(filepath2[indices !=i]), " files:\n", missing.file)
    if(length(missing.file)>=1) {
      cat(result)
      cat("\n")
    }
  }#END for

}#END checkMissingfiles



#' @title (HRJ) Read in data from the HRJ (MS Access) data base.
#'
#' @description This function relies on the RODBC package for reading MS Access
#'   data bases. Due to intermittent, non-transparent changes by Microsoft, the
#'   RODBC package has broken in the past. Additionally, 32 bit Access files
#'   require the use of 32bit R. Based on these extra steps, it is recommended
#'   that the output from this function (a list) be saved to a .Rdata file for
#'   future use. Specifically, the example demonstrates the use of
#'   \code{\link{readHRJAccessDatabase}} in conjunction with
#'   \code{\link{reshapeHRJtolong}} for flexible data manipulation.
#'
#' @param filename A string of length one. The MS Access database name. Can be
#'   *.mdb or *.accdb file.
#' @inheritParams readCatchData
#'
#' @return A list comprising two elements. Each list element is a data frame,
#'   one is the 'C' table and one is the 'B' table. Their formats match that
#'   found in the mdb file.
#' @export
#'
#' @examples
#' \dontrun{
#' # reading 32 bit mdb files requires using 32bit R
#' hrj.list.wide <- readHRJAccessDatabase("HRJ_database 2016b.mdb")
#' hrj.list.long <- reshapeHRJtolong(hrj.list.wide, data.stock)
#' data.hrj <- list(hrj.list.wide=hrj.list.wide, hrj.list.long=hrj.list.long)
#' filename <- "hrj_from_mdb.RData"
#' save(data.hrj, file = filename)
#' load(filename)
#' }
readHRJAccessDatabase <- function(filename){
  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop("The package 'RODBC' is needed for this function to work -
         and may only work in MS Windows. Please install it.",
         call. = FALSE)
  }

  # reading 32 bit mdb files requires using 32bit R
  filename <- paste0("./", filename)
  driver.name <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  driver.name <- paste0(driver.name, "DBQ=", filename)
  con <- RODBC::odbcDriverConnect(driver.name)
  tables.df <- RODBC::sqlTables(con, tableType = 'TABLE')
  data.list <- list()
  data.list <- apply(tables.df, 1, function(x,con2){

    df.tmp <- RODBC::sqlFetch(con, x["TABLE_NAME"])
    return(df.tmp)
  }, con)
  names(data.list)  <- tables.df$TABLE_NAME
  RODBC::odbcCloseAll()

  hrj.list <- list(sourcefile=filename, hrj.cwt.list=data.list)


  return(hrj.list)
}#END readHRJAccessDatabase



#' (HRJ) Read in and combine multiple HRJ text files.
#'
#' @param filepath A character vector, which can have length greater than one.
#'   Each element of the vector is the path and filename for each HRJ file to be
#'   read in. As each file has its own path informartion, files can be read in
#'   from multiple folders.
#'
#' @description The function reads in multiple HRJ files and produces a list
#'   comprising two lists. The first list consists of the HRJ data in two data
#'   frames, one for 'b' data and one for the 'c' data. The format is wide,
#'   matching that found in the HRJ database. The escapment data rows in the HRJ
#'   text files are extracted separately and reshaped into a 'long' format with
#'   structure matching the output from \code{\link{reshapeHRJtolong}}. Thus if
#'   desired, the user could \code{rbind} the escapement data and HRJ (long
#'   format) data frame (produced by \code{\link{reshapeHRJtolong}}) for additional
#'   analyses.  Additionally, there is a check for matching copies of 'b' and
#'   'c' files, by stock. If a file is imported without its matching file, the
#'   user is warned at the console output.
#'
#' @return A list comprising two lists. The first list consists of the HRJ data
#'   in two data frames in 'wide' format, one for 'b' data and one for the 'c'
#'   data. The second list is a data frame, in 'long' format, of the escapement
#'   data with structure matching the output of \code{\link{reshapeHRJtolong}}.
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- list.files(".", pattern = "HRJ")
#' hrj.list <- readHRJtext(filename)
#' }
readHRJtext <- function(filepath){
	sourcefile <- dirname(filepath)[1]
  if(!is.list(filepath)) {
    filename.string <- list("c1.hrj", 'b1.hrj')
    filepath <- lapply(filename.string, function(x){as.list(filepath[grep(x, tolower(filepath))])})
    names(filepath) <- sapply(filename.string, function(x)substr(x,1,1))
    names(filepath)[names(filepath)=="b"] <- "HRJ_BY"
    names(filepath)[names(filepath)=="c"] <- "HRJ_CY"
  }

  .import.fn <- function(file.ind){
    #this .import.fn works on each hrj file
    StockID <- substr(file.ind, nchar(file.ind)-8,nchar(file.ind)-6 )

    hrj.vec <- readLines(file.ind)
    hrj.vec <- trimws(hrj.vec)

    #make data comma delimited:
    hrj.vec <- gsub(pattern = "  *", replacement = ",", x = hrj.vec)

    hrj.list <- strsplit(hrj.vec, ",")

    line.zero <- which(sapply(hrj.list,FUN = function(x){x[2]==0 })==TRUE )
    data.indices <- data.frame(start=line.zero+1, end=c(line.zero[-1]-1,length(hrj.list)))
    data.indices$agecount.broodyear <- sapply(hrj.list[line.zero], function(x){ (length(x)-3)/3  })
    data.indices$age.max <- sapply(hrj.list[line.zero], function(x) as.integer(x[3]))

    agecount.broodyear.max <- max(data.indices$agecount.broodyear)

    # this apply function acts on a brood year group the index row range of the
    # brood year group is taken from the data.frame: data.indices
    hrj.wide.list <- apply(data.indices,1,FUN = function(x,hrj.list){

      #it seems some hrj files have brood years listed but no data.
      #if there is no data then agecount.broodyear will =0 so ignore this BY:
      if(x['agecount.broodyear'] !=0){

        hrj.subset <- unlist(hrj.list[x['start']:x['end']])
        ncol <- 3+x['agecount.broodyear']*5
        hrj.df <- data.frame(matrix(as.numeric(hrj.subset), ncol=ncol , byrow=T),stringsAsFactors=FALSE)
        hrj.df[,1:3] <- as.integer(unlist(hrj.df[,1:3]))

        col.index <- c(sapply(1:5, FUN = function(vec.val, x){
          c(paste0("hrj.df[," ,
                   (((vec.val-1)*x['agecount.broodyear']) + (3+ (1:x['agecount.broodyear']))), "]"),
            rep("NA",max(data.indices$agecount.broodyear) - x['agecount.broodyear']) )}, x))

        col.comm <- paste("data.frame(", paste(col.index, collapse = ", "), ")" )

        hrj.df <- data.frame(hrj.df[,1:3], eval(parse(text = col.comm)))

        colnames.vec <- c("brood", "fishery", "oldestAge",
                          paste0("AEQCat",2:(agecount.broodyear.max+1) ),
                          paste0("AEQTot",2:(agecount.broodyear.max+1) ),
                          paste0("NomCat",2:(agecount.broodyear.max+1) ),
                          paste0("NomTot",2:(agecount.broodyear.max+1) ),
                          paste0("Pop",2:(agecount.broodyear.max+1) ))

        if(length(colnames(hrj.df)) != length(colnames.vec) ) browser()
        colnames(hrj.df) <- colnames.vec

        # next six lines bring in the escapement data and append it:
        hrj.esc <- data.frame(matrix(as.integer( unlist(hrj.list[x['start']-1])[1:(x['agecount.broodyear']+3)]), ncol=3+x['agecount.broodyear'] , byrow=T),stringsAsFactors=FALSE)
        #   hrj.esc<- as.integer(unlist(hrj.esc))
        colnames.vec <- c("brood", "fishery", "oldestAge",
                          paste0("escapement", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))))
        colnames(hrj.esc) <- colnames.vec
        hrj.esc <- reshape(hrj.esc, dir='long', idvar=c('brood', 'fishery', 'oldestAge'),  varying = list(4:ncol(hrj.esc)), v.names= 'value', timevar = 'column.ind')
        hrj.esc$column.ind <- colnames.vec[4:length(colnames.vec)][hrj.esc$column.ind]

        hrj.esc$agecount.broodyear <- x['agecount.broodyear']

        return(list(hrj.wide=hrj.df, hrj.esc=hrj.esc ))
      }#END if(x['agecount.broodyear'] !=0)
    }, hrj.list)

    # this combines the brood year groups into one long df:
    hrj.wide <- do.call('rbind', lapply(hrj.wide.list,"[[",1))
    hrj.wide$StockID <- StockID
    hrj.esc <- do.call('rbind', lapply(hrj.wide.list,"[[",2))
    hrj.esc$StockID <- StockID

    # new grouping fields created in escapement data frame (age and data.type):
    hrj.esc$age <- as.integer(substr(hrj.esc$column.ind, nchar(hrj.esc$column.ind), nchar(hrj.esc$column.ind)))

    return(list(hrj.wide=hrj.wide, hrj.esc=hrj.esc))
  }#END .import.fn


  #this allows multiple files to be concatinated:
  hrj.list <- lapply(filepath, function(x){lapply(x, .import.fn)})

  hrj.cwt.list <- lapply(hrj.list, function(x) lapply(x,"[[",1))
  hrj.cwt.list <- lapply(hrj.cwt.list, function(x) {
  	do.call('.rbind.named.fill', list(x))
  })
  
  #rounding & convert to integer to comply with vb code:
  hrj.cwt.list <- lapply(hrj.cwt.list, function(x) {
    
    cols.numeric <- sapply(x, class)=="numeric"
  	x[cols.numeric] <- round(x[cols.numeric])
  	x[cols.numeric] <- apply(x[cols.numeric], 2, as.integer)
  	return(x)
  })

  hrj.esc.list <- lapply(hrj.list, function(x) lapply(x,"[[",2))
  hrj.esc.list <- lapply(hrj.esc.list, function(x) do.call('.rbind.named.fill', list(x)))

  checkMissingfiles(filepath)

  return(list(sourcefile=sourcefile, hrj.cwt.list=hrj.cwt.list, hrj.esc.list=hrj.esc.list))

}#END readHRJtext




#' @title (HRJ) Reshape wide formatted HRJ file to long format.
#'
#' @param hrj.list A list comprising data frames of HRJ data in wide format with
#'   the same structure as seen in the output from
#'   \code{\link{readHRJAccessDatabase}}.
#' @param data.stock A list. Output from \code{\link{readStockData}}.
#' @param fishery.def.df A data frame. The default is NULL, and if so the
#'   \code{\link{fishery.def}} data are utilized.
#' @param jurisdiction.df A data frame. The default is NULL, and if so the
#'   \code{\link{jurisdiction}} data are utilized.
#'
#' @description In addition to reshaping the wide tables to a long format, age
#'   and return year are calculated and the columns included. The data are
#'   evaluated for completeness of return year data, by stock. A (Boolean value)
#'   column named \code{return.year.complete} is included. A third data frame
#'   named "workingdata" is created. This consists of data from the 'c' table,
#'   which are revised with 'b' data based on the rules outlined in the details
#'   section below. The three data frames are also merged with the fishery
#'   definition \code{\link{fishery.def}} and jurisdiction
#'   \code{\link{jurisdiction}} data frames, which add additional meta data
#'   columns.
#'
#' @details The data frame 'workingdata' represents a combination of data from
#'   the 'C' and 'B' data frames. The 'C' data frame is used to populate the
#'   'workingdata' data frame. The maximum number of age classes allowed to be
#'   missing, by stock and return year, is one. If more than one age class is
#'   absent, the 'C' value is replaced by its equivalent "B" value.
#'
#'   The fishery definition \code{\link{fishery.def}} and jurisdiction
#'   \code{\link{jurisdiction}} data can be accessed by looking to their help
#'   file example documentation. If either data frame needs modification, the
#'   user can create new data frames with structure matching the two data frames
#'   included. The revised data frames can then be used as arguments to
#'   \code{reshapeHRJtolong}.
#'
#' @return A list comprising three elements, all are data frames of the HRJ data
#'   tables 'B', 'C', and 'workingdata', in long format.
#' @export
#'
#' @examples
#' \dontrun{
#' # reading 32 bit mdb files requires using 32bit R
#' hrj.list.wide <- readHRJAccessDatabase("HRJ_database 2016b.mdb")
#' hrj.list.long <- reshapeHRJtolong(hrj.list.wide, data.stock)
#' }
#'
reshapeHRJtolong <- function(hrj.list, data.stock, fishery.def.df=NULL, jurisdiction.df=NULL){
  if(exists('fishery.def.df') & is.null(fishery.def.df)) {
    data("fishery.def")
    fishery.def.df <- fishery.def
  }

  if(exists('jurisdiction.df') & is.null(jurisdiction.df)) {
    data("jurisdiction")
    jurisdiction.df <- jurisdiction
  }

	sourcefile <- hrj.list$sourcefile
	hrj.cwt.list <- hrj.list$hrj.cwt.list

	#first test that all stocks needed in the stock file also exist in the hrj data:
	SN.absent.from.stocfile <- unique(hrj.cwt.list[[1]]$stock[!hrj.cwt.list[[1]]$stock %in% data.stock$stocks.df$Stock.Number])
	SN.flagged <- unique(data.stock$SPFIFlag.long$Stock.Number[data.stock$SPFIFlag.long$value==1])

	SN.absent.from.hrj <- SN.flagged[!SN.flagged %in% unique(hrj.cwt.list[[1]]$stock)]

	if(length(c(SN.absent.from.stocfile, SN.absent.from.hrj))>0){
		cat("\n")
		str1 <- "The stock file and HRJ data do not have perfectly matching stock numbers. If the HRJ data have stock numbers not available in the stock file, the function will still proceed. If there are flagged stocks in the stock file that not found in the HRJ data, this function will indicate the missing stocks below and terminate without completion. The stock file should be updated and this function should be rerun. These checks are repeated when calc_SPFI is run."
		#browser()
		str2 <- ifelse(length(SN.absent.from.stocfile)>=1, c("The following stock numbers are in the HRJ but absent from the stock file:\n", basename(data.stock$filename),"\n", "so they will be exluded from the data for SPFI estimation:\n",paste(SN.absent.from.stocfile, collapse = ", ")), "")

		if(length(SN.absent.from.hrj)>=1){
			str3 <-c("The following stock numbers are in the stock file:\n", basename(data.stock$filename),"\n", "but absent from the HRJ data:\n", paste(SN.absent.from.hrj, collapse = ", "))
		}else{
			str3 <- ""
		}

		cat(stringi::stri_wrap(c(str1,"\n\n",str2, "\n\n",str3,"\n\n", "Either the missing stocks should be added to the HRJ data or in the stock file set those flagged stock-ages to zero.\n")), sep="\n")
	}
	#this terminates the function without results:
	if(length(SN.absent.from.hrj)>=1) {return("Function terminated without results.")}

  hrj.list.long <- lapply(hrj.cwt.list, function(x){

    age.index.count <- (ncol(x)-4)/5
    colnames.vec <- colnames(x)
    dat.tmp <- reshape(x, dir="long", varying=list(5:ncol(x)), timevar = 'column.index', v.names= 'value')

    dat.tmp$column.ind <- colnames.vec[5:length(colnames.vec)][dat.tmp$column.ind]
    dat.tmp$age.index <- as.integer(substr(dat.tmp$column.ind, nchar(dat.tmp$column.ind), nchar(dat.tmp$column.ind)))
    dat.tmp$data.type <- substr(dat.tmp$column.ind, 1, nchar(dat.tmp$column.ind)-1)

    #rename three columns to match syntax use in hrj files:
    colnames(dat.tmp)[colnames(dat.tmp)=='stock'] <- 'Stock.Number'
    colnames(dat.tmp)[colnames(dat.tmp)=='fishery'] <- 'fishery.index'
    colnames(dat.tmp)[colnames(dat.tmp)=='brood'] <- 'brood.year'

    #combine with stock file to get stock details
     dat.tmp <- merge(dat.tmp, data.stock$stocks.df, by.x = "Stock.Number", by.y = "Stock.Number", all.x=TRUE)


    #next two lines create an age column:
    dat.tmp$age <- dat.tmp$age.index- min(dat.tmp$age.index) + dat.tmp$Start.Age
    dat.tmp$value[dat.tmp$age>dat.tmp$oldestAge] <- NA

    #dat.tmp$stock <- dat.tmp$StockID


    dat.tmp$return.year <- dat.tmp$brood.year + dat.tmp$age

    dat.tmp <- dat.tmp[,-which(colnames(dat.tmp) %in% c("id", "column.ind", "Stock.Name", "StockID", "Start.Age"))]

    return(dat.tmp)
  }
  )

  names(hrj.list.long) <- names(hrj.cwt.list)


  #The following defines incomplete return years:
  hrj.list.long2 <- lapply(hrj.list.long, FUN=function(x){

    x$return.year.complete <- TRUE
    by.range <- aggregate(brood.year~Stock.Number, data=x, range, na.rm=TRUE)
    x.bystock <- apply(by.range, 1, FUN = function(by.range.sub, x){

      by.series <- seq(by.range.sub['brood.year.1'], by.range.sub['brood.year.2'])
      by.missing <- by.series[!by.series %in% unique(x$brood.year[x$Stock.Number==by.range.sub['Stock.Number']])]

      if(length(by.missing)>=1){

        return.year.incomplete <- unique(c(outer(by.missing, unique(x$age[x$Stock.Number==by.range.sub['Stock.Number']]), "+")))
        x$return.year.complete[x$Stock.Number==by.range.sub['Stock.Number'] & x$return.year %in% return.year.incomplete] <- FALSE

      }
      return(x[x$Stock.Number==by.range.sub['Stock.Number'],])

    },x)#end apply

    return(do.call(what = 'rbind', args = x.bystock))

  })

  #The data frame 'workingdata' will be c data and c data revised with 'b' based
  #the update rule.
  hrj.list.long2$workingdata <- hrj.list.long2$HRJ_CY

  # this is to estimate the maximum number of age classes by stock:
  agecount.st.by.dt.fi <- aggregate(age~Stock.Number+brood.year+data.type+fishery.index, data=hrj.list.long2$workingdata, length)
  agecount.stock <- aggregate(age~Stock.Number, data=agecount.st.by.dt.fi, max)
  colnames(agecount.stock)[colnames(agecount.stock)=='age'] <- "expected.age.count"

  #this will add column of age counts by return year to help
  #identify what data should be replaced by "B" files.
  agecount.st.ry.dt.fi <- aggregate(age~Stock.Number+return.year+data.type+fishery.index, data=hrj.list.long2$workingdata, length)
  agecount.st.ry <- aggregate(age~Stock.Number+return.year, data=agecount.st.ry.dt.fi, max )
  colnames(agecount.st.ry)[colnames(agecount.st.ry)=='age'] <- 'age.count'
  agecount.st.ry <- merge(agecount.st.ry, agecount.stock, by='Stock.Number')
  agecount.st.ry$age.count.diff <- agecount.st.ry$expected.age.count - agecount.st.ry$age.count

  #this is the maximum count of allowable ages to be absent by return year
  #without being replaced by "B" data:
  max.ages.absent <- 1
  agecount.st.ry$data.from.b <- FALSE
  agecount.st.ry$data.from.b[agecount.st.ry$age.count.diff > max.ages.absent] <- TRUE

  hrj.list.long2$HRJ_BY$value.b <- hrj.list.long2$HRJ_BY$value
  hrj.list.long2$workingdata <- merge(hrj.list.long2$workingdata, hrj.list.long2$HRJ_BY[,c('fishery.index', 'Stock.Number', 'brood.year', 'data.type', 'age', 'value.b')], by=c('fishery.index', 'Stock.Number', 'brood.year', 'data.type', 'age'), all.x = TRUE)

  hrj.list.long2$workingdata$value.c <- hrj.list.long2$workingdata$value

  hrj.list.long2$workingdata <- merge(hrj.list.long2$workingdata, agecount.st.ry[,c('Stock.Number', 'return.year', 'data.from.b')], by=c("Stock.Number", 'return.year'))
  hrj.list.long2$workingdata$value[hrj.list.long2$workingdata$data.from.b==TRUE] <- hrj.list.long2$workingdata$value.b[hrj.list.long2$workingdata$data.from.b==TRUE]

  #bring StockID column back in:
  hrj.list.long2 <- lapply(hrj.list.long2, function(x){
  	if(!is.null(x)) merge(x, data.stock$stocks.df[,c("Stock.Number", "StockID")], by="Stock.Number", all.x = TRUE)} )

  #merging in the fishery.def.df and jurisdiction files:
  hrj.list.long2 <- lapply(hrj.list.long2, function(x,fishery.def.df){
    if(!is.null(x)) merge(x, fishery.def.df, by="fishery.index", all.x = TRUE)
  }, fishery.def )

  hrj.list.long2 <- lapply(hrj.list.long2, function(x,jurisdiction.df){
    if(!is.null(x)) merge(x, jurisdiction.df,  by="StockID", all.x = TRUE)
  }, jurisdiction )

  return(hrj.list.long2)
}#END reshapeHRJtolong



#' @title (HRJ) Reshape HRJ data frame from long to wide format for export to database.
#'
#' @param hrj.list  A list with at least one named data frame, in long format.
#'
#' @description  This function was intended to prepare the 'workingdata' data frame for export, but could also be applied to the 'C' and 'B' data frames (long format).
#'
#' @return A list of with the same number of data frames, in wide format (same structure as HRJ database tables), as was found in the list included in the function argument.
#' @export
#'
#' @examples
#' \dontrun{
#' hrj.list.wide <- reshapeHRJtowide(hrj.list.long)
#  writeHRJaccess(hrj = hrj.list.wide, filename = 'test.accdb')
#' }
reshapeHRJtowide <- function(hrj.list){
	sourcefile <- hrj.list$sourcefile
	hrj.list <- hrj.list$data

  hrj.list.wide <- lapply(hrj.list, FUN = function(data.tmp){
    colnames(data.tmp) <- tolower(colnames(data.tmp))

    data.tmp <- data.tmp[,c("stock.index", "brood.year", "fishery.index", "oldestAge", "data.type", "age", "value")]
    colnames(data.tmp)[1:4] <- c("stock", "brood", "fishery", "oldestAge")
    data.tmp <- data.tmp[data.tmp$age<= data.tmp$oldestAge,]

    highest.ageindex <- aggregate(oldestAge~stock+brood, data=data.tmp, FUN = function(x){ as.integer(min(x, 5)) })

    colnames(highest.ageindex)[colnames(highest.ageindex)=="oldestAge"] <- "highest.ageindex"

    data.tmp <- merge(data.tmp, highest.ageindex, by=c('stock', 'brood'))

    data.tmp$age.index <- apply(data.tmp[,c('highest.ageindex','oldestAge', 'age')],1,FUN = function(x) {
      (x['highest.ageindex']:2)[x['oldestAge'] - x['age'] +1]
    })

    data.tmp$timevar <- paste0(data.tmp$data.type, data.tmp$age.index)
    data.wide <- reshape(data = data.tmp, dir='wide', timevar = 'timevar', drop=c("data.type", 'age'), idvar= c("stock", "brood", "fishery", "oldestAge"))
    cols.forupdate <- grep(pattern = "value", colnames(data.wide))
    colnames(data.wide)[cols.forupdate] <- substr(colnames(data.wide)[cols.forupdate], 7, nchar(colnames(data.wide)[cols.forupdate]))

    data.wide <- data.wide[,c(c("stock", "brood", "fishery", "oldestAge"), sort(colnames(data.wide)[cols.forupdate]) )]

    return(data.wide)
  })

  names(hrj.list.wide) <- names(hrj.list)

  return(hrj.list.wide)

}#END reshapeHRJtowide



#' @title (HRJ) Add stock number to HRJ data frame based on its three letter stock name.
#'
#' @param df A data frame with column "stock.name" containing the three letter
#'   stock name.
#' @param stockdat A data frame with columns "Stock.Number" and "StockID".
#'   "StockID" contains the three letter stock name. This data frame is obtained
#'   using \code{\link{readStockData}}.
#'
#' @return The function returns a data frame, same as used for the first
#'   argument, but with a "stock.number" column.
#' @export
#'
#' @examples
#' \dontrun{
#' data.stock <- readStockData( 'STOCFILE.STF')
#' hrj.list <- readHRJtext(filepath)
#' hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list, updateStockByName, data.stock$stocks.df)
#' }
updateStockByName <- function(df, stockdat, by.x = 'StockID', by.y = "StockID"){

  df.tmp <- merge(df, stockdat[,c("Stock.Number", "StockID")], by.x = by.x, by.y = by.y)
  df.tmp$stock <- df.tmp$Stock.Number
  col.prefixes <- c("AEQCat", "AEQTot", "NomCat", "NomTot", "Pop")
  df.tmp.colnames <- colnames(df.tmp)

  res <- sapply(col.prefixes, FUN = function(x, df.tmp.colnames){
    gregexpr(pattern = x, text = df.tmp.colnames)
  }, df.tmp.colnames )

  datacol.indecies <- c(apply(res, 2, FUN=function(x){which(x==1)}))
  metacol.indecies <- match(c("stock", "brood", "fishery", "oldestAge"), df.tmp.colnames)

  return(df.tmp[,c(metacol.indecies, datacol.indecies)])
}#END updateStockByName



.update_datatype <- function(hrj.list.long){
 #revise data.type column so that values equate to those in the HRJ data base.
 #hrj.list.long is a list comprising data frames in long format of the hrj data.

  df.temp <- as.data.frame(matrix(ncol=2, byrow = TRUE, data = c(
    "landed.AEQ.catch",      "AEQCat",
    "total.AEQ.mortalities", "AEQTot",
    "nominal.landed.catch",  "NomCat",
    "nominal.total.catch",   "NomTot",
    "cohort.size",           "Pop",
    "escapement",            "escapement",
    "terminal.run",          "terminal.run")
    ), stringsAsFactors = FALSE)

  colnames(df.temp) <- c("data.type", "data.type.new")

  hrj.list.long <- lapply(hrj.list.long, FUN=function(x, df.temp) {
    x <- merge(x, df.temp, by='data.type', all.x=TRUE)
    colnames(x)[colnames(x)=="data.type"] <- "data.type.old"
    colnames(x)[colnames(x)=="data.type.new"] <- "data.type"
    return(x)
  }, df.temp)

  return(hrj.list.long)


}#EMD update_datatype



#' @title (HRJ) Write HRJ "B" & "C" tables to MS Access database.
#'
#' @param hrj A list usually comprising of two data frames, which are the 'b'
#'   and 'c' HRJ tables in wide format with fields exactly matching those
#'   defined in the MS Access data base.
#' @param extraTables A list comprising of one or more named elements that must
#'   all be data frames. Each data frame will be written to a table that has the
#'   same name as the data frame (MS Access can't have periods in table names).
#'   If the list argument is not supplied, only a readme table is created with
#'   the creation date in its contents.
#' @param filename A character string of length one. The MS Access filename.
#' @description The Access data base must already be created, but can be empty.
#'   If there are tables with the same names as the data frames, then they will
#'   be over-written.
#'
#' @return A MS database is written. Nothing is returned to R.
#' @export
#'
#' @examples
#' \dontrun{
#' hrj.list <- readHRJtext(filepath)
#' hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list,updateStockByName, data.stock$stocks.df)
#' writeHRJAccessDatabase(hrj = hrj.list$hrj.cwt.list, filename = 'test.accdb')
#'
#' #to add the "workingdata" table (which has C data, updated with B data):
#' hrj.list.long <- reshapeHRJtolong(hrj.list$hrj.cwt.list, data.stock)
#' workdingdata.wide <- reshapeHRJtowide(hrj.list.long$workingdata)
#' extraTables = list(stocks=data.stock$stocks.df, readme=data.frame(comment="this is a comment"))
#' #be sure to create the empty data base before next line:
#' writeHRJAccessDatabase(hrj = list(workingdata= workdingdata.wide), filename = 'test.accdb', extraTables=extraTables)
#' }
writeHRJAccessDatabase <- function(hrj, extraTables=list(readme=data.frame(creationDate= format(Sys.Date(), "%d-%b-%Y"))), filename){

  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop("The package 'RODBC' is needed for this function to work -
         and may only work in MS Windows. Please install it.",
         call. = FALSE)
  }

  filename <- paste0("./", filename)
  driver.name <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  driver.name <- paste0(driver.name, "DBQ=", filename)
  con <- RODBC::odbcDriverConnect(driver.name)
  invisible(
  lapply(names(hrj), FUN=function(x){

    table.name <- x
    table.name <- gsub(pattern = "\\.", replacement = "_", x = table.name)
    RODBC::sqlDrop(con, table.name, errors = FALSE)
    hrj.tmp <-  hrj[[x]]
    #rename according to original mdb nameing:
    colnames(hrj.tmp)[colnames(hrj.tmp)=="Stock.Number"] <- "stock"
    hrj.tmp <- hrj.tmp[order(hrj.tmp$stock, hrj.tmp$brood, hrj.tmp$fishery),]
    RODBC::sqlSave(con, hrj.tmp, table.name ,rownames=FALSE)
  })
  )

  #Write additional tables supplied by user
  #a list of extra tables is supplied
  	for(tbl.ind in 1:length(extraTables)){
  		table.name <- names(extraTables)[tbl.ind]
  		#access can't have periods in table names:
  		table.name <- gsub(pattern = "\\.", replacement = "_", x = table.name)
  		RODBC::sqlDrop(con, table.name, errors = FALSE)
  		RODBC::sqlSave(con, extraTables[[tbl.ind]], table.name ,rownames=FALSE)
  	}



  RODBC::odbcCloseAll()
}#END writeHRJAccessDatabase



#' @title (HRJ) Write HRJ "B" & "C" tables to csv (text) files.
#'
#' @param hrj A list usually comprising of two data frames, which are the 'b'
#'   and 'c' HRJ tables in wide format with fields exactly matching those
#'   defined in the MS Access data base.
#' @description This function has been created in case there are future problems
#'   writing direct to the MS Access database. The user can create the csv files
#'   then import them from within Access.
#'
#' @return Nothing is returned. But one csv file is written for each data frame
#'   in the HRJ list. The csv filename is the same as the data frame name.
#' @export
#'
#' @examples
#' \dontrun{
#' hrj.list <- readHRJtext(filepath)
#' hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list,updateStockByName, data.stock$stocks.df)
#' writeHRJcsv(hrj = hrj.list$hrj.cwt.list)
#' }
writeHRJcsv <- function(hrj){
  invisible(
  lapply(names(hrj), FUN=function(x){
    table.name <- x
    hrj.tmp <-  hrj[[x]]
    hrj.tmp[is.na(hrj.tmp)] <- ""
    hrj.tmp <- hrj.tmp[order(hrj.tmp$stock, hrj.tmp$brood, hrj.tmp$fishery),]
    write.csv(x = hrj.tmp, file = paste0(table.name, ".csv"), row.names = FALSE)
    })
  )
}#END writeHRJcsv



#' @title (SPFI) Build, save, and open an R script to help manage HRJ data.
#'
#' @description This creates and opens a script named "hrj_script.R". This is a
#'   template for the user to work with when managing HRJ data (import,
#'   manipulate, export). This is intended to help the user understand the work
#'   flow and due to file path differences, is unlikely to work as is. Some
#'   object values will need updating (for example the datapath).
#'
#' @return Opens an R script that includes the template of functions.
#' @export
#'
#' @examples
#' writeScriptHRJ()
writeScriptHRJ <- function(){

  script.str <- c("
####### SETUP #######
rm(list=ls())
###### COMMENTS ########

# this script allows for import (text or MS Access), manipulation, and export (text or MS Access) of HRJ data.

####### DATA #######
region <- 'nbc'
data.catch <- readCatchData('nbc7914.cat', strLocation = region)
data.stock <- readStockData('STOCFILE.STF')

### reading 32 bit mdb files requires using 32bit R

hrj.list.wide <- readHRJAccessDatabase('HRJ_database 2016b.mdb')
hrj.list.long <- reshapeHRJtolong(hrj.list.wide, data.stock)
hrj.list <- list(sourcefile=hrj.list.wide$sourcefile, hrj.list.wide=hrj.list.wide$data, hrj.list.long=hrj.list.long)
#filename <- 'hrj_from_mdb.RData'
#save(hrj.list, file = filename)
#load(filename)

###this is the method to use with hrj text files:

filename <- list.files(data.path, pattern = 'HRJ')
filepath <- paste(data.path, filename, sep='/')
hrj.list <- readHRJtext(filepath)

#add stock number column to the data frames:

hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list, updateStockByName, data.stock$stocks.df)
#write to a prebuilt access data base (R cannot create data base files):
writeHRJAccessDatabase(hrj = hrj.list$hrj.cwt.list, filename = 'test.accdb')
#write csv files in same format as found in data base:
writeHRJcsv(hrj = hrj.list$hrj.cwt.list)

#long format is what the spfi code uses:
hrj.list.long <- reshapeHRJtolong(hrj.list$hrj.cwt.list, data.stock)

#reshape to wide format prior to writing to access:
workdingdata.wide <- reshapeHRJtowide(hrj.list.long)
writeHRJAccessDatabase(hrj = workdingdata.wide, filename = 'test.accdb')

")
  write(script.str, file="hrj_script.R")
  file.edit("hrj_script.R" )


}#writeScriptHRJ
