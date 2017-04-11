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
#'
#' @examples
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

  driver.name <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  driver.name <- paste0(driver.name, "DBQ=", filename)
  con <- RODBC::odbcDriverConnect(driver.name)
  tables.df <- RODBC::sqlTables(con, tableType = 'TABLE')
  hrj.list <- list()
  hrj.list <- apply(tables.df, 1, function(x,con2){

    df.tmp <- RODBC::sqlFetch(con, x["TABLE_NAME"])
    return(df.tmp)
  }, con)
  names(hrj.list)  <- tables.df$TABLE_NAME
  RODBC::odbcCloseAll()

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
  if(!is.list(filepath)) {
    filename.string <- list("c1.hrj", 'b1.hrj')
    filepath <- lapply(filename.string, function(x){as.list(filepath[grep(x, tolower(filepath))])})
    names(filepath) <- sapply(filename.string, function(x)substr(x,1,1))
    names(filepath)[names(filepath)=="b"] <- "HRJ_BY"
    names(filepath)[names(filepath)=="c"] <- "HRJ_CY"
  }

  .import.fn <- function(file.ind){
    #this .import.fn works on each hrj file
    stock.val <- substr(file.ind, nchar(file.ind)-8,nchar(file.ind)-6 )

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

        colnames.vec <- c("brood", "fishery", "oldestage",
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
        colnames.vec <- c("brood", "fishery", "oldestage",
                          paste0("escapement", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))))
        colnames(hrj.esc) <- colnames.vec
        hrj.esc <- reshape(hrj.esc, dir='long', idvar=c('brood', 'fishery', 'oldestage'),  varying = list(4:ncol(hrj.esc)), v.names= 'value', timevar = 'column.ind')
        hrj.esc$column.ind <- colnames.vec[4:length(colnames.vec)][hrj.esc$column.ind]

        hrj.esc$agecount.broodyear <- x['agecount.broodyear']

        return(list(hrj.wide=hrj.df, hrj.esc=hrj.esc ))
      }#END if(x['agecount.broodyear'] !=0)
    }, hrj.list)

    # this combines the brood year groups into one long df:
    hrj.wide <- do.call('rbind', lapply(hrj.wide.list,"[[",1))
    hrj.wide$stock.name <- stock.val
    hrj.esc <- do.call('rbind', lapply(hrj.wide.list,"[[",2))
    hrj.esc$stock.name <- stock.val

    # new grouping fields created in escapement data frame (age and data.type):
    hrj.esc$age <- as.integer(substr(hrj.esc$column.ind, nchar(hrj.esc$column.ind), nchar(hrj.esc$column.ind)))

    return(list(hrj.wide=hrj.wide, hrj.esc=hrj.esc))
  }#END .import.fn


  #this allows multiple files to be concatinated:
  hrj.list <- lapply(filepath, function(x){lapply(x, .import.fn)})

  hrj.cwt.list <- lapply(hrj.list, function(x) lapply(x,"[[",1))
  hrj.cwt.list <- lapply(hrj.cwt.list, function(x) do.call('.rbind.named.fill', list(x)))

  hrj.esc.list <- lapply(hrj.list, function(x) lapply(x,"[[",2))
  hrj.esc.list <- lapply(hrj.esc.list, function(x) do.call('.rbind.named.fill', list(x)))

  checkMissingfiles(filepath)

  return(list(hrj.cwt.list=hrj.cwt.list, hrj.esc.list=hrj.esc.list))

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

  hrj.list.long <- lapply(hrj.list, function(x){
    age.index.count <- (ncol(x)-4)/5
    colnames.vec <- colnames(x)
    dat.tmp <- reshape(x, dir="long", varying=list(5:ncol(x)), timevar = 'column.index', v.names= 'value')

    dat.tmp$column.ind <- colnames.vec[5:length(colnames.vec)][dat.tmp$column.ind]
    dat.tmp$age.index <- as.integer(substr(dat.tmp$column.ind, nchar(dat.tmp$column.ind), nchar(dat.tmp$column.ind)))
    dat.tmp$data.type <- substr(dat.tmp$column.ind, 1, nchar(dat.tmp$column.ind)-1)

    #rename three columns to match syntax use in hrj files:
    colnames(dat.tmp)[colnames(dat.tmp)=='stock'] <- 'stock.index'
    colnames(dat.tmp)[colnames(dat.tmp)=='fishery'] <- 'fishery.index'
    colnames(dat.tmp)[colnames(dat.tmp)=='brood'] <- 'brood.year'


    #next two lines create an age column:
    dat.tmp <- merge(dat.tmp, data.stock$stocks.df, by.x = "stock.index", by.y = "Stock.Number", all.x=TRUE)
    dat.tmp$age <- dat.tmp$age.index- min(dat.tmp$age.index) + dat.tmp$Start.Age
    dat.tmp$value[dat.tmp$age>dat.tmp$oldestage] <- NA

    dat.tmp$stock <- dat.tmp$StockID


    dat.tmp$return.year <- dat.tmp$brood.year + dat.tmp$age

    dat.tmp <- dat.tmp[,-which(colnames(dat.tmp) %in% c("id", "column.ind", "Stock.Name", "StockID", "Start.Age"))]

    return(dat.tmp)
  }
  )
  names(hrj.list.long) <- names(hrj.list)


  #The following defines incomplete return years:
  hrj.list.long2 <- lapply(hrj.list.long, FUN=function(x){

    x$return.year.complete <- TRUE
    by.range <- aggregate(brood.year~stock.index, data=x, range, na.rm=TRUE)
    x.bystock <- apply(by.range, 1, FUN = function(by.range.sub, x){

      by.series <- seq(by.range.sub['brood.year.1'], by.range.sub['brood.year.2'])
      by.missing <- by.series[!by.series %in% unique(x$brood.year[x$stock.index==by.range.sub['stock.index']])]

      if(length(by.missing)>=1){

        return.year.incomplete <- unique(c(outer(by.missing, unique(x$age[x$stock.index==by.range.sub['stock.index']]), "+")))
        x$return.year.complete[x$stock.index==by.range.sub['stock.index'] & x$return.year %in% return.year.incomplete] <- FALSE

      }
      return(x[x$stock.index==by.range.sub['stock.index'],])

    },x)#end apply

    return(do.call(what = 'rbind', args = x.bystock))

  })

  #The data frame 'workingdata' will be c data and c data revised with 'b' based
  #the update rule.
  hrj.list.long2$workingdata <- hrj.list.long2$HRJ_CY

  # this is to estimate the maximum number of age classes by stock:
  agecount.st.by.dt.fi <- aggregate(age~stock.index+brood.year+data.type+fishery.index, data=hrj.list.long2$workingdata, length)
  agecount.stock <- aggregate(age~stock.index, data=agecount.st.by.dt.fi, max)
  colnames(agecount.stock)[colnames(agecount.stock)=='age'] <- "expected.age.count"

  #this will add column of age counts by return year to help
  #identify what data should be replaced by "B" files.
  agecount.st.ry.dt.fi <- aggregate(age~stock.index+return.year+data.type+fishery.index, data=hrj.list.long2$workingdata, length)
  agecount.st.ry <- aggregate(age~stock.index+return.year, data=agecount.st.ry.dt.fi, max )
  colnames(agecount.st.ry)[colnames(agecount.st.ry)=='age'] <- 'age.count'
  agecount.st.ry <- merge(agecount.st.ry, agecount.stock, by='stock.index')
  agecount.st.ry$age.count.diff <- agecount.st.ry$expected.age.count - agecount.st.ry$age.count

  #this is the maximum count of allowable ages to be absent by return year
  #without being replaced by "B" data:
  max.ages.absent <- 1
  agecount.st.ry$data.from.b <- FALSE
  agecount.st.ry$data.from.b[agecount.st.ry$age.count.diff > max.ages.absent] <- TRUE

  hrj.list.long2$HRJ_BY$value.b <- hrj.list.long2$HRJ_BY$value
  hrj.list.long2$workingdata <- merge(hrj.list.long2$workingdata, hrj.list.long2$HRJ_BY[,c('fishery.index', 'stock.index', 'brood.year', 'data.type', 'age', 'value.b')], by=c('fishery.index', 'stock.index', 'brood.year', 'data.type', 'age'), all.x = TRUE)

  hrj.list.long2$workingdata$value.c <- hrj.list.long2$workingdata$value

  hrj.list.long2$workingdata <- merge(hrj.list.long2$workingdata, agecount.st.ry[,c('stock.index', 'return.year', 'data.from.b')], by=c("stock.index", 'return.year'))
  hrj.list.long2$workingdata$value[hrj.list.long2$workingdata$data.from.b==TRUE] <- hrj.list.long2$workingdata$value.b[hrj.list.long2$workingdata$data.from.b==TRUE]


  #merging in the fishery.def.df and jurisdiction files:

  hrj.list.long2 <- lapply(hrj.list.long2, function(x,fishery.def.df){
    if(!is.null(x)) merge(x, fishery.def.df, by="fishery.index", all.x = TRUE)
  }, fishery.def )

  hrj.list.long2 <- lapply(hrj.list.long2, function(x,jurisdiction.df){
    if(!is.null(x)) merge(x, jurisdiction.df,  by="stock", all.x = TRUE)
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

  hrj.list.wide <- lapply(hrj.list, FUN = function(data.tmp){
    colnames(data.tmp) <- tolower(colnames(data.tmp))

    data.tmp <- data.tmp[,c("stock.index", "brood.year", "fishery.index", "oldestage", "data.type", "age", "value")]
    colnames(data.tmp)[1:4] <- c("stock", "brood", "fishery", "oldestage")
    data.tmp <- data.tmp[data.tmp$age<= data.tmp$oldestage,]

    highest.ageindex <- aggregate(oldestage~stock+brood, data=data.tmp, FUN = function(x){ as.integer(min(x, 5)) })

    colnames(highest.ageindex)[colnames(highest.ageindex)=="oldestage"] <- "highest.ageindex"

    data.tmp <- merge(data.tmp, highest.ageindex, by=c('stock', 'brood'))

    data.tmp$age.index <- apply(data.tmp[,c('highest.ageindex','oldestage', 'age')],1,FUN = function(x) {
      (x['highest.ageindex']:2)[x['oldestage'] - x['age'] +1]
    })

    data.tmp$timevar <- paste0(data.tmp$data.type, data.tmp$age.index)
    data.wide <- reshape(data = data.tmp, dir='wide', timevar = 'timevar', drop=c("data.type", 'age'), idvar= c("stock", "brood", "fishery", "oldestage"))
    cols.forupdate <- grep(pattern = "value", colnames(data.wide))
    colnames(data.wide)[cols.forupdate] <- substr(colnames(data.wide)[cols.forupdate], 7, nchar(colnames(data.wide)[cols.forupdate]))

    data.wide <- data.wide[,c(c("stock", "brood", "fishery", "oldestage"), sort(colnames(data.wide)[cols.forupdate]) )]

    return(data.wide)
  })

  names(hrj.list.wide) <- names(hrj.list)

  return(hrj.list.wide)

}#END reshapeHRJtowide




#' Retired function. Read in and combine multiple HRJ text files.
#'
#' @param filepath A character vector, which can have length greater than one.
#'   Each element of the vector is the path and filename for each HRJ file to be
#'   read in. As each file has its own path informartion, files can be read in
#'   from multiple folders.
#' @param fishery.def.df A data frame. The fishery definition file named
#'   "fishery_def.csv".
#' @param jurisdiction.df A data frame. The jurisdiction file named
#'   "jurisdiction.csv".
#'
#' @description The function reads in multiple HRJ files, combines them into two
#'   'long' structure data frame, one for the 'b' data and one for the 'c' data.
#'
#' @return A list comprising two data frames. One data frame is the "B" data and
#'   the other the "C" data.
#'
#' @examples
#' \dontrun{
#' filename <- list.files(".", pattern = "HRJ")
#' hrj.list.long <- .readHRJtext.full(filename)
#' }
.readHRJtext.full <- function(filepath, fishery.def.df=NA, jurisdiction.df=NA){
 if(!is.list(filepath)) {
    filename.string <- list("c1.hrj", 'b1.hrj')
    filepath <- lapply(filename.string, function(x){as.list(filepath[grep(x, tolower(filepath))])})
    names(filepath) <- sapply(filename.string, function(x)substr(x,1,1))
    }


  jurisdiction$stock <- toupper(jurisdiction$stock)

  .import.fn <- function(file.ind){
    #this .import.fn works on each hrj file
    stock.val <- substr(file.ind, nchar(file.ind)-8,nchar(file.ind)-6 )

    hrj.vec <- readLines(file.ind)
    hrj.vec <- trimws(hrj.vec)

    #make data comma delimited:
    hrj.vec <- gsub(pattern = "  *", replacement = ",", x = hrj.vec)

    hrj.list <- strsplit(hrj.vec, ",")

    line.zero <- which(sapply(hrj.list,FUN = function(x){x[2]==0 })==TRUE )
    data.indices <- data.frame(start=line.zero+1, end=c(line.zero[-1]-1,length(hrj.list)))
    data.indices$agecount.broodyear <- sapply(hrj.list[line.zero], function(x){ (length(x)-3)/3  })
    data.indices$age.max <- sapply(hrj.list[line.zero], function(x) as.integer(x[3]))

    # this apply function acts on a brood year group the index row range of the
    # brood year group is taken from the data.frame: data.indices
    hrj.long <- apply(data.indices,1,FUN = function(x,hrj.list){

      #it seems some hrj files have brood years listed but no data.
      #if there is no data then agecount.broodyear will =0 so ignore this BY:
     if(x['agecount.broodyear'] !=0){

        hrj.subset <- unlist(hrj.list[x['start']:x['end']])
        ncol <- 3+x['agecount.broodyear']*5
        hrj.df <- data.frame(matrix(as.numeric(hrj.subset), ncol=ncol , byrow=T),stringsAsFactors=FALSE)
        hrj.df[,1:3] <- as.integer(unlist(hrj.df[,1:3]))

        colnames.vec <- c("brood.year", "fishery.index", "age.max",
                          paste0("landed.AEQ.catch", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))),
                          paste0("total.AEQ.mortalities", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))),
                          paste0("nominal.landed.catch", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))),
                          paste0("nominal.total.catch", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))),
                          paste0("cohort.size", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1)))
                         )

  if(length(colnames(hrj.df)) != length(colnames.vec) ) browser()
        colnames(hrj.df) <- colnames.vec
        hrj.long <- reshape(hrj.df, dir='long', idvar=c('brood.year', 'fishery.index', 'age.max'),  varying = list(4:ncol(hrj.df)), v.names= 'value', timevar = 'column.ind')
        hrj.long$column.ind <- colnames.vec[4:length(colnames.vec)][hrj.long$column.ind]

  # next six lines bring in the escapement data and append it:
        hrj.esc <- data.frame(matrix(as.integer( unlist(hrj.list[x['start']-1])[1:(x['agecount.broodyear']+3)]), ncol=3+x['agecount.broodyear'] , byrow=T),stringsAsFactors=FALSE)
     #   hrj.esc<- as.integer(unlist(hrj.esc))
        colnames.vec <- c("brood.year", "fishery.index", "age.max",
                          paste0("escapement", ".age", rev(seq(x['age.max'], length.out = x['agecount.broodyear'], by=-1))))
        colnames(hrj.esc) <- colnames.vec
        hrj.esc <- reshape(hrj.esc, dir='long', idvar=c('brood.year', 'fishery.index', 'age.max'),  varying = list(4:ncol(hrj.esc)), v.names= 'value', timevar = 'column.ind')
        hrj.esc$column.ind <- colnames.vec[4:length(colnames.vec)][hrj.esc$column.ind]

        hrj.long <- rbind(hrj.long, hrj.esc)
        hrj.long$agecount.broodyear <- x['agecount.broodyear']

        #this evaluates the "cohort.size" data to remain same or change to terminal
        #fishery.index 1 is known to be cohort size for all ages:
        preterminal.cohortsize <- hrj.long[hrj.long$fishery.index==1 & substr(hrj.long$column.ind,1,11)=="cohort.size", c('column.ind', 'value')]
        # now check for values that don't match and updates data.type to "terminal.run"
        for(row.val in 1:nrow(preterminal.cohortsize)){
          search.column.ind <- preterminal.cohortsize$column.ind[row.val]
          search.cohortsize <- as.character(preterminal.cohortsize$value[row.val])
          age.val <- substr(search.column.ind, nchar(search.column.ind)-4,nchar(search.column.ind))

          hrj.long$column.ind[hrj.long$column.ind==search.column.ind & as.character(hrj.long$value)!= search.cohortsize] <- paste0("terminal.run", age.val )
        }#END for

        return(hrj.long)
      }#END if(x['agecount.broodyear'] !=0)
    }, hrj.list)

    # this combines the brood year groups into one long df:
    hrj.long <- do.call('rbind', hrj.long)

    # new grouping fields created (age and data.type):

    hrj.long$age <- as.integer(substr(hrj.long$column.ind, nchar(hrj.long$column.ind), nchar(hrj.long$column.ind)))

    starting.age <- aggregate(age~brood.year, data=hrj.long, min, na.rm=TRUE)
    colnames(starting.age)[2] <- "age.min"
    hrj.long <- merge(hrj.long, starting.age, by="brood.year", all = TRUE)
    hrj.long$age.index <- hrj.long$age - hrj.long$age.min +1

    hrj.long$data.type <- substr(hrj.long$column.ind, 1, nchar(hrj.long$column.ind)-5)

    hrj.long$stock <- stock.val
    hrj.long$return.year <- hrj.long$brood.year+hrj.long$age

    # this is the maximum number of age classes by stock:
    hrj.long$agecount.broodyear.max <- max(hrj.long$agecount.broodyear)


    #The following defines incomplete return years:
    by.range <- range(hrj.long$brood.year)
    by.series <- seq(min(by.range), max(by.range))
    by.missing <- by.series[!by.series %in% unique(hrj.long$brood.year)]

    hrj.long$return.year.complete <- TRUE
    if(length(by.missing)>=1){
      return.year.incomplete <- unique(c(outer(by.missing, unique(hrj.long$age), "+")))
      hrj.long$return.year.complete[hrj.long$return.year %in% return.year.incomplete] <- FALSE
    }

    #this will add column of age counts by return year to help
    #identify what data should be replaced by "B" files.
      age.count.byreturn <- aggregate(age~return.year, data=hrj.long[hrj.long$data.type=="escapement" & hrj.long$fishery.index==0,], length )
    colnames(age.count.byreturn)[colnames(age.count.byreturn)=="age"] <- "agecount.returnyear"
    hrj.long <- merge(hrj.long, age.count.byreturn, by='return.year', all.x=TRUE)

    #keeping only required fields:
    hrj.long <- hrj.long[,c('stock', 'brood.year',"return.year", "return.year.complete", "fishery.index", "data.type", "age", "age.min", "age.index", "agecount.broodyear", "agecount.broodyear.max", "agecount.returnyear", 'value')]

    return(hrj.long)
  }#END .import.fn

  #this allows multiple files to be concatinated:
  hrj.list <- lapply(filepath, function(x){lapply(x, .import.fn)})
  hrj.list.long <- lapply(hrj.list, function(x) do.call('rbind', x))

  #merging in the fishery.def and jurisdiction files:
  hrj.list.long <- lapply(hrj.list.long, function(x,fishery.def){
    if(!is.null(x)) merge(x, fishery.def, by="fishery.index", all.x = TRUE)
  }, fishery.def )

  hrj.list.long <- lapply(hrj.list.long, function(x,jurisdiction){
    if(!is.null(x)) merge(x, jurisdiction,  by="stock", all.x = TRUE)
  }, jurisdiction )

  #the df called 'workingdata' will become the combination of b and c files
  hrj.list.long$workingdata <- hrj.list.long$c
  hrj.list.long$workingdata$data.from.b <- FALSE

  #this is the maximum count of allowable ages to be absent by return year
  #without being replaced by "B" data:
  max.ages.absent <- 1

  if(!is.null(hrj.list.long$b)){
    hrj.list.long$b$value.b <- hrj.list.long$b$value
    hrj.list.long$workingdata <- merge(hrj.list.long$workingdata, hrj.list.long$b[,c('fishery.index', 'stock', 'brood.year', 'data.type', 'age', 'value.b')], by=c('fishery.index', 'stock', 'brood.year', 'data.type', 'age'), all.x = TRUE)

    #the rows to update C data with B data:
    # agecount.broodyear.max is the maximum number of age classes by stock:
    update.index <- which( (hrj.list.long$workingdata$agecount.broodyear.max - hrj.list.long$workingdata$agecount.returnyear) > max.ages.absent & !is.na(hrj.list.long$workingdata$value.b))

    hrj.list.long$workingdata$data.from.b[update.index] <- TRUE
    hrj.list.long$workingdata$value.c <- hrj.list.long$workingdata$value
    hrj.list.long$workingdata$value[update.index] <- hrj.list.long$workingdata$value.b[update.index]
  }#if(!is.null(hrj.list.long$b)){


  checkMissingfiles(filepath)

  return(hrj.list.long)

}#END .readHRJtext.full


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
updateStockByName <- function(df, stockdat){

  df.tmp <- merge(df, stockdat[,c("Stock.Number", "StockID")], by.x = 'stock.name', by.y = "StockID")
  df.tmp$stock <- df.tmp$Stock.Number
  col.prefixes <- c("AEQCat", "AEQTot", "NomCat", "NomTot", "Pop")
  df.tmp.colnames <- colnames(df.tmp)

  res <- sapply(col.prefixes, FUN = function(x, df.tmp.colnames){
    gregexpr(pattern = x, text = df.tmp.colnames)
  }, df.tmp.colnames )

  datacol.indecies <- c(apply(res, 2, FUN=function(x){which(x==1)}))
  metacol.indecies <- match(c("stock", "brood", "fishery", "oldestage"), df.tmp.colnames)

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
#' @param filename A character string of length one. The MS Access filename.
#' @description The Access data base must already be created, but can be empty.
#'   If there are tables with the same names as the data frames, then they will
#'   be over-written.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' hrj.list <- readHRJtext(filepath)
#' hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list,updateStockByName, data.stock$stocks.df)
#' writeHRJaccess(hrj = hrj.list$hrj.cwt.list, filename = 'test.accdb')
#'
#' #to add the "workingdata" table (which has C data, updated with B data):
#' hrj.list.long <- reshapeHRJtolong(hrj.list$hrj.cwt.list, data.stock)
#' workdingdata.wide <- reshapeHRJtowide(hrj.list.long$workingdata)
#' writeHRJaccess(hrj = list(workingdata= workdingdata.wide), filename = 'test.accdb')
#' }
writeHRJaccess <- function(hrj, filename){

  if (!requireNamespace("RODBC", quietly = TRUE)) {
    stop("The package 'RODBC' is needed for this function to work -
         and may only work in MS Windows. Please install it.",
         call. = FALSE)
  }

  driver.name <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  driver.name <- paste0(driver.name, "DBQ=", paste0("./", filename))
  con <- RODBC::odbcDriverConnect(driver.name)
  invisible(
  lapply(names(hrj), FUN=function(x){
    table.name <- x
    RODBC::sqlDrop(con, table.name, errors = FALSE)
    hrj.tmp <-  hrj[[x]]
    hrj.tmp <- hrj.tmp[order(hrj.tmp$stock, hrj.tmp$brood, hrj.tmp$fishery),]
    RODBC::sqlSave(con, hrj.tmp, table.name ,rownames=FALSE)
  })
  )
  RODBC::odbcCloseAll()
}#END writeHRJaccess



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


####### END #######
