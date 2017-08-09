#' @title (FP) Calculate fisheries policy (FP) scaler.
#'
#' @param dat.er.long Data frame returned by \code{\link{read_stkfile}}.
#' @param dat.mdl.long Data frame returned by \code{\link{read_mdl}}.
#' @param dat.spfi Data frame in long format with same structure as the element
#' \code{S.ty}, which is output of \code{\link{calc_SPFI}}.
#'
#' @return A data frame comprising the FP values.
#' @export
#'
#' @examples
#' \dontrun{
#' dat.fp <- calc_fp(dat.er.long = dat.er.long.sub, dat.mdl.long = dat.mdl.long.sub, dat.spfi = dat.spfi.long)
#'
#' }
calc_fp <- function(dat.er.long, dat.mdl.long, dat.spfi){

  dat.for.calc <- merge(dat.mdl.long, dat.er.long, by=c("stock.short", "baseperiodfishery.name", "age"))

  dat.mdl.long.sum <- aggregate(r~age+stock.short, FUN = sum, data=dat.mdl.long)
  colnames(dat.mdl.long.sum)[colnames(dat.mdl.long.sum)=="r"] <- "r.sum"

  dat.for.calc <- merge(dat.for.calc, dat.mdl.long.sum, by=c("stock.short", "age"))
  dat.for.calc$sbper <- dat.for.calc$r * dat.for.calc$bper/dat.for.calc$r.sum

  dat.spfi <- dat.spfi[dat.spfi$fishery.index %in% unique(dat.for.calc$fishery.index), c("fishery.index", "return.year", "S.ty")]

  dat.for.calc <- merge(dat.spfi, dat.for.calc, by="fishery.index", all.x = TRUE)
  dat.for.calc$ser <- dat.for.calc$sbper * dat.for.calc$S.ty

  ser.sum <- aggregate(ser~stock.short+age+return.year, data = dat.for.calc, sum)
  colnames(ser.sum)[colnames(ser.sum)=="ser"] <- "ser.sum"

  dat.fp <- merge(ser.sum, dat.er.long, by=c("stock.short", "age"))
  dat.fp$fp <- dat.fp$ser.sum/dat.fp$bper
  return(dat.fp)

}#END calc_fp



#' @title (FP) Plot time series of FP values.
#'
#' @param dat.fp A data frame. Output from \code{\link{calc_fp}}.
#' @param savepng A Boolean. Option to send output to a png file instead of plot
#'   window. Default is FALSE.
#'
#' @return A lattice plot of the FP series, by stock and age. If \code{savepng}
#'   is TRUE then each stock is sent to an individual png file and the stock
#'   name is included in the file name.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_fpseries(dat.fp = dat.fp, savepng = TRUE)
#' }
plot_fpseries <- function(dat.fp, savepng=FALSE, filename=NA){
  dat.fp <- dat.fp[order(dat.fp$return.year),]
  for(stock in unique(dat.fp$stock.short)){
    dat.fp.sub <- dat.fp[dat.fp$stock.short==stock,]

    xyplot.tmp <- lattice::xyplot(fp~return.year|stock.short+as.factor(age), data=dat.fp.sub, as.table=TRUE,
     panel=function(x,y, subscripts,...){
     lattice::panel.xyplot(x, y, type='b', pch=16, cex=.5)
     if(any(colnames(dat.fp) %in% "fp.xls")){lattice::panel.lines(x,dat.fp$fp.xls[subscripts],type='b', pch=1,cex=0.75, col='black')}
           })

    if(savepng){
      filename <- ifelse(is.na(filename), paste("fp_validate", stock, ".png", sep = "_"), filename)
      png(filename = filename, width = 8, hei=8, units = "in", res = 600)
      print(xyplot.tmp)
      dev.off()
    }else{
      print(xyplot.tmp)
    }
    }#for(stock
}#END plot_fpseries



#' @title Read in stk file of base period exploitation rates.
#'
#' @param filename A character vector of length 1. The stk file of base period
#'   exploitation rates.
#' @param baseperiodfishery.names A character vector. Likely based on the file
#'   "48FisheryName.txt".
#'
#' @return A list comprising two elements. The first, named \code{dat.meta.long}
#'   is a data frame of data other than the ER values. This includes the initial
#'   cohort abundance (\code{initcohortabun}), the maturation rates
#'   (\code{matrates}), and the AEQ factor (\code{aeqfactor}). The second list
#'   element is \code{dat.er.long}, which is a data frame of the ER data.
#' @export
#'
#' @examples
#' \dontrun{
#' baseperiodfishery.names <- readLines("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/48FisheryName.txt")
#' dat.stk <- read_stkfile("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/2015BPC_PII_V1.5.STK", baseperiodfishery.names)
#' }
read_stkfile <- function(filename, baseperiodfishery.names){
  filename <- filename[1]
  dat.tmp <- readLines(filename)

  stock.short.ind <- which(nchar(dat.tmp) ==3)

  stocks <- dat.tmp[stock.short.ind]

  #first line is initial cohort abundance
  #second line is maturation rates
  #third line is the AEQ factor
  dat.meta <- dat.tmp[sort(rep(stock.short.ind, 3))+1:3]
  dat.meta <- do.call("rbind", strsplit(dat.meta, split = "  "))
  dat.meta <- as.data.frame(apply(dat.meta, 2, as.numeric))
  colnames(dat.meta) <- paste0("age",1:ncol(dat.meta))
  dat.meta$data.type <- c("initcohortabun", "matrates", "aeqfactor")
  dat.meta$stock.short <- rep(stocks, rep(3, length(stocks)))

  dat.meta.long <- reshape(dat.meta[,c('stock.short', 'data.type', colnames(dat.meta)[1:(ncol(dat.meta)-2)])], dir='long', varying = list(3:ncol(dat.meta)), timevar = 'age.index', v.names= 'value' , idvar = c('stock.short', 'data.type'))
  rownames(dat.meta.long) <- NULL


  #4th to end of fishery designation is the ER rates by fisheries
  dat.er <- dat.tmp[-(sort(rep(stock.short.ind, 4))+0:3)]
  dat.er <- do.call("rbind", strsplit(dat.er, split = "  "))
  dat.er <- as.data.frame(apply(dat.er, 2, as.numeric))
  colnames(dat.er) <- paste0("age",1:ncol(dat.er))
  dat.er$data.type <- "bper"
  dat.er$baseperiodfishery.name <- baseperiodfishery.names
  dat.er$stock.short <- rep(stocks, rep(length(baseperiodfishery.names), length(stocks)))
  nondata.colnames <- c('baseperiodfishery.name', "stock.short", 'data.type')
  dat.er <- dat.er[,c(nondata.colnames, colnames(dat.er)[!colnames(dat.er) %in% nondata.colnames])]

  dat.er.long <- reshape(dat.er, dir='long', varying = list((length(nondata.colnames)+1):ncol(dat.er)), timevar = 'age.index', v.names= 'value' )

  #this is making the assumption that all starting ages=2 :
  dat.er.long$age <- dat.er.long$age.index+1

  dat.er.long <- subset(dat.er.long, select = -id)

  return(list(dat.meta.long=dat.meta.long, dat.er.long=dat.er.long))
}#END read_stkfile


#' @title Read in mdl files of cwt recovery data.
#'
#' @param filenames A character vector of the mdl file names.
#'
#' @return A list of two elements. The first element, named \code{dat.mdl}, is also a list. Each element of \code{dat.mdl} is also a list and represents the data from one mdl file. The second element of the output list, named \code{dat.mdl.long}, is a data frame of all the combined mdl files. This latter list element is the preferred source of data for analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' mdl.filenames <- list.files(path = "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL", pattern = "MDL$")
#' mdl.filepath <- paste("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL", mdl.filenames, sep="/")
#' dat.mdl <- read_mdl(mdl.filepath)
#' fisheries.needed <- fishery.def[fishery.def$aabm %in% aabm,]
#' dat.mdl.long.sub <- dat.mdl$dat.mdl.long[dat.mdl$dat.mdl.long$fishery.name %in% fisheries.needed$fishery.name,]
#' }
read_mdl <- function(filenames){

  .read_singlemdl <- function(filename){

     mdl.tmp <- readLines(filename)
     stock.short <- mdl.tmp[1]
     #number of fish tagged
     tagged.n <- as.numeric(mdl.tmp[3])
     #number of fish released
     released.n <- as.numeric(mdl.tmp[4])
     #max age
     age.max <- as.numeric(mdl.tmp[5])
     #number of fisheries
     fisheries.n <- as.numeric(mdl.tmp[6])

     fisheries.names <- as.vector(mdl.tmp[7:(6+fisheries.n)])
     #Last lines of the file contain the number of recoveries by age and fishery
     age.n <- length(mdl.tmp)-6 - fisheries.n

     cwt.recoveries <- sapply(mdl.tmp[(7+fisheries.n):length(mdl.tmp)], FUN = function(x){
       as.integer(substring(x,  c(1,1+(1:(fisheries.n-1))*5), c(5,5+(1:(fisheries.n-1))*5)))
     })
     cwt.recoveries <- as.data.frame(cwt.recoveries)
     ages <- rev(seq(age.max,by=-1, len=age.n))
     colnames(cwt.recoveries) <- paste0("age", ages)

     cwt.recoveries$fishery.name <- fisheries.names
     cwt.recoveries.long <- reshape(cwt.recoveries, dir='long', varying = list(1:age.n), v.names= 'value')
     cwt.recoveries.long$age <- ages[cwt.recoveries.long$time]
     cwt.recoveries.long <- subset(cwt.recoveries.long, select= -c(id, time))

     return(list(filename= filename, stock.short=stock.short, tagged.n=tagged.n, released.n=released.n, age.max=age.max, fisheries.n=fisheries.n, fisheries.names=fisheries.names, cwt.recoveries=cwt.recoveries, cwt.recoveries.long=cwt.recoveries.long))
  }#END .read_singlemdl

  dat.mdl <- lapply(filenames, FUN = .read_singlemdl)

  dat.mdl <- lapply(dat.mdl, FUN = function(x){
    x$cwt.recoveries.long$stock.short <- x$stock.short
    return(x)
  })
  dat.mdl.long <- lapply(dat.mdl, "[[", "cwt.recoveries.long")
  dat.mdl.long <- do.call('rbind', args = dat.mdl.long)
  colnames(dat.mdl.long)[colnames(dat.mdl.long)=="value"] <- "r"

  dat.mdl.long <- merge(dat.mdl.long, fishery.def[,c('fishery.name', "fishery.index", "baseperiodfishery.name")], by="fishery.name")

  return(list(dat.mdl=dat.mdl, dat.mdl.long=dat.mdl.long))

}#END read_mdl



#' @title (FP) Build, save, and open an R script to help execute FP calculations.
#'
#' @description This creates and opens a script named "fp_script.R". This is a
#'   template for the user to work with when doing FP estimates. This is
#'   intended to help the user understand the work flow and due to file path
#'   differences, is unlikely to work as is. Some object values will need
#'   updating (for example the datapath).
#'
#' @return Opens an R script that includes the template of functions to
#'   calculate FP values.
#' @export
#'
#' @examples
#' writeFPscript()
writeFPscript <- function(){

script.str <- c('
####### SETUP #######
rm(list=ls())
###### COMMENTS ########


#### LIBRARIES ####


#### DATA ####

aabm <- "seak"

#these fishery subsets match what is defined in the VB
fishery.df <- data.frame(aabm=c(rep("seak",6), rep("nbc",1), rep("wcvi",3)), fishery.index=c(1:6, 8, 10:12), baseperiodfishery.name=c(rep("ALASKA_T",6), rep("NORTH_T",1), rep("WCVI_T",3)), stringsAsFactors = FALSE)
fishery.def <- merge(fishery.def, fishery.df, by="fishery.index", all.x = TRUE)

## this is needed if wishing to look at recovery counts in hrj files:

# load("../../spfi/data/SPFI_20170503/hrj/hrj_from_text.RData")
# hrj.df <- hrj.list$hrj.list.long$HRJ_BY
# fishery.subset <- fishery.def$fishery.index[fishery.def$aabm %in% aabm]
# file.path <- paste(data.path, spfi.output$stock.filename, sep="/")
# data.stock <- readStockData(file.path)
# stock.subset <- unique(data.stock$SPFIFlag.long$Stock.Number)
#
# cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
# if(aabm=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
# #note that r.tsa.sum is limited to base period years
# r.tsa.sum <- calc_tsa.sum(x = cwtcatch[cwtcatch$return.year %in% years.baseperiod,], newvar.name = "r.tsa.sum")


#read in spfi file:
data.path <- paste("../../spfi/data/SPFI_20170503", aabm, sep="/")
file.path <- paste(data.path, "spfi.output_STOCFILE.STF_.RData", sep="/")
load(file.path)
dat.spfi.long <- spfi.output$S.ty


# or use spfi from AK spreadsheet:
dat.spfi <- read.csv("../data/AKFP_CLB16b_spfi.csv" )
dat.spfi <- dat.spfi[,c("YEAR", colnames(dat.spfi[,4:ncol(dat.spfi)]))]
dat.spfi.long <- reshape(dat.spfi, dir="long", varying = list(2:ncol(dat.spfi)), v.names="S.ty", timevar = "fishery.index")
colnames(dat.spfi.long)[colnames(dat.spfi.long)=="YEAR"] <- "return.year"
dat.spfi.long <- subset(dat.spfi.long, select = -id)
dat.spfi.long$source <- "AKFP_CLB16b.xls"


#read in STK file:
baseperiodfishery.names <- readLines("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/48FisheryName.txt")
dat.stk <- read_stkfile("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/2015BPC_PII_V1.5.STK", baseperiodfishery.names)
baseperiodfishery.name <- unique(fishery.def$baseperiodfishery.name[fishery.def$aabm %in% aabm])
dat.er.long.sub <- dat.stk$dat.er.long[dat.stk$dat.er.long$baseperiodfishery.name == baseperiodfishery.name,]

# As the ER and mdl data are being merged, the common column named "value" needs
# to be renamed:
colnames(dat.er.long.sub)[colnames(dat.er.long.sub)=="value"] <- "bper"



#read in MDL files
mdl.filenames <- list.files(path = "../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL", pattern = "MDL$")
mdl.filepath <- paste("../data/2015BPC_fpa_AABM_troll_spfi - 9-20-2016/56FMDL", mdl.filenames, sep="/")
dat.mdl <- read_mdl(mdl.filepath)

fisheries.needed <- fishery.def[fishery.def$aabm %in% aabm,]
dat.mdl.long.sub <- dat.mdl$dat.mdl.long[dat.mdl$dat.mdl.long$fishery.name %in% fisheries.needed$fishery.name,]

#### FP CALCULATION ####

dat.fp <- calc_fp(dat.er.long = dat.er.long.sub, dat.mdl.long = dat.mdl.long.sub, dat.spfi = dat.spfi.long)


####### VALIDATION ########

fp.xls <- read.csv("../data/fp.xls.csv", stringsAsFactors = FALSE)

dat.fp <- merge(dat.fp, fp.xls, by=c("stock.short", "age", "return.year"))

#plot_fpseries(dat.fp = dat.fp, savepng = TRUE, filename = "fp_validate_ssa_mfSPFI.png")
plot_fpseries(dat.fp = dat.fp, savepng = TRUE)
')

write(script.str, file="fp_script.R")
file.edit("fp_script.R" )

}#END writeFPscript
