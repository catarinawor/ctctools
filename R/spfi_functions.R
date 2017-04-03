#' @title (SPFI) Sum SEAK CWT data
#'
#' @description CWT data in SEAK fishery 4 is revised to equal sum of fishery
#'   4+6. This is a summation that is unique to SPFI estimation for Alaska only.
#'   If estimating for Alaska, then the hrj database variables: NomCat# (VB
#'   variable:cwtcatch), AEQCat# (VB variable:aeqcwtcat), and AEQTot# (VB
#'   variable:aeqcwttotmort) for fishery 4 are revised to equal sum of fishery
#'   4+6. The basis of this summation is not documented, but it may be partly
#'   associated with the fact that fishery 6 doesn't exist in the catch data
#'   file. These are the fishery definitions, comma separated:
#'   \tabular{ccccccc}{ fishery \tab gear \tab fishery \tab fishery \tab fishery
#'   \tab fishery \tab fishery \cr index   \tab      \tab type    \tab country
#'   \tab name    \tab region \tab  psc\cr 4 \tab T \tab P \tab US \tab AK JLO T
#'   \tab AK \tab AABM\cr 6 \tab T \tab P \tab US \tab AK FALL T \tab AK \tab
#'   AABM }
#'
#' @param x A data frame, representation of the CWT data. Equivalent to a subset
#'   of the hrj data frame where data.type=="NomCat"
#' @param data.catch A list with strucutre equivalent to the output from the
#'   function \code{readCatchData}.
#'
#' @details
#'
#' @return The function returns a data frame representation of the CWT data, in
#'   same format as the first input argument.
#' @export
#'
#' @examples
#' \dontrun{
#' #user needs to define catch file:
#' data.catch <- readCatchData(...)
#' #this assumes hrj.df has been read in:
#' cwtcatch <- hrj.df[hrj.df$data.type=="NomCat"
#'                    & hrj.df$fishery.index %in% 1:6
#'                    & hrj.df$stock.index %in% C(1,9,10,15,17,20),]
#' cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' }
#'
adjustAlaska <- function(x, data.catch){

  fishery.toupdate <- sort(unique(x$fishery.index))[data.catch$intTopStrata-1]
  fishery.source <- sort(unique(x$fishery.index))[data.catch$intLastStrata]
  fishery.source.data <- x[x$fishery.index==fishery.source,c("stock.index", "age", "brood.year", "value")]
  colnames(fishery.source.data)[colnames(fishery.source.data)=='value'] <- "value2"

  x <- merge(x, fishery.source.data, by=c("stock.index", "age", "brood.year"))

  x$value[x$fishery.index==fishery.toupdate] <- x$value[x$fishery.index==fishery.toupdate] + x$value2[x$fishery.index==fishery.toupdate]

  return(x)
}#END adjustAlaska



#' @title (SPFI) Build, save, and open an R script to help execute SPFI.
#'
#' @description This creates and opens a script named "spfi_script.R". This is a
#'   template for the user to work with when doing SPFI estimates. This is
#'   intended to help the user understand the work flow and due to file path
#'   differences, is unlikely to work as is. Some object values will need
#'   updating (for example the datapath). If the user does not have a copy of
#'   the hrj data base saved  into a .Rdata file, then the functions
#'   \code{readHRJAccessDatabase}, \code{reshapeHRJ}, and the \code{list}
#'   creation will have to executed. It might be wise to then save the
#'   \code{list} to a .Rdata file.
#'
#' @return
#' @export
#'
#' @examples
#' buildSPFIscript()
buildSPFIscript <- function(){

  date.creation <- Sys.time()
  script.str <- c("
####### SETUP #######
rm(list=ls())
###### COMMENTS ########
script.name <- 'spfi_script'
if(!exists('script.metadata')) script.metadata <- list()

script.metadata[[script.name]] <- list(
fileName = paste0(script.name,'.R'),
author= 'Michael Folkes',",
paste0("date.creation=", "'",as.character(date.creation),"',"),
"date.edit= NA,
comments = NA
)# END list

####### DATA #######
data.type <- 'AEQCat' # 'AEQCat' # or 'AEQTot'

region <- 'wcvi' # 'wcvi' # 'nbc' #  'alaska'

#only one aabm in a data folder:
datapath <- paste('./', region, sep='/')

catch.filename <- list.files(path = datapath, pattern = '*.cat')
data.catch <- readCatchData(paste(datapath, catch.filename, sep='/'), strLocation = region)
data.stock <- readStockData(paste(datapath, 'STOCFILE.STF', sep='/') )

fishery.def <- read.csv('fishery_def.csv', stringsAsFactors = FALSE)
jurisdiction <- read.csv('jurisdiction.csv', stringsAsFactors = FALSE)

# reading 32 bit mdb files requires using 32bit R
#hrj.list.wide <- readHRJAccessDatabase('../data/HRJ_database 2016b.mdb')
#hrj.list.long <- reshapeHRJ(hrj.list.wide, data.stock, fishery.def = fishery.def, jurisdiction = jurisdiction)
#hrj.list <- list(hrj.list.wide=hrj.list.wide, hrj.list.long=hrj.list.long)
filename <- '../spfi/data/hrj_from_mdb.RData'
#save(data.hrj, file = filename)
load(filename)

#this is the method to use with hrj text files:
#filename <- list.files(data.path, pattern = 'HRJ')
#stocks22 <- unique(data.stock$stocks.df$StockID)
#filename <- unlist(lapply(X = stocks22, FUN = function(x, filename){
   filename[unlist(regexpr( pattern = x, filename))==1]
  }, filename))
#filepath <- paste(data.path, filename, sep='/')
#hrj.list <- readHRJtext.new(filepath)
#hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list,updateStockByName, data.stock$stocks.df)
#writeHRJaccess(hrj = hrj.list$hrj.cwt.list, filename = paste(datapath, 'test.accdb', sep='/'))
#writeHRJcsv(hrj = hrj.list$hrj.cwt.list)
#hrj.list.long <- reshapeHRJ(hrj.list$hrj.cwt.list, data.stock)
#workdingdata.wide <- reshapeWorkingData(hrj.list.long$working.data)
#writeHRJaccess(hrj = list(workingdata= workdingdata.wide), filename = paste(datapath, 'test.accdb', sep='/'))


####### MAIN #######
#data.hrj is available from the load() above:
hrj.df <- hrj.list$hrj.list.long$HRJ_BY

#all data sets include data prior to 1979. These values are excluded in the VB,
#which has 'intFirstYear As Integer = 1979' (line 83)
# And if those early years are included results are slightly affected.
year.range <- 1979:(data.stock$stockmeta$intLastBrood$value+1)
hrj.df <- hrj.df[hrj.df$return.year %in% year.range,]

fishery.subset <- read.csv(file =  paste(datapath, 'fishery.subset.txt', sep='/'))
stock.subset <- unique(data.stock$SPFIFlag.long$Stock.Number)

#the function calcSPFI calls all the required intermediate function steps and the output is a list that has all intermediate data and the spfi values (S.y)
spfi.output <- calc_SPFI(data.type = data.type, region = region, hrj.df = hrj.df, data.catch = data.catch, data.stock = data.stock, fishery.subset = fishery.subset$fishery.index, stock.subset = stock.subset)

write.csv(x = spfi.output$S.y, file = paste('spfi', region, '.csv', sep = '_'), row.names = FALSE)

####### END #######

")
  write(script.str, file="spfi_script.R")
  file.edit("spfi_script.R" )
}#END buildSPFIscript



#' @title (SPFI) Calculate difference between distribution parameters after each
#'   iteration.
#'
#' @param d.tsa.prior A data frame. This is a copy of \code{d.tsa}, made before
#'   an iteration loop during which \code{d.tsa} will be revised.
#' @param d.tsa A data frame. Output of \code{calc_d.tsa}
#'
#' @return A vector of length one.
#' @export
#'
#' @examples
calc_Difference <- function(d.tsa.prior, d.tsa){

  colnames(d.tsa.prior)[which(colnames(d.tsa.prior)=="d.tsa")] <- "d.tsa.prior"
  d.tsa <- merge(d.tsa,d.tsa.prior, by=c("fishery.index", "stock.index", "age"))
  d.tsa$d.tsa.diff <- abs(d.tsa$d.tsa.prior - d.tsa$d.tsa)

  difference.max <- aggregate(d.tsa.diff~fishery.index, data = d.tsa, max, na.rm=TRUE)
  colnames(difference.max)[2] <- "maxdifference"

  maxmaxdifference <- sum(difference.max$maxdifference, na.rm=TRUE)
  return(maxmaxdifference)

}#END calc_Difference



#' @title (SPFI) Calculate the distribution parameters grouped by fishery,
#'   stock, and age.
#'
#' @description This is equivalent to equation 1 in the draft SPFI document.
#'
#' @param r.tsa.sum Output from \code{calc_tsa.sum}
#' @param n.ysa Synonymous with \code{CWTPop} in VB or:
#'   \code{hrj.df[hrj.df$data.type=="Pop" & hrj.df$fishery.index == 1,]}
#' @param hcwt.ty Output from \code{calc_hcwt.ty}
#' @param standardize.bol A Boolean, default=FALSE.
#'
#' @details The initial call of this function will have no data for
#'   \code{hcwt.ty}. If this is true, then the values are set to 0.01 as is done
#'   in the Visual Basic.
#'
#' @return A data frame of the distribution parameter estimates grouped by
#'   fishery stratum, stock, and age.
#' @export
#'
#' @examples
#' \dontrun{ d.tsa <- calc_d.tsa(r.tsa.sum = r.tsa.sum, n.ysa = cwtpop,
#' standardize.bol = TRUE) }
#'
calc_d.tsa <- function(r.tsa.sum, n.ysa, hcwt.ty=NULL, standardize.bol=FALSE){
  #n.ysa = cwt catch by stock, fishery(strata), age, across years (line 808):

  #initialize hr = 0.01 taken from line 812 of frmMain.vb calc_SPFI()
  if( is.null(hcwt.ty)){
    print("restarting hcwt.ty")
    hcwt.ty <- expand.grid(fishery.index=unique(r.tsa.sum$fishery.index), return.year=unique(n.ysa$return.year), hcwt.ty=0.01 )
  }
  colnames(n.ysa)[colnames(n.ysa)=="value"] <- "n.ysa"

  denom.tmp <- merge( hcwt.ty, n.ysa[,c("stock.index", 'return.year', 'age', 'n.ysa')], by='return.year', all = TRUE)

  denom.tmp$h.by.n <- denom.tmp$hcwt.ty * denom.tmp$n.ysa
  denom.sum <- aggregate(h.by.n~stock.index+fishery.index+age, data = denom.tmp, sum, na.rm=TRUE)
  d.tsa <- merge(r.tsa.sum, denom.sum, by=c('stock.index', 'fishery.index', 'age'))

  d.tsa$d.tsa <- d.tsa$r.tsa.sum/d.tsa$h.by.n

  if(standardize.bol){
    d.prime.max <- aggregate(d.tsa~fishery.index, data = d.tsa, max, na.rm=TRUE)
    colnames(d.prime.max)[2] <- "d.prime.max"
    d.tsa <- merge(d.tsa, d.prime.max, by='fishery.index', all=TRUE)
    d.tsa$d.tsa <- d.tsa$d.tsa/d.tsa$d.prime.max
  }

  attr(d.tsa, "standardize.bol") <- standardize.bol
  return(d.tsa)
}#END calc_d.tsa



#' @title (SPFI) Calculate the CWT harvest rate parameters grouped by fishery,
#'   and year.
#'
#' @description This is equivalent to equation 2 in the draft SPFI document.
#'
#' @param r.ty.sum Output from \code{calc_ty.sum}.
#' @param d.tsa Output from \code{calc_d.tsa}.
#' @param n.ysa
#' @inheritParams calc_d.tsa
#'
#' @return
#' @export
#'
#' @examples
calc_hcwt.ty <- function(r.ty.sum, d.tsa, n.ysa){

  colnames(n.ysa)[colnames(n.ysa)=="value"] <- "n.ysa"

  denom.tmp <- merge(d.tsa, n.ysa, by=c('stock.index', 'age'), all = TRUE)
  denom.tmp$d.by.n <- denom.tmp$d.tsa * denom.tmp$n.ysa
  denom.sum <- aggregate(d.by.n~fishery.index+return.year, data = denom.tmp, sum, na.rm=TRUE)

  hcwt.ty <- merge(r.ty.sum, denom.sum, by=c('fishery.index', 'return.year'), all = TRUE)
  hcwt.ty$hcwt.ty <- hcwt.ty$r.ty.sum / hcwt.ty$d.by.n

  return(hcwt.ty[,c("fishery.index", "return.year", "r.ty.sum", "d.by.n", "hcwt.ty")])

}#END calc_hcwt.ty



#' @title (SPFI) Calculate the AEQ stratum harvest rate parameters grouped by
#'   fishery, and year.
#'
#' @description This is equivalent to equation 6 in the draft SPFI document. In
#'   the Visual Basic this is referred to as the AEQScaler.
#'
#' @param c.ty.sum Output from \code{calc_ty.sum}
#' @param r.ty.sum Output from \code{calc_ty.sum}
#' @param hcwt.ty Output from \code{calc_hcwt.ty}
#'
#' @return
#' @export
#'
#' @examples
calc_H.ty <- function(c.ty.sum, r.ty.sum, hcwt.ty){

  H.ty <- merge(c.ty.sum, r.ty.sum, by=c("fishery.index", 'return.year'))
  H.ty <- merge(H.ty, hcwt.ty[,c('fishery.index', "return.year", "hcwt.ty")], by=c("fishery.index", 'return.year'))

  H.ty$r.ty.sum[H.ty$r.ty.sum==0] <- NA
  H.ty$H.ty <- H.ty$c.ty / H.ty$r.ty.sum * H.ty$hcwt.ty
  return(H.ty)

}#END calc_H.ty



#' @title (SPFI) Calculate the AEQ stratum harvest rate parameters grouped by
#'   fishery, and year.
#'
#' @description This is equivalent to equation 6a in the draft SPFI document. In
#' the Visual Basic this is referred to as the AEQScaler.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param hcwt.ty
#' @param T.ty
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return
#' @export
#'
#' @examples
calc_H.ty2 <- function(c.ty.sum, r.ty.sum, hcwt.ty, T.ty){
  T.ty <- T.ty[,c('fishery.index', "return.year", "T.ty")]
  hcwt.ty <- hcwt.ty[,c('fishery.index', "return.year", "hcwt.ty")]

  H.ty <- merge(c.ty.sum, r.ty.sum, by=c("fishery.index", 'return.year'))
  H.ty <- merge(H.ty, T.ty, by=c("fishery.index", 'return.year'))
  H.ty <- merge(H.ty, hcwt.ty, by=c("fishery.index", 'return.year'))

  H.ty$r.ty.sum[H.ty$r.ty.sum==0] <- NA
  H.ty$hcwt.ty[H.ty$hcwt.ty==0] <- NA

  H.ty$numerator <- H.ty$c.ty / H.ty$r.ty.sum * H.ty$T.ty
  H.ty$denominator <- H.ty$T.ty / H.ty$hcwt.ty
  H.ty$H.ty <- H.ty$numerator/H.ty$denominator
  return(H.ty)

}#END calc_H.ty2



#' @title (SPFI) Calculate the AEQ stratum harvest rate parameters grouped by
#'   fishery, and year.
#'
#' @description This is equivalent to equation 6b in the draft SPFI document. In
#' the Visual Basic this is referred to as the AEQScaler.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param T.ty
#' @param N.ty
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return
#' @export
#'
#' @examples
calc_H.ty3 <- function(c.ty.sum, r.ty.sum, T.ty, N.ty){
  T.ty <- T.ty[,c('fishery.index', "return.year", "T.ty")]
  N.ty <- N.ty[,c('fishery.index', "return.year", "N.ty")]

  H.ty <- merge(c.ty.sum, r.ty.sum, by=c("fishery.index", 'return.year'))
  H.ty <- merge(H.ty, T.ty, by=c("fishery.index", 'return.year'))
  H.ty <- merge(H.ty, N.ty, by=c("fishery.index", 'return.year'))

  H.ty$r.ty.sum[H.ty$r.ty.sum==0] <- NA
  H.ty$N.ty[H.ty$N.ty==0] <- NA

  H.ty$numerator <- H.ty$c.ty / H.ty$r.ty.sum * H.ty$T.ty
  H.ty$denominator <- H.ty$N.ty
  H.ty$H.ty <- H.ty$numerator/H.ty$denominator
  return(H.ty)
}#END calc_H.ty3



#' @title (SPFI) Calculate the AEQ fishery harvest rate parameters grouped by
#'   year.
#'
#' @description This is equivalent to equation 7 in the draft SPFI document.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param hcwt.ty
#' @param T.ty
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return
#' @export
#'
#' @examples
calc_H.y <- function(c.ty.sum, r.ty.sum, hcwt.ty, T.ty){
  T.ty <- T.ty[,c('fishery.index', "return.year", "T.ty")]
  hcwt.ty <- hcwt.ty[,c('fishery.index', "return.year", "hcwt.ty")]

  numerator.tmp <- merge(c.ty.sum, r.ty.sum, by=c("fishery.index", 'return.year'))
  numerator.tmp <- merge(numerator.tmp, T.ty, by=c("fishery.index", 'return.year'))
  numerator.tmp$r.ty.sum[numerator.tmp$r.ty.sum==0] <- NA
  numerator.tmp$numerator <- numerator.tmp$c.ty.sum/numerator.tmp$r.ty.sum * numerator.tmp$T.ty
  numerator <- aggregate(numerator~return.year, data=numerator.tmp, sum, na.rm=TRUE)

  denominator.tmp <- merge(T.ty, hcwt.ty, by=c("fishery.index", 'return.year'))
  denominator.tmp$hcwt.ty[denominator.tmp$hcwt.ty==0] <- NA
  denominator.tmp$denominator <- denominator.tmp$T.ty / denominator.tmp$hcwt.ty
  denominator <- aggregate(denominator~return.year, data=denominator.tmp, sum, na.rm=TRUE)

  H.y <- merge(numerator, denominator, by='return.year')
  H.y$H.y <- H.y$numerator / H.y$denominator
  return(H.y)

}#END calc_H.y



#' @title (SPFI) Calculate the harvest rate parameters grouped by year.
#'
#' @description This is equivalent to equation 7b in the draft SPFI document.
#' This function produces the same output as \code{calc_H.y}.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param T.ty
#' @param N.y Output from \code{calc_N.y}
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return
#' @export
#'
#' @examples
calc_H.y2 <- function(c.ty.sum, r.ty.sum, T.ty, N.y){

  T.ty <- T.ty[,c('fishery.index', "return.year", "T.ty")]

  numerator.tmp <- merge(c.ty.sum, r.ty.sum, by=c("fishery.index", 'return.year'))
  numerator.tmp <- merge(numerator.tmp, T.ty, by=c("fishery.index", 'return.year'))

  numerator.tmp$r.ty.sum[numerator.tmp$r.ty.sum==0] <- NA

  numerator.tmp$numerator <- numerator.tmp$c.ty.sum/numerator.tmp$r.ty.sum * numerator.tmp$T.ty

  numerator <- aggregate(numerator~return.year, data=numerator.tmp, sum, na.rm=TRUE)
  H.y <- merge(numerator, N.y, by='return.year')

  H.y$H.y <- H.y$numerator / H.y$N.y
  return(H.y)

}#END calc_H.y2



#' @title (SPFI) Calculate the abundance by fishery stratum and year.
#'
#' @description
#' This is equivalent to equation 4 in the draft SPFI document.
#'
#' @param T.ty Output from \code{calc_T.ty}.
#' @param hcwt.ty Output from \code{calc_hcwt.ty}.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' N.ty <- calc_N.ty(T.ty = T.ty, hcwt.ty = hcwt.ty)
#' }
calc_N.ty <- function(T.ty, hcwt.ty){

  #T.ty and hcwt.ty don't have same nrows, I'd expect them to be equal
  T.ty <- T.ty[,c('fishery.index', "return.year", "T.ty")]
  hcwt.ty <- hcwt.ty[,c('fishery.index', "return.year", "hcwt.ty")]

  N.ty <- merge(T.ty, hcwt.ty, by=c('fishery.index', "return.year"), all.x = TRUE)
  N.ty$hcwt.ty[N.ty$hcwt.ty==0] <- NA

  N.ty$N.ty <- N.ty$T.ty / N.ty$hcwt.ty
  return(N.ty)

}#END calc_N.ty



#' @title (SPFI) Calculate the total yearly abundance across fishery strata.
#'
#' @description
#' This is equivalent to equation 5 in the draft SPFI document.
#'
#' @param N.ty Output from \code{calc_N.ty}.
#'
#' @return
#' @export
#'
#' @examples
calc_N.y <- function(N.ty){
  N.y <- aggregate(N.ty~return.year, data=N.ty, sum)
  colnames(N.y)[which(colnames(N.y)=="N.ty")] <- "N.y"

  return(N.y)
}#END calc_N.y



#' @title (SPFI) Calculate the stata specific harvest rate indices by year.
#'
#' @description
#' This is equivalent to equation 8 in the draft SPFI document.
#'
#' @param H.ty Output from \code{calc_H.ty} or \code{calc_H.ty2} or \code{calc_H.ty3}.
#'
#' @return
#' @export
#'
#' @examples
calc_S.ty <- function(H.ty){

  H.t.base <- aggregate(H.ty~fishery.index, data=H.ty[H.ty$return.year %in% 1979:1982,], mean, na.rm = TRUE)
  colnames(H.t.base)[2] <- "H.t.base"
  H.ty <- merge(H.ty, H.t.base, by='fishery.index', all = TRUE)
  complete <- complete.cases(H.ty[,c('H.ty', 'H.t.base')])
  H.ty$S.ty[complete] <- H.ty$H.ty[complete] / H.ty$H.t.base[complete]
  return(H.ty)

}#END calc_S.ty



#' @title (SPFI) Calculate the SPFI grouped by year.
#'
#' @description
#' This is equivalent to equation 9 in the draft SPFI document.
#'
#' @param H.y Output from \code{calc_H.y} or \code{calc_H.y2}.
#'
#' @return A data frame with the SPFI values by year.
#' @export
#'
#' @examples
calc_S.y <- function(H.y){

  H.y$H.y.base <- mean(H.y$H.y[H.y$return.year %in% 1979:1982], na.rm = TRUE)
  H.y$S.y <- H.y$H.y / H.y$H.y.base

  year.series <- data.frame(return.year=min(H.y$return.year, na.rm = TRUE):max(H.y$return.year, na.rm = TRUE))
  H.y <- merge(year.series, H.y, by='return.year', all=TRUE)
  return(H.y)

}#END calc_S.y



#' @title (SPFI) Wrapper function to calculate the stratified proportional
#'   fishery index (SPFI).
#'
#' @param data.type A character vector of length one. The value can be either
#'   "AEQCat" or "AEQTot".
#' @param region A character vector of length one. The value can be "wcvi",
#'   "nbc", or "alaska".
#' @param hrj.df A data frame. This is a long format table from the HRJ list.
#' @param data.catch Output from \code{readCatchData}.
#' @param data.stock Output from \code{readStockData}.
#' @param fishery.subset A vector or one column data frame.
#' @param stock.subset A vector of the stock numbers.
#'
#' @description After reading in the catch, stock, and HRJ data, the user can
#'   run this funtion alone (with appropriate arguments) to obtain the SPFI
#'   estimates.
#'
#' @return A list comprising nine elements. Each element is a data frame
#'   comprising the results of the intermediate (and final) calculations. The
#'   element names are similar to the function name for the calculation. For
#'   example the distribution parameters are calculated by \code{calc_d.tsa} and
#'   found in the list element named \code{d.tsa}. The nine elements are named:
#'   \code{d.tsa, hcwt.ty, T.ty, N.ty, N.y, H.ty, H.y, S.ty, S.y}. The SPFI
#'   estimates can be found in the element named \code{S.y}.
#' @export
#'
#' @examples
#' \dontrun{
#' data.type <- "AEQTot" # "AEQCat" # or "AEQTot"
#' region <- "alaska" # "wcvi" # "nbc" #  "alaska" #only one aabm in a data
#' folder: data.catch <- readCatchData("wcvi7914.cat", strLocation = region)
#' data.stock <- readStockData( "STOCFILE.STF") load("hrj_from_mdb.RData")
#' #data.hrj is available from the load() above: hrj.df <-
#' data.hrj$hrj.list.long$HRJ_BY #all data sets include data prior to 1979.
#' These values are excluded in the VB. year.range <-
#' 1979:(data.stock$stockmeta$intLastBrood$value+1) hrj.df <-
#' hrj.df[hrj.df$return.year %in% year.range,] fishery.subset <- read.csv(file =
#' "fishery.subset.txt") stock.subset <-
#' unique(data.stock$SPFIFlag.long$Stock.Number) calc_SPFI(data.type =
#' data.type, region = region, hrj.df = hrj.df, data.catch = data.catch,
#' data.stock = data.stock, fishery.subset = fishery.subset$fishery.index,
#' stock.subset = stock.subset) }
calc_SPFI <- function(data.type =c("AEQCat", "AEQTot"), region = c("wcvi", "nbc", "alaska"), hrj.df=NA, data.catch, data.stock, fishery.subset=NA, stock.subset=NA ){

  time.start <- Sys.time()

  SPFIFlag.long <- data.stock$SPFIFlag.long[,c("Stock.Number", "age", "value")]
  colnames(SPFIFlag.long)[colnames(SPFIFlag.long)=="value"] <- "spfiflag"
  hrj.df <- merge(hrj.df,SPFIFlag.long, by.x=c("stock.index", "age"), by.y=c("Stock.Number", "age") )
  hrj.df <- hrj.df[hrj.df$spfiflag==1,]


  cwtpop <- hrj.df[hrj.df$data.type=="Pop"
                   & hrj.df$fishery.index == 1
                   & hrj.df$stock.index %in% stock.subset,]

  cwtpop <- subset(cwtpop,select = -fishery.index) #n.ysa

  cwtcatch <- hrj.df[hrj.df$data.type=="NomCat"
                     & hrj.df$fishery.index %in% fishery.subset
                     & hrj.df$stock.index %in% stock.subset,]

  if(region=="alaska") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)

  aeqcwt <- hrj.df[hrj.df$data.type==data.type
                        & hrj.df$fishery.index %in% fishery.subset
                        & hrj.df$stock.index %in% stock.subset,]

  if(region=="alaska") aeqcwt <- adjustAlaska(x = aeqcwt, data.catch = data.catch)


  r.tsa.sum <- calc_tsa.sum(x = cwtcatch, newvar.name = "r.tsa.sum") # same as SumCWTCat in VB
  r.ty.sum <- calc_ty.sum(x = cwtcatch, newvar.name = "r.ty.sum") #same as SumCWTCat2 in vb
  c.ty.sum <- calc_ty.sum(x = aeqcwt, newvar.name = "c.ty.sum") #same as SumAEQCWTCat in vb


  maxmaxdifference.limit <-  0.0000001
  maxmaxdifference <- 1
  counter <- 0
  maxmaxdifference.df <- data.frame(time.val=as.POSIXct(character()), maxmaxdifference=numeric())

  d.tsa <- calc_d.tsa(r.tsa.sum = r.tsa.sum, n.ysa = cwtpop, standardize.bol = TRUE)

  while(maxmaxdifference > maxmaxdifference.limit){
    hcwt.ty <- calc_hcwt.ty(r.ty.sum=r.ty.sum,  d.tsa = d.tsa, n.ysa = cwtpop)

    d.tsa.prior <- d.tsa

    d.tsa <- calc_d.tsa(r.tsa.sum = r.tsa.sum, n.ysa = cwtpop, hcwt.ty = hcwt.ty, standardize.bol = TRUE)

    maxmaxdifference <- calc_Difference(d.tsa.prior, d.tsa)

    counter <- counter+1
    cat(paste(counter, maxmaxdifference,"\n"))
    maxmaxdifference.df <- rbind(maxmaxdifference.df, data.frame(time.val=Sys.time(), maxmaxdifference=maxmaxdifference))
  }
  cat("Minimum reached\n")
  T.ty <- calc_T.ty(catch.df = data.catch$data.catch)

  N.ty <- calc_N.ty(T.ty = T.ty, hcwt.ty = hcwt.ty)

  N.y <- calc_N.y(N.ty = N.ty)

  H.ty <- calc_H.ty(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, hcwt.ty = hcwt.ty)
  #H.ty <- calc_H.ty2(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, hcwt.ty = hcwt.ty, T.ty = T.ty)
  #H.ty <- calc_H.ty3(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, T.ty = T.ty, N.ty = N.ty)

  H.y <- calc_H.y(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, hcwt.ty = hcwt.ty, T.ty = T.ty)
  #H.y <- calc_H.y2(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, T.ty = T.ty, N.y = N.y)

  S.ty <- calc_S.ty(H.ty = H.ty)

  S.y <- calc_S.y(H.y = H.y)
  cat("Completed\n")
  cat(paste(round(Sys.time()- time.start,1), "seconds"))

  return(list(d.tsa=d.tsa, hcwt.ty=hcwt.ty, T.ty=T.ty, N.ty=N.ty, N.y=N.y, H.ty=H.ty, H.y=H.y, S.ty=S.ty, S.y=S.y))

}#END calc_SPFI



#' @title (SPFI)
#'
#' @param x
#' @param newvar.name
#'
#' @return
#' @export
#'
#' @examples
calc_tsa.sum <- function(x, newvar.name ='value.sum'){
  tsa.sum <- aggregate(value~stock.index+fishery.index+age, data= x, sum , na.rm=TRUE)
  colnames(tsa.sum)[colnames(tsa.sum)=="value"] <- newvar.name
  return(tsa.sum)
}#END calc.r.tsa.sum



#' @title (SPFI)
#'
#' @param x
#' @param newvar.name
#'
#' @return
#' @export
#'
#' @examples
calc_ty.sum <- function(x, newvar.name ='value.sum'){
  ty.sum <- aggregate(value~fishery.index+return.year, data= x, sum , na.rm=TRUE)
  colnames(ty.sum)[colnames(ty.sum)=="value"] <- newvar.name
  return(ty.sum)
}#END calc.r.ty.sum



#' @title (SPFI) Calculate the Pacific Salmon Treaty catch, grouped by fishery
#'   stratum and year.
#' @description This is equivalent to equation 3 in the draft SPFI document. The
#'   output is the same as found in the Visual Basic variable:
#'   \code{CatchContribution}. The SEAK data includes values for "Alaska
#'   hatchery contribution". The "Alaska hatchery contribution" is subtrated
#'   from the catch (by stratum and year) to produce the PSC catch.
#'
#' @param catch.df A data frame, extracted from the output of
#'   \code{readCatchData}, such as: \code{data.catch$data.catch}.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' data.catch <- readCatchData("wcvi7914.cat", strLocation = 'wcvi')
#' T.ty <- calc_T.ty(catch.df = data.catch$data.catch)
#' }
calc_T.ty <- function(catch.df){

  T.ty <- catch.df
  colnames(T.ty)[colnames(T.ty)=="CatchContribution"] <- "T.ty"
  colnames(T.ty)[colnames(T.ty)=="TempYear"] <- "return.year"
  colnames(T.ty)[colnames(T.ty)=="TempStrata"] <- "fishery.index"
  return(T.ty)

}#END calc_T.ty



#' @title Read the CTC catch data files (*.cat).
#'
#' @param filename A string of length one, defining the name of the catch file.
#'   If more than one file name is included, only the first is used.
#' @param strLocation A string of length one, defining the AABM location
#'   ("alaska", "nbc", "wcvi").
#'
#' @details
#' @return A list of six elements. The sixth element is a data frame of the
#'   catch data.
#' @export
#'
#' @examples
#' \dontrun{
#' readCatchData("wcvi7914.cat", strLocation = "wcvi")
#' }
readCatchData <- function(filename, strLocation=NA){
  if(!is.character(filename)) stop("'filename' must be a character string")
  filename <- filename[1]

  dat.tmp <- read.csv(filename, stringsAsFactors = FALSE, header = FALSE)
  colnames(dat.tmp) <- c("TempYear", "TempStrata", "strTempStrata", "TempCatch", "TempContribution")
  dat.tmp$strStrata <- dat.tmp$strTempStrata
  dat.tmp$CatchContribution <- dat.tmp$TempCatch - dat.tmp$TempContribution

  intFirstStrata <- min(dat.tmp$TempStrata)
  intTopStrata <-  max(dat.tmp$TempStrata)
  if(!is.na(strLocation) & tolower(strLocation) == "alaska"){
    intLastStrata <-  intTopStrata + 1
  } else {
    intLastStrata <-  intTopStrata
  }

  intLastYear <- max(dat.tmp$TempYear)
  intFirstYear <- min(dat.tmp$TempYear)

  #in frmMain.vb line 769
  # If strLocation = "Alaska" Then strStrata(intLastStrata) = "FALL"
  if(!is.na(strLocation) & tolower(strLocation) == "alaska") dat.tmp$strStrata[dat.tmp$TempStrata ==intLastStrata] <- "FALL"

  return(list(intFirstStrata=intFirstStrata, intTopStrata=intTopStrata, intLastStrata=intLastStrata, intFirstYear=intFirstYear, intLastYear=intLastYear, data.catch=dat.tmp) )
}#END readCatchData



#' @title Read text files that are stored in a PBS archive format.
#'
#' @details This format is created by Michael Folkes (DFO) and not in
#'   circulation.
#'
#' @param filepaths.list A list of individual filenames for importing.
#'
#' @return A list comprising any meta data included in the original text file
#'   and the data, which may be a vector or data frame.
#' @export
#'
#' @examples
readDataArchive <- function(filepaths.list){
  results <- lapply(filepaths.list$filepaths, FUN = function(x){
    data.list <- list(metadata=NULL, data=NULL)
    data.tmp <- readLines(x)
    data.tmp <- trimws(data.tmp)

    start.ind <- as.integer(which(data.tmp %in% c("$metadata.name:", "$data.name:")))
    stop.ind <- as.integer(c(start.ind[2:length(start.ind)]-1, length(data.tmp)))
    element.name <-  substr(data.tmp[start.ind],2,nchar(data.tmp[start.ind])-6 )
    search.df <- data.frame(element.name=element.name, start.ind=start.ind, stop.ind=stop.ind , stringsAsFactors = FALSE)

    for(row.ind in 1:nrow(search.df)){
      start.ind <- search.df[row.ind, 'start.ind']
      stop.ind <- search.df[row.ind, 'stop.ind']
      element.name <- search.df[row.ind, 'element.name']
      data.tmp.sub <- data.tmp[start.ind:stop.ind]
      subelement.name <- data.tmp.sub[2]

      sub.ind <- grep(pattern = "\\$", x = data.tmp.sub)[-1]
      subsubelement.name <- substr( data.tmp.sub[sub.ind], unlist(gregexpr(pattern = "\\.", text = data.tmp.sub[sub.ind]) )+1, nchar(data.tmp.sub[sub.ind])-1 )

      list.tmp <- vector(mode = 'list', length = length(subsubelement.name))
      names(list.tmp) <- subsubelement.name
      sub.ind.df <- data.frame(subsubelement.name=subsubelement.name,start.ind=sub.ind+1, end.ind=c(sub.ind[-1]-1, length(data.tmp.sub)), stringsAsFactors = FALSE)
        for(row.ind2 in 1:nrow(sub.ind.df)){

          #removing empty elements:
          data.tmp.sub2 <- data.tmp.sub[sub.ind.df$start.ind[row.ind2]:sub.ind.df$end.ind[row.ind2]]
          data.tmp.sub2 <- data.tmp.sub2[data.tmp.sub2 !=""]
          list.tmp[sub.ind.df$subsubelement.name[row.ind2]] <- list(data.tmp.sub2)
        }

      if(list.tmp["class"]=="data.frame"){
        data.tmp <- do.call(rbind,strsplit(list.tmp$value, ","))
        #x2[[1]]$class <- 'data.frame'
        data.tmp <- data.frame(data.tmp, stringsAsFactors=FALSE)

        if(exists('colnames', where=list.tmp)) colnames(data.tmp) <- trimws(unlist(strsplit(list.tmp$colnames, ",")))

        if(exists('classes', where=list.tmp))  data.tmp[] <- Map(`class<-`, data.tmp, trimws(unlist(strsplit(list.tmp$classes, ","))) )
        list.tmp$value <- data.tmp
      }

      temp.list <- list(list.tmp)
      names(temp.list) <- subelement.name
      data.list[[element.name]] <-  c(data.list[[element.name]], temp.list)
    }#END for(row.ind in

    return(data.list)
  })



  names(results) <- filepaths.list$filenames

  return(results)

}#END readDataArchive



#' @title Read in data from the HRJ (MS Access) data base.
#'
#' @description This function relies on the RODBC package for reading MS Access
#'   data bases. Due to intermittent, non-transparent changes by Microsoft, the
#'   RODBC package has broken in the past. Additionally, 32 bit mdb files
#'   require the use of 32bit R. Based on these extra steps, it is recommended
#'   that the output from this function (a list) be saved to a .Rdata file for
#'   future use. Specifically, the examples demonstrate the use of
#'   \code{readHRJAccessDatabase} in conjunction with \code{reshapeHRJ} for
#'   flexible data manipulation.
#'
#'
#'
#' @param filename
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
#' hrj.list.long <- reshapeHRJ(hrj.list.wide, data.stock)
#' data.hrj <- list(hrj.list.wide=hrj.list.wide, hrj.list.long=hrj.list.long)
#' filename <- "../data/hrj_from_mdb.RData"
#' save(data.hrj, file = filename)
#' load(filename)
#' }
readHRJAccessDatabase <- function(filename){
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



#' Read CTC stock file (stocfile.stf).
#'
#' @param filename
#' @inheritParams readCatchData
#'
#' @return A list of four elements. The first element, \code{stockmeta}, holds
#'   the meta data found in the first five lines of the stock file. The second
#'   element, \code{SPFIFlag}, is a data frame defining what stock-age
#'   combinations will be included in the analysis. The third element,
#'   \code{SPFIFlag.long}, is the same as \code{SPFIFlag}, but reshaped to a
#'   long format. The fourth element, \code{stocks.df}, is the table of stocks
#'   (found in the stock file).
#' @export
#'
#' @examples
readStockData <- function(filename){

  varNames <- c("intNumStocks", "intNumFisheries","intFirstBrood", "intLastBrood","intNumSPFIStocks")
  stock.vec <- readLines(filename)
  stock.vec <- trimws(stock.vec)
  stockmeta <- strsplit(stock.vec[1:5],"," )
  stockmeta <- lapply(stockmeta, FUN = function(x){
    x.tmp <- trimws(x)
    list(description=x.tmp[2], value=as.integer(x.tmp[1]))
  })
  names(stockmeta) <- varNames

  SPFIFlag <- read.csv(filename,skip=length(varNames), nrows = stockmeta$intNumSPFIStocks$value )
  colnames(SPFIFlag)[2] <- "StartAge.0"
  colnames.vec <- colnames(SPFIFlag)
  SPFIFlag.long <- reshape(data=SPFIFlag, dir='long', varying=list(2:ncol(SPFIFlag)), timevar = 'column.index', v.names= 'value' )
  SPFIFlag.long$grouping.var <- colnames.vec[2:length(colnames.vec)][SPFIFlag.long$column.index]
  SPFIFlag.long$age.index <- as.integer(substr(SPFIFlag.long$grouping.var, nchar(SPFIFlag.long$grouping.var), nchar(SPFIFlag.long$grouping.var) ))

  SPFIFlag.long <- SPFIFlag.long[,-which(colnames(SPFIFlag.long) %in% c("id", "column.index", "grouping.var"))]

  stocks.df <- read.csv(filename, skip=length(varNames) + stockmeta$intNumSPFIStocks$value +1, stringsAsFactors = FALSE, strip.white = TRUE )

  SPFIFlag.long <- merge(SPFIFlag.long, stocks.df, by="Stock.Number", all.x=TRUE)
  SPFIFlag.long$age <- SPFIFlag.long$Start.Age + SPFIFlag.long$age.index




  return(list(stockmeta=stockmeta, SPFIFlag=SPFIFlag, SPFIFlag.long=SPFIFlag.long, stocks.df=stocks.df))
}#END readStockData



#' @title Reshape wide formatted HRJ file to long format.
#'
#' @param hrj.list A list. Output from \code{readHRJAccessDatabase}.
#' @param data.stock A list. Output from \code{readStockData}.
#'
#' @return A list comprising two elements, which are both data frames of the HRJ
#'   data tables 'B' & 'C' in long format.
#' @export
#'
#' @examples
#' \dontrun{
#' # reading 32 bit mdb files requires using 32bit R
#' hrj.list.wide <- readHRJAccessDatabase("HRJ_database 2016b.mdb")
#' hrj.list.long <- reshapeHRJ(hrj.list.wide, data.stock)
#' }
#'
reshapeHRJ <- function(hrj.list, data.stock, fishery.def=NULL, jurisdiction=NULL){
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

  #  colnames(age.count.byreturn)[colnames(age.count.byreturn)=="age"] <- "agecount.returnyear"
  #  hrj.long <- merge(hrj.long, age.count.byreturn, by='return.year', all.x=TRUE)


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

 #The data frame 'working.data' will be c data and c data revised with 'b' based
 #the update rule.
  hrj.list.long2$working.data <- hrj.list.long2$HRJ_CY

  # this is to estimate the maximum number of age classes by stock:
  agecount.st.by.dt.fi <- aggregate(age~stock.index+brood.year+data.type+fishery.index, data=hrj.list.long2$working.data, length)
  agecount.stock <- aggregate(age~stock.index, data=agecount.st.by.dt.fi, max)
  colnames(agecount.stock)[colnames(agecount.stock)=='age'] <- "expected.age.count"

  #this will add column of age counts by return year to help
  #identify what data should be replaced by "B" files.
  agecount.st.ry.dt.fi <- aggregate(age~stock.index+return.year+data.type+fishery.index, data=hrj.list.long2$working.data, length)
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
  hrj.list.long2$working.data <- merge(hrj.list.long2$working.data, hrj.list.long2$HRJ_BY[,c('fishery.index', 'stock.index', 'brood.year', 'data.type', 'age', 'value.b')], by=c('fishery.index', 'stock.index', 'brood.year', 'data.type', 'age'), all.x = TRUE)

  hrj.list.long2$working.data$value.c <- hrj.list.long2$working.data$value

  hrj.list.long2$working.data <- merge(hrj.list.long2$working.data, agecount.st.ry[,c('stock.index', 'return.year', 'data.from.b')], by=c("stock.index", 'return.year'))
  hrj.list.long2$working.data$value[hrj.list.long2$working.data$data.from.b==TRUE] <- hrj.list.long2$working.data$value.b[hrj.list.long2$working.data$data.from.b==TRUE]


  #merging in the fishery.def and jurisdiction files:
  if(exists('fishery.def') & !is.null(fishery.def)){
    hrj.list.long2 <- lapply(hrj.list.long2, function(x,fishery.def){
      if(!is.null(x)) merge(x, fishery.def, by="fishery.index", all.x = TRUE)
    }, fishery.def )
  }

  if(exists('jurisdiction') & !is.null(jurisdiction)){
    hrj.list.long2 <- lapply(hrj.list.long2, function(x,jurisdiction){
      if(!is.null(x)) merge(x, jurisdiction,  by="stock", all.x = TRUE)
      }, jurisdiction )
  }

  return(hrj.list.long2)
}#END reshapeHRJ



#' Reshape HRJ data frame from long to wide format for export to database.
#'
#' @param workingdata  A data frame, in long format and typically form the HRJ list.
#'
#' @return A data frame in wide format (same structure as HRJ database tables).
#' @export
#'
#' @examples
#' \dontrun{
#' workdingdata.wide <- reshapeWorkingData(hrj.list.long$working.data)
#  writeHRJaccess(hrj = list(workingdata= workdingdata.wide), filename = 'test.accdb')
#' }
#'
reshapeWorkingData <- function(workingdata){

  data.tmp <- workingdata[,c("stock.index", "brood.year", "fishery.index", "oldestage", "data.type", "age", "value")]
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


}#END reshapeWorkingData

