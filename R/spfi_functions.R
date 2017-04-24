#' @title (SPFI) Sum SEAK CWT data
#'
#' @description CWT data in SEAK fishery 4 is revised to equal sum of fishery
#'   4+6. This is a summation that is unique to SPFI estimation for Alaska only.
#'   If estimating for Alaska, then the hrj database variables: NomCat# (VB
#'   variable:cwtcatch), AEQCat# (VB variable:aeqcwtcat), and AEQTot# (VB
#'   variable:aeqcwttotmort) for fishery 4 are revised to equal sum of fishery
#'   4+6. The basis of this summation is not documented, but it may be partly
#'   associated with the fact that fishery 6 doesn't exist in the catch data
#'   file. These are the fishery definitions:
#'   \tabular{ccccccc}{ fishery \tab gear \tab fishery \tab fishery \tab fishery
#'   \tab fishery \tab fishery \cr index   \tab      \tab type    \tab country
#'   \tab name    \tab region \tab  psc\cr 4 \tab T \tab P \tab US \tab AK JLO T
#'   \tab AK \tab AABM\cr 6 \tab T \tab P \tab US \tab AK FALL T \tab AK \tab
#'   AABM }
#'
#' @param x A data frame, representation of the CWT data. Equivalent to a subset
#'   of the hrj data frame where data.type=="NomCat"
#' @param data.catch A list with structure equivalent to the output from the
#'   function \code{\link{readCatchData}}.
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
#' #this assumes hrj.df has been read in and transformed to long format:
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
#'   \code{\link{readHRJAccessDatabase}}, \code{\link{reshapeHRJtolong}}, and the
#'   \code{list} creation will have to executed. It might be wise to then save
#'   the \code{list} to a .Rdata file.
#'
#' @return Opens an R script that includes the template of functions to
#'   calculate SPFI.
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

region <- 'wcvi' # 'wcvi' # 'nbc' #  'seak'

#only one aabm in a data folder:
#datapath <- paste('./', region, sep='/')

catch.filename <- list.files(pattern = '*.cat')
data.catch <- readCatchData(catch.filename, strLocation = region)
data.stock <- readStockData('STOCFILE.STF')

# reading 32 bit mdb files requires using 32bit R
hrj.list.wide <- readHRJAccessDatabase('HRJ_database 2016b.mdb')
hrj.list.long <- reshapeHRJtolong(hrj.list.wide, data.stock)
hrj.list <- list(hrj.list.wide=hrj.list.wide, hrj.list.long=hrj.list.long)
#filename <- 'hrj_from_mdb.RData'
#save(hrj.list, file = filename)
#load(filename)

#this is the method to use with hrj text files:
#filename <- list.files(data.path, pattern = 'HRJ')
#filepath <- paste(data.path, filename, sep='/')
#hrj.list <- readHRJtext(filepath)
#add stock number column to the data frames:
#hrj.list$hrj.cwt.list <- lapply(hrj.list$hrj.cwt.list, updateStockByName, data.stock$stocks.df)
#write to a prebuilt access data base (R cannot create data base files):
#writeHRJAccessDatabase(hrj = hrj.list$hrj.cwt.list, filename = 'test.accdb')
#write csv files in same format as found in data base:
#writeHRJcsv(hrj = hrj.list$hrj.cwt.list)

#long format is what the spfi code uses:
#hrj.list.long <- reshapeHRJtolong(hrj.list$hrj.cwt.list, data.stock)

#reshape to wide format prior to writing to access:
#workdingdata.wide <- reshapeHRJtowide(hrj.list.long)
#writeHRJAccessDatabase(hrj = workdingdata.wide, filename = 'test.accdb')


####### MAIN #######
#hrj.list is available from the load() above:
hrj.df <- hrj.list$hrj.list.long$HRJ_BY

#all data sets include data prior to 1979. These values are excluded in the VB,
#which has 'intFirstYear As Integer = 1979' (line 83)
# And if those early years are included results are slightly affected.
year.range <- 1979:(data.stock$stockmeta$intLastBrood$value+1)
hrj.df <- hrj.df[hrj.df$return.year %in% year.range,]


#the function calcSPFI calls all the required intermediate function steps and
#the output is a list that has all intermediate data and the spfi values (S.y)
spfi.output <- calc_SPFI(data.type = data.type, region = region, hrj.df = hrj.df, data.catch = data.catch, data.stock = data.stock, apc=FALSE)

write.csv(x = spfi.output$S.y, file = paste('spfi', region, '.csv', sep = '_'), row.names = FALSE)

write_table6.6(spfi.output)

####### END #######

")
  write(script.str, file="spfi_script.R")
  file.edit("spfi_script.R" )
}#END buildSPFIscript



#' @title (SPFI) Data imputation by the Average Proportion Correction method.
#'
#' @param data.df A data frame. Typically output from \code{\link{calc_N.ty}}.
#' @param stratum.var A string. The name of the stratum variable. Default is
#'   "fishery.index".
#' @param year.var  A string. The name of the year variable. Default is
#'   "return.year".
#' @param value.var  A string. The name of the data variable. Default is "N.ty".
#'
#' @description This reproduces the results of the APC imputation as described on page 99 and Appendix 5 of REPORT TCCHINOOK (09)-2 (\url{http://www.psc.org/download/35/chinook-technical-committee/2120/tcchinook09-2.pdf}).
#'
#' @return A data frame of two columns. The first column usually has the same
#'   name as the \code{year.var} argument and the second is \code{N.t}
#' @export
#'
#' @examples
calc_APC <- function(data.df, stratum.var="fishery.index", year.var="return.year", value.var="N.ty"){

  colnames(data.df)[colnames(data.df)==stratum.var] <- "stratum"
  colnames(data.df)[colnames(data.df)==year.var] <- "return.year"
  colnames(data.df)[colnames(data.df)==value.var] <- "value"

  year.NA <- unique(data.df$return.year[is.na(data.df$value)])

  data.df.sum <- aggregate(value~return.year, data = data.df, sum, na.action = na.pass)
  colnames(data.df.sum)[colnames(data.df.sum)=='value'] <- "sum.complete"
  data.df.sum$apc[!is.na(data.df.sum$sum.complete)] <- 1
  data.df <- merge(data.df, data.df.sum, by='return.year')

  data.df$proportion <- data.df$value / data.df$sum.complete
  ap <- aggregate(proportion~stratum, data = data.df, mean)
  colnames(ap)[colnames(ap)=='proportion'] <- "proportion.avg"
  data.df <- merge(data.df, ap, by='stratum')

  ### alternate approach is to calculate a total abundance estimate based on the values in each stratum then take the mean of all the estimates.
  data.df$estimate.mean <- NA
  data.df$estimate.mean[data.df$return.year %in% year.NA] <- data.df$value[data.df$return.year %in% year.NA] / data.df$proportion.avg[data.df$return.year %in% year.NA]

  annual.estimate <- aggregate(estimate.mean~return.year, data.df, mean)

  ### tech report method, summing proportions before division:
  data.df.sum3 <- aggregate(value~return.year, data.df[data.df$return.year %in% year.NA,], sum)
  data.df.sum4 <- aggregate(proportion.avg~return.year, data.df[data.df$return.year %in% year.NA & !is.na(data.df$value),], sum)
  data.df.sum5 <- merge(data.df.sum3, data.df.sum4, by='return.year')
  data.df.sum5$estimate.sum <- data.df.sum5$value/data.df.sum5$proportion.avg
  colnames(data.df.sum5)[colnames(data.df.sum5)=="proportion.avg"] <- "apc"


  annual.estimate <- merge(annual.estimate, data.df.sum5[,c('return.year', 'estimate.sum', "apc")], by='return.year')

  results <- rbind(data.df.sum[!is.na(data.df.sum$sum.complete),], data.frame(return.year=annual.estimate$return.year, sum.complete=annual.estimate$estimate.sum, apc=annual.estimate$apc))
  results <- results[order(results$return.year),]
  colnames(results) <- c(year.var, "N.y", "APCscalar")

  return(list(apc.results=results, annual.estimate))
}#END calc_APC



#' @title (SPFI) Calculate difference between distribution parameters after each
#'   iteration.
#'
#' @param d.tsa.prior A data frame. This is a copy of \code{d.tsa}, made before
#'   an iteration loop during which \code{d.tsa} will be revised.
#' @param d.tsa A data frame. Output of \code{\link{calc_d.tsa}}.
#'
#' @description The user is not typically going to use this function. It is
#'   called by \code{\link{calc_SPFI}}.
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
#'   This function does a single estimate of the \code{d} values. It does not
#'   perform iterations to optimize.
#'
#' @param r.tsa.sum Output from \code{\link{calc_tsa.sum}}.
#' @param n.ysa Synonymous with \code{CWTPop} in VB or:
#'   \code{hrj.df[hrj.df$data.type=="Pop" & hrj.df$fishery.index == 1,]}
#' @param hcwt.ty Output from \code{\link{calc_hcwt.ty}}.
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
#' \dontrun{
#' #look to \code{\link{buildSPFIscript}} for creating hrj.df
#' hrj.df <- hrj.df[hrj.df$spfiflag==1,]
#' cwtpop <- hrj.df[hrj.df$data.type=="Pop" & hrj.df$fishery.index == 1 & hrj.df$stock.index %in% stock.subset,]
#' cwtpop <- subset(cwtpop,select = -fishery.index) #n.ysa
#' cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' r.tsa.sum <- calc_tsa.sum(x = cwtcatch, newvar.name = "r.tsa.sum")
#' d.tsa <- calc_d.tsa(r.tsa.sum = r.tsa.sum, n.ysa = cwtpop, standardize.bol = TRUE)
#' }
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
#'   This function does a single estimate of the harvest rate values. It does
#'   not perform iterations to optimize.
#'
#' @param r.ty.sum Output from \code{\link{calc_ty.sum}}.
#' @param d.tsa Output from \code{\link{calc_d.tsa}}.
#' @param n.ysa
#' @inheritParams calc_d.tsa
#'
#' @return A data frame of the harvest rate parameter estimates grouped by
#'   fishery stratum and year.
#' @export
#'
#' @examples
#' \dontrun{
#' #look to \code{\link{buildSPFIscript}} for creating hrj.df
#' hrj.df <- hrj.df[hrj.df$spfiflag==1,]
#' cwtpop <- hrj.df[hrj.df$data.type=="Pop" & hrj.df$fishery.index == 1 & hrj.df$stock.index %in% stock.subset,]
#' cwtpop <- subset(cwtpop,select = -fishery.index) #n.ysa
#' cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' r.ty.sum <- calc_ty.sum(x = cwtcatch, newvar.name = "r.ty.sum")
#' d.tsa <- calc_d.tsa(r.tsa.sum = r.tsa.sum, n.ysa = cwtpop, standardize.bol = TRUE)
#' hcwt.ty <- calc_hcwt.ty(r.ty.sum=r.ty.sum,  d.tsa = d.tsa, n.ysa = cwtpop)
#' }
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
#' @param c.ty.sum Output from \code{\link{calc_ty.sum}}.
#' @param r.ty.sum Output from \code{\link{calc_ty.sum}}.
#' @param hcwt.ty Output from \code{\link{calc_hcwt.ty}}.
#'
#' @return A data frame of the AEQ stratum harvest rate parameter estimates
#'   grouped by fishery stratum and year.
#' @export
#'
#' @examples
#' \dontrun{
#' aeqcwt <- hrj.df[hrj.df$data.type==data.type & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") aeqcwt <- adjustAlaska(x = aeqcwt, data.catch = data.catch)
#' c.ty.sum <- calc_ty.sum(x = aeqcwt, newvar.name = "c.ty.sum")
#'
#' cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' r.ty.sum <- calc_ty.sum(x = cwtcatch, newvar.name = "r.ty.sum")
#'
#' hcwt.ty <- calc_hcwt.ty(...)
#'
#' H.ty <- calc_H.ty(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, hcwt.ty = hcwt.ty)
#' }
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
#'   the Visual Basic this is referred to as the AEQScaler.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param hcwt.ty
#' @param T.ty
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return A data frame of the AEQ stratum harvest rate parameter estimates
#'   grouped by fishery stratum and year.
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
#'   the Visual Basic this is referred to as the AEQScaler.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param T.ty
#' @param N.ty
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return  A data frame of the AEQ stratum harvest rate parameter estimates
#'   grouped by fishery stratum and year.
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
#' @return A data frame of the AEQ fishery harvest rate parameter estimates
#'   grouped by year.
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
#' This function produces the same output as \code{\link{calc_H.y}}.
#'
#' @param c.ty.sum
#' @param r.ty.sum
#' @param T.ty
#' @param N.y Output from \code{\link{calc_N.y}}.
#' @inheritParams calc_H.ty
#' @inheritParams calc_N.ty
#'
#' @return A data frame of the AEQ fishery harvest rate parameter estimates
#'   grouped by year.
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
#' @description This is equivalent to equation 4 in the draft SPFI document.
#'
#' @param T.ty Output from \code{\link{calc_T.ty}}.
#' @param hcwt.ty Output from \code{\link{calc_hcwt.ty}}.
#'
#' @return A data frame of the abundance estimates, grouped by fishery stratum
#'   and year.
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
#' @description This is equivalent to equation 5 in the draft SPFI document.
#'
#' @param N.ty Output from \code{\link{calc_N.ty}}.
#'
#' @return A data frame of the abundance estimates, grouped by year.
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
#' @param H.ty Output from \code{\link{calc_H.ty}} or \code{\link{calc_H.ty2}} or \code{\link{calc_H.ty3}}.
#'
#' @return A data frame of the stata specific harvest rate indices, grouped by year.
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
#' @param H.y Output from \code{\link{calc_H.y}} or \code{\link{calc_H.y2}}.
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
#'   "nbc", or "seak".
#' @param hrj.df A data frame. This is a long format table from the HRJ list.
#' @param data.catch Output from \code{\link{readCatchData}}.
#' @param data.stock Output from \code{\link{readStockData}}.
#' @param fishery.subset A vector of the fishery numbers. If left as NULL then
#'   the default fisheries are the same as used in the VB. These are: SEAk
#'   (1:6), NBC (1:8), WCVI (1:12)
#' @param stock.subset A vector of the stock numbers. Can be left as NULL and
#'   the function will grab from the stocfile.stf.
#' @param apc A Boolean. Run the APC method, which includes calling
#'   \code{link{calc_APC}}. Default is FALSE.
#'
#' @description After reading in the catch, stock, and HRJ data, the user can
#'   run this function alone (with appropriate arguments) to obtain the SPFI
#'   estimates.
#'
#' @return A list comprising nine elements. Each element is a data frame
#'   comprising the results of the intermediate (and final) calculations. The
#'   element names are similar to the function name for the calculation. For
#'   example the distribution parameters are calculated by
#'   \code{\link{calc_d.tsa}} and found in the list element named \code{d.tsa}.
#'   The nine elements are named: \code{d.tsa, hcwt.ty, T.ty, N.ty, N.y, H.ty,
#'   H.y, S.ty, S.y}. The SPFI estimates can be found in the element named
#'   \code{S.y}.
#' @export
#'
#' @examples
#' \dontrun{
#' data.type <- "AEQTot" # "AEQCat" # or "AEQTot"
#' region <- "seak" # "wcvi" # "nbc" #  "seak"
#' #only one aabm in a data folder:
#' data.catch <- readCatchData("wcvi7914.cat", strLocation = region)
#' data.stock <- readStockData( "STOCFILE.STF") load("hrj_from_mdb.RData")
#' #data.hrj is available from the load() above:
#' hrj.df <- data.hrj$hrj.list.long$HRJ_BY
#' #all data sets include data prior to 1979.
#' #These values are excluded in the VB.
#' year.range <- 1979:(data.stock$stockmeta$intLastBrood$value+1)
#' hrj.df <- hrj.df[hrj.df$return.year %in% year.range,]
#' calc_SPFI(data.type = data.type, region = region, hrj.df = hrj.df,
#' data.catch = data.catch, data.stock = data.stock)
#' }
calc_SPFI <- function(data.type =c("AEQCat", "AEQTot"), region = c("wcvi", "nbc", "seak"), hrj.df=NA, data.catch, data.stock, fishery.subset=NULL, stock.subset=NULL, apc=FALSE ){

  time.start <- Sys.time()

  if(is.null(fishery.subset)) {
    #these fishery subsets match what is defined in the VB
    fishery.df <- data.frame(aabm=c(rep('seak',6), rep('nbc',8), rep('wcvi',12)), fishery.index=c(1:6, 1:8, 1:12))
    fishery.subset <- fishery.df$fishery.index[fishery.df$aabm==region]
  }
  if(is.null(stock.subset)) stock.subset <- unique(data.stock$SPFIFlag.long$Stock.Number)

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

  if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)

  aeqcwt <- hrj.df[hrj.df$data.type==data.type
                        & hrj.df$fishery.index %in% fishery.subset
                        & hrj.df$stock.index %in% stock.subset,]

  if(region=="seak") aeqcwt <- adjustAlaska(x = aeqcwt, data.catch = data.catch)


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

  if(apc){
    #do the apc on abundance:
    N.y.list <- calc_APC(N.ty)
    N.y <- N.y.list$apc.results
    H.y <- calc_H.y2(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, T.ty = T.ty, N.y = N.y)

  }else{
    H.y <- calc_H.y(c.ty.sum = c.ty.sum, r.ty.sum = r.ty.sum, hcwt.ty = hcwt.ty, T.ty = T.ty)
  }



  S.ty <- calc_S.ty(H.ty = H.ty)

  S.y <- calc_S.y(H.y = H.y)
  cat("Completed\n")
  cat(paste(round(Sys.time()- time.start,1), "seconds"))

  return(list(d.tsa=d.tsa, hcwt.ty=hcwt.ty, T.ty=T.ty, N.ty=N.ty, N.y=N.y, H.ty=H.ty, H.y=H.y, S.ty=S.ty, S.y=S.y))

}#END calc_SPFI



#' @title (SPFI) Sum CWT catch by fishery, stock, and age.
#'
#' @param x A data frame. Typically the \code{cwtcatch} object.
#' @param newvar.name A string of length one. This is the new name for the
#'   column of summed data. The default is "value.sum". It's recommended not to
#'   change the default unless using this function for purposes independent of
#'   the SPFI estimation.
#'
#' @return A data frame of the CWT catch sum grouped by fishery, stock, and age.
#' @export
#'
#' @examples
#' \dontrun{
#'   cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' r.tsa.sum <- calc_tsa.sum(x = cwtcatch, newvar.name = "r.tsa.sum")
#' }
calc_tsa.sum <- function(x, newvar.name ='value.sum'){
  tsa.sum <- aggregate(value~stock.index+fishery.index+age, data= x, sum , na.rm=TRUE)
  colnames(tsa.sum)[colnames(tsa.sum)=="value"] <- newvar.name
  return(tsa.sum)
}#END calc.r.tsa.sum



#' @title (SPFI) Sum CWT catch by fishery, and year.
#'
#' @param x A data frame. Typically the \code{cwtcatch} object.
#' @param newvar.name A string of length one. This is the new name for the
#'   column of summed data. The default is "value.sum". It's recommended not to
#'   change the default unless using this function for purposes independent of
#'   the SPFI estimation.
#'
#' @return A data frame of the CWT catch sum by fishery, and year.
#' @export
#'
#' @examples
#' \dontrun{
#'   cwtcatch <- hrj.df[hrj.df$data.type=="NomCat" & hrj.df$fishery.index %in% fishery.subset & hrj.df$stock.index %in% stock.subset,]
#' if(region=="seak") cwtcatch <- adjustAlaska(x = cwtcatch, data.catch = data.catch)
#' r.ty.sum <- calc_ty.sum(x = cwtcatch, newvar.name = "r.ty.sum")
#' }
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
#'   \code{\link{readCatchData}}, such as: \code{data.catch$data.catch}.
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



#' @title (SPFI) Read the CTC catch data files (*.cat).
#'
#' @param filename A string of length one, defining the name of the catch file.
#'   If more than one file name is included, only the first is used.
#' @param strLocation A string of length one, defining the AABM location
#'   ("seak", "nbc", "wcvi").
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
readCatchData <- function(filename, strLocation= c("seak", "nbc", "wcvi") ){
  strLocation <- match.arg(strLocation)

  if(!is.character(filename)) stop("'filename' must be a character string")
  filename <- filename[1]

  dat.tmp <- read.csv(filename, stringsAsFactors = FALSE, header = FALSE)
  colnames(dat.tmp) <- c("TempYear", "TempStrata", "strTempStrata", "TempCatch", "TempContribution")
  dat.tmp$strStrata <- dat.tmp$strTempStrata
  dat.tmp$CatchContribution <- dat.tmp$TempCatch - dat.tmp$TempContribution

  intFirstStrata <- min(dat.tmp$TempStrata)
  intTopStrata <-  max(dat.tmp$TempStrata)
  if(!is.na(strLocation) & tolower(strLocation) == "seak"){
    intLastStrata <-  intTopStrata + 1
  } else {
    intLastStrata <-  intTopStrata
  }

  intLastYear <- max(dat.tmp$TempYear)
  intFirstYear <- min(dat.tmp$TempYear)

  #in frmMain.vb line 769
  # If strLocation = "Alaska" Then strStrata(intLastStrata) = "FALL"
  if(!is.na(strLocation) & tolower(strLocation) == "seak") dat.tmp$strStrata[dat.tmp$TempStrata ==intLastStrata] <- "FALL"

  return(list(intFirstStrata=intFirstStrata, intTopStrata=intTopStrata, intLastStrata=intLastStrata, intFirstYear=intFirstYear, intLastYear=intLastYear, data.catch=dat.tmp) )
}#END readCatchData



#' @title (SPFI) Read CTC stock file (stocfile.stf).
#'
#' @param filename A string of length one. The default value is "stocfile.stf".
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
#' \dontrun{
#' data.stock <- readStockData('STOCFILE.STF')
#' }
readStockData <- function(filename= "stocfile.stf"){

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

#' @title (SPFI) Write csv file of summarized SPFI results.
#'
#' @param spfi.output A list. The output of \code{\link{calc_SPFI}}.
#'
#' @description This writes a csv file with columns matching those found in table 6-6 on page 103 of REPORT TCCHINOOK (09)-2 (\url{http://www.psc.org/download/35/chinook-technical-committee/2120/tcchinook09-2.pdf}).
#'
#' @return A csv file.
#' @export
#'
#' @examples
#' \dontrun{
#' write_table6.6(spfi.output)
#' }
write_table6.6 <- function(spfi.output, data.catch){
  strata.subset <- unique(spfi.output$N.ty$fishery.index)
  hcwt.ty.wide <-  reshape(spfi.output$S.ty[spfi.output$S.ty$fishery.index %in% strata.subset, c('return.year', 'fishery.index', "hcwt.ty")], direction = 'wide', idvar = "return.year", timevar = 'fishery.index')

  #c.ty.sum.wide <-  reshape(spfi.output$S.ty[spfi.output$S.ty$fishery.index %in% strata.subset, c('return.year', 'fishery.index', "c.ty.sum")], direction = 'wide', idvar = "return.year", timevar = 'fishery.index')

  #r.ty.sum.wide <-  reshape(spfi.output$S.ty[spfi.output$S.ty$fishery.index %in% strata.subset, c('return.year', 'fishery.index', "r.ty.sum")], direction = 'wide', idvar = "return.year", timevar = 'fishery.index')

  tmpcatch <- data.frame(return.year=data.catch$data.catch$TempYear, fishery.index=data.catch$data.catch$TempStrata, AEQcatch=data.catch$data.catch$CatchContribution)

  aeqcatch <- reshape(tmpcatch, dir='wide', idvar='return.year', timevar = "fishery.index")

  results <- merge(hcwt.ty.wide, aeqcatch, 'return.year')

  N.ty.wide <-  reshape(spfi.output$N.ty[spfi.output$N.ty$fishery.index %in% strata.subset, c('return.year', 'fishery.index', "N.ty")], direction = 'wide', idvar = "return.year", timevar = 'fishery.index')

  results <- merge(results, N.ty.wide, by='return.year')

  results <- merge(results, spfi.output$N.y[, c('return.year', "APCscalar")], by='return.year')

  results <- merge(results, spfi.output$H.y[, c('return.year', "H.y")], by='return.year')

  results <- merge(results, spfi.output$S.y[, c('return.year', "S.y")], by='return.year')

  write.csv(x = results, file = "table6-6.csv", row.names = FALSE)
  cat("Results written to file: table6-6.csv, but likely not complete.")

}#END write_table6.6

