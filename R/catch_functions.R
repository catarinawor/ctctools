#' @title Import catch csv file made by Jonathan Carey.
#'
#' @param filename A character. The name of the csv file for import.
#' @param startingyear An integer. The default is 1, which means the series will start with the ealiest year.
#' @param finalyear An integer. The default is 9999, which means the series will end with the most recent year.
#' @param calibration.name.new A character. The default name is "new".
#' @param calibration.name.old  A character. The default name is "old".
#' @param commonstocks A logical. The default is FALSE.
#' @param fishery.names.exclude A character vector of fishery names to be excluded from the data.
#'
#' @return A list of three data frames. The data frame named \code{data.combined} comprises the fishery specific catches, reshaped to long format. The data frame named \code{total} is the recalculated totals after exclusion of named fisheries. The data frame \code{data.original} is the unmodified initial import.
#' @export
#'
#' @examples
importCatch.jon <- function(filename, startingyear=1, finalyear=9999, calibration.name.new="new", calibration.name.old="old", commonstocks=FALSE, fishery.names.exclude=NA){

    data.original <- read.csv(filename, stringsAsFactors = FALSE)
    data.catch <- data.original

    #remove totals as they will be recalculated:
    data.catch <- data.catch[!grepl(pattern = "Total", data.catch$Fishery),]

    #remove select fisheries:
    if(!any(is.na(fishery.names.exclude))) data.catch <- data.catch[!tolower(data.catch$Fishery) %in% tolower(fishery.names.exclude),]

    data.catch$stock <- data.catch$Fishery
    data.catch$data.type <- "catch"
    data.catch$agegroup <- "totalabundance"
    colnames(data.catch)[colnames(data.catch)=="Year"] <- "return.year"

    filter.val <- (is.na(data.catch$Observed_New) | data.catch$Observed_New>=10) & (is.na(data.catch$Observed_Old) | data.catch$Observed_Old>=10) & (is.na(data.catch$Model_New) | data.catch$Model_New>=10) & (is.na(data.catch$Model_Old) | data.catch$Model_Old>=10)

    data.catch <- data.catch[filter.val,]
    data.catch <- data.catch[data.catch$return.year>=startingyear & data.catch$return.year<=finalyear,]

    #exclude any fisheries that are absent from old nor new model:
    if(commonstocks){
      data.catch <- data.catch[complete.cases(data.catch[,c("Observed_Old", "Model_Old", "Observed_New", "Model_New")]),]
      #added this to deal with situation that some observed data are revised during time from old to new:
      # data.catch$obs.diff <- data.catch$Observed_Old - data.catch$Observed_New
      # fisheries.exclude <- unique(data.catch$Fishery[data.catch$obs.diff !=0])
      # data.catch <- data.catch[! data.catch$Fishery %in% fisheries.exclude,]

      }#if(commonstocks){

    data.old <- data.catch[,c("Fishery", "return.year", "stock", "data.type", "agegroup", "Observed_Old", "Model_Old")]
    data.old$calibration <- calibration.name.old
    colnames(data.old)[colnames(data.old)=="Observed_Old"] <- 'value.fcs'
    colnames(data.old)[colnames(data.old)=="Model_Old"] <- 'value.ccc'

    data.new <- data.catch[,c("Fishery", "return.year", "stock", "data.type", "agegroup", "Observed_New", "Model_New")]
    data.new$calibration <- calibration.name.new
    colnames(data.new)[colnames(data.new)=="Observed_New"] <- 'value.fcs'
    colnames(data.new)[colnames(data.new)=="Model_New"] <- 'value.ccc'

    data.combined <- rbind(data.old, data.new)
    data.combined$agemetagroup <- 1
    data.combined$year <- data.combined$return.year

    #the pre-terminal totals:
    total.ccc <- aggregate(value.ccc~return.year+data.type+agegroup+agemetagroup+calibration, data=data.combined, sum)
    total.fcs <- aggregate(value.fcs~return.year+calibration, data=data.combined, sum)
    total <- merge(total.ccc, total.fcs, by=c("return.year", "calibration"))
    total$stock <- total$Fishery <- "Total Pre-Terminal Catch"
    total$year <- total$return.year

    return(list(data.combined=data.combined, total=total, data.original=data.original))

}#END importCatch.jon
