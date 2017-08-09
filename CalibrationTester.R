
# !!!! look for USER INPUT ZONE below.!!!!


####### SETUP #######
rm(list=ls())
###### COMMENTS ########
script.name <- 'CalibrationTester'
if(!exists('script.metadata')) script.metadata <- list()

script.metadata[[script.name]] <- list(
fileName = paste0(script.name,'.R'),
author= 'Michael Folkes',
date.creation='2017-08-09 11:38:41',
date.edit= NA,
comments = NA
)# END list

##########################################################################
## USER INPUT ZONE ##

# NOTE, the pound symbol '#' is a comment line, which R ignores.

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
model.list <- buildmodel.list(commonstocks = commonstocks, stockmap.pathname = stockmap.pathname, data.path.vec = data.path.vec, stocks.key.pathname.vec = stocks.key.pathname.vec, grouping.year = grouping.year, age.structure = age.structure, totalabundance =totalabundance, data.type=data.type, results.path = results.path, stock.names = stock.names, groupingby=c( 'agegroup'), ranking = ranking)


### Import data ###
data.combined <- importFCSCCC(model.list = model.list)


##########################################################################
### Table 1 export ###
writeCalibrationTable1(data.combined, results.path = model.list$results.path)

##########################################################################
### Performance tables ###
### Table 2 export and create metrics list object ###
metrics <- calcPMs(data.combined, pm = list(PBSperformance::mpe,  PBSperformance::mape), writecsv = TRUE, samplesize.min = samplesize.min, results.path = model.list$results.path)

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


