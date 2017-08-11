
####### SETUP #######
rm(list=ls())
###### COMMENTS ########

# this script allows for import (text or MS Access), manipulation, and export (text or MS Access) of HRJ data.

####### DATA #######
data.type <- 'AEQCat' # 'AEQCat' # or 'AEQTot'

region <- 'wcvi' # 'wcvi' # 'nbc' #  'seak'

#only one aabm in a data folder:
#datapath <- paste('./', region, sep='/')

catch.filename <- list.files(pattern = '*.cat')
data.catch <- readCatchData(catch.filename, strLocation = region)
data.stock <- readStockData('STOCFILE.STF')

### reading 32 bit mdb files requires using 32bit R

hrj.list.wide <- readHRJAccessDatabase('HRJ_database 2016b.mdb')
hrj.list.long <- reshapeHRJtolong(hrj.list.wide, data.stock)
hrj.list <- list(hrj.list.wide=hrj.list.wide, hrj.list.long=hrj.list.long)
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


