
####### SETUP #######
rm(list=ls())
###### COMMENTS ########


#### LIBRARIES ####


#### DATA ####

aabm <- "seak"

#these fishery subsets match what is defined in the VB
fishery.df <- data.frame(aabm=c(rep("seak",6), rep("nbc",1), rep("wcvi",3)), fishery.index=c(1:6, 8, 10:12), baseperiodfishery.name=c(rep("ALASKA_T",6), rep("NORTH_T",1), rep("WCVI_T",3)), stringsAsFactors = FALSE)
fishery.def <- merge(fishery.def, fishery.df, by="fishery.index", all.x = TRUE)

#read in spfi file:
datapath <- "C:\Users\folkesm\Documents\Projects\chinook\spfi\data\20180920_2016estimate_stockfilenew\output"
filepath <- list.files(path = datapath, pattern = "\.rds", ignore.case = TRUE, full.names = TRUE)
spfi.output <- readRDS(filepath[5])
dat.spfi.long <- spfi.output$S.ty
View(dat.spfi.long[order(dat.spfi.long$fishery.index, dat.spfi.long$return.year), ])

#this adds zero data for missing years
#wcvi
if(aabm=="wcvi"){
dat.spfi.long.append <- expand.grid(fishery.index=10:12, return.year=c(1996, 1998))
dat.spfi.long <- plyr::rbind.fill(dat.spfi.long, dat.spfi.long.append)
}
dat.spfi.long$S.ty[is.na(dat.spfi.long$S.ty)] <- 0

spfi.output$S.ty <- dat.spfi.long
saveRDS(object = spfi.output, filepath[5])

# or use spfi from AK spreadsheet:
# dat.spfi <- read.csv("../data/AKFP_CLB16b_spfi.csv" )
# dat.spfi <- dat.spfi[,c("YEAR", colnames(dat.spfi[,4:ncol(dat.spfi)]))]
# dat.spfi.long <- reshape(dat.spfi, dir="long", varying = list(2:ncol(dat.spfi)), v.names="S.ty", timevar = "fishery.index")
# colnames(dat.spfi.long)[colnames(dat.spfi.long)=="YEAR"] <- "return.year"
# dat.spfi.long <- subset(dat.spfi.long, select = -id)
# dat.spfi.long$source <- "AKFP_CLB16b.xls"


#read in STK file:
stk.filepath <- list.files(pattern = "\.stk", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
fisheryname.filepath <- list.files(pattern = "FisheryName", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
baseperiodfishery.names <- readLines(fisheryname.filepath)
dat.stk <- read_stkfile(filename = stk.filepath[1], baseperiodfishery.names)

baseperiodfishery.name <- unique(fishery.def$baseperiodfishery.name[fishery.def$aabm %in% aabm])
dat.er.long.sub <- dat.stk$dat.er.long[dat.stk$dat.er.long$baseperiodfishery.name == baseperiodfishery.name,]

# As the ER and mdl data are being merged, the common column named "value" needs
# to be renamed:
colnames(dat.er.long.sub)[colnames(dat.er.long.sub)=="value"] <- "bper"



#read in MDL files
mdl.filepath <- list.files(path="BPC_PII_V1.22_25Sep/56F-adj/56F-adj" , pattern = "MDL$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
dat.mdl <- read_mdl(mdl.filepath)

#View(dat.mdl$dat.mdl.long[order(dat.mdl$dat.mdl.long$stock.short, dat.mdl$dat.mdl.long$age, dat.mdl$dat.mdl.long$fishery.index),])

fisheries.needed <- fishery.def[fishery.def$aabm %in% aabm,]
dat.mdl.long.sub <- dat.mdl$dat.mdl.long[dat.mdl$dat.mdl.long$fishery.name %in% fisheries.needed$fishery.name,]

#### FP CALCULATION ####

dat.fp <- calc_fp(dat.er.long = dat.er.long.sub, dat.mdl.long = dat.mdl.long.sub, dat.spfi = dat.spfi.long, allowMissingStocks = TRUE)
filename <- paste("dat.fp", aabm, "ctctools.rds", sep=".")
saveRDS(object = dat.fp, filename)

stocks <- read_BSE("2017BPC_PII_V1.22.BSE")
stocks <- data.frame(stocks$stocks.df$stock.acronym, 1:nrow(stocks$stocks.df))

filename <- paste0(aabm, format(Sys.Date(), "%Y%m%d"), ".fpa")

write_fpa(fpdat = dat.fp$dat.fp, spfi = spfi.output$S.y[,c("return.year", "S.y"),],  stocks = stocks, filename=filename, comment = aabm)



####### VALIDATION ########

fp.xls <- read.csv("../data/fp.xls.csv", stringsAsFactors = FALSE)

dat.fp <- merge(dat.fp, fp.xls, by=c("stock.short", "age", "return.year"))

#plot_fpseries(dat.fp = dat.fp, savepng = TRUE, filename = "fp_validate_ssa_mfSPFI.png")
plot_fpseries(dat.fp = dat.fp, savepng = TRUE)

