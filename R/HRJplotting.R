
plotER01 <- function(working.data, grouping.year.type=c("brood.year", 'return.year'), results.path=".", savepng=TRUE, return.data=FALSE){
  require(lattice)

  grouping.year.type <- match.arg(grouping.year.type)

  working.data$age.index2 <- working.data$age.index
  #this will allow pooling of age indices >3
  working.data$age.index2[working.data$age.index2>3] <- 4

  working.data$age.pooled <- working.data$age
  working.data$age.pooled[working.data$age.index2>3] <- "56"
  working.data$age.pooled <- factor(working.data$age.pooled, levels = c("2", "3", "4", "5", "56"), labels = c("2", "3", "4", "5", "56") )

  working.data$age.field <- working.data$age.pooled

  working.data$grouping.year <- working.data[,which(colnames(working.data)==grouping.year.type)]

  denom <- aggregate(value~stock+grouping.year+age.field, data=working.data[working.data$data.type %in% c("total.AEQ.mortalities", "escapement"),], sum, na.rm=TRUE)
  colnames(denom)[colnames(denom)=="value"] <- "denominator"

  numerator <- aggregate(value~stock+grouping.year+age.field, data=working.data[working.data$data.type=="total.AEQ.mortalities" & tolower(working.data$fishery.type)=="p",], sum, na.rm=TRUE)
  colnames(numerator)[colnames(numerator)=="value"] <- "numerator"

  data.tmp <- merge(numerator,denom, by=c('stock', 'grouping.year', 'age.field'), all=TRUE)
  data.tmp$er <- data.tmp$numerator/data.tmp$denominator
  data.tmp$er[data.tmp$er>=1] <- NA
  df1 <- expand.grid(stock=unique(data.tmp$stock), grouping.year=min(data.tmp$grouping.year, na.rm = TRUE):max(data.tmp$grouping.year, na.rm = TRUE), age.field=unique(data.tmp$age.field))
  data.tmp <- merge(df1, data.tmp, by=c("stock", "grouping.year", "age.field"), all=TRUE)

  plot.tmp <- xyplot(er~grouping.year|stock, data=data.tmp, groups=age.field, type='l', as.table=TRUE, ylab="ER", xlab= .simpleCap(grouping.year.type, split = "\\.") ,
                     scales=list(alternating=FALSE, y=list(relation="same")),
                     auto.key=list(space="right",
                                   title="Age", cex.title=1,
                                   lines=TRUE, points=FALSE))

  if(savepng){
    stock.str <- paste(sort(unique(data.tmp$stock)), collapse = "+" )
    age.str <- paste(sort(unique(data.tmp$age.field)), collapse = "+" )
    filename <- paste("plot01", "er","by", grouping.year.type, "ages", age.str, 'stocks', stock.str, ".png", sep="_")
    .makeDir(results.path)

    png(file= paste(results.path, filename, sep="/"), hei=8.5, wid=11, units="in", res=600)
    print(plot.tmp)
    .add_axes()
    dev.off()
  }else{
    print(plot.tmp)
    .add_axes()
  }

  if(return.data) return(data.tmp)

}#END plotER01

plotER05 <- function(working.data, grouping.year.type=c("brood.year", 'return.year'), results.path=".", savepng=TRUE, return.data=FALSE){
  require(lattice)

  grouping.year.type <- match.arg(grouping.year.type)

  working.data$age.index2 <- working.data$age.index
  #this will allow pooling of age indices >3
  working.data$age.index2[working.data$age.index2>3] <- 4

  working.data$age.pooled <- working.data$age
  working.data$age.pooled[working.data$age.index2>3] <- "56"
  working.data$age.pooled <- factor(working.data$age.pooled, levels = c("2", "3", "4", "5", "56"), labels = c("2", "3", "4", "5", "56") )

  working.data$age.field <- working.data$age.pooled

  working.data$grouping.year <- working.data[,which(colnames(working.data)==grouping.year.type)]

  denom <- aggregate(value~stock+grouping.year+age.field, data=working.data[working.data$data.type %in% c("total.AEQ.mortalities", "escapement"),], sum, na.rm=TRUE)
  colnames(denom)[colnames(denom)=="value"] <- "denominator"

  numerator <- aggregate(value~stock+grouping.year+age.field+fishery.country+fishery.psc, data=working.data[working.data$data.type=="total.AEQ.mortalities" & tolower(working.data$fishery.type)=="p",], sum, na.rm=TRUE)
  colnames(numerator)[colnames(numerator)=="value"] <- "numerator"

  data.tmp <- merge(numerator,denom, by=c('stock', 'grouping.year', 'age.field'), all=TRUE)
  data.tmp$er <- data.tmp$numerator/data.tmp$denominator
  data.tmp$er[data.tmp$er>=1] <- NA
  df1 <- expand.grid(stock=unique(data.tmp$stock), grouping.year=min(data.tmp$grouping.year, na.rm = TRUE):max(data.tmp$grouping.year, na.rm = TRUE), age.field=unique(data.tmp$age.field), fishery.country= unique(data.tmp$fishery.country), fishery.psc= unique(data.tmp$fishery.psc))
  data.tmp <- merge(df1, data.tmp, by=c("stock", "grouping.year", "age.field", "fishery.country", "fishery.psc"), all=TRUE)

  levels(data.tmp$fishery.psc) <- sort(levels(data.tmp$fishery.psc))

  for(stock.val in unique(data.tmp$stock)){
    data.sub <- data.tmp[data.tmp$stock==stock.val,]

    plot.tmp <- xyplot(er~grouping.year|fishery.country+fishery.psc, data=data.sub, groups=age.field, type='l', as.table=TRUE, ylab="ER", xlab= .simpleCap(grouping.year.type, split = "\\.") ,
                       #layout(c(2,2)),
                       scales=list(alternating=FALSE, y=list(relation="same")),
                       auto.key=list(space="right",
                                     title="Age", cex.title=1,
                                     lines=TRUE, points=FALSE))

    if(savepng){
      stock.str <- paste(sort(unique(data.sub$stock)), collapse = "+" )
      age.str <- paste(sort(unique(data.sub$age.field)), collapse = "+" )
      filename <- paste("plot05_er","by", grouping.year.type, "ages", age.str, 'stocks', stock.str, ".png", sep="_")
      .makeDir(results.path)

      png(file= paste(results.path, filename, sep="/"), hei=8.5, wid=11, units="in", res=600)
      print(plot.tmp)
      .add_axes()
      dev.off()
    }else{
      print(plot.tmp)
      .add_axes()
    }#END if(savepng)
  }#END for stock.val

  if(return.data) return(data.tmp)

}#END plotER05

plotER07 <- function(working.data, grouping.year.type=c("brood.year", 'return.year'), results.path=".", savepng=TRUE, return.data=FALSE){
  require(lattice)

  grouping.year.type <- match.arg(grouping.year.type)

  working.data$age.pooled <- "1to2"
  working.data$age.pooled[working.data$age.index>=3] <- "3to6"
  working.data$age.field <- working.data$age.pooled

  working.data$grouping.year <- working.data[,which(colnames(working.data)==grouping.year.type)]

  denom <- aggregate(value~stock+grouping.year+age.field, data=working.data[working.data$data.type %in% c("total.AEQ.mortalities", "escapement"),], sum, na.rm=TRUE)
  colnames(denom)[colnames(denom)=="value"] <- "denominator"

  numerator <- aggregate(value~stock+grouping.year+age.field+fishery.region+fishery.psc, data=working.data[working.data$data.type=="total.AEQ.mortalities" & tolower(working.data$fishery.type)=="p" & tolower(working.data$fishery.region) %in% c("ak", "nbc", "sbc") & tolower(working.data$fishery.psc)=="aabm",], sum, na.rm=TRUE)
  colnames(numerator)[colnames(numerator)=="value"] <- "numerator"

  data.tmp <- merge(numerator,denom, by=c('stock', 'grouping.year', 'age.field'), all=TRUE)
  data.tmp$er <- data.tmp$numerator/data.tmp$denominator
  data.tmp$er[data.tmp$er>=1] <- NA
  df1 <- expand.grid(stock=unique(data.tmp$stock), grouping.year=min(data.tmp$grouping.year, na.rm = TRUE):max(data.tmp$grouping.year, na.rm = TRUE), age.field=unique(data.tmp$age.field), fishery.region= unique(data.tmp$fishery.region))
  data.tmp <- merge(df1, data.tmp, by=c("stock", "grouping.year", "age.field", "fishery.region"), all=TRUE)

  levels(data.tmp$fishery.region) <- sort(levels(data.tmp$fishery.region))

  for(stock.val in unique(data.tmp$stock)){
    data.sub <- data.tmp[data.tmp$stock==stock.val,]

    plot.tmp <- xyplot(er~grouping.year|age.field +fishery.region, data=data.sub, type='l', as.table=TRUE, ylab="ER", xlab= .simpleCap(grouping.year.type, split = "\\.") , col='black',
                       #layout(c(2,2)),
                       scales=list(alternating=FALSE, y=list(relation="same")),
                       auto.key=list(space="right",
                                     title="Age", cex.title=1,
                                     lines=TRUE, points=FALSE))

    if(savepng){
      stock.str <- paste(sort(unique(data.sub$stock)), collapse = "+" )
      age.str <- paste(sort(unique(data.sub$age.field)), collapse = "+" )
      filename <- paste("plot07_er","by", grouping.year.type, "ages", age.str, 'stocks', stock.str, ".png", sep="_")
      .makeDir(results.path)

      png(file= paste(results.path, filename, sep="/"), hei=8.5, wid=11, units="in", res=600)
      print(plot.tmp)
      .add_axes()
      dev.off()
    }else{
      print(plot.tmp)
      .add_axes()
    }#END if(savepng)
  }#END for stock.val

  if(return.data) return(data.tmp)

}#END plotER07

