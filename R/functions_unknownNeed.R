.DTK.plot <- function (x = "DTK.test output", ...){
  args <- list(...)
  stop("There was a hidden call to .DTK.plot()")

  out = x[[2]]
  a = x[[1]]
  n = nrow(out)
  mai.tmp <- par('mai')
  par(mai=c(mai.tmp[1],args$left.mai, mai.tmp[3:4]))
  #par(mgp=c(15,1,0))

  plot(c(max(out[, 3]), min(out[, 2])), c(1, n), type = "n",
       xlab = "Mean Difference", ylab = "",
       yaxt = "n")
  axis(2, at = seq(1,n), labels = rownames(out)[n:1], las=1)
  title(main = paste(paste(((1 - a) * 100), "%", sep = ""),
                     "Confidence Intervals", "\n",
                     "Red intervals are significant and black non-significant"))
  for (i in 1:n) {
    i.rev <- rev(1:n)[i]
    lines(x = c(-1e+100, 1e+100), y = c(i.rev, i.rev), col = "gray",
          lty = 2)
    if (out[i, 2] > 0 || out[i, 3] < 0) {
      lines(out[i, 2:3], y = c(i.rev, i.rev), col = "red", lwd = 2)
      points(x = out[i, 1], y = i.rev, col = "red")
    }
    else {
      lines(out[i, 2:3], y = c(i.rev, i.rev))
      points(x = out[i, 1], y = i.rev)
    }
  }
}#END .DTKplot



getlinkID <- function(stockmap){
  stop("call to getlinkID()")

  tempID <- rbind(setNames(stockmap[,c("acronym.old", "linkID")], c("acronym.replace", "linkID")), stockmap[,c("acronym.replace", "linkID")])
  tempID <- tempID[complete.cases(tempID),]
  tempID <- unique(tempID)
  #colnames(tempID) <- c("acronym", "linkID")
  return(tempID)
}#END getlinkID

testDTK <- function(data, results.path){
  stop("call to testDTK()")
  #data is a data frame with same structure as metrics$metrics.long

  with(data,by(data, list(agemetagroup, agegroup, pm.type), FUN = function(x){
    x$calibration.fac <- factor(x$calibration, levels=sort(unique(x$calibration)))

    DTK.result <- DTK.test(x=x$value, f=x$calibration.fac)

    data.type <- paste0(unique(x$data.type), collapse = "+")
    filename.prefix <- paste("DTK",unique(x$agemetagroup), unique(x$agegroup), data.type, unique(x$pm.type), sep="_")

    write.csv(DTK.result[[2]], file = paste(results.path, paste0(filename.prefix,".csv"), sep="/"))

    png(filename = paste(results.path, paste0(filename.prefix,".png"), sep="/"), width = 8,height = 8, units = "in", res = 600)
    .DTK.plot(DTK.result, left.mai=3)
    dev.off()

  }))

}#END testDTK
