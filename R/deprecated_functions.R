#' Deprecated functions in ctctools
#'
#' These functions still work but will be eventually be removed (defunct).
#'
#' \itemize{
#' The deprecated functions include:
#'  \item \code{importData}
#'  \item \code{mergeData}
#'  \item \code{plotCompare}
#'  \item \code{writeTable1}
#'  \item \code{writeTable3}
#'  \item \code{writeTable4}
#'  \item \code{writeTable5}
#'  \item \code{write_table6.6}

#'
#' }
#'
#' @name ctctools-deprecated
NULL




#' @export
#' @seealso \code{\link{importFCSCCC}} replaces \code{importData}.
#' @rdname ctctools-deprecated
importData <- function(data.path.vec=NA, model.list=NULL){
  .Deprecated("importFCSCCC") #include a package argument, too
  importFCSCCC(data.path.vec=NA, model.list=NULL)
}#END importData


#' @export
#' @seealso \code{\link{mergeFCSCCC}} replaces \code{mergeData}.
#' @rdname ctctools-deprecated
mergeData <- function(ccc.list, fcs.list, stocks.names='all'){
   .Deprecated("mergeFCSCCC") #include a package argument, too
  mergeFCSCCC(ccc.list, fcs.list, stocks.names='all')
}#END mergeData



#' @export
#' @seealso \code{\link{plotFCSCCC}} replaces \code{plotCompare}.
#' @rdname ctctools-deprecated
plotCompare <- function(data.combined, savepng=FALSE, results.path = ".", point.col.df, ...){
  .Deprecated("plotFCSvsCCC") #include a package argument, too
  plotFCSvsCCC(data.combined, savepng=FALSE, results.path = ".", point.col.df, ...)
}#END plotCompare


#' @export
#' @seealso \code{\link{writeCalibrationTable1}} replaces \code{writeTable1}.
#' @rdname ctctools-deprecated
writeTable1 <- function(data.combined, results.path="."){
  .Deprecated("writeCalibrationTable1") #include a package argument, too
  writeCalibrationTable1(data.combined, results.path=".")
}#END writeTable1


#' @export
#' @seealso \code{\link{writeCalibrationTable3}} replaces \code{writeTable3}.
#' @rdname ctctools-deprecated
writeTable3 <- function(data.combined, results.path="."){
  .Deprecated("writeCalibrationTable3") #include a package argument, too
  writeCalibrationTable3(data.combined, results.path=".")
}#END writeTable3



#' @export
#' @seealso \code{\link{writeCalibrationTable4}} replaces \code{writeTable4}.
#' @rdname ctctools-deprecated
writeTable4 <- function(data.combined, results.path="."){
  .Deprecated("writeCalibrationTable4") #include a package argument, too
  writeCalibrationTable4(data.combined, results.path=".")
}#END writeTable4



#' @export
#' @seealso \code{\link{writeCalibrationTable5}} replaces \code{writeTable5}.
#' @rdname ctctools-deprecated
writeTable5 <- function(data.combined, results.path="."){
  .Deprecated("writeCalibrationTable5") #include a package argument, too
  writeCalibrationTable5(data.combined, results.path=".")
}#END writeTable5


#' @export
#' @seealso \code{\link{writeSPFItable6.6}} replaces \code{write_table6.6}.
#' @rdname ctctools-deprecated
write_table6.6 <- function(spfi.output, data.catch){
  .Deprecated("writeSPFItable6.6") #include a package argument, too
  writeSPFItable6.6(spfi.output, data.catch)
}#END writeTable5


