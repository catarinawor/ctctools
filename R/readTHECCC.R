
#' Read the model output CCC raw file ("THE" CCC file)
#'
#' @param filename
#' @param df.rows
#' @param agegroups.n
#'
#' @description This is a draft function. Variable naming isn't useful. But data
#'   structure is easily manipulated in R.
#'
#' @return A list comprising as many lists as data sets in the CCC file.
#' @export
#'
#' @examples
readTHECCC <- function(filename, df.rows=25, agegroups.n=4){

  dat.tmp <- readLines(filename)
  dat.rows <- length(dat.tmp)
  defining.row <- seq(1, dat.rows, by=agegroups.n*df.rows+1)
  tail(defining.row)
  table.row.start <- defining.row+1
  table.row.end <- table.row.start+df.rows*agegroups.n-1


  rowindex.df <- data.frame(defining.row=defining.row, table.row.start=table.row.start, table.row.end=table.row.end)

  dat.list <- apply(rowindex.df,1, FUN = function(x){

    first.row <- unlist(strsplit(trimws(dat.tmp[x["defining.row"]]), split = "  *"))
    #this allows each value to be converted based on its unique trait:
    first.row <- lapply(first.row, type.convert)
    groupvar.1 <- first.row[[1]]
    groupvar.2 <- first.row[[2]]
    var.list <- lapply(c(3,4,11,12),FUN=function(start.ind){
      unlist(first.row[seq(start.ind, by=2, length.out = 4)])
    })

    tc <- textConnection(dat.tmp[x["table.row.start"]:x["table.row.end"]])
    data.df<-read.table(tc, fill=TRUE)
    close(tc)

  # browser()
   return(list(groupvar.1=groupvar.1, groupvar.2=groupvar.2, var1=var.list[[1]], var2=var.list[[2]], var3=var.list[[3]], var4=var.list[[4]], data.df=data.df))

  })#END apply

  return(dat.list)

}#END readTHECCC

