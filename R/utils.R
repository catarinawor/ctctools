
.add_axes <- function() {
  #this should be a hidden function.

  l <- lattice::trellis.currentLayout()
  pan <- which(l[nrow(l), ]==0)
  if(length(pan) > 0) {
    g <- grid::grid.ls(print=FALSE)
    # use an existing panel as a template for ticks
    ticks <- grid::grid.get(g$name[grep("ticks.bottom.panel", g$name)][[1]])
    # use an existing panel as a template for labels
    labels <- grid::grid.get(g$name[grep("ticklabels.bottom.panel", g$name)][[1]])
    ax <- grid::grobTree(ticks, labels)
    invisible(sapply(pan, function(x) {
      lattice::trellis.focus("panel", x, nrow(l)-1, clip.off=TRUE)
      grid::grid.draw(ax)
      lattice::trellis.unfocus()
    }))
  }
}#END .add_axes


.expand_args <- function(...){
  dots <- list(...)
  max_length <- max(sapply(dots, length))
  lapply(dots, rep, length.out = max_length)
}#END .expand_args


.getBrokeninterval <- function(x){
  result <- c(1,diff(x))
  df <- data.frame(x,result)
  df$res2 <- df$result>1
  df$res2[1] <- TRUE
  df$res3 <- cumsum(df$res2)
  df
  df2 <- aggregate(x~res3, data=df, FUN = function(x2){

    range.temp <- range(x2)
    range.str <- ifelse(diff(range.temp)==0,range.temp[1],paste(range.temp,collapse = "-"))
    return(range.str)
  }
  )
  return(paste(df2$x,collapse = ","))
}#END .getBrokeninterval


.makeDir <- function(path){
  if(!dir.exists(path)) dir.create(path)
}#END .makeDir


.rbind.named.fill <- function(x) {
  #x is a list of data frames
  nam <- sapply(x, names)
  unam <- unique(unlist(nam))

  out <- lapply(x,function(x, unam ) {
    missing.cols <- unam[! unam %in% names(x)]
    if(length(missing.cols)>0){
      df.tmp2 <- eval(parse(text= paste0("data.frame(", paste0(missing.cols, " = NA", collapse = ","), ")" )))
      return(data.frame(x, df.tmp2))
    }else{
      return(x)
    }
  }, unam )

  return(do.call('rbind', out))
}#END .rbind.named.fill


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



.simpleCap <- function(x,split=" ") {
  s <- strsplit(x, split)[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
