# rewrite of RSeQC functions, optimized for R





# parsers for tools


#' Title
#'
#' @param allLogs
#'
#' @return A value
#' @export
#'
#' @examples # An example
parseRSEQCdistro <- function(allLogs){
  logsIn <- lapply(allLogs, function(arg){(strsplit(readLines(arg),"|\t",fixed = TRUE))})
  names(logsIn) <- basename(allLogs)
  groups <- c("CDS_Exons","5'UTR_Exons","3'UTR_Exons","Introns","TSS_up_1kb","TSS_up_5kb","TSS_up_10kb","TES_down_1kb","TES_down_5kb","TES_down_10kb")
  sampleValues <- sapply(9:18,function(arg){strsplit(gsub("^ *|(?<= ) | *$", "", logsIn[[1]][[arg]], perl=T)," ",fixed=TRUE)[[1]][[4]]})

  res <- data.frame(matrix(NA,length(allLogs),length(groups)))
  names(res) <- groups
  row.names(res) <-  basename(allLogs)
  for(i in 1:length(allLogs)){
    res[i,] <-sapply(9:18,function(arg){strsplit(gsub("^ *|(?<= ) | *$", "", logsIn[[i]][[arg]], perl=T)," ",fixed=TRUE)[[1]][[4]]})
  }
  return(res)
}




#' Title
#'
#' @param folder
#'
#' @return A value
#' @export
#'
#' @examples # An example
parseStarLogs <- function(folder){
  allLogs <- list.files(path = folder, pattern = ".Log.final.out$",full.names = TRUE)
  logsIn <- lapply(allLogs, function(arg){(strsplit(readLines(arg),"|\t",fixed = TRUE))})
  names(logsIn) <- list.files(path = folder, pattern = ".Log.final.out$",full.names = FALSE)

  rawReads <- sapply(logsIn,function(arg){as.numeric(arg[[6]][2])})
  uniquemappedReads <- sapply(logsIn,function(arg){as.numeric(arg[[9]][2])})
  multimappedReads <- sapply(logsIn,function(arg){as.numeric(arg[[24]][2])})
  overallMappedReads <- uniquemappedReads + multimappedReads
  percentUnique <- uniquemappedReads/rawReads * 100
  percentMulti <- multimappedReads/rawReads * 100
  percentOverall <- (uniquemappedReads+multimappedReads)/rawReads * 100

  df_reads <- data.frame(rawReads=rawReads,uniquemappedReads=uniquemappedReads,multimappedReads=multimappedReads,
                         overallMappedReads=overallMappedReads,percentUnique=percentUnique,
                         percentMulti=percentMulti,percentOverall=percentOverall)
  names(df_reads) <- c("# raw reads","# uniquely mapped reads","# multimapped reads","# overall mapped reads","percent uniquely mapping",
                       "percent multimapping","percent overall mapped")
  return(df_reads)


}



## stats after running featureCounts

#' Title
#'
#' @param fcoutput
#'
#' @return
#' @export
#'
#' @examples
statsFC <- function(fcoutput){
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  fc_stats <- fcoutput$stat

  fcdf <- as.data.frame(t(fc_stats))
  colnames(fcdf) <- fc_stats[,1]
  fcdf <- fcdf[-1,]

  # make a "sample" column from the rownames of the object.
  # remove the ".star.Aligned.out.sam" from the pattern
  fcdf %>%
    mutate(sample=gsub("_Aligned.sortedByName.bam", "", rownames(.))) %>%
    # Reorder the columns
    select(sample, Assigned:Unassigned_NoFeatures) %>% # the other columns are already always 0s
    # Make it tidy
    gather(Assignment, ReadCounts, -sample) -> fctidy

  fctidy$sample <- gsub("X_tempSortedByName..","",fctidy$sample)

  fctidy$ReadCounts <- as.numeric(fctidy$ReadCounts)/1e6 # to scale to millions of reads

  p <- ggplot(fctidy,aes(Assignment, ReadCounts)) + geom_bar(stat="identity", aes(fill=sample), position="dodge") +
    ylab("read counts (millions)") + xlab("assigned read status")
  print(p)

  return(fctidy)


}
