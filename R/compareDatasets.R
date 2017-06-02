#' @title Compare columns of multiple datasets NOT WORKING YET
#' @description A way to evaluate which columns from one dataset match up with columns from another dataset
#' @param targetDf The name of the dataframe, in quotes, that you want to summarize.
#' @param otherDfs A vector of strings containing the names of other dataframes to compare.
#' @return Dataframe.
#' @export
compareDatasets <- function(targetDf, otherDfs){
  df <- data.frame(targetColNames = colnames(targetDf)
                   )
  for(i in 1:ncol(targetDf)){
    targetColName <- colnames(targetDf)[u]
    targetColValues <- unique(targetDf[,u])

    for(j in 1:length(otherDfs)){
      otherDfName <- otherDfs[j]
      for(k in 1:otherDfs[j]){
        # otherColName
      }
    }
  }
}
