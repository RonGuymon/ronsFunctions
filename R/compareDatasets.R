#' @title Compare columns of multiple datasets NOT WORKING YET
#' @description Perhaps a more concise summary of a dataframe relative to base::summary(df).
#'
#' It includes information about the modal value for character and factor classes. It also identifies columns that could be a primary key
#' @param targetDf The name of the dataframe, in quotes, that you want to summarize.
#' @param otherDfs A vector of strings containing the names of other dataframes to compare.
#' @return Dataframe.
#' @export
compareDatasets <- function(targetDf, otherDfs){
  df <- data.frame(paste0(targetDf, "ColNames") = colnames(targetDf)
                   )
  for(i in 1:ncol(targetDf)){
    targetColName <- colnames(targetDf)[u]
    targetColValues <- unique(targetDf[,u])

    for(j in 1:length(otherDfs)){
      otherDfName <- otherDfs[j]
      for(k in 1:otherDfs[j]){
        otherColName
      }
    }
  }
}
