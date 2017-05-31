#' @title Remove columns with standard deviation of zero
#' @description Useful for analyses that assume variation in every column of a dataframe.
#' @param dataframe The name of the dataframe
#' @return Dataframe.
#' @export
remove0sd <- function(dataframe){
  dataframe[,which(apply(dataframe, 2, function(x) length(unique(x)))>1)]
}
