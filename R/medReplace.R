#' @title Replace missing values of a column with the median
#' @description This is useful for data that has missing values and for which it makes sense to replace missing values with the median.
#'
#' Especially useful when used with dplyr::group_by so that you can replace missing values with medians from the same strata.
#' @param x The name of the vector for which you want to replace missing values with the median.
#' @return A number.
#' @export
medReplace=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
