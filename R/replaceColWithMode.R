#' @title Replace missing values of a column with the modal value
#' @description This is useful for data that has missing values and for which it makes sense to replace missing values with the modal value.
#'
#' Works with numeric or categorical columns
#'
#' Especially useful when used with dplyr::group_by so that you can replace missing values with medians from the same strata.
#' @param df The name of the dataframe for which the columns will replace missing value with modal values
#' @export
replaceColWithMode <- function(df){
  a <- table(df) %>% as.data.frame() %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character()
  df[is.na(df)] <- a
}
