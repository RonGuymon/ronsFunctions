#' @title Remove columns that exceed a specified percentage of missing values
#' @description Remove columns that exceed a specified percentage of missing values. The default is set to remove anything above 85%.
#' @param df The name of the dataframe, not in quotes, for which the columns need to be converted to character class.
#' @param pct The minimum percentage of missing data that will cause a column to be deleted.
#' @return Dataframe
#' @export
removeNAs <- function(df, pct = .85){
  df <- df[, colSums(is.na(df))/nrow(df) < pct]
}
