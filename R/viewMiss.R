#' @title View a simple histogram of missing data
#' @description Returns a histogram with a bar for each column in the dataframe. The height is the percent missing.
#' @param df The name of the dataframe, not in quotes, for which the columns need to be converted to character class.
#' @return Plot
#' @export
viewMiss <- function(df){
  MissingVals <- colMeans(is.na(df)) %>% as.data.frame() #Calculates the % missing for each column
  colnames(MissingVals) <- "PctMissing"
  MissingVals$VarName <- rownames(MissingVals)
  ggplot(MissingVals, aes(x=reorder(VarName, -PctMissing), y=PctMissing)) + geom_bar(stat="identity") + #Bar chart with frequencies of missing values for each column
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
}
