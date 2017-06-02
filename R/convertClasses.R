#' @title Convert classes of columns from one dataset to classes of another
#' @description Useful when trying to bind rows of a dataset that has the same column names of another dataset, but different classes.
#' @param baseDf The dataframe with classes that will be used as the standard.
#' @param dfToConvert The dataframe with classes that will be converted to the same classes as the base.
#' @return Dataframe.
#' @export
convertClasses <- function(baseDf, dfToConvert){
  for(u in 1:length(colnames(baseDf))){
    colName <- colnames(baseDf)[u]
    df <- baseDf[sample(1:nrow(baseDf), 100),u]
    colnames(df) <- "temp"
    colClass <- class(df$temp)
    if(colName %in% colnames(dfToConvert) == F){
      next()
    }
    if(colClass %in% c("numeric", "integer")){
      dfToConvert[,colName] <- as.numeric(dfToConvert[,colName])
    } else if(colClass %in% c("character")){
      dfToConvert[,colName] <- as.character(dfToConvert[,colName])
    }
    cat(u, "\n")
  }
}
