#' @title Convert all columns to characters and convert NAs to blanks
#' @description Converts all columns of a dataframe to character class, and also converts NAs to blanks.
#'
#' I used to use this a lot, but not so much anymore.
#' @param dataframe The name of the dataframe, not in quotes, for which the columns need to be converted to character class.
#' @return Dataframe
#' @export
convColsToCharacter <- function(dataframe){
  dataframe <- sapply(dataframe, as.character) #Change all columns to character
  dataframe[is.na(dataframe)] <- "" #Convert NAs to blanks
  dataframe <- as.data.frame(dataframe, stringsAsFactors = F) #Convert back to a dataframe, which also converts everything to factors unless stringsAsFActors = F.
}
