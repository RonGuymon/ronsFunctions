#' @title Summary of a single dataset
#' @description Perhaps a more concise summary of a dataframe relative to base::summary(df).
#'
#' It includes information about the modal value for character and factor classes. It also identifies columns that could be a primary key
#' @param dataframe The name of the dataframe that you want to summarize.
#' @return Dataframe.
#' @export
datasetSummary <- function(dataframe){
for(i in 1:ncol(dataframe)){
  dataframe <- as.data.frame(dataframe)
  if(i == 1){
    dfs <- data.frame()
  }
  temp <- dataframe[,i]

  # Guess at the class
  tempdf <- as.character(temp)
  topVals <- table(tempdf) %>%
    as.data.frame(stringsAsFactors = F) %>%
    dplyr::arrange(desc(Freq)) %>%
    .[1:5,1] %>%
    paste(., collapse = "||")

  charCheck <- topVals %>%
    gsub("NA|\\|", "", .) %>%
    gsub("[^a-zA-Z]", "", .)

  numCheck <- topVals %>%
    gsub("\\|NA\\|", "", .) %>%
    gsub("[^0-9]", "", .)

  dateTimeCheck <- topVals %>%
    gsub("\\|\\|.*$", "", .) %>%
    parse_date_time(orders = c("Ymd HMS", "mdY HMS"
                               , "Ymd HM", "mdY HM"))

  dateCheck <- topVals %>%
    gsub("\\|\\|.*$", "", .) %>%
    ymd()

  if(nchar(charCheck) > 0 & is.na(dateTimeCheck) & is.na(dateCheck)){
    classGuess <- "character"
  }else if(!is.na(dateTimeCheck)){
    classGuess <- "POSIXct"
  }else if(!is.na(dateCheck)){
    classGuess <- "Date"
  }else if(nchar(numCheck) > 0 & nchar(charCheck) == 0){
    classGuess <- "numeric"
  }else{
    classGuess <- "character"
  }


  # Column summary
  if(class(temp)[1] == "character" & classGuess == "numeric"){
    temp <- as.numeric(temp)
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = "character",
      classGuess = classGuess,
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
      NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
      NAs_and_blanksPct = (temp[which(temp == "" | is.na(temp))] %>% length() / length(temp)) %>% round(2),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1,1] %>% as.character(),
      max = max(temp, na.rm = T) %>% round(2) %>% as.character(),
      mean = mean(temp, na.rm = T) %>% round(2)  %>% as.character(),
      median = median(temp, na.rm = T) %>% round(2)  %>% as.character(),
      min = min(temp, na.rm = T) %>% round(2)  %>% as.character()
      , stringsAsFactors = F
    )
  }else if(class(temp)[1] == "factor" & classGuess == "numeric"){
    temp <- as.character(temp) %>% as.numeric()
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = "factor",
      classGuess = classGuess,
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
      NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
      NAs_and_blanksPct = (temp[which(temp == "" | is.na(temp))] %>% length() / length(temp)) %>% round(2),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1,1] %>% as.character(),
      max = max(temp, na.rm = T) %>% round(2) %>% as.character(),
      mean = mean(temp, na.rm = T) %>% round(2)  %>% as.character(),
      median = median(temp, na.rm = T) %>% round(2)  %>% as.character(),
      min = min(temp, na.rm = T) %>% round(2)  %>% as.character()
      , stringsAsFactors = F
    )
  }else if(class(temp)[1] == "integer" | class(temp)[1] == "numeric"){
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = class(temp),
      classGuess = class(temp),
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
      NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
      NAs_and_blanksPct = (temp[which(temp == "" | is.na(temp))] %>% length() / length(temp)) %>% round(2),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1,1] %>% as.character(),
      max = max(temp, na.rm = T) %>% round(2) %>% as.character(),
      mean = mean(temp, na.rm = T) %>% round(2)  %>% as.character(),
      median = median(temp, na.rm = T) %>% round(2)  %>% as.character(),
      min = min(temp, na.rm = T) %>% round(2)  %>% as.character()
      , stringsAsFactors = F
    )
  }else if(class(temp)[1] == "character" & classGuess == "POSIXct"){
    temp <- parse_date_time(temp, orders = c("Ymd HMS", "mdY HMS"
                                             , "Ymd HM", "mdY HM"))
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = "character",
      classGuess = class(temp),
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
      NAs_and_blanks = temp[which(is.na(temp))] %>% length(),
      NAs_and_blanksPct = temp[which(is.na(temp))] %>% length() / length(temp),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
      max = max(temp, na.rm = T) %>% as.character(),
      mean = mean(temp, na.rm = T) %>% as.character(),
      median = median(temp, na.rm = T) %>% as.character(),
      min = min(temp, na.rm = T) %>% as.character()
      , stringsAsFactors = F
    )
  }else if(class(temp)[1] %in% c("POSIXct", "POSIXt", "Date", "Date", "POSIXlt")){
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = class(temp)[1],
      classGuess = class(temp)[1],
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
      NAs_and_blanks = temp[which(is.na(temp))] %>% length(),
      NAs_and_blanksPct = temp[which(is.na(temp))] %>% length() / length(temp),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
      max = max(temp, na.rm = T) %>% as.character(),
      mean = mean(temp, na.rm = T) %>% as.character(),
      median = median(temp, na.rm = T) %>% as.character(),
      min = min(temp, na.rm = T) %>% as.character()
      , stringsAsFactors = F
    )
  }else{
    tempForDf <- class(temp)
    if(class(temp) == "factor"){
      temp <- as.character(temp)
    }
    tdf <- data.frame(
      name = colnames(dataframe)[i],
      class = tempForDf,
      classGuess = class(tempForDf),
      uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
      uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
      top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1:5,1] %>% paste(collapse = "||"),
      uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F))/length(temp), 2),
      NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
      NAs_and_blanksPct = round(temp[which(temp == "" | is.na(temp))] %>% length() / length(temp), 4),
      mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
      max = NA %>% as.character(),
      mean = NA %>% as.character(),
      median = NA %>% as.character(),
      min = NA %>% as.character()
      , stringsAsFactors = F
    )
  }
  dfs <- bind_rows(dfs, tdf)
  cat("Done with ", i, "\n")
}

return(dfs)
}
# datasetSummary <- function(dataframe){
#   for(i in 1:ncol(dataframe)){
#     dataframe <- as.data.frame(dataframe)
#     if(i == 1){
#       dfs <- data.frame()
#     }
#     temp <- dataframe[,i]
#
#     # Guess at the class
#     topVals <- table(temp) %>%
#     charCheck <- temp %>%
#       gsub("\\|NA\\|", "", .) %>%
#       gsub("[^a-zA-Z]", "", .)
#
#     numCheck <- houstons$top5UniqueValues[i] %>%
#       gsub("\\|NA\\|", "", .) %>%
#       gsub("[^0-9]", "", .)
#
#     dateTimeCheck <- houstons$top5UniqueValues[i] %>%
#       gsub("\\|\\|.*$", "", .) %>%
#       ymd_hms()
#
#     dateCheck <- houstons$top5UniqueValues[i] %>%
#       gsub("\\|\\|.*$", "", .) %>%
#       ymd()
#     # Column summary
#     if(class(temp)[1] == "integer" | class(temp)[1] == "numeric"){
#       tdf <- data.frame(
#         name = colnames(dataframe)[i],
#         class = class(temp),
#         uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
#         uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
#         top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1:5,1] %>% paste(collapse = "||"),
#         uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
#         NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
#         NAs_and_blanksPct = (temp[which(temp == "" | is.na(temp))] %>% length() / length(temp)) %>% round(2),
#         mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1,1] %>% as.character(),
#         max = max(temp, na.rm = T) %>% round(2) %>% as.character(),
#         mean = mean(temp, na.rm = T) %>% round(2)  %>% as.character(),
#         median = median(temp, na.rm = T) %>% round(2)  %>% as.character(),
#         min = min(temp, na.rm = T) %>% round(2)  %>% as.character()
#         , stringsAsFactors = F
#       )
#     } else if(class(temp)[1] %in% c("POSIXct", "POSIXt", "Date", "Date", "POSIXlt")){
#       tdf <- data.frame(
#         name = colnames(dataframe)[i],
#         class = class(temp)[1],
#         uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
#         uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
#         top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1:5,1] %>% paste(collapse = "||"),
#         uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
#         NAs_and_blanks = temp[which(is.na(temp))] %>% length(),
#         NAs_and_blanksPct = temp[which(is.na(temp))] %>% length() / length(temp),
#         mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
#         max = max(temp, na.rm = T) %>% as.character(),
#         mean = mean(temp, na.rm = T) %>% as.character(),
#         median = median(temp, na.rm = T) %>% as.character(),
#         min = min(temp, na.rm = T) %>% as.character()
#         , stringsAsFactors = F
#       )
#     }else{
#       tempForDf <- class(temp)
#       if(class(temp) == "factor"){
#         temp <- as.character(temp)
#       }
#       tdf <- data.frame(
#         name = colnames(dataframe)[i],
#         class = tempForDf,
#         uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
#         uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
#         top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1:5,1] %>% paste(collapse = "||"),
#         uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F))/length(temp), 2),
#         NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
#         NAs_and_blanksPct = round(temp[which(temp == "" | is.na(temp))] %>% length() / length(temp), 4),
#         mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
#         max = NA %>% as.character(),
#         mean = NA %>% as.character(),
#         median = NA %>% as.character(),
#         min = NA %>% as.character()
#         , stringsAsFactors = F
#       )
#     }
#     dfs <- bind_rows(dfs, tdf)
#     cat("Done with ", i, "\n")
#   }
#   return(dfs)
# }
