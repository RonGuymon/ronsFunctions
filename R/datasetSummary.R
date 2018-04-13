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
    if(class(temp)[1] == "integer" | class(temp)[1] == "numeric"){
      tdf <- data.frame(
        name = colnames(dataframe)[i],
        class = class(temp),
        uniqueKey = ifelse(nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)) == length(temp), "Yes", "No"),
        uniqueValues = nrow(table(temp) %>% as.data.frame(stringsAsFactors = F)),
        top5UniqueValues = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% mutate(temp = round(as.numeric(temp), 2)) %>% .[1:5,1] %>% paste(collapse = "||"),
        uniqueValuesPct = round(nrow(table(temp) %>% as.data.frame())/length(temp), 2),
        NAs_and_blanks = temp[which(temp == "" | is.na(temp))] %>% length(),
        NAs_and_blanksPct = temp[which(temp == "" | is.na(temp))] %>% length() / length(temp),
        mode = table(temp) %>% as.data.frame(stringsAsFactors = F) %>% dplyr::arrange(-Freq) %>% .[1,1] %>% as.character(),
        max = max(temp, na.rm = T) %>% round(2) %>% as.character(),
        mean = mean(temp, na.rm = T) %>% round(2)  %>% as.character(),
        median = median(temp, na.rm = T) %>% round(2)  %>% as.character(),
        min = min(temp, na.rm = T) %>% round(2)  %>% as.character()
        , stringsAsFactors = F
      )
    } else if(class(temp)[1] %in% c("POSIXct", "POSIXt", "Date", "Date", "POSIXlt")){
      tdf <- data.frame(
        name = colnames(dataframe)[i],
        class = class(temp)[1],
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
