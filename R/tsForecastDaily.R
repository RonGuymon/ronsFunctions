#' @title Daily Time Series Forecast
#' @description Uses recursive partitioning and auto arima to make daily forecasts.
#'
#' Requires the data to already be summarized into daily amounts
#' @param df The unquoted name of the dataframe that you want to summarize.
#' @param dateColumn The quoted name of the column that contains the daily dates.
#' @param valueColumn The quoted name of the column that has the daily values to be forecasted.
#' @param covs Optional. A dataframe of covariates. This dataframe should include at least two columns: (1) A date column with the number of rows equal to the number of periods to forecast into the future, and (2) a column with values for each day.
#' @param algo Quoted name of algorithm to use. Defaults to rpart, which is fast. The other option is randomForest.
#' @param lossFunction Defaults to "mape" (mean absolute percentage error). Other option is "mae" (mean absolute error).
#' @param period The number of periods forecasted into the future.
#' @param seasonalPeriods The other periods, in addition to the period parameter, that may be influential
#' @param K The number of fourier terms. Must be one lesss than the number of periods
#' @param returnMePlot Return the model evaluation plot?
#' @param returnYoyPlot Return the year-over-year plot?
#' @return List the contains a dataframe with the test, training, and forecasted data (dataFor), a dataframe with only the forecasted data (dataForOnly), variable importance plot if randomForest is selected (viPlot), loss (either mape or mae), model evaluation plot (modelEvalPlot), and year over year dataframe including the forecast (yoySales).
#' @export
#'
tsForecastDaily <- function(df, dateColumn, valueColumn, covs = NULL, algo = "rpart", lossFunction = "mape", period = 28, seasonalPeriods = c(7, 364), K = 2, returnMePlot = F, returnYoyPlot = F){
  # covdf should be a dataframe that has at least two columns: date, and value of covariate that includes only future covariate observations
  outputList <- list()

  # Reorder and rename the columns
  if(!is.null(covs)){
    # If there is a covariance matrix, include its column names
    covMatrixCols <- colnames(covs) %>% .[which(!. %in% "begDay")]
    df <- df[,c(dateColumn, valueColumn, covMatrixCols)]
    colnames(df) <- c("begDay", "TotalSales", covMatrixCols)
  }else{
    df <- df[,c(dateColumn, valueColumn)]
    colnames(df) <- c("begDay", "TotalSales")
  }

  # Make sure that there are no missing dates
  daties <- data.frame(begDay = seq.Date(from = min(df$begDay), to = max(df$begDay), by = "day"))
  tsAll <- full_join(df, daties, by = "begDay") %>%
    dplyr::arrange(begDay)
  tsAll$TotalSales[is.na(tsAll$TotalSales)] <- 0
  tsAll %<>% mutate(
    # Indicators based on date
    dow = lubridate::wday(begDay)
    , dom = lubridate::mday(begDay)
    , moy = lubridate::month(begDay)
    , christmas = ifelse(dom == 25 & moy == 12, 1, 0)
    , july4 = ifelse(dom == 4 & moy == 7, 1, 0)
    , newYears = ifelse(dom == 1 & moy == 1, 1, 0)
    # For yoy plot
    , bdy = lubridate::year(begDay) %>% as.character()
    , begDaySy = as.character(begDay)
    , begDaySy = ymd(paste(as.character(lubridate::year(Sys.Date())), substr(begDaySy, 6, 7), substr(begDaySy, 9, 10), sep = "-"))
  )

  # Create a matrix of covariates
  if(!is.null(covs)){
    # Prepare the covs df to be used by setting the same date name, putting it in the first column, and adding in the holidays
    covs <- covs[,c(dateColumn, setdiff(colnames(covs), dateColumn))]
    colnames(covs)[1] <- "begDay"
    covs %<>% mutate(
      # Indicators based on date
      dow = lubridate::wday(begDay)
      , dom = lubridate::mday(begDay)
      , moy = lubridate::month(begDay)
      , christmas = ifelse(dom == 25 & moy == 12, 1, 0)
      , july4 = ifelse(dom == 4 & moy == 7, 1, 0)
      , newYears = ifelse(dom == 1 & moy == 1, 1, 0)
      # For yoy plot
      , bdy = lubridate::year(begDay) %>% as.character()
      , begDaySy = as.character(begDay)
      , begDaySy = ymd(paste(as.character(lubridate::year(Sys.Date())), substr(begDaySy, 6, 7), substr(begDaySy, 9, 10), sep = "-"))
    ) %>%
      dplyr::select(-dom, -bdy, -begDaySy)

    # Prepare the historical covariate information for training
    xReg <- tsAll[,colnames(covs)] %>%
      bind_rows(., covs) %>% # Bind the future dates
      dplyr::select(-begDay)
    down <- model.matrix(~ as.factor(xReg$dow)) %>% .[,-1] # Dummify day of week into six columns
    colnames(down) <- c("Mon", "Tue", "Wed", "Thr", "Fri", "Sat") # Rename
    xReg <- cbind(xReg, down) # Add to covariate matrix
    moyn <- model.matrix(~ as.factor(xReg$moy)) %>% .[,-1] # Dummify month of year into 11 columns
    colnames(moyn) <- c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    xReg <- cbind(xReg, moyn) %>% # Add to covariate matrix
      dplyr::select(-dow, -moy)
    xReg %<>% as.matrix(nrow = nrow(xReg), ncol = ncol(xReg))
  }


  # Key row numbers
  rowTrainStart <- period + 1 # We need the first rows for lag features
  rowTrainEnd <- nrow(tsAll) - period # We need the last rows for the test
  rowTestStart <- rowTrainEnd + 1
  rowTestEnd <- nrow(tsAll)

  # STL (seasonal, trend, left over) decomposition----
  # Idea is to extract the seasonal components so that they can be forecasted
  dataTs <- stats::ts(tsAll$TotalSales[1:rowTrainEnd], freq = period) # Create a time series object
  decompTs <- stats::stl(dataTs, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.
  if(!is.null(covs)){
    xRegTrain <- xReg[1:rowTrainEnd,]
    # findLinearCombos(xRegTrain)
  }

  # For future
  dataTsf<- stats::ts(tsAll$TotalSales, freq = period) # Create a time series object
  decompTsf <- stats::stl(dataTsf, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.
  if(!is.null(covs)){
    xRegf <- xReg[1:rowTestEnd,]
  }
  # Get double or triple season fourier terms----
  seasonalPeriods <- c(seasonalPeriods, period) %>% .[order(.)]
  dataMsts <- forecast::msts(tsAll$TotalSales[1:rowTrainEnd], seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
  # K is the number of fourier terms. More is better, but takes exponentially longer.
  fuur <- fourier(dataMsts, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns

  # # For the future
  dataMstsf <- forecast::msts(tsAll$TotalSales, seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
  # K is the number of fourier terms. More is better, but takes exponentially longer.
  fuurf <- forecast::fourier(dataMstsf, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns

  # Forecast the trend part of the time series----
  trendPart <- stats::ts(decompTs[,2]) # The second column is the trend column.
  if(is.null(covs)){
    trendFit <- auto.arima(trendPart)
    trendFor <- forecast(trendFit, h = period)$mean # Forecasts the trend out the number of days as the period
  }else{
    trendFit <- auto.arima(trendPart, xreg = xRegTrain) # Fits arima model to the trend and also uses covariates.
    trendFor <- forecast(trendFit, xreg = xReg[rowTestStart:rowTestEnd,])$mean # Forecasts the trend using covariates.
  }




  # For the future
  trendPartf <- stats::ts(decompTsf[,2]) # The second column is the trend column.
  if(is.null(covs)){
    trendFitf <- forecast::auto.arima(trendPartf) # Fits arima model to the trend.
    trendForf <- forecast::forecast(trendFitf, period)$mean # Forecasts the trend out the number of days as the period
  }else{
    trendFitf <- forecast::auto.arima(trendPartf, xreg = xRegf) # Fits arima model to the trend.
    trendForf <- forecast::forecast(trendFitf, xreg = xReg[(rowTestEnd+1):nrow(xReg),])$mean # Forecasts the trend out the number of days as the period
  }


  # Make final feature and construct the training matrix----
  newTotalSales <- rowSums(decompTs[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
  lagSeas <- decompTs[1:(rowTrainEnd - period), 1] # Seasonal part of time series as lag feature.
  if(is.null(covs)){
    matrixTrain <- data.frame(TotalSales = newTotalSales[rowTrainStart:rowTrainEnd] # Gets the last window*period rows of the newTotalSales
                              , fuur[rowTrainStart:rowTrainEnd,] # Gets the last rows of the fourier transformations to be ivs
                              , Lag = lagSeas) # Gets the lagged seasonal part of the time series as an iv
  }else{
    matrixTrain <- data.frame(TotalSales = newTotalSales[rowTrainStart:rowTrainEnd] # Gets the last window*period rows of the newTotalSales
                              , fuur[rowTrainStart:rowTrainEnd,] # Gets the last rows of the fourier transformations to be ivs
                              , Lag = lagSeas # Gets the lagged seasonal part of the time series as an iv
                              , xReg[rowTrainStart:rowTrainEnd,]
    )
  }

  # For the future
  newTotalSalesf <- rowSums(decompTsf[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
  lagSeasf <- decompTsf[1:rowTrainEnd, 1] # Seasonal part of time series as lag feature.
  if(is.null(covs)){
    matrixTrainf <- data.frame(TotalSales = newTotalSalesf[rowTrainStart:rowTestEnd]
                               , fuurf[rowTrainStart:rowTestEnd,] # Gets the last rows of the fourier transformations to be ivs
                               , Lag = lagSeasf) # Gets the lagged seasonal part of the time series as an iv
  }else{
    matrixTrainf <- data.frame(TotalSales = newTotalSalesf[rowTrainStart:rowTestEnd]
                               , fuurf[rowTrainStart:rowTestEnd,] # Gets the last rows of the fourier transformations to be ivs
                               , Lag = lagSeasf # Gets the lagged seasonal part of the time series as an iv
                               , xReg[rowTrainStart:rowTestEnd,]
    )
  }

  # Function to measure accuracy: mape = mean absolue percentage error, mae = mean absolute error----
  if(lossFunction == "mape"){
    lossF <- function(real, pred){
      real[real == 0] <- 1 # Replace 0s with ones or else one zero sets it to infinity
      return(100* mean(abs((real - pred)/real)))
    }
  }else if(lossFunction == "mae"){
    lossF <- function(real, pred){
      return(mean(abs(real - pred)))
    }
  }

  # Fit the model----
  if(algo == "rapart"){
    fit <- rpart::rpart(TotalSales ~ ., data = matrixTrain
                        , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
                                                  , maxdepth = 30 # Max depth of the tree. Max is 30.
                                                  , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
                        )
    )

    # For the future
    fitf <- rpart::rpart(TotalSales ~ ., data = matrixTrainf
                         , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
                                                   , maxdepth = 30 # Max depth of the tree. Max is 30.
                                                   , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
                         )
    )
  }else if(algo == "randomForest"){
    fit <- randomForest::randomForest(TotalSales ~ ., data = matrixTrain
                                      , ntree = 1000
                                      , mtry = 8 # Number of variables sampled in each split
                                      , nodesize = 3 # Minimum size for end node. Larger values means fewer trees are grown. Default is 5 for regression
                                      , importance = T
    )

    impd <- caret::varImp(fit) %>%
      dplyr::mutate(
        varName = row.names(.)
      ) %>%
      dplyr::arrange(Overall)
    if(nrow(impd) > 40){
      impd <- impd[1:40,]
    }
    clustFit <- kmeans(impd$Overall, centers = floor(nrow(impd)/3), nstart = 20)
    impd$cluster <- clustFit$cluster %>% as.factor()
    impd$varName <- factor(impd$varName, levels = impd$varName)
    outputList$viPlot <- ggplot(impd, aes(x = varName, y = Overall, fill = cluster)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Variable", y = "Overall Importance", title = "Variable Importance of Random Forest Model") +
      guides(fill = F) +
      theme_minimal()



    # For the future
    fitf <- randomForest::randomForest(TotalSales ~ ., data = matrixTrainf
                                       , ntree = 2000
                                       , mtry = 8 # Number of variables sampled in each split
                                       , nodesize = 3 # Minimum size for end node. Larger values means fewer trees are grown. Default is 5 for regression
                                       , importance = T
    )
  }

  # Create forecasts----
  # For the test period to evaluate of the model
  testLag <- decompTs[(rowTrainEnd - period + 1):rowTrainEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
  fuurTest <- forecast::fourier(dataMsts
                                , K = rep(K, times = length(seasonalPeriods))
                                , h = period # The number of periods out to forecast
  )
  if(is.null(covs)){
    matrixTest <- data.frame(fuurTest
                             , Lag = testLag)
  }else{
    matrixTest <- data.frame(fuurTest
                             , Lag = testLag
                             , xReg[(rowTrainEnd - period + 1):rowTrainEnd,]
    )
  }
  forecastedData <- stats::predict(fit, matrixTest) + trendFor # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA


  # For the future, the part we really want
  testLagf <- decompTsf[rowTestStart:rowTestEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
  fuurTestf <- forecast::fourier(dataMstsf
                                 , K = rep(K, times = length(seasonalPeriods))
                                 , h = period # The number of periods out to forecast
  )
  if(is.null(covs)){
    matrixTestf <- data.frame(fuurTestf
                              , Lag = testLagf)
  }else{
    matrixTestf <- data.frame(fuurTestf
                              , Lag = testLagf
                              , xReg[rowTestStart:rowTestEnd,]
    )
  }
  forecastedDataf <- stats::predict(fitf, matrixTestf) + trendForf # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA

  # Plot the results and compare it with real values----
  # Training, test, and forecast data
  dataFor <- data.frame(TotalSales = c(tsAll$TotalSales
                                       , forecastedData
                                       , forecastedDataf)
                        , begDay = c(tsAll$begDay
                                     , tsAll$begDay[rowTestStart:rowTestEnd]
                                     , tsAll$begDay[rowTestStart:rowTestEnd] + days(period))
                        , type = c(rep("Train Data", rowTrainEnd)
                                   , rep("Test Data", rowTestEnd - rowTestStart + 1)
                                   , rep("Forecast", rowTestEnd - rowTestStart + period + 1)
                        )
  )
  dataFor$TotalSales <- as.numeric(dataFor$TotalSales) %>% round(0)

  algoLoss <- lossF(tsAll$TotalSales[rowTestStart:rowTestEnd], forecastedData) %>% round(2)
  outputList$loss <- algoLoss
  if(lossFunction == "mape"){
    plotSubtitle <- paste0("Mean absolute percentage error for ", toupper(algo), " = ", algoLoss, "%.")
  }else if(lossFunction == "mae"){
    plotSubtitle <- paste0("Mean absolute error for ", toupper(algo), " = ", algoLoss, ".")
  }

  plotTitle <- paste0("Forecast from ", toupper(algo))
  plotCaption <- ""
  if(!is.null(covs)){
    plotTitle <- paste0(plotTitle, " with Covariates")
    plotCaption <- paste0("Covariates: ", paste(setdiff(colnames(covs), "begDay"), collapse = ", "))
  }


  if(returnMePlot == T){
    modelEvalPlot <- ggplot2::ggplot(dataFor, aes(x = begDay, y = TotalSales, color = type)) +
      geom_line(alpha = 0.75) +
      ggforce::facet_zoom(x = begDay %in% c(tsAll$begDay[rowTestStart:rowTestEnd], tsAll$begDay[rowTestStart:rowTestEnd] + days(period)), zoom.size = 1.2) +
      labs(x = dateColumn, y = valueColumn, title = plotTitle, subtitle = plotSubtitle, caption = plotCaption)

    outputList$modelEvalPlot <- modelEvalPlot
  }

  if(returnYoyPlot == T){
    yoySales <- dataFor %>%
      dplyr::arrange(begDay, type) %>%
      dplyr::mutate(
        keep = ifelse(begDay == lead(begDay), 0, 1)
        , Year = lubridate::year(begDay)
        , Year = ifelse(type == "Forecast", paste(Year, type, sep = " "), Year) %>% factor()
        , begDayYoy = as.character(begDay)
        , begDayYoy = paste(lubridate::year(Sys.Date()), substr(begDayYoy, 6,7), substr(begDayYoy, 9, 10), sep = "-") %>% ymd()
      ) %>%
      dplyr::filter(keep == 1) %>%
      dplyr::select(-keep)

    outputList$yoySales <- yoySales
  }

  # Data to return----
  colnames(dataFor) <- c(valueColumn, dateColumn, "type")
  outputList$dataFor <- dataFor
  # Only the forecast data
  dataForOnly <- data.frame(begDay = tsAll$begDay[rowTestStart:rowTestEnd] + days(period)
                            , TotalSales = forecastedDataf
                            , stringsAsFactors = F)
  dataForOnly$TotalSales <- as.numeric(dataForOnly$TotalSales) %>% round(0)
  colnames(dataForOnly) <- c(dateColumn, valueColumn)
  outputList$dataForOnly <- dataForOnly

  return(outputList)
}

# tsForecastDaily <- function(df, dateColumn, valueColumn, period = 28, seasonalPeriods = c(7, 364), K = 2, returnMePlot = F, returnYoyPlot = F){
#   outputList <- list()
#   # Reorder and rename the columns
#   df <- df[,c(dateColumn, valueColumn)]
#   colnames(df) <- c("begDay", "TotalSales")
#
#   # Make sure that there are no missing dates
#   daties <- data.frame(begDay = seq.Date(from = min(df$begDay), to = max(df$begDay), by = "day"))
#   tsAll <- full_join(df, daties, by = "begDay") %>%
#     dplyr::arrange(begDay)
#   tsAll[is.na(tsAll)] <- 0
#   tsAll %<>% mutate(  # For yoy plot
#     bdy = lubridate::year(begDay) %>% as.character()
#     , begDaySy = as.character(begDay)
#     , begDaySy = ymd(paste(as.character(lubridate::year(Sys.Date())), substr(begDaySy, 6, 7), substr(begDaySy, 9, 10), sep = "-"))
#   )
#
#   # Key row numbers
#   rowTrainStart <- period + 1 # We need the first rows for lag features
#   rowTrainEnd <- nrow(tsAll) - period # We need the last rows for the test
#   rowTestStart <- rowTrainEnd + 1
#   rowTestEnd <- nrow(tsAll)
#
#   # STL (seasonal, trend, left over) decomposition----
#   # Idea is to extract the seasonal components so that they can be forecasted
#   dataTs <- stats::ts(tsAll$TotalSales[1:rowTrainEnd], freq = period) # Create a time series object
#   decompTs <- stats::stl(dataTs, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.
#
#   # For future
#   dataTsf<- stats::ts(tsAll$TotalSales, freq = period) # Create a time series object
#   decompTsf <- stats::stl(dataTsf, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.
#
#   # Get double or triple season fourier terms----
#   seasonalPeriods <- c(seasonalPeriods, period) %>% .[order(.)]
#   dataMsts <- forecast::msts(tsAll$TotalSales[1:rowTrainEnd], seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
#   # K is the number of fourier terms. More is better, but takes exponentially longer.
#   fuur <- fourier(dataMsts, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns
#
#   # # For the future
#   dataMstsf <- forecast::msts(tsAll$TotalSales, seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
#   # K is the number of fourier terms. More is better, but takes exponentially longer.
#   fuurf <- forecast::fourier(dataMstsf, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns
#
#   # Forecast the trend part of the time series----
#   trendPart <- stats::ts(decompTs[,2]) # The second column is the trend column.
#   trendFit <- auto.arima(trendPart) # Fits arima model to the trend.
#   trendFor <- forecast(trendFit, period)$mean # Forecasts the trend out the number of days as the period
#
#   # For the future
#   trendPartf <- stats::ts(decompTsf[,2]) # The second column is the trend column.
#   trendFitf <- forecast::auto.arima(trendPartf) # Fits arima model to the trend.
#   trendForf <- forecast::forecast(trendFitf, period)$mean # Forecasts the trend out the number of days as the period
#
#
#   # Make final feature and construct the training matrix----
#   newTotalSales <- rowSums(decompTs[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
#   lagSeas <- decompTs[1:(rowTrainEnd - period), 1] # Seasonal part of time series as lag feature.
#   matrixTrain <- data.frame(TotalSales = newTotalSales[rowTrainStart:rowTrainEnd] # Gets the last window*period rows of the newTotalSales
#                             , fuur[rowTrainStart:rowTrainEnd,] # Gets the last rows of the fourier transformations to be ivs
#                             , Lag = lagSeas) # Gets the lagged seasonal part of the time series as an iv
#
#   # For the future
#   newTotalSalesf <- rowSums(decompTsf[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
#   lagSeasf <- decompTsf[1:rowTrainEnd, 1] # Seasonal part of time series as lag feature.
#   matrixTrainf <- data.frame(TotalSales = newTotalSalesf[rowTrainStart:rowTestEnd]
#                              , fuurf[rowTrainStart:rowTestEnd,] # Gets the last rows of the fourier transformations to be ivs
#                              , Lag = lagSeasf) # Gets the lagged seasonal part of the time series as an iv
#
#   # Function to measure accuracy: mape = mean absolue percentage error----
#   mape <- function(real, pred){
#     real[real == 0] <- 1 # Replace 0s with ones or else one zero sets it to infinity
#     return(100* mean(abs((real - pred)/real)))
#   }
#
#   # Fit the model----
#   fit_rpart <- rpart::rpart(TotalSales ~ ., data = matrixTrain
#                             , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
#                                                       , maxdepth = 30 # Max depth of the tree. Max is 30.
#                                                       , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
#                             )
#   )
#
#   # For the future
#   fit_rpartf <- rpart::rpart(TotalSales ~ ., data = matrixTrainf
#                              , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
#                                                        , maxdepth = 30 # Max depth of the tree. Max is 30.
#                                                        , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
#                              )
#   )
#
#   # Forecast the test data----
#   testLag <- decompTs[(rowTrainEnd - period + 1):rowTrainEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
#   fuurTest <- forecast::fourier(dataMsts
#                                 , K = rep(K, times = length(seasonalPeriods))
#                                 , h = period # The number of periods out to forecast
#   )
#   matrixTest <- data.frame(fuurTest
#                            , Lag = testLag)
#   forRpart <- stats::predict(fit_rpart, matrixTest) + trendFor # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA
#
#   # For the future
#   testLagf <- decompTsf[rowTestStart:rowTestEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
#   fuurTestf <- forecast::fourier(dataMstsf
#                                  , K = rep(K, times = length(seasonalPeriods))
#                                  , h = period # The number of periods out to forecast
#   )
#   matrixTestf <- data.frame(fuurTestf
#                             , Lag = testLagf)
#   forRpartf <- stats::predict(fit_rpartf, matrixTestf) + trendForf # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA
#
#
#   # Plot the results and compare it with real values----
#   # Training, test, and forecast data
#   dataFor <- data.frame(TotalSales = c(tsAll$TotalSales
#                                        , forRpart
#                                        , forRpartf)
#                         , begDay = c(tsAll$begDay
#                                      , tsAll$begDay[rowTestStart:rowTestEnd]
#                                      , tsAll$begDay[rowTestStart:rowTestEnd] + days(period))
#                         , type = c(rep("Train Data", rowTrainEnd)
#                                    , rep("Test Data", rowTestEnd - rowTestStart + 1)
#                                    , rep("Forecast", rowTestEnd - rowTestStart + period + 1)
#                         )
#   )
#
#   rpartMape <- mape(tsAll$TotalSales[rowTestStart:rowTestEnd], forRpart) %>% round(2)
#   outputList$mape <- rpartMape
#
#   if(returnMePlot == T){
#     modelEvalPlot <- ggplot2::ggplot(dataFor, aes(x = begDay, y = TotalSales, color = type)) +
#       geom_line(alpha = 0.75) +
#       ggforce::facet_zoom(x = begDay %in% c(tsAll$begDay[rowTestStart:rowTestEnd], tsAll$begDay[rowTestStart:rowTestEnd] + days(period)), zoom.size = 1.2) +
#       labs(x = "Date", y = "Total Sales", title = "Forecast from RPART", subtitle = paste0("Mean absolute percentage error for RPART = ", rpartMape, "%."))
#
#     outputList$modelEvalPlot <- modelEvalPlot
#   }
#
#   if(returnYoyPlot == T){
#     yoySales <- dataFor %>%
#       dplyr::arrange(begDay, type) %>%
#       dplyr::mutate(
#         keep = ifelse(begDay == lead(begDay), 0, 1)
#         , Year = lubridate::year(begDay)
#         , Year = ifelse(type == "Forecast", paste(Year, type, sep = " "), Year) %>% factor()
#         , begDayYoy = as.character(begDay)
#         , begDayYoy = paste(lubridate::year(Sys.Date()), substr(begDayYoy, 6,7), substr(begDayYoy, 9, 10), sep = "-") %>% ymd()
#       ) %>%
#       dplyr::filter(keep == 1) %>%
#       dplyr::select(-keep)
#
#     outputList$yoySales <- yoySales
#   }
#
#   # Data to return----
#   colnames(dataFor) <- c(valueColumn, dateColumn, "type")
#   outputList$dataFor <- dataFor
#   # Only the forecast data
#   dataForOnly <- data.frame(begDay = tsAll$begDay[rowTestStart:rowTestEnd] + days(period)
#                             , TotalSales = forRpartf
#                             , stringsAsFactors = F)
#   colnames(dataForOnly) <- c(dateColumn, valueColumn)
#   outputList$dataForOnly <- dataForOnly
#
#   return(outputList)
# }
