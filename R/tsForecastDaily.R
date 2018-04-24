#' @title Daily Time Series Forecast
#' @description Uses recursive partitioning and auto arima to make daily forecasts.
#'
#' Requires the data to already be summarized into daily amounts
#' @param df The unquoted name of the dataframe that you want to summarize.
#' @param dateColumn The quoted name of the column that contains the daily dates.
#' @param valueColumn The quoted name of the column that has the daily values to be forecasted.
#' @param period The number of periods forecasted into the future.
#' @param seasonalPeriods The other periods, in addition to the period parameter, that may be influential
#' @param K The number of fourier terms. Must be one lesss than the number of periods
#' @param returnMePlot Return the model evaluation plot?
#' @param returnYoyPlot Return the year-over-year plot?
#' @return List the contains a dataframe with the test, training, and forecasted data (dataFor), a dataframe with only the forecasted data (dataForOnly), mean absolute percentage error (mape), model evaluation plot (modelEvalPlot), and year over year dataframe including the forecast (yoySales).
#' @export
tsForecastDaily <- function(df, dateColumn, valueColumn, period = 28, seasonalPeriods = c(7, 364), K = 2, returnMePlot = F, returnYoyPlot = F){
  outputList <- list()
  # Reorder and rename the columns
  df <- df[,c(dateColumn, valueColumn)]
  colnames(df) <- c("begDay", "TotalSales")

  # Make sure that there are no missing dates
  daties <- data.frame(begDay = seq.Date(from = min(df$begDay), to = max(df$begDay), by = "day"))
  tsAll <- full_join(df, daties, by = "begDay") %>%
    dplyr::arrange(begDay)
  tsAll[is.na(tsAll)] <- 0
  tsAll %<>% mutate(  # For yoy plot
    bdy = lubridate::year(begDay) %>% as.character()
    , begDaySy = as.character(begDay)
    , begDaySy = ymd(paste(as.character(lubridate::year(Sys.Date())), substr(begDaySy, 6, 7), substr(begDaySy, 9, 10), sep = "-"))
  )

  # Key row numbers
  rowTrainStart <- period + 1 # We need the first rows for lag features
  rowTrainEnd <- nrow(tsAll) - period # We need the last rows for the test
  rowTestStart <- rowTrainEnd + 1
  rowTestEnd <- nrow(tsAll)

  # STL (seasonal, trend, left over) decomposition----
  # Idea is to extract the seasonal components so that they can be forecasted
  dataTs <- stats::ts(tsAll$TotalSales[1:rowTrainEnd], freq = period) # Create a time series object
  decompTs <- stats::stl(dataTs, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.

  # For future
  dataTsf<- stats::ts(tsAll$TotalSales, freq = period) # Create a time series object
  decompTsf <- stats::stl(dataTsf, s.window = "periodic", robust = T)$time.series # Seasonal decomposition of time series by Loess. The time series portion returns a dataframe with the seasonal, trend, and remainder for each day.

  # Get double or triple season fourier terms----
  seasonalPeriods <- c(seasonalPeriods, period) %>% .[order(.)]
  dataMsts <- msts(tsAll$TotalSales[1:rowTrainEnd], seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
  # K is the number of fourier terms. More is better, but takes exponentially longer.
  fuur <- fourier(dataMsts, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns

  # # For the future
  dataMstsf <- forecast::msts(tsAll$TotalSales, seasonal.periods = seasonalPeriods) # Create multiple seasonal objects. Use one for each period that is influential. E.g., 7 if a week before is influential, 364 if a year before is influential, etc.
  # K is the number of fourier terms. More is better, but takes exponentially longer.
  fuurf <- forecast::fourier(dataMstsf, K = rep(K, times = length(seasonalPeriods))) # If K is 2, then it made two pairs of sine and cosine signals for each seasonal.period. The resulting matrix will have the same number of rows as the training matrix, and # of seasonal.periods * 2 * K columns

  # Forecast the trend part of the time series----
  trendPart <- stats::ts(decompTs[,2]) # The second column is the trend column.
  trendFit <- auto.arima(trendPart) # Fits arima model to the trend.
  trendFor <- forecast(trendFit, period)$mean # Forecasts the trend out the number of days as the period

  # For the future
  trendPartf <- stats::ts(decompTsf[,2]) # The second column is the trend column.
  trendFitf <- forecast::auto.arima(trendPartf) # Fits arima model to the trend.
  trendForf <- forecast::forecast(trendFitf, period)$mean # Forecasts the trend out the number of days as the period


  # Make final feature and construct the training matrix----
  newTotalSales <- rowSums(decompTs[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
  lagSeas <- decompTs[1:(rowTrainEnd - period), 1] # Seasonal part of time series as lag feature.
  matrixTrain <- data.frame(TotalSales = newTotalSales[rowTrainStart:rowTrainEnd] # Gets the last window*period rows of the newTotalSales
                            , fuur[rowTrainStart:rowTrainEnd,] # Gets the last rows of the fourier transformations to be ivs
                            , Lag = lagSeas) # Gets the lagged seasonal part of the time series as an iv

  # For the future
  newTotalSalesf <- rowSums(decompTsf[,c(1,3)]) # Detrended Total sales by adding up the seasonal and the remainder (leaves out the trend) for each row of the training data.
  lagSeasf <- decompTsf[1:rowTrainEnd, 1] # Seasonal part of time series as lag feature.
  matrixTrainf <- data.frame(TotalSales = newTotalSalesf[rowTrainStart:rowTestEnd]
                             , fuurf[rowTrainStart:rowTestEnd,] # Gets the last rows of the fourier transformations to be ivs
                             , Lag = lagSeasf) # Gets the lagged seasonal part of the time series as an iv

  # Function to measure accuracy: mape = mean absolue percentage error----
  mape <- function(real, pred){
    real[real == 0] <- 1 # Replace 0s with ones or else one zero sets it to infinity
    return(100* mean(abs((real - pred)/real)))
  }

  # Fit the model----
  fit_rpart <- rpart::rpart(TotalSales ~ ., data = matrixTrain
                            , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
                                                      , maxdepth = 30 # Max depth of the tree. Max is 30.
                                                      , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
                            )
  )

  # For the future
  fit_rpartf <- rpart::rpart(TotalSales ~ ., data = matrixTrainf
                             , control = rpart.control(minsplit = 2 # Minimum number of observations before a tree can be split. 2 is the min.
                                                       , maxdepth = 30 # Max depth of the tree. Max is 30.
                                                       , cp = 0.000001 # Threshhold for deciding if each branch fulfills conditions for further processing
                             )
  )

  # Forecast the test data----
  testLag <- decompTs[(rowTrainEnd - period + 1):rowTrainEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
  fuurTest <- forecast::fourier(dataMsts
                                , K = rep(K, times = length(seasonalPeriods))
                                , h = period # The number of periods out to forecast
  )
  matrixTest <- data.frame(fuurTest
                           , Lag = testLag)
  forRpart <- stats::predict(fit_rpart, matrixTest) + trendFor # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA

  # For the future
  testLagf <- decompTsf[rowTestStart:rowTestEnd, 1] # Get lagged seasonal values from the decomposed training data for the test data
  fuurTestf <- forecast::fourier(dataMstsf
                                 , K = rep(K, times = length(seasonalPeriods))
                                 , h = period # The number of periods out to forecast
  )
  matrixTestf <- data.frame(fuurTestf
                            , Lag = testLagf)
  forRpartf <- stats::predict(fit_rpartf, matrixTestf) + trendForf # Predict detrended time series part with tree2 model and then add the trend part of the time series forecasted by ARIMA


  # Plot the results and compare it with real values----
  # Training, test, and forecast data
  dataFor <- data.frame(TotalSales = c(tsAll$TotalSales
                                       , forRpart
                                       , forRpartf)
                        , begDay = c(tsAll$begDay
                                     , tsAll$begDay[rowTestStart:rowTestEnd]
                                     , tsAll$begDay[rowTestStart:rowTestEnd] + days(period))
                        , type = c(rep("Train Data", rowTrainEnd)
                                   , rep("Test Data", rowTestEnd - rowTestStart + 1)
                                   , rep("Forecast", rowTestEnd - rowTestStart + period + 1)
                        )
  )

  rpartMape <- mape(tsAll$TotalSales[rowTestStart:rowTestEnd], forRpart) %>% round(2)
  outputList$mape <- rpartMape

  if(returnMePlot == T){
    modelEvalPlot <- ggplot2::ggplot(dataFor, aes(x = begDay, y = TotalSales, color = type)) +
      geom_line(alpha = 0.75) +
      ggforce::facet_zoom(x = begDay %in% c(tsAll$begDay[rowTestStart:rowTestEnd], tsAll$begDay[rowTestStart:rowTestEnd] + days(period)), zoom.size = 1.2) +
      labs(x = "Date", y = "Total Sales", title = "Forecast from RPART", subtitle = paste0("Mean absolute percentage error for RPART = ", rpartMape, "%."))

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
                            , TotalSales = forRpartf
                            , stringsAsFactors = F)
  colnames(dataForOnly) <- c(dateColumn, valueColumn)
  outputList$dataForOnly <- dataForOnly

  return(outputList)
}
