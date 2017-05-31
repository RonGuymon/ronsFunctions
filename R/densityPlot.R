#' @title Creates a density plot of a quantitative vector
#' @description This is useful for getting a quick idea of the distribution of a column of data. It also draws a line for the median.
#' @param df The name of the dataframe, not in quotes.
#' @param column The name of the column, not in quotes, that will be graphed.
#' @param lowerx The lower bound of the x-axis to show on the chart. Default is -10.
#' @param upperx The upper bound of the x-axis to show on the chart. Default is 10.
#' @param title The title, in quotes, that you want to give to the plot. Default = "Density of Whatever You're Plotting".
#' @return Densit plot.
#' @export
densityPlot <- function(df, column, lowerx = -10,  upperx = 10, title = "Density of Whatever You're Plotting"){
  ggplot(df, aes(column)) + geom_density(alpha = 0.5) +
    geom_vline(aes(xintercept = median(column, na.rm = T), color = "Median")) +
    xlim(lowerx, upperx) +
    labs(x = "Value", y = "Density", title = title)
}
