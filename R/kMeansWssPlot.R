#' @title Returns an elbow plot of within group SS for a sequential number of K-means clusters
#' @description Useful for eyeballing the elbow point for determining the optimal number of clusters.
#'
#' The columns used in the dataframe must already be numeric values.
#' @param data The name of the dataframe whose columns will be clustered.
#' @param nc The number of clusters to try. Defaults either to 40 or the number of rows in the dataframe.
#' @return Elbow plot.
#' @export
kMeansWssPlot <- function(data, nc=40){
  wss <- (nrow(data)-1)*sum(apply(data, 2, function(x) var(x, na.rm = T)))
  if(nc > nrow(data)){ # If the dataset has fewer rows than the number of clusters, then it uses the number of rows
    nc <- nrow(data)
  }
  for (i in 1:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
