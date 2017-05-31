#' @title Function to mathematically calculate the optimal number of clusters
#' @description It should come close to calculating the elbow point mathematically for the optimal number of clusters using K-Means clustering.
#'
#' It essentially draws a line from the least number of clusters to the max number of clusters and then calculates the point that's the farthest away from that line.
#'
#' It assumes all columns are already numeric values.The columns used in the dataframe must already be numeric values, and they should already be standardized.
#'
#' It's heavily dependent on the max number of clusters. A larger nc will have a higher accuracy.
#' @param data The name of the dataframe whose columns will be clustered.
#' @param nc The number of clusters to try. Defaults either to 40 or the number of rows in the dataframe.
#' @return Number.
#' @export
kMeansCalcClusters <- function(data, nc = 40){
  if(nc > nrow(data)){ # If the dataset has fewer rows than the number of clusters, then it uses the number of rows
    nc <- nrow(data)
  }
  # Include the whole dataframe because this removes the first column
  wss <- data.frame(clusters = c(2:nc), withinSS = c(2:nc)) # Initialize a dataframe
  for(h in 2:nc){ # Puts the within sum of squares in the second column
    wss[h-1,2] <- sum(kmeans(data, centers = h)$withinss)
  }
  dv <- data.frame(clusters = c(3:(nc-1)), distance = c(3:(nc-1)))
  line <- wss[1,] - wss[nrow(wss),] # Line between first and last point
  for(i in 2:(nc-2)){
    distToFirst <- wss[1,] - wss[i,] # Distance between first and second point
    m <- as.matrix(rbind(line, distToFirst)) # matricize
    dv[i-1,2] <- abs(det(m))/sqrt(sum(line*line)) # Distance between point and line
  }
  nclusts <- dv$clusters[dv$distance == max(dv$distance)]
  # return(list(wss, dv, nclusts))
  return(nclusts)
}
