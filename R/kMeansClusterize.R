#' @title Function to assign K-Means cluster number to each row of data
#' @description This will create a new column, called "clusterNum", with a numeric value for each row of data. It assumes that the first column has the identifying information, and that the rest of the columns are numeric.
#'
#' It automatically removes columns that have 0 variation, and abbreviates the column names.
#' @param clusterData The name of the dataframe whose columns will be clustered. The first column should be the primary key.
#' @param appendDataTo The name of the dataframe to which the "clusterNum" column will be added.
#' @param nc The number of clusters to create. Defaults to 5.
#' @param heatMap True/False indicator if you want to display a heatmap of cluster centroids. Defaults to False.
#' @param corrplot True/False indicator for displaying a correlation plot of cluster centroids. Defaults to False.
#' @return New column to a dataframe, and optional plots.
#' @export
kMeansClusterize <- function(clusterData, appendDataTo, nc = 5, heatMap = F, corrplot = F){
  # Include the whole dataframe because this removes the first column
  cluster.out <- kmeans(clusterData[,-1], centers = nc, nstart = 20)

  clusterSummary <- data.frame(cluster.out$centers)
  clusterSummary <- Filter(function(x) sd(x) != 0, clusterSummary) # Remove attributes that have no variation
  colnames(clusterSummary) <- abbreviate(colnames(clusterSummary), minlength = 6, method = c("both.sides")) # Abbreviate the name
  # Put column names in for cluster values if the cell value is greater than 0
  for(j in 1:ncol(clusterSummary)){
    for(i in 1:nrow(clusterSummary)){
      clusterSummary[i,j] <- ifelse(clusterSummary[i,j] > 0, colnames(clusterSummary[j]), "delete")
      # cat(i, j, sep = "\n")
    }
  }
  clusterSummary$des <- apply(clusterSummary, 1, paste, collapse = ' & ') # Collapse all cell values to a single column
  clusterSummary$des <- gsub("delete & | & delete", "", clusterSummary$des) # Remove delete & values
  clusterSummary$size <- cluster.out$size # Add in the size of the clusters
  clusterSummary$des <- paste(clusterSummary$des, clusterSummary$size, sep = " n=") # size to the description column
  clusterSummary$des <- paste(row.names(clusterSummary), clusterSummary$des, sep = ": ")
  clusterSummary <- dplyr::select(clusterSummary, des) # Keep only the description column
  clusterSummary$clusterNum <- row.names(clusterSummary) # Add in the row number for the cluster number
  clusterSummary$clusterNum <- as.numeric(clusterSummary$clusterNum) # Make the cluster number numeric
  appendDataTo <- cbind(appendDataTo, cluster.out$cluster) # append cluster number to the data
  appendDataTo <- left_join(appendDataTo, clusterSummary, by = c("cluster.out$cluster" = "clusterNum"))
  appendDataTo <- appendDataTo[,-(ncol(appendDataTo)-1)]
  if(corrplot == T){
    dev.new()
    M <- cor(cluster.out$centers)
    corrplot(M, method = "square", order = "hclust", addrect = nc) # Addrect draws rectangles around clusters.
  }

  if(heatMap == T){
      dev.new()
      heatmap3(as.matrix(cluster.out$centers),
               scale="column",
               col=heat.colors(256),
               cexRow = 1,
               cexCol = 1,
               margins = c(20, 5),
               main="Characteristics of Clusters")
  }
  return(appendDataTo)
  #   # dev.new()
  #   # parcoord(clusterCenters, col = c(1:nrow(clusterCenters)), var.label = T)
  #     appendDataTo <- cbind(appendDataTo, cluster.out$cluster) # append cluster number to the data
}

