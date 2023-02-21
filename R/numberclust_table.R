                      numberclust_table <- function(catchdata,max_clusternumber=ifelse(nrow(catchdata)<= 15,(nrow(catchdata)-1),15),style="basic", distance="jaccard", method="average") {
  # calculate distance matrix
  distmat <- vegdist(catchdata, method = distance)
  # perform clustering
  hc1_average <- hclust(distmat,method = method)
  # best number of clusters - silhouette
  silhouette <- fviz_nbclust(catchdata, hcut, method = "silhouette" ,k.max=max_clusternumber,linecolor = "#00AAAA") + theme_bw() + ggtitle("Average silhouettes")
  silhouette_df <- silhouette$data
  silhouette_df$clusters <- as.numeric(silhouette_df$clusters)
  silhouette_best <- as.numeric(which.max(silhouette_df$y))
  silhouette_index <- round(max(silhouette_df$y),3)
  # best number of clusters - mantel test
  #Mantel test
  # Function to compute a binary distance matrix from groups
  grpdist <- function(X)
  {
    gr <- as.data.frame(as.factor(X))
    distgr <- daisy(gr, "gower")
    distgr
  }