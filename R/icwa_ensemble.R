#' Computes an ensemble score using inverse cluster weighted averaging method by Chiang et al (2017)
#'
#' This function computes an ensemble score using inverse cluster weighted averaging in the paper titled A Study on Anomaly Detection Ensembles by Chiang et al (2017) <doi:10.1016/j.jal.2016.12.002>. The ensemble is detailed in  Algorithm 2.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#'
#' @return The ensemble scores.
#'
#' @examples
#' set.seed(123)
#' if (requireNamespace("dbscan", quietly = TRUE)) {
#' X <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
#' X[199, ] <- c(4, 4)
#' X[200, ] <- c(-3, 5)
#' # Using different parameters of lof for anomaly detection
#' y1 <- dbscan::lof(X, minPts = 10)
#' y2 <- dbscan::lof(X, minPts = 20)
#' knnobj <- dbscan::kNN(X, k = 20)
#' # Using different KNN distances as anomaly scores
#' y3 <- knnobj$dist[ ,10]
#' y4 <- knnobj$dist[ ,20]
#' # Dense points are less anomalous. Hence 1 - pointdensity is used.
#' y5 <- 1 - dbscan::pointdensity(X, eps = 0.8, type = "gaussian")
#' y6 <- 1 - dbscan::pointdensity(X, eps = 0.5, type = "gaussian")
#' Y <- cbind.data.frame(y1, y2, y3, y4, y5, y6)
#' ens <- icwa_ensemble(Y)
#' ens
#' }
#'
#' @export icwa_ensemble
#'
icwa_ensemble <- function(X){

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  # Y is normalized to 0 - 1
  cory <- stats::cor(X)
  apcl <- apcluster::apcluster(cory)
  # apcl@clusters
  # colnames(cory)
  numclust <- length(apcl@clusters) # gives clusters

  clusts <- c()
  for(jj in 1:numclust){
    clusts <- c(clusts, length(apcl@clusters[[jj]]))
  }
  icwa <- rep(0, dim(X)[1])
  for(kk in 1:numclust){
    if(clusts[kk]>1){
      icwa_temp <- apply(X[ ,apcl@clusters[[kk]]], 1, mean)
    }else{
      icwa_temp <- X[ ,apcl@clusters[[kk]]]
    }
    icwa <- icwa + icwa_temp
  }
  icwa <- icwa/sum(1/clusts)
  return(icwa)
}
