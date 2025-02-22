#' Computes an ensemble score using the maximum score of each observation
#'
#' This function computes an ensemble score using the maximum score for each observation as detailed in Aggarwal and Sathe (2015) <doi:10.1145/2830544.2830549>.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#'
#' @return The ensemble scores.
#'
#' @examples
#' set.seed(123)
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
#' ens <- max_ensemble(Y)
#' ens
#'
#' @export max_ensemble
max_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  Y <- apply(X, 1, max)
  return(Y)
}


#' Computes an ensemble score by aggregating values above the mean
#'
#' This function computes an ensemble score by aggregating values above the mean as detailed in Aggarwal and Sathe (2015) <doi:10.1145/2830544.2830549>.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#'
#' @return The ensemble scores.
#'
#' @examples
#' set.seed(123)
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
#' ens <- threshold_ensemble(Y)
#' ens
#'
#' @export threshold_ensemble
threshold_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  xmean <- apply(X, 2, mean)
  B <- apply(X, 1, function(x) x < xmean)
  B <- t(B)
  X[B==TRUE] <- 0
  Y <- apply(X, 1, sum)
  return(Y)
}


#' Uses the mean as the ensemble score
#'
#' This function uses the mean as the ensemble score.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#'
#' @return The ensemble scores.
#'
#' @examples
#' set.seed(123)
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
#' ens <- average_ensemble(Y)
#' ens
#'
#' @export average_ensemble
average_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  Y <- apply(X, 1, mean)
  return(Y)
}
