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
#' X <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
#' X[199, ] <- c(4, 4)
#' X[200, ] <- c(-3, 5)
#' y1 <- DDoutlier::KNN_AGG(X)
#' y2 <- DDoutlier::LOF(X)
#' y3 <- DDoutlier::COF(X)
#' y4 <- DDoutlier::INFLO(X)
#' y5 <- DDoutlier::KDEOS(X)
#' y6 <- DDoutlier::LDF(X)
#' y7 <- DDoutlier::LDOF(X)
#' Y <- cbind.data.frame(y1, y2, y3, y4, y5, y6, y7)
#' ens <- icwa_ensemble(Y)
#' ens
#'
#' @export icwa_ensemble
#'
icwa_ensemble <- function(Y){

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  # Y is normalized to 0 - 1
  cory <- cor(Y)
  apcl <- apcluster::apcluster(cory)
  # apcl@clusters
  # colnames(cory)
  numclust <- length(apcl@clusters) # gives clusters

  clusts <- c()
  for(jj in 1:numclust){
    clusts <- c(clusts, length(apcl@clusters[[jj]]))
  }
  icwa <- rep(0, dim(Y)[1])
  for(kk in 1:numclust){
    if(clusts[kk]>1){
      icwa_temp <- apply(Y[ ,apcl@clusters[[kk]]], 1, mean)
    }else{
      icwa_temp <- Y[ ,apcl@clusters[[kk]]]
    }
    icwa <- icwa + icwa_temp
  }
  icwa <- icwa/sum(1/clusts)
  return(icwa)
}
