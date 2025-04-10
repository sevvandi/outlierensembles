% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_thres_mean_ensembles.R
\name{threshold_ensemble}
\alias{threshold_ensemble}
\title{Computes an ensemble score by aggregating values above the mean}
\usage{
threshold_ensemble(X)
}
\arguments{
\item{X}{The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.}
}
\value{
The ensemble scores.
}
\description{
This function computes an ensemble score by aggregating values above the mean as detailed in Aggarwal and Sathe (2015) <doi:10.1145/2830544.2830549>.
}
\examples{
set.seed(123)
if (requireNamespace("dbscan", quietly = TRUE)) {
X <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
X[199, ] <- c(4, 4)
X[200, ] <- c(-3, 5)
# Using different parameters of lof for anomaly detection
y1 <- dbscan::lof(X, minPts = 10)
y2 <- dbscan::lof(X, minPts = 20)
knnobj <- dbscan::kNN(X, k = 20)
# Using different KNN distances as anomaly scores
y3 <- knnobj$dist[ ,10]
y4 <- knnobj$dist[ ,20]
# Dense points are less anomalous. Hence 1 - pointdensity is used.
y5 <- 1 - dbscan::pointdensity(X, eps = 0.8, type = "gaussian")
y6 <- 1 - dbscan::pointdensity(X, eps = 0.5, type = "gaussian")
Y <- cbind.data.frame(y1, y2, y3, y4, y5, y6)
ens <- threshold_ensemble(Y)
ens
}

}
