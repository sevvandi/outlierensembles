% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irt_ensemble.R
\name{irt_ensemble}
\alias{irt_ensemble}
\title{Computes an ensemble score using Item Response Theory}
\usage{
irt_ensemble(X)
}
\arguments{
\item{X}{The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.}
}
\value{
A list with the components:
\item{\code{scores}}{The ensemble scores.}
\item{\code{model}}{The IRT model. }
}
\description{
This function computes an ensemble score using Item Response Theory (IRT). This was proposed as an ensemble method for anomaly/outlier detection in  Kandanaarachchi (2021) <doi:10.13140/RG.2.2.18355.96801>.
}
\details{
For outlier detection, higher ensemble scores indicate higher levels of anomalousness. This ensemble uses IRT's latent trait to uncover  the hidden ground truth, which is used as the ensemble score. It uses the R packages airt and EstCRM to fit the IRT models. It can also be used for other ensembling tasks.
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
ens <- irt_ensemble(Y)
ens$scores
}

}
