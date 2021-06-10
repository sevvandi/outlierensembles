#' Computes an ensemble score using Item Response Theory
#'
#' This function computes an ensemble score using Item Response Theory (IRT). This was proposed as an ensemble method for anomaly/outlier detection in  Kandanaarachchi (2021) <doi:10.13140/RG.2.2.18355.96801>.
#'
#' For outlier detection, higher ensemble scores indicate higher levels of anomalousness. This ensemble uses IRT's latent trait to uncover  the hidden ground truth, which is used as the ensemble score. It uses the R packages airt and EstCRM to fit the IRT models. It can also be used for other ensembling tasks.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#'
#' @return A list with the components:
#' \item{\code{scores}}{The ensemble scores.}
#' \item{\code{model}}{The IRT model. }
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
#' ens <- irt_ensemble(Y)
#' ens$scores
#'
#'@export irt_ensemble
irt_ensemble <- function(X){
  dd <- dim(X)[2]

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  modout <- airt::cirtmodel(X, max.item = rep(1,dd), min.item = rep(0, dd))
  obj <- EstCRM::EstCRMperson(X,modout$model$param, min.item = rep(0,dd), max.item = rep(1,dd) )

  Z <- obj$thetas[ ,1:2]
  Z[ ,2] <- Z[ ,2] - min(Z[ ,2])
  colnames(Z)[2] <- "Ensemble_Score"

  colnames(modout$model$param) <- c("alpha", "beta", "gamma")

  out <- list()
  out$scores <- Z[ ,2]
  out$model <- modout$model

  return(out)
}
