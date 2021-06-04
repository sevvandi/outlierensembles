#' Computes an ensemble score using the greedy algorithm proposed by Schubert et al (2012)
#'
#' This function computes an ensemble score using the greedy algorithm in the paper titled Evaluation of Outlier Rankings and Outlier Scores by Schubert et al (2012) <doi:10.1137/1.9781611972825.90>. The greedy ensemble is detailed in  Section 4.3.
#'
#' @param X The input data containing the outlier scores in a dataframe, matrix or tibble format. Rows contain observations and columns contain outlier detection methods.
#' @param kk The number of estimated outliers.
#'
#' @return A list with the components:
#' \item{\code{scores}}{The ensemble scores.}
#' \item{\code{methods}}{The methods that are chosen for the ensemble. }
#' \item{\code{chosen}}{The chosen subset of original anomaly scores.}
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
#' ens <- greedy_ensemble(Y, kk=5)
#' ens$scores
#'
#' @export greedy_ensemble
#'
greedy_ensemble <- function(X, kk=5){
  # CODED FROM On Evaluation of Outlier Rankings and Outlier Scores
  # BY Erich Schubert, Remigius Wojdanowski, Arthur Zimek and Hans-Peter Kriegel
  # SECTION 4.3 GREEDY ENSEMBLE

  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  dd <- dim(X)[2]
  nn <- dim(X)[1]
  # CONSTRUCT THE TARGET VECTOR
  tgt <- c()
  for(i in 1:dd){
    xx <- X[ ,i]
    inds <- order(xx, decreasing = TRUE)[1:kk]
    tgt <- c(tgt, inds)
  }
  tgt <- unique(tgt)
  target <- rep(0, nn)
  target[tgt] <- 1

  # COMPUTE WEIGHTS FOR CORRELATION
  wts <- rep(1/(2*(nn - kk)), nn)
  wts[target==1] <- 1/(2*kk)
  cw <- rep(0, dd)
  for(ll in 1:dd){
    Z <- cbind(X[ ,ll], target)
    obj <- psych::cor.wt(Z, w=wts)
    cw[ll] <- obj$r[1,2]
  }

  # INITIALIZE ENSEMBLE WITH METHOD THAT HAS THE HIGHEST CORRELATION WITH TARGET
  ens <- X[ ,which.max(cw)]
  methods <- which.max(cw)
  ens_corr <- max(cw)
  cws <- order(cw)
  # Remove the last one as it is already in the ensemble
  cws <- cws[-length(cws)]

  # TEST IF ENSEMBLE CAN BE IMPROVED BY INCREASING THE CORRELATION WITH THE TARGET VECTOR BY INCLUDING THE NEXT METHOD FROM THE SORTED LIST
  for(jj in 1:length(cws)){
    ens_pr <- cbind(ens, X[ ,cws[jj]])
    score <- apply(ens_pr, 1, mean)
    Z <- cbind(score, target)
    obj <- psych::cor.wt(Z, w=wts)
    cor_val <- obj$r[1,2]
    if(cor_val > ens_corr){
      ens <- cbind(ens, X[ ,cws[jj]])
      methods <- c(methods, cws[jj])
    }
  }
  ens <- as.data.frame(ens)

  # OUTPUT
  out <- list()
  out$scores <- apply(ens, 1, mean)
  out$methods <- methods
  out$chosen <- X[ ,methods]
  return(out)
}
