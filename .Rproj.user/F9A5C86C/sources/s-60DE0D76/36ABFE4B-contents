greedy_ensemble <- function(X, kk){
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
    Z <- cbind(Y[ ,ll], target)
    obj <- psych::cor.wt(Z, w=wts)
    cw[ll] <- obj$r[1,2]
  }

  # INITIALIZE ENSEMBLE WITH METHOD THAT HAS THE HIGHEST CORRELATION WITH TARGET
  ens <- Y[ ,which.max(cw)]
  methods <- which.max(cw)
  ens_corr <- max(cw)
  cws <- order(cw)
  # Remove the last one as it is already in the ensemble
  cws <- cws[-length(cws)]

  # TEST IF ENSEMBLE CAN BE IMPROVED BY INCREASING THE CORRELATION WITH THE TARGET VECTOR BY INCLUDING THE NEXT METHOD FROM THE SORTED LIST
  for(jj in 1:length(cws)){
    ens_pr <- cbind(ens, Y[ ,cws[jj]])
    score <- apply(ens_pr, 1, mean)
    Z <- cbind(score, target)
    obj <- psych::cor.wt(Z, w=wts)
    cor_val <- obj$r[1,2]
    if(cor_val > ens_corr){
      ens <- cbind(ens, Y[ ,cws[jj]])
      methods <- c(methods, cws[jj])
    }
  }
  ens <- as.data.frame(ens)

  # OUTPUT
  out <- list()
  out$ens_score <- apply(ens, 1, mean)
  out$methods <- methods
  out$chosen <- Y[ ,methods]
  return(out)
}
