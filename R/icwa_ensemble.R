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
