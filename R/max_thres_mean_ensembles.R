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

average_ensemble <- function(X){
  # normalize data to [0, 1]
  maxs <- apply(X, 2, max)
  mins <- apply(X, 2, min)
  divs <- maxs - mins
  X <- as.data.frame(X)
  X <- sweep(X, 2, mins)
  X <- sweep(X, 2, divs, "/")

  Y <- apply(X, 2, mean)
  return(Y)
}
