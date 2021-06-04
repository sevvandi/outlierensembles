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
  modout$model$param
  obj <- EstCRM::EstCRMperson(X,modout$model$param, min.item = rep(0,dd), max.item = rep(1,dd) )

  out <- list()
  out$thetas <- obj$thetas
  out$model <- modout$model

  return(out)
}
