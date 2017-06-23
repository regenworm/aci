bootstrapgfci <- function(data_proportion=0.9999,
                          n=11,
                          N=500,
                          data,
                          verbose=FALSE,
                          repeat_bootstrap=10) {

  listOfModels <- list()
  for (i in 1:repeat_bootstrap) {
    m <- bootstrapgfci.loop(data_proportion=data_proportion,
                        n=n,
                        N=N,
                        data=data,
                        bootstrap_iter = i,
                        verbose=verbose)
    listOfModels[[i]] <- m
  }

  # Reduce all models:
  cadd <- function(x) Reduce("+", x, accumulate = FALSE)
  L <- list()
  L$causalMatrix <- cadd(lapply(listOfModels, function(x) x$causalMatrix))
  # L$solving_time <- cadd(lapply(listOfModels, function(x) x$solving_time))
  L$test_time <- cadd(lapply(listOfModels, function(x) x$test_time))

  # Average
  L$causalMatrix <- L$causalMatrix/repeat_bootstrap
  
  return(L)
}


# one bootstrap iteration
bootstrapgfci.loop <- function(data_proportion=0.9999,
                          n=11,
                          N=500, # number of datapoints
                          data,
                          bootstrap_iter, # current bootstrap iteration
                          verbose=FALSE) {

  # Pick randomly data_proportion (e.g. half) of the samples.
  indices <- as.integer(runif(N*data_proportion)*N*data_proportion)
  halfD <- data[indices, ]


  test_time <- tic()
  L <- gfci(df=halfD,verbose=verbose)
  test_time <- toc(test_time)
  
  L$causalMatrix <- gfci2causal(n=dim(data)[1],results = L,vnames=colnames(data))
  L$test_time <- test_time
  
  return(L)
}