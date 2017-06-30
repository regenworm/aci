# bootstrapgfci:      run method with bootstrap and specified parameters
# bootstrapgfci.loop: Single loop iteration for bootstrapping method
bootstrapgfci <- function(data_proportion=0.9999,
                          n=18,
                          N=500,
                          data,
                          verbose=FALSE,
                          repeat_bootstrap=10,
                          prior=c(),
                          method='gfci') {

  listOfModels <- list()
  for (i in 1:repeat_bootstrap) {
    m <- bootstrapgfci.loop(data_proportion=data_proportion,
                        n=n,
                        N=N,
                        data=data,
                        bootstrap_iter = i,
                        verbose=verbose,
                        prior=prior,
                        method=method)
    listOfModels[[i]] <- m
  }

  # Reduce all models:
  cadd <- function(x) Reduce("+", x, accumulate = FALSE)
  L <- list()
  L$causalMatrix <- cadd(lapply(listOfModels, function(x) x$causalMatrix))
  L$test_time <- cadd(lapply(listOfModels, function(x) x$test_time))

  # Average
  L$causalMatrix <- L$causalMatrix/repeat_bootstrap
  
  L$nodes <- colnames(data)
  vals <- which(L$causalMatrix>0, arr.ind = TRUE) 
  v1 <- colnames(data)[vals[,1]]
  v2 <- colnames(data)[vals[,2]]
  L$edges <- paste(v2,v1,sep=" -> ")
  L$edgeDegrees <- L$causalMatrix[L$causalMatrix>0]
  return(L)
}


# one bootstrap iteration
bootstrapgfci.loop <- function(data_proportion=0.9999,
                          n=18,
                          N=500, # number of datapoints
                          data,
                          bootstrap_iter, # current bootstrap iteration
                          verbose=FALSE,
                          prior=c(),
                          method='gfci') {

  # Pick randomly data_proportion (e.g. half) of the samples.
  indices <- as.integer(runif(N*data_proportion)*N*data_proportion)
  halfD <- data[indices, ]


  test_time <- tic()
  if (method == 'gfci') {
    L <- gfci(df=halfD,verbose=verbose,priorKnowledge=prior,completeRuleSetUsed=TRUE,maxDegree = -1,penaltydiscount = 1)
  } else if (method == 'fges') {
    L <- fges(df = halfD, penaltydiscount = 2, verbose = verbose, priorKnowledge = prior, maxDegree = -1, numOfThreads = 2)
  }
  test_time <- toc(test_time)
  
  L$causalMatrix <- gfci2causal(n=n,results = L,vnames=colnames(data))
  L$test_time <- test_time
  
  return(L)
}