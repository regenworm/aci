# loadSimulatedData:  generate datasets with specified parameters
sourceDir('./simulate_data/',trace=FALSE)
loadSimulatedData <- function (n=11, # number of variables to simulate
                               N=500, # number of data points
                               numInts=8,# number of datasets (interventions)
                               i=399589 #seed 
){  
  set.seed(i)
  simulationConfig <- list(n=n, topology="random", exconf="passive", N=N, pedge = 1/(n-1), numInts=numInts, restrict = 'acyclic', confounder_proportion=0.5)
  MD <- simulate_data(simulationConfig=simulationConfig, samples=NULL, model=NULL, indPath=NULL, returnData=TRUE, typeOfSimulator = "int")
  data <- MD$D[[1]]$data # A table with the data to use for GFCI
  trueGraph <- MD$M$fullC # The true graph (in matrix form) to compare with the results of GFCI
  trueGraph <- trueGraph[-(n+1),-(n+1)]
  
  N <- dim(data)[1]
  # create matrix with zeros
  intv <- matrix(0,N,numInts+1) 
  
  # name columns
  
  dnames <- c()
  inames <- c()
  for (v in 1:n) {
    dnames <- c(dnames, paste("V", v,sep=""))
  }
  for (v in 1:(numInts+1)) {
    inames <- c(inames, paste("I", (v-1),sep=""))
  }
  
  colnames(data) <- dnames
  colnames(intv) <- inames
  
  # set values for intervention variables in sets 2 through 9
  for (i in 1:numInts) 
  {
    intv[which(data[,12] == i+1),i] <- 1
  }
  
  # set values for intervention variables in sets 10 through 14
  intv[which(data[,12] > 9),1] <- 1
  for (i in 10:14)
  {
    intv[which(data[,12] == i),i-numInts] <- 1
  }
  
  obsData <-data[which(data[,12] <= 1),]
  obsData <- obsData[,-12] # remove regime
  
  data1 <- cbind(data,intv) # combine intervention variables and data
  data1 <- data1[which(data[,12] <= numInts),] # only keep number of interventions specified
  data1 <- data1[,-(n+1)] # remove regime variable
  
  
  return(list(data=data1, trueGraph=trueGraph, obsData=obsData))
}