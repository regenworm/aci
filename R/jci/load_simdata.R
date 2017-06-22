sourceDir('./simulate_data/',trace=FALSE)
loadSimulatedData <- function (n=11, # number of variables to simulate
                               N=500, # number of data points
                               numInts=7,# number of datasets (interventions)
                               i=399589 #seed 
){  
  set.seed(i)
  simulationConfig <- list(n=n, topology="random", exconf="passive", N=N, pedge = 1/(n-1), numInts=numInts, restrict = 'acyclic', confounder_proportion=0.5)
  MD <- simulate_data(simulationConfig=simulationConfig, samples=NULL, model=NULL, indPath=NULL, returnData=TRUE, typeOfSimulator = "int")
  data <- MD$D[[1]]$data # A table with the data to use for GFCI
  trueGraph <- MD$M$C # The true graph (in matrix form) to compare with the results of GFCI
  
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
  
  # colnames(data) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11")
  # colnames(intv) <- c('I0','I1', 'I2', 'I3', 'I4', 'I5', 'I6', 'I7')
  
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
  
  data1 <- cbind(data,intv) # combine intervention variables and data
  data1 <- data1[which(data[,12] <= numInts),] # only keep number of interventions specified
  data1 <- data1[,-(n+1)] # remove regime variable
  
  return(list(data=data1, trueGraph=trueGraph))
}