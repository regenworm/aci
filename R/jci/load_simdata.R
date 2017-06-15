
sourceDir('./simulate_data/',trace=FALSE)
i <- 1248237
set.seed(i)
n <- 11 # number of variables to simulate
N <- 500 # number of data points
numInts <- 7 # number of datasets (interventions)
simulationConfig <- list(n=n, topology="random", exconf="passive", N=N, pedge = 1/(n-1), numInts=numInts, restrict = 'acyclic', confounder_proportion=0.5)

MD <- simulate_data(simulationConfig=simulationConfig, samples=NULL, model=NULL, indPath=NULL, returnData=TRUE, typeOfSimulator = "int")

data <- MD$D[[1]]$data # A table with the data to use for GFCI
trueGraph <- MD$M$C # The true graph (in matrix form) to compare with the results of GFCI