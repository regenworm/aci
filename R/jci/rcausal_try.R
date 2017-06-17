library(rcausal)
source('jci/gfci_graph.R')
a <- 'gfci' # either gfci or fges
data_type <- 'sachs' # either sachs or sim
addPrior <- TRUE
prior <- c()

if (data_type == 'sachs') {
  source('jci/load_sachs.R')
  
  if (addPrior) {
    forbiddenWithin <- colnames(intv)
    class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
    temporal <- list(forbiddenWithin,colnames(raw_data)[1:11]) # List of temporal node tiers
    prior <- priorKnowledge(addtemporal = temporal)
  }
} else if (data_type == 'sim') {
  source('jci/load_simdata.R')
  colnames(data) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "R1")
  
  if (addPrior) {
    forbiddenWithin <- colnames(data[12])
    class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
    temporal <- list(forbiddenWithin,colnames(data)[1:11]) # List of temporal node tiers
    prior <- priorKnowledge(addtemporal = temporal)
  }
}


if (a == 'fges') {
  results <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = TRUE, priorKnowledge = prior)
  plot(results$graphNEL, nodeAttrs=makeNodeAttrs(results$graphNEL, fontsize=20))
} else if (a == 'gfci') {
  results <- gfci(df = data, priorKnowledge = prior)
  gfci_graph(results)
}

if (data_type == 'sim') {
  source('jci/process_trueGraph.R')
  
  experimentalMatrix <- matrix(-1,11,11)
  names <- c("V1"=1, "V2"=2, "V3"=3, "V4"=4, "V5"=5, "V6"=6, "V7"=7, "V8"=8, "V9"=9, "V10"=10, "V11"=11)
  
  for (edge in results$edges ) {
    edge <- strsplit(edge, " ")
    cause <- edge[[1]][1]
    result <- edge[[1]][3]
    
    experimentalMatrix[names[cause],names[result]] = 1
  }
  
  cmp <- experimentalMatrix[experimentalMatrix == trueGraph]
  results$hits <- length(cmp[cmp>0])
  results$wrongs <-length(experimentalMatrix[experimentalMatrix > 0])-results$hits
  
  print(paste("Hits: ", results$hits))
  print(paste("Number of wrongs:", results$wrongs ))
}

