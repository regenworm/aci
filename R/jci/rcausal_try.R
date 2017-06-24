# source('./jci/load_simdata.R')
# source('./jci/graphviz_dot.R')
# source('./jci/process_trueGraph.R')
# sourceDir('./jci',trace=FALSE)
runGFCI <- function(method = 'gfci', # either gfci or fges
                    data_type = 'sim', # either sachs or sim
                    addPrior = TRUE, # use background knowledge
                    prior = c(), # specify background knowledge
                    n = 11, # number of variables
                    numInts = 7, # number of interventions
                    N = 500,
                    howmany = 100, # howmany simulated experiments 
                    showGraphs = TRUE,
                    bootstrap=0 # how many bootstrap iterations
){
  true_models <- c() # list that will contain all causal matrices for all simulated data.
  learnt_models <- c() # list that will contain all causal matrices for learnt models for simulated data.
  allHits <- c()
  allfPositives <- c()
  allTotals <- c()
  recall <- c()
  precision <- c()
  start  <- proc.time()

  for (i in 1:howmany){
      # get data
      if (data_type == 'sachs') {
          
          if (howmany>1) {
              stop("Doesn't make sense to run Sachs experiments more than once.")
          }
          
          data <- loadSachsData()
          
          if (addPrior) {
              intv <- colnames(data[(n+1):ncol(data)])
              forbiddenWithin <- intv
              class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
              temporal <- list(forbiddenWithin,colnames(data)[1:n]) # List of temporal node tiers
              prior <- priorKnowledge(addtemporal = temporal)
          }
      } else if (data_type == 'sim') {
          sim_data <-loadSimulatedData(n=n, N=N, numInts=numInts, i=i)
          data <- sim_data$data
          
          trueGraph  <- sim_data$trueGraph
          true_models <- c(true_models, as.vector(trueGraph))
          
          
          # TODO: not correct, use intervention variables and discard the regime, then use background knowledge
          # than no intervention variable causes the others (similar to sachs)
          if (addPrior) {
              intv <- colnames(data[(n+1):ncol(data)])
              forbiddenWithin <- intv
              class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
              temporal <- list(forbiddenWithin,colnames(data)[1:n]) # List of temporal node tiers
              prior <- priorKnowledge(addtemporal = temporal)
          }
      }
      
      # execute method
      if (method == 'fges') {
          results <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = FALSE, priorKnowledge = prior)
          causalMatrix <- gfci2causal(n=(n+numInts),results=results,vnames=colnames(data))
          if (showGraphs) {
            plot(results$graphNEL, nodeAttrs=makeNodeAttrs(results$graphNEL, fontsize=20))
          }
      } else if (method == 'gfci') {
          if (bootstrap == 0) {
            results <- gfci(df = data, priorKnowledge = prior, verbose=FALSE)
            causalMatrix <- gfci2causal(n=(n+numInts),results=results,vnames=colnames(data))
          } else {
            results <- bootstrapgfci(data=data,n=(n+numInts),N=N,repeat_bootstrap = bootstrap)
            causalMatrix <- results$causalMatrix
          }
          if (showGraphs) {
            gfci_graph(results)
          }
      }
      
      if (data_type == 'sim') {
          
          if (showGraphs) {
            processed_tgraph <- process_trueGraph(trueGraph)
            gfci_graph(processed_tgraph)
          }
          
          results$tPositives <- length(intersect(which(causalMatrix> 0), which(trueGraph> 0)))
          results$fPositives <-length(which(causalMatrix> 0))-results$tPositives
          results$total <- length(trueGraph[trueGraph>0])
          
          print(paste("Hits: ", results$tPositives))
          print(paste("Number of false positives:", results$fPositives ))
          
          # ratio 
          allHits[i] <- results$tPositives
          allTotals[i] <- results$total
          allfPositives[i] <- results$fPositives
          recall[i] <- sum(allHits)/sum(allTotals)
          precision[i] <- sum(allHits)/(sum(allfPositives) + sum(allHits))
          
      }
      
      learntGraph <- causalMatrix
      
      learnt_models <- c(learnt_models, as.vector(learntGraph))
  }
  stop <- proc.time()
  print(stop - start)
  if (data_type == 'sim') {
    plot(recall,precision)
    printSingleRocCurve(learnt_models, true_models, "gfci", paste("./jci/results/rocCurve_", howmany, "_bootstrap", bootstrap,"_prior", addPrior, ".pdf", sep=""))
  }
  
}