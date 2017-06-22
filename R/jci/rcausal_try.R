# source('./jci/load_simdata.R')
# source('./jci/graphviz_dot.R')
# source('./jci/process_trueGraph.R')
# sourceDir('./jci',trace=FALSE)
runGFCI <- function( a = 'gfci', # either gfci or fges
                            data_type = 'sim', # either sachs or sim
                            addPrior = TRUE, # use background knowledge
                            prior = c(), # specify background knowledge
                            n = 11, # number of variables
                            numInts = 7, # number of interventions
                            N = 500,
                            howmany = 100, # howmany simulated experiments 
                            showGraphs = TRUE
){
  true_models <- c() # list that will contain all causal matrices for all simulated data.
  learnt_models <- c() # list that will contain all causal matrices for learnt models for simulated data.
  allHits <- c()
  allTotals <- c()
  totalratio <- c()
  start  <- proc.time()

  for (i in 1:howmany){
      # get data
      if (data_type == 'sachs') {
          
          if (howmany>1) {
              stop("Doesn't make sense to run Sachs experiments more than once.")
          }
          
          # TODO: change to function, then can just call it directly, and return the objects
          # Now if there is a name clash an object gets overwritten.
          source('jci/load_sachs.R')
          
          if (addPrior) {
              forbiddenWithin <- colnames(intv)
              class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
              temporal <- list(forbiddenWithin,colnames(data)[1:11]) # List of temporal node tiers
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
              forbiddenWithin <- colnames(data[(n+1):ncol(data)])
              class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
              temporal <- list(forbiddenWithin,colnames(data)[1:n]) # List of temporal node tiers
              prior <- priorKnowledge(addtemporal = temporal)
          }
      }
      
      # execute method
      if (a == 'fges') {
          results <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = TRUE, priorKnowledge = prior)
          if (showGraphs) {
            plot(results$graphNEL, nodeAttrs=makeNodeAttrs(results$graphNEL, fontsize=20))
          }
      } else if (a == 'gfci') {
          results <- gfci(df = data, priorKnowledge = prior)
          if (showGraphs) {
            gfci_graph(results)
          }
      }
      
      if (data_type == 'sim') {
          
          if (showGraphs) {
            processed_tgraph <- process_trueGraph(trueGraph)
            gfci_graph(processed_tgraph)
          }
        
          
          causalMatrix <- matrix(0,n,n)
          names <- c()
          for (v in 1:n) {
            names[paste("V", v,sep="")] <- v
          }
          
          for (edge in results$edges ) {
              edge <- strsplit(edge, " ")
              v1 <- edge[[1]][1]
              v2 <- edge[[1]][3]
              
              if (edge[[1]][2] == 'o-o') {
                causalMatrix[names[v2], names[v1]] = 1
                causalMatrix[names[v1], names[v2]] = 1
              } else if (edge[[1]][2] == 'o->') {
                causalMatrix[names[v2], names[v1]] = 2
                causalMatrix[names[v1], names[v2]] = 1
              } else if (edge[[1]][2] == '<-o') {
                causalMatrix[names[v2], names[v1]] = 1
                causalMatrix[names[v1], names[v2]] = 2
              } else if (edge[[1]][2] == '-->') {
                causalMatrix[names[v2], names[v1]] = 3
                causalMatrix[names[v1], names[v2]] = 1
              } else if (edge[[1]][2] == '<--') {
                causalMatrix[names[v2], names[v1]] = 1
                causalMatrix[names[v1], names[v2]] = 3
              } else if (edge[[1]][2] == '<->') {
                causalMatrix[names[v2], names[v1]] = 3
                causalMatrix[names[v1], names[v2]] = 3
              } else if (edge[[1]][2] == '--o') {
                causalMatrix[names[v2], names[v1]] = 3
                causalMatrix[names[v1], names[v2]] = 1
              } else if (edge[[1]][2] == 'o--') {
                causalMatrix[names[v2], names[v1]] = 1
                causalMatrix[names[v1], names[v2]] = 3
              } else if (edge[[1]][2] == '---') {
                causalMatrix[names[v2], names[v1]] = 3
                causalMatrix[names[v1], names[v2]] = 3
              }
          }
          
          results$tPositives <- length(intersect(which(causalMatrix> 0), which(trueGraph> 0)))
          results$fPositives <-length(which(causalMatrix> 0))-results$tPositives
          results$total <- length(trueGraph[trueGraph>0])
          
          print(paste("Hits: ", results$tPositives))
          print(paste("Number of false positives:", results$fPositives ))
          
          # ratio 
          allHits[i] <- results$tPositives
          allTotals[i] <- results$total
          totalratio[i] <- sum(allHits)/sum(allTotals)
      }
      
      learntGraph <- causalMatrix
      
      learnt_models <- c(learnt_models, as.vector(learntGraph))
  }
  stop <- proc.time()
  print(stop - start)
  plot(totalratio)
  
  printSingleRocCurve(learnt_models, true_models, "gfci", paste("./jci/rocCurve_", howmany, ".pdf", sep=""))
}