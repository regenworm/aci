# runGFCI:      run method with specified parameters
# doSimtests:   Run method for simulated data with all parameter combinations
# doSachsTests: Generate results on Sachs dataset with and without prior knowledge (no bootstrapping)
runGFCI <- function(method = 'gfci', # either gfci or fges
                    data_type = 'sim', # either sachs or sim or obsS or obsSim
                    addPrior = TRUE, # use background knowledge
                    prior = c(), # specify background knowledge
                    n = 11, # number of variables
                    numInts = 8, # number of interventions
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
          
          
          if (addPrior) {
              intv <- colnames(data[(n+1):ncol(data)])
              forbiddenWithin <- intv
              class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
              temporal <- list(forbiddenWithin,colnames(data)[1:n]) # List of temporal node tiers
              prior <- priorKnowledge(addtemporal = temporal)
          }
      } else if (data_type == 'obsS') {
        
        if (howmany>1) {
          stop("Doesn't make sense to run Sachs experiments more than once.")
        }
        data <- loadObservational()
        N <- dim(data)[1]
      } else if (data_type == 'obsSim') {
        sim_data <- loadSimulatedData(n=n, N=N, numInts=numInts, i=i)
        data <- sim_data$obsData
        full_data <- sim_data$data
        
        trueGraph  <- sim_data$trueGraph
        true_models <- c(true_models, as.vector(trueGraph))
      }
    
      if (data_type == 'obsSim') {
        vnames <- colnames(full_data)
      } else {
        vnames <- colnames(data)
      }
      
      # execute method
      if (method == 'fges') {
          if (bootstrap==0) {
            results <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = FALSE, priorKnowledge = prior)
            causalMatrix <- gfci2causal(n=(n+numInts),results=results,vnames=vnames)
          } else {
            results <- bootstrapgfci(data=data,n=(n+numInts),N=N,repeat_bootstrap = bootstrap,prior=prior,method=method)
            causalMatrix <- results$causalMatrix
          }
          if (showGraphs) {
            gfci_graph(results)
            # plot(results$graphNEL, nodeAttrs=makeNodeAttrs(results$graphNEL, fontsize=20))
          }
      } else if (method == 'gfci') {
          if (bootstrap == 0) {
            results <- gfci(df = data, priorKnowledge = prior,maxDegree = -1,penaltydiscount = 1,completeRuleSetUsed=TRUE, verbose=FALSE)
            causalMatrix <- gfci2causal(n=(n+numInts),results=results,vnames=vnames)
          } else {
            results <- bootstrapgfci(data=data,n=(n+numInts),N=N,repeat_bootstrap = bootstrap,prior=prior,method=method)
            causalMatrix <- results$causalMatrix
          }
          if (showGraphs) {
            gfci_graph(results)
          }
      }
      
      if (data_type == 'sim' || data_type == 'obsSim') {
          
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
  if (data_type == 'sachs') {
    gfci_graphviz(results,loc= paste("./jci/results/sachsGraph", "_bootstrap", bootstrap,"_prior", addPrior, method, ".dot", sep=""))
    causal2AncGraph(causalMatrix = causalMatrix,names = colnames(data),loc= paste("./jci/results/sachsAncGraph", "_bootstrap", bootstrap,"_prior", addPrior, method, ".dot", sep=""))
  } else if (data_type == 'obsS') {
    gfci_graphviz(results,loc= paste("./jci/results/obsSachsGraph", "_bootstrap", bootstrap,"_prior", addPrior, method, ".dot", sep=""))
    causal2AncGraph(causalMatrix = causalMatrix,names = colnames(data),loc= paste("./jci/results/obsSachsAncGraph", "_bootstrap", bootstrap,"_prior", addPrior, method, ".dot", sep=""))
  }
  stop <- proc.time()
  print(stop - start)
  if (data_type == 'sim' || data_type == 'obsSim') {
    obs <- ''
    if (data_type == 'obsSim') {
      obs <- 'obs'
    }
    print(paste("Mean recall: "), mean(recall))
    print(paste("Mean precision: "), mean(precision))
    printSingleRocCurve(learnt_models, true_models, method, paste("./jci/results/",obs,"rocCurve_", howmany, "_bootstrap", bootstrap,"_prior", addPrior,method, ".pdf", sep=""))
  }

  return(list(eval = list(recall=recall,precision=precision), models = list(learnt_models=learnt_models, true_models=true_models), time = (stop-start)))
}

doSimTests <- function(method='gfci',data_type='sim') {
  clearnt <- c()
  if (data_type == 'sim') {
    a <- runGFCI(method=method,addPrior=TRUE,howmany=100,bootstrap=0,data_type = data_type)
    clearnt <- cbind(clearnt, a$models$learnt_models)
  }
  a <- runGFCI(method=method,addPrior=FALSE,howmany=100,bootstrap=0,data_type = data_type)
  clearnt <- cbind(clearnt, a$models$learnt_models)
  ctrue <- a$models$true_models
  
  if (data_type == 'sim') {
    a <- runGFCI(method=method,addPrior=TRUE,howmany=100,bootstrap=10,data_type = data_type)
    clearnt <- cbind(clearnt, a$models$learnt_models)
  }
  a <- runGFCI(method=method,addPrior=FALSE,howmany=100,bootstrap=10,data_type = data_type)
  clearnt <- cbind(clearnt, a$models$learnt_models)

  if (data_type == 'sim') {
    printRocCurves(clearnt, ctrue, c(paste(method,"1",sep=""), paste(method,"2",sep=""),paste(method,"3",sep=""),paste(method,"4",sep="")), paste("./jci/results/combinedSimCurves",method,data_type,".pdf",sep=""))
  } else if (data_type == 'obsSim') {
    printRocCurves(clearnt, ctrue, c(paste(method,"2",sep=""),paste(method,"4",sep="")), paste("./jci/results/combinedSimCurves",method,data_type,".pdf",sep=""))
  }
  return(list(clearnt=clearnt, ctrue=ctrue))
}

doSachsTests <- function(method='gfci',data_type='sachs') {
  clearnt <- c()
  if (data_type == 'sachs') {
    a <- runGFCI(method=method,data_type=data_type,addPrior=TRUE,howmany=1,bootstrap=0,numInts = 8)
    clearnt <- cbind(clearnt, a$models$learnt_models)
  }
  a <- runGFCI(method=method,data_type=data_type,addPrior=FALSE,howmany=1,bootstrap=0,numInts = 8)
  clearnt <- cbind(clearnt, a$models$learnt_models)
  return(clearnt)
}


doAllTests <- function() {
  doSimTests()
  doSimTests(data_type = 'obsSim', method='gfci')
  doSimTests(method='fges')
  doSimTests(data_type = 'obsSim', method='fges')
  
  doSachsTests()
  doSachsTests(data_type='obsS',method='gfci')
  doSachsTests(method='fges')
  doSachsTests(data_type='obsS',method='fges')
}

