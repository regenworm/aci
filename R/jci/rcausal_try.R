library(rcausal)
source('jci/gfci_graph.R')
a <- 'gfci' # either gfci or fges
data_type <-'sim' # either sachs or sim

if (data_type == 'sachs') {
  source('jci/load_sachs.R')
} else if (data_type == 'sim') {
  source('jci/load_simdata.R')
  colnames(data) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "R1")
}

# add prior knowledge (1st bullet point in paper)
forbiddenWithin <- colnames(data[12]) #colnames(intv)
class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
temporal <- list(forbiddenWithin,colnames(raw_data)[1:11]) # List of temporal node tiers
prior <- priorKnowledge(addtemporal = temporal)


if (a == 'fges') {
  fges <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = TRUE, priorKnowledge = prior)
  plot(fges$graphNEL, nodeAttrs=makeNodeAttrs(fges$graphNEL, fontsize=70))
} else if (a == 'gfci') {
  gfci <- gfci(df = data, priorKnowledge = prior)
  gfci_graph(gfci)
}