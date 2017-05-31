# fges <- fges(df = charity, penaltydiscount = 2, maxDegree = -1, faithfulnessAsummed = TRUE,
#              numOfThreads = 2, verbose = TRUE)    #Compute FGES search
# fges$parameters #Show the FGES's parameters
# fges$datasets #Show the dataset
# fges$nodes #Show the result's nodes
# fges$edges #Show the result's edges
library(rcausal)
source('load_sachs.R')
forbiddenWithin <- colnames(intv)
class(forbiddenWithin) <- 'forbiddenWithin' # Make this tier forbidden within
temporal <- list(forbiddenWithin,colnames(raw_data)[1:11]) # List of temporal node tiers
prior <- priorKnowledge(addtemporal = temporal)



fges <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = TRUE, priorKnowledge = prior) # priorKnowledgeFromFile('knowledge.prior'))
# fges <- fges(df = data, penaltydiscount = 2, maxDegree = -1, numOfThreads = 2, verbose = TRUE, priorKnowledgeFromFile('knowledge.prior')) 
plot(fges$graphNEL, nodeAttrs=makeNodeAttrs(fges$graphNEL, fontsize=70))

gfci <- gfci(df = data, priorKnowledge = prior)