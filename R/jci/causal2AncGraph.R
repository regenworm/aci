# causal2AncGraph:  Generate graphviz dotfile for causalMatrix
causal2AncGraph <- function(causalMatrix, 
                            names = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","I1","I2","I3","I4","I5","I6","I7"),
                            loc='./jci/results/sachsAncGraph.dot') 
{
  L <- list()
  L$nodes <- names
  vals <- which(causalMatrix>0, arr.ind = TRUE) 
  v1 <- names[vals[,1]]
  v2 <- names[vals[,2]]
  L$edges <- paste(v2,v1,sep=" -> ")
  L$edgeDegrees <- causalMatrix[causalMatrix>0]
 
  gfci_graphviz(L, loc, cmp = TRUE) 
}