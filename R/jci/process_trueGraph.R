show_trueGraph <- function (trueGraph) {
  cause <- 1
  result <- 1
  
  trueModel <- list(edges = c(), nodes = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11"))
  
  while (cause < 12) {
    while (result < 12) {
      if (trueGraph[cause,result] > 0) {
        trueModel$edges <- c(trueModel$edges, paste(trueModel$nodes[cause], "-->", trueModel$nodes[result]))
      }
      result <- result + 1
    } 
    result <- 1
    cause <- cause + 1
  }
  # gfci_graph(trueModel)
  
  return(trueModel)
}
