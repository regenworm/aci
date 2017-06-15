library(Rgraphviz)
gfci_graph <- function(gfci) {
  edges <- gfci$edges
  nodes <- gfci$nodes
  
  rEG <- new("graphNEL", nodes=gfci$nodes, edgemode="directed");
  
  connections <- list(arrowtail=c())
  for (edge in edges) {
    edgeNodes <- strsplit(edge, " ")
    
    connection <- edgeNodes[[1]][2]
    edgeNodes <- edgeNodes[[1]][-2]
    
    if (connection == "o->") {
      eN <- paste(edgeNodes[1], edgeNodes[2], sep="~")
      connections$arrowtail[eN] <- "odot"
    } 

    rEG <- addEdge(edgeNodes[1],edgeNodes[2], rEG, 1)
  }
  edgeRenderInfo(rEG) <- connections
  nodeRenderInfo(rEG) <- list(shape="ellipse")
  rEG <- layoutGraph(rEG)
  graph.par(list(nodes=list(col="darkgreen", fontsize=20)))
  renderGraph(rEG)
}