library(Rgraphviz)
gfci_graphviz <- function(gfci, loc="./jci/results/sachsGraph.dot") {
  # file.create(loc)
  # fileConn<-file(loc)
  edges <- gfci$edges
  nodes <- gfci$nodes
  dotfile <- c("digraph ofzo {", "node [color=lightblue2, style=filled];")
  # 
  
  for (node in nodes) {
    dotfile <- c(dotfile, paste("\"", node, "\";", sep=""))
  }
  
  for (edge in edges) {
    edgeNodes <- strsplit(edge, " ")

    connection <- edgeNodes[[1]][2]
    edgeNodes <- edgeNodes[[1]][-2]

    if (connection == "o->") {
      dotfile <- c(dotfile, paste("\"", edgeNodes[1], "\" -> \"", edgeNodes[2], "\" [dir=both arrowtail=dot];",sep=""))
    } else if (connection == "o-o") {
      dotfile <- c(dotfile, paste("\"", edgeNodes[1], "\" -> \"", edgeNodes[2], "\" [dir=both arrowtail=dot arrowhead=dot]",  sep=""))
    } else {
      dotfile <- c(dotfile, paste("\"", edgeNodes[1], "\" -> \"", edgeNodes[2], "\";",sep=""))
    }
  }
  
  dotfile <- c(dotfile, "}")
  write(x=dotfile,file=loc)
  # writeLines(dotfile, fileConn)
  # close(fileConn)
}