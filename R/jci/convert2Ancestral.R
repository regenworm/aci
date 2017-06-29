convert2Ancestral <- function(loc = './graph.dot', n=18) {
  file <- as.matrix(read.table(loc,sep='\n'))
  connections <- c()
  for (i in 1:dim(file)[1]) {
    if (grepl(pattern='->',x=file[i,])) {
      connections <- c(connections,i)
    }
  }
  
  # return(file[connections,])
  
  # look for ancestral relations
  newEdges <- c()
  for (edge in file[connections,]){
    # get parent node and child node
    edge <- strsplit(edge, " ")
    v1 <- edge[[1]][1]
    v2 <- strsplit(edge[[1]][3],";")[[1]][1]
    
    # look for children of child
    # add those relations to v1
    for (i in 1:dim(file)[1]) {
      if (grepl(pattern=paste(v2, '->'),x=file[i,])) {
        newv2 <- strsplit(file[i,], " ")[[1]][3]# b
        newEdges <- c(newEdges,paste(v1, ' -> ', newv2, sep=''))
      }
    }
    
    # look for parents of parent
    # add children to parents of v1
    for (i in 1:dim(file)[1]) {
      if (grepl(pattern=paste('->',v1),x=file[i,])) {
        newv1 <- strsplit(file[i,], " ")[[1]][1] # a
        newEdges <- c(newEdges,paste(newv1, '->', v2, ';', sep=''))
      }
    }
  }
  newLoc <- paste(strsplit(loc, '.dot')[[1]][1], 'Ancestral.dot',sep='')
  newFile <- c(file[1:(length(file)-4),], newEdges, file[(length(file)-3):length(file)]) 
  write(x=newFile,file=newLoc)
  return(newFile)
}
