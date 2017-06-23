gfci2causal <- function(n, # number of variables
                        results, # rcausal gfci object
                        data_type, # sachs or sim data
                        vnames # intervention variable names
) {
  causalMatrix <- matrix(0,n,n)
  names <- c()
  # if (data_type == 'sim') {
  #   for (v in 1:n) {
  #     names[paste("V", v,sep="")] <- v
  #   }
  # } else if (data_type == 'sachs') {
    for (v in 1:n) {
      names[vnames[v]] <- v
    }
  # }
  
  for (edge in results$edges ) {
    edge <- strsplit(edge, " ")
    v1 <- edge[[1]][1]
    v2 <- edge[[1]][3]
    
    # if intervention variable, skip
    # if (substr(v1,1,1) == "I" || substr(v2,1,1) == "I") {
    #   next
    # }
    
    # if left
    if (substr(edge[[1]][2],1,1) == 'o') {
      causalMatrix[names[[v1]], names[[v2]]] <- 1
    } else if (substr(edge[[1]][2],1,1) == '<') {
      causalMatrix[names[[v1]], names[[v2]]] <- 2
    } else if (substr(edge[[1]][2],1,1) == '-') {
      causalMatrix[names[[v1]], names[[v2]]] <- 3
    }
    
    # if right
    if (substr(edge[[1]][2],3,3) == 'o') {
      causalMatrix[names[[v2]], names[[v1]]] <- 1
    } else if (substr(edge[[1]][2],3,3) == '>') {
      causalMatrix[names[[v2]], names[[v1]]] <- 2
    } else if (substr(edge[[1]][2],3,3) == '-') {
      causalMatrix[names[[v2]], names[[v1]]] <- 3
    }
  }
  L <- cpag_to_mc(causalMatrix)
  
  return(L$C)
}