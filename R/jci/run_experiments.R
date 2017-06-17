allResults <- c()
allHits <- c()
allWrongs <- c()
totalratio <- c()

for (iter in 1:100) {
  source('./jci/rcausal_try.R')
  # allResults[iter] <- results
  allHits[iter] <- results$hits
  allWrongs[iter] <- results$wrongs
  totalHits <- sum(allHits)
  totalWrongs <- sum(allWrongs)
  
  totalratio[iter] <- totalHits/(totalHits+totalWrongs)
}

plot(totalratio)