# loadObservational:  Load observational dataset from Sachs datasets without JCI
loadObservational <- function (data_location = '../experiments/sachs/data/sachs.csv'
){
  
  # read data from location
  raw_data <- read.csv(data_location, sep='\t');
  
  ############ map interventions, non-deterministically
  # get number of datapoints
  N <- dim(raw_data)[1]
  
  # combine mapping/data and use only n_of_exp of the datasets
  # remove regime variable
  data <- raw_data
  data <- data[which(raw_data[,12] <= 1),]
  data <- data[,-12] # remove regime
  n <- dim(data)[2]
  
  return(as.matrix(data))
}
