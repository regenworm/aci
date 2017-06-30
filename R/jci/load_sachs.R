# loadSachsData:  process sachs dataset with JCI framework and return adjusted dataset with specified parameters
loadSachsData <- function ( n_of_exp = 14, # number of datasets used out of 14 available
                            n_of_intv = 8, # number of intervention variables
                            data_location = '../experiments/sachs/data/sachs.csv'
){

  # read data from location
  raw_data <- read.csv(data_location, sep='\t');
  
  ############ map interventions, non-deterministically
  # get number of datapoints
  N <- dim(raw_data)[1]
  # create matrix with zeros
  intv <- matrix(0,N,n_of_intv) 

  # name columns with intervention names
  colnames(intv) <- c('I_ICAM', 'I_AKT', 'I_G0076', 'I_Psitectorigenin', 'I_U0126', 'I_LY294002', 'I_PMA', 'I_B2CAMP')
  
  # set values for intervention variables in sets 2 through 9
  for (i in 1:n_of_intv) 
  {
    intv[which(raw_data[,12] == i+1),i] <- 1
  }
  
  # set values for intervention variables in sets 10 through 14
  intv[which(raw_data[,12] > 9),1] <- 1
  for (i in 10:14)
  {
    intv[which(raw_data[,12] == i),i-n_of_intv] <- 1
  }
  
  # combine mapping/data and use only n_of_exp of the datasets
  # remove regime variable
  data <- cbind(raw_data,intv)
  data <- data[which(raw_data[,12] <= n_of_exp),]
  data <- data[,-12] # remove regime
  data <- data[,-12] # remove icam (intervention 1,experiment2)
  n <- dim(data)[2]
  
  return(data)
}
