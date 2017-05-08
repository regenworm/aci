
# setwd('bachelorthesis/aci-master/R')
# source('load.R')
# loud()


############ set up
# number of datasets used out of 14 available
n_of_exp <- 9
# number of intervention variables 
n_of_intv <- 8 
# read data
raw_data <- read.csv('../experiments/sachs/data/sachs.csv', sep='\t');
############

############ map interventions, non-deterministically
# get number of datapoints
N <- dim(raw_data)[1]
# create matrix with zeros
intv <- matrix(0,N,n_of_intv) 

# name columns with intervention names
colnames(intv) <- c('ICAM', 'AKT', 'G0076', 'Psitectorigenin', 'U0126', 'LY294002', 'PMA', 'B2CAMP')

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
data <- cbind(raw_data,intv)
data <- data[which(raw_data[,12] <= n_of_exp),]
data <- data[,-12]
n <- dim(data)[2]
############

############ run solver
solverConfig <- list(n=n, solver='pcalg-fci',MD=list(D=list(list(data=data))))
testConfig <- list(n=n, test="logp", schedule=n-2, p=0.05, alpha=1.5, weight="log", currentDir='../tmp/')
bootstrap(solverConfig=solverConfig, repeat_bootstrap=100,filename_template='test_fci',testConfig=testConfig, N=N)
# pipeline(samples=data,solver_conf=solverConfig,schedule=0.5,solver='pcalg-fci')
############