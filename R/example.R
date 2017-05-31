# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
edgemat <- matrix(0,4,4)
edgemat[1,4] <- 1
edgemat[4,1] <- 1
## create the graph g
p <- 4
L <- 1 # '1' is latent
V <- c("Ghost", "Max","Urs","Anna","Eva")
edL <- setNames(vector("list", length=length(V)), V)
edL[[1]] <- list(edges=c(2,4),weights=c(1,1))
edL[[2]] <- list(edges=3,weights=c(1))
edL[[3]] <- list(edges=5,weights=c(1))
edL[[4]] <- list(edges=5,weights=c(1))
g <- new("graphNEL", nodes=V, edgeL=edL, edgemode="directed")
## compute the true covariance matrix of g
cov.mat <- trueCov(g)
## delete rows and columns belonging to latent variable L
true.cov <- cov.mat[-L,-L]
## transform covariance matrix into a correlation matrix
true.corr <- cov2cor(true.cov)
## The same, for the following three examples
indepTest <- gaussCItest
suffStat <- list(C = true.corr, n = 10^9)
## find PAG with FCI algorithm.
## As dependence "oracle", we use the true correlation matrix in
## gaussCItest() with a large "virtual sample size" and a large alpha:
normal.pag <- fci(suffStat, indepTest, alpha = 0.9999, labels = V[-L],
                  verbose=TRUE, fixedEdges = edgemat)
plot(normal.pag)


edgemat[1,2] <- 1
edgemat[2,1] <- 1
#normal.pag <- fci(suffStat, indepTest, alpha = 0.9999, labels = V[-L],
#                  verbose=TRUE, fixedGaps = edgemat)
#plot(normal.pag)
