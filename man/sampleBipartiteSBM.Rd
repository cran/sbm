% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample.R
\name{sampleBipartiteSBM}
\alias{sampleBipartiteSBM}
\title{Sampling of Bipartite SBMs}
\usage{
sampleBipartiteSBM(
  nbNodes,
  blockProp,
  connectParam,
  model = "bernoulli",
  dimLabels = c(row = "row", col = "col"),
  covariates = list(),
  covariatesParam = numeric(0)
)
}
\arguments{
\item{nbNodes}{number of nodes in the network}

\item{blockProp}{parameters for block proportions: list of size two with row and column block proportions}

\item{connectParam}{list of parameters for connectivity with a matrix of means 'mean' and an optional matrix of variances 'var', the sizes of which must match \code{blockProp} length (in row, respectively in column)}

\item{model}{character describing the model for the relation between nodes (\code{'bernoulli'}, \code{'poisson'}, \code{'gaussian'}, \code{'ZIgaussian'}). Default is \code{'bernoulli'}.}

\item{dimLabels}{an optional list of labels for each dimension (in row, in column)}

\item{covariates}{a list of matrices with same dimension as mat describing covariates at the edge level. No covariate per Default.}

\item{covariatesParam}{optional vector of covariates effect. A zero length numeric vector by default.}
}
\value{
an object with class \code{\link{BipartiteSBM}}
}
\description{
This function samples a simple Stochastic Block Models, with various model
for the distribution of the edges:  Bernoulli, Poisson, or Gaussian models, and possibly with covariates
}
\examples{
### =======================================
### BIPARTITE BERNOULLI SBM
## Graph parameters
nbNodes <- c(100, 120)
blockProp <- list(c(.5, .5), c(1/3, 1/3, 1/3)) # group proportions
means <- matrix(runif(6), 2, 3)  # connectivity matrix
# In Bernoulli SBM, parameters is a list with
# a matrix of means 'mean' which are probabilities of connection
connectParam <- list(mean = means)

## Graph Sampling
dimLabels = c(row='Reader',col='Book')
mySampler <- sampleBipartiteSBM(nbNodes, blockProp, connectParam, model = 'bernoulli',dimLabels)
plot(mySampler)
plot(mySampler,type='meso',plotOptions = list(vertex.label.name=list(row='Reader',col='Book')))
plot(mySampler,type='meso',plotOptions = list(vertex.label.name=c('A','B'),vertex.size = 1.4))
mySampler$rMemberships() # sample new memberships
mySampler$rEdges()   # sample new edges
mySampler$rNetwork()   # sample a new networrk (blocks and edges)
### =======================================
### BIPARTITE POISSON SBM
## Graph parameters
nbNodes <- c(100, 120)
blockProp <- list(c(.5, .5), c(1/3, 1/3, 1/3)) # group proportions
means <- matrix(rbinom(6, 30, 0.25), 2, 3)  # connectivity matrix
# In Poisson SBM, parameters is a list with a matrix of
# means 'mean' which are a mean integer value taken by edges
connectParam <- list(mean = means)

## Graph Sampling
dimLabels = c(row = 'Ind', col = 'Service')
mySampler <- sampleBipartiteSBM(nbNodes, blockProp, connectParam, model = 'poisson', dimLabels)
plot(mySampler,type='expected')
plotOptions = list(vertex.label.name=c('U','V'),vertex.size = c(1.4,1.3))
plot(mySampler, type='meso', plotOptions = plotOptions)
hist(mySampler$networkData)

### =======================================
### BIPARTITE GAUSSIAN SBM
## Graph parameters
nbNodes <- c(100, 120)
blockProp <- list(c(.5, .5), c(1/3, 1/3, 1/3)) # group proportions
means <- 20 * matrix(runif(6), 2, 3)  # connectivity matrix
# In Gaussian SBM, parameters is a list with a matrix
# of means 'mean' and a matrix of variances 'var'
connectParam <- list(mean = means, var = 1)

## Graph Sampling
mySampler <- sampleBipartiteSBM(nbNodes, blockProp, connectParam, model = 'gaussian')
plot(mySampler)
hist(mySampler$networkData)

}
