## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(sbm)

## ----param all----------------------------------------------------------------
nbLayers <- 2

## ----Independant Poisson bernoulli--------------------------------------------
Nnodes <- 40
blockProp <- c(.4,.6)
set.seed(1)
connectParam <- list(list(mean=matrix(rbeta(4,.5,.5),2,2)),list(mean=matrix(rexp(4,.5),2,2)))
model <- c("bernoulli","poisson")
type <- "directed"
mySampleMultiplexSBM_PB <-
   sampleMultiplexSBM(
   nbNodes = Nnodes,
    blockProp = blockProp,
   nbLayers = nbLayers,
   connectParam = connectParam,
   model=model,
   dimLabels = c('Individuals'), # generic name of the nodes
   type=type,
   seed = 1)
listSBM_PB <- mySampleMultiplexSBM_PB$listSBM
names(listSBM_PB) <- c("Bernoulli","Poisson")  
plotMyMultiplexMatrix(listSBM_PB)

## -----------------------------------------------------------------------------
listSBM_PB

## -----------------------------------------------------------------------------
blockProp <- list(c(.3,.3,.4),c(0.5,0.5))
Q <- sapply(blockProp, function(p) length(p))
nbNodes <- c(80,30)
connectParam <- list()
connectParam$mu <- vector("list",nbLayers)
connectParam$mu[[1]] <-  matrix(.1,Q[1],Q[2]) + matrix(c(1,1,1,0,1,0),Q[1],Q[2]) 
connectParam$mu[[2]] <- matrix(-2,Q[1],Q[2]) + matrix(c(1,3,2,1,2,3),Q[1],Q[2]) 
connectParam$Sigma <- matrix(c(2,1,0.1,4),nbLayers,nbLayers)
model <- rep("gaussian",2)
mySampleMultiplexSBM_GG <-
  sampleMultiplexSBM(
     nbNodes = nbNodes,
     blockProp = blockProp,
     nbLayers = nbLayers,
     connectParam = connectParam,
     model=model,
     type="bipartite",
     dependent=TRUE,
     dimLabels = c('row','col'),
     seed = 1)
listSBM_GG <- mySampleMultiplexSBM_GG$listSBM
plotMyMultiplexMatrix(listSBM_GG)

## -----------------------------------------------------------------------------
## MultiplexSBM Bernoulli with dependence
Q <- 2
set.seed(94)
P00<-matrix(runif(Q*Q),Q,Q)
P10<-matrix(runif(Q*Q),Q,Q)
P01<-matrix(runif(Q*Q),Q,Q)
P11<-matrix(runif(Q*Q),Q,Q)
SumP<-P00+P10+P01+P11
P00<-P00/SumP
P01<-P01/SumP
P10<-P10/SumP
P11<-P11/SumP
connectParam <- list()
connectParam$prob00 <- P00
connectParam$prob01 <- P01
connectParam$prob10 <- P10
connectParam$prob11 <- P11
model <- rep("bernoulli",2)
type <- "directed"
nbLayers <- 2
Nnodes <- 40
blockProp <- c(.6,.4)
mySampleMultiplexSBM <-
   sampleMultiplexSBM(
     nbNodes = Nnodes,
     blockProp = blockProp,
     nbLayers = nbLayers,
     connectParam = connectParam,
     model=model,
     type=type,
     dependent=TRUE,
     seed = 1)
listSBM_BB <- mySampleMultiplexSBM$listSBM
plotMyMultiplexMatrix(listSBM_BB)

## ---- echo = FALSE,eval=TRUE--------------------------------------------------
load(file='resVignetteSimuMultiplex.rda')

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  res_PB <- estimateMultiplexSBM(listSBM_PB)
#  res_PB$storedModels

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
res_PB$storedModels

## ---- echo = TRUE, eval = TRUE------------------------------------------------
plot(res_PB)

## ---- echo = TRUE, eval = TRUE------------------------------------------------
plot(res_PB,type='expected')

## -----------------------------------------------------------------------------
All <- plotAlluvial(list(simulated  = mySampleMultiplexSBM_PB$memberships$Individuals, estim=res_PB$memberships$Individuals))
All

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  res_GG <- estimateMultiplexSBM(listSBM_GG,dependent = TRUE,estimOptions = list(plot = FALSE,verbosity = 0 ))
#  res_GG$storedModels

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
res_GG$storedModels

## ---- echo = TRUE, eval = TRUE------------------------------------------------
plot(res_GG)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  res_BB <- estimateMultiplexSBM(listSBM_BB,dependent =  TRUE,estimOptions = list(plot = FALSE,verbosity = 0 ))
#  res_BB$storedModels

## ---- echo = FALSE, eval = TRUE-----------------------------------------------
res_BB$storedModels

