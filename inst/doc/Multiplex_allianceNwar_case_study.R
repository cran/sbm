## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-data-set------------------------------------------------------------
library(sbm)
data("war")

## ----manipulation-------------------------------------------------------------
library(igraph)
A = as.matrix(get.adjacency(war$alliance))
A = A[1:83,1:83]
B = as.matrix(get.adjacency(war$belligerent))

## -----------------------------------------------------------------------------
netA = defineSBM(A,model="bernoulli",dimLabels = "country") 
netB = defineSBM(B,model="bernoulli",dimLabels = "country")
plotMyMultiplexMatrix(list(netA,netB))

## ---- echo = FALSE, results='hide'--------------------------------------------
MultiplexFitIndep <- readRDS("Multiplex_allianceNwar_case_study.rds")

## ---- eval = FALSE------------------------------------------------------------
#  MultiplexFitIndep = estimateMultiplexSBM(list(netA,netB), dependent = FALSE, estimOptions = list(verbosity=0))

## -----------------------------------------------------------------------------
clust_country_indep = MultiplexFitIndep$memberships[[1]]
sort(clust_country_indep)

## -----------------------------------------------------------------------------
plot(MultiplexFitIndep)
plot(MultiplexFitIndep,type="expected")

## -----------------------------------------------------------------------------
MultiplexFitdep = estimateMultiplexSBM(list(netA,netB),dependent = TRUE,estimOptions = list(verbosity=0))

## -----------------------------------------------------------------------------
clust_country_dep = MultiplexFitdep$memberships[[1]]
sort(clust_country_indep)
aricode::ARI(clust_country_indep,clust_country_dep) 

## -----------------------------------------------------------------------------
MultiplexFitdep$ICL
MultiplexFitIndep$ICL

## -----------------------------------------------------------------------------
plot(MultiplexFitdep)
plot(MultiplexFitdep,type="expected")

## -----------------------------------------------------------------------------
p11 = MultiplexFitdep$connectParam$prob11
p01 = MultiplexFitdep$connectParam$prob01
p10 = MultiplexFitdep$connectParam$prob10
# conditional probabilities of being at war while having been or will be allied
round(p11/(p11+p10),2)
# marginal probabilities of being at war
round(p11+p01,2)

