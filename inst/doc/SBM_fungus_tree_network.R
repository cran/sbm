## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(sbm)
library(ggplot2)
library(igraph)
library(knitr)
library(alluvial)
theme_set(theme_bw())

## ----import dataset-----------------------------------------------------------
data("fungus_tree_network")
str(fungus_tree_network, max.level = 1)

## ----tree_tree_binary network-------------------------------------------------
tree_tree_binary <- 1 * (fungus_tree_network$tree_tree != 0)

## ----tree_tree_binary network plot data---------------------------------------
plotMyMatrix(tree_tree_binary, rowLabel = 'tree', colLabel = 'tree')

## ----simpleSBM----------------------------------------------------------------
mySimpleSBM <- tree_tree_binary %>% 
  estimateSimpleSBM("bernoulli", estimOptions = list(verbosity = 0, plot = FALSE))

## ----simpleSBMfit-------------------------------------------------------------
class(mySimpleSBM)
mySimpleSBM

## ----impleSBMfit fields-------------------------------------------------------
mySimpleSBM$nbBlocks
mySimpleSBM$nbNodes
mySimpleSBM$nbCovariates

## ----simpleSBMfit plot1-------------------------------------------------------
plot(mySimpleSBM, type = "data", rowLabel = 'tree', colLabel = 'tree')

## ----simpleSBMfit plot2-------------------------------------------------------
plot(mySimpleSBM, type = "expected", rowLabel = 'tree', colLabel = 'tree')

## ----simpleSBM storedModel----------------------------------------------------
mySimpleSBM$storedModels %>% kable()

## ----simpleSBM ICL------------------------------------------------------------
mySimpleSBM$storedModels %>% 
  ggplot() + aes(x = nbBlocks, y = ICL) + geom_line() + geom_point(alpha = 0.5)

## ----simpleSBMfit changeModel-------------------------------------------------
mySimpleSBM$setModel(4)
mySimpleSBM$nbBlocks
mySimpleSBM$plot(type = 'expected', rowLabel = 'tree', colLabel = 'tree')

## ----tree_tree network plot data----------------------------------------------
tree_tree <- fungus_tree_network$tree_tree
plotMyMatrix(tree_tree, rowLabel = 'tree', colLabel = 'tree')

## ----simpleSBM Poisson--------------------------------------------------------
mySimpleSBMPoisson <- tree_tree %>% 
  estimateSimpleSBM("poisson", estimOptions = list(verbosity = 0, plot = FALSE))

## ----simpleSBMfitPoisson------------------------------------------------------
class(mySimpleSBMPoisson)
mySimpleSBMPoisson

## ----impleSBMfitPoison fields-------------------------------------------------
mySimpleSBMPoisson$nbBlocks
mySimpleSBMPoisson$nbNodes
mySimpleSBMPoisson$nbCovariates

## ----simpleSBMfitPoisson plot1------------------------------------------------
plot(mySimpleSBMPoisson, type = "data", rowLabel = 'tree', colLabel = 'tree')

## ----simpleSBMfitPoisson plot2------------------------------------------------
plot(mySimpleSBMPoisson, type = "expected", rowLabel = 'tree', colLabel = 'tree')

## ----covar SBM,echo=TRUE,eval= TRUE-------------------------------------------
mySimpleSBMCov<- 
  tree_tree %>% 
  estimateSimpleSBM(
    model = 'poisson', 
    directed = FALSE, 
    covariates  = fungus_tree_network$covar_tree, 
    estimOptions = list(verbosity = 0, plot = FALSE, nbCores = 1)
  )

## ----select SBM covar, echo=TRUE, eval = TRUE---------------------------------
mySimpleSBMCov$nbBlocks

## ----extract param SBM poisson covar, echo=TRUE, eval = TRUE------------------
mySimpleSBMCov$connnectParam
mySimpleSBMCov$blockProp
mySimpleSBMCov$memberships
mySimpleSBMCov$covarParam

## ----plot incidence-----------------------------------------------------------
plotMyMatrix(fungus_tree_network$fungus_tree, rowLabel = 'fungis', colLabel = 'tree')

## ----tree_fungi_bipartite network---------------------------------------------
myBipartiteSBM <- 
  fungus_tree_network$fungus_tree %>% 
  estimateBipartiteSBM(model = 'bernoulli', estimOptions = list(verbosity = 0, plot = FALSE))

## ----bipartite.sbm fields-----------------------------------------------------
myBipartiteSBM$nbNodes
myBipartiteSBM$nbBlocks
myBipartiteSBM$connectParam

## ----plot bipartite-----------------------------------------------------------
plot(myBipartiteSBM, rowLabel = 'fungis',colLabel = 'tree')

