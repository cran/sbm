## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(sbm)

## ----loading dataset, eval=TRUE-----------------------------------------------
data(multipartiteEcologicalNetwork)
str(multipartiteEcologicalNetwork)
names(multipartiteEcologicalNetwork)

## ----transform dataset,  eval=TRUE--------------------------------------------
Net <- multipartiteEcologicalNetwork
type='bipartite'
model = 'bernoulli'
directed = FALSE
PlantFlovis = defineSBM(Net$Inc_plant_flovis, model,type,directed,
                        dimLabels = c("Plants", "Flovis"))
PlantAnt = defineSBM(Net$Inc_plant_ant,model,type,directed,
                      dimLabels = c("Plants", "Ants"))
PlantBird = defineSBM(Net$Inc_plant_bird,model,type,directed,
                     dimLabels = c("Plants", "Birds"))

## ----example of dataset, eval=TRUE--------------------------------------------
PlantFlovis$netMatrix[1:2,1:2]

## ----plot data----------------------------------------------------------------
plotMyMultipartiteMatrix(list(PlantFlovis,PlantAnt,PlantBird))

## ----load result, echo = FALSE, eval = TRUE-----------------------------------
load('resMultipartiteEcological.rda')

## ----MBM, echo = TRUE, eval = FALSE-------------------------------------------
#  estimOptions = list(initBM = FALSE)
#  listSBM <- list(PlantFlovis, PlantAnt, PlantBird)
#  myMSBM <- estimateMultipartiteSBM(listSBM, estimOptions)

## ----MBM what-----------------------------------------------------------------
myMSBM

## ----MBM v_K------------------------------------------------------------------
myMSBM$nbBlocks

## ----MBM param----------------------------------------------------------------
myMSBM$blockProp
myMSBM$connectParam

## ----MBM Z--------------------------------------------------------------------
table(myMSBM$memberships$Plants)
table(myMSBM$memberships$Ants)      

## ----storedmodels-------------------------------------------------------------
myMSBM$storedModels

## ----plot, eval = TRUE--------------------------------------------------------
plot(myMSBM) 

## ----plot meso, eval = TRUE---------------------------------------------------
plotOptions=list(vertex.size = c(12,6,4,4))
plotOptions$vertex.shape = rep('circle',4)
plotOptions$vertex.color = c('darkolivegreen3','darkgoldenrod2','salmon2','cadetblue2')
plotOptions$edge.curved = 0.1
plot(myMSBM,type = 'meso',plotOptions=plotOptions)

