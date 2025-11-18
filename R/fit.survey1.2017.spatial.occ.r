###########################################
# Goal: Fit a spatial occupancy model to the 2017 wolverine survey 
#       (first) data.
#
# Author: Brian D. Gerber
# Last Modified: 8/18/2025
#
# NOTE: the fitted model here does not recreate the manuscript results exactly
#       because the spatial locations are rounded to 10 km
#
##############################################
# Setup Environment
  rm(list=ls())
  library(stocc)
  library(HDInterval)
#######################
# Load data and visualize
  load("./outputs/wolv2017.spatial.data")
  
#IF need be, you can also load csv objects to get the data  
 # habData2017 = read.csv("./data/habDataWolv2017_07-16-25.csv")
 # visitData2017 = read.csv("./data/visitDataWolv2017_07-16-25.csv")
  #Note that these files don't have 'site.id.for.occ' column used below
  #for occupancy analysis


# Visualize data  
  head(visitData2017)
  head(habData2017)
  
#######################################

# Spatial Occupancy Model Fitting    
  
# Setup object that links the encounter history (eh), site name,
# and site coordinates in the two dataframes used by stocc

names <- list(
              visit = list(site = "site.id.for.occ", obs = "eh"),
              site = list(site = "site.id.for.occ", coords = c("Albers_X","Albers_Y"))
              )

# Final model structure - threshold of 23K, RSR, general prior on tau
# and covariate of bait/lure on detection. Also habitat predictions (propCopelandInman)

# NOTE THAT HIS WONT RECREATE THE MANUSCRIPT FINDINGS BECAUSE THE XY LOCATIONS
# HAVE BEEN ROUNDED AND THIS MODEL IS SPATIALLY-EXPLICIT
    
  sp.occ2017.3 <- spatial.occupancy(
    detection.model = ~ SiteType2017,
    occupancy.model = ~ propCopelandInman,
    spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
    so.data = make.so.data(visitData2017, habData2017, names),
    prior = list(a.tau=5, b.tau=1000, 
                 mu.b = c(0,0), Q.b = 0.1, 
                 mu.g = c(0,0), Q.g=0.1),
    control = list(burnin=5000, iter=100000, thin=20)
  )    
  save(sp.occ2017.3,file="./outputs/sp.occ2017.3")

  

##########################################  
##########################################  

  # Load fitted model objects
  load("./outputs/sp.occ2017.3")
  
  
  
  # Look at the model- sp.occ2022.gr.hab.bait
  
  plot(sp.occ2017.3$beta,main="Detection (probit)")
  apply(sp.occ2017.3$beta,2,quantile,probs=c(0.025,0.5,0.975))
  apply(sp.occ2017.3$beta,2,hdi)
  
  length(which(sp.occ2017.3$beta[,2]<0))/nrow(sp.occ2017.3$beta)
  
  plot(sp.occ2017.3$gamma,main="Occupancy (probit)") 
  apply(sp.occ2017.3$gamma,2,quantile,probs=c(0.025,0.5,0.975))
  apply(sp.occ2017.3$gamma,2,hdi)
  
  plot(sp.occ2017.3$tau, main = "Spatial Scale")
  apply(sp.occ2017.3$tau,2,quantile,probs=c(0.025,0.5,0.975))
  apply(sp.occ2017.3$tau,2,hdi)
  
  