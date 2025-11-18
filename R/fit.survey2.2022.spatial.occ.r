###########################################
# Goal: Fit a spatial occupancy model to the 2022 wolverine data
#       (second survey). The model output will be used for prediction/inference.

# Author: Brian D. Gerber
# Last Modified: 7/17/2025

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
  load("./outputs/wolv2022.spatial.data")
  
#IF need be, you can also load csv objects to get the data  
 #habData2022 = read.csv("./data/habDataWolv2022_07-16-25.csv")
 #visitData2022 = read.csv("./data/visitDataWolv2022_07-31-25.csv")
#Note that these files don't have 'site.id.for.occ' column used below
#for occupancy analysis

# Visualize data  
  head(visitData2022)
  head(habData2022)
###########################################################  
# Setup object that links the encounter history (eh), site name,
# and site coordinates in the two dataframes used by stocc

# NOTE: you need to use 'site'. 

names <- list(
              visit = list(site = "site.id.for.occ", obs = "eh"),
              site = list(site = "site.id.for.occ", coords = c("Albers_X","Albers_Y"))
              )

#Groupings 
  habData2022$Grouping1=factor(habData2022$Grouping1)  
  levels(habData2022$Grouping1)

#####################################################  
# Spatial Occupancy Model Fitting

# This model is using RSR (restricted spatial regression) with a threshold of 23k; the direct ICAR model parameterization
# leads to very difficult convergence. 
  
# This model has a general prior on tau that leads to plausible spatial site variations in eta.
  
# This model will use Groupings1, which allows a prior to be assigned on occupancy coefficents (gamma)  
# so that we can inform the model of our prior knowledge of no occurrences of wolverines in CO, UT, and
# the southern Cascades (OR).

#NOTE- The threshold to 23K. This will allow the spatial process
# effect the cells diagonal to each sampled cell. This was decided on June 3, 2025 TEAMS meeting
# call
  
  
# NOTE THAT HIS WONT RECREATE THE MANUSCRIPT FINDINGS BECAUSE THE XY LOCATIONS
# HAVE BEEN ROUNDED AND THIS MODEL IS SPATIALLY-EXPLICIT
  

# Final Model Structure 
  sp.occ2022.gr.hab.bait <- spatial.occupancy(
    detection.model = ~ SiteType2022,
    occupancy.model = ~ Grouping1+propCopelandInman,
    spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
    so.data = make.so.data(visitData2022, habData2022, names),
    prior = list(a.tau=5, b.tau=1000, 
                 mu.b = c(0,0), Q.b = 0.1, 
                 mu.g = c(0,0,-15,-15,-15,0), Q.g=c(0.1,0.1,5,5,5,0.1)),
    control = list(burnin=5000, iter=100000, thin=20)
  )    
  save(sp.occ2022.gr.hab.bait,file="./outputs/sp.occ2022.gr.hab.bait")  
  
##################################################
##################################################

# Load fitted model objects
  load("./outputs/sp.occ2022.gr.hab.bait")
  

  
# Look at the model- sp.occ2022.gr.hab.bait
  
    plot(sp.occ2022.gr.hab.bait$beta,main="Detection (probit)")
    apply(sp.occ2022.gr.hab.bait$beta,2,quantile,probs=c(0.025,0.5,0.975))
    apply(sp.occ2022.gr.hab.bait$beta,2,hdi)
    
    plot(sp.occ2022.gr.hab.bait$gamma,main="Occupancy (probit)") 
    apply(sp.occ2022.gr.hab.bait$gamma,2,quantile,probs=c(0.025,0.5,0.975))
    apply(sp.occ2022.gr.hab.bait$gamma,2,hdi)
    
    length(which(sp.occ2022.gr.hab.bait$gamma[,6]>0))/nrow(sp.occ2022.gr.hab.bait$gamma)
    
    plot(sp.occ2022.gr.hab.bait$tau, main = "Spatial Scale")
    apply(sp.occ2022.gr.hab.bait$tau,2,quantile,probs=c(0.025,0.5,0.975))
    apply(sp.occ2022.gr.hab.bait$tau,2,hdi)

    
    
    
