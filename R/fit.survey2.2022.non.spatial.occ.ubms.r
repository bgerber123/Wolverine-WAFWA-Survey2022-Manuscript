###########################################
# Goal: Fit non-spatial occupancy models with covariates, for inference purposes
#       using Wolverine Survey 2 data (2022 survey)
#
# Author: Brian Gerber
# Last Modified: 7/28/2025
#
##############################################
# Setup Environment
  rm(list=ls())
  library(unmarked)
  library(ubms)
  library(loo)
  library(ggplot2)
  library(ggpubr)

# Load data objects. These are created in script: data.mgmt.spatial.to.nonspatial.frame.r
  #load("./outputs/wolv2017.non.spatial.data")
  load("./outputs/wolv2022.non.spatial.data")

# Confirm data for shared sites
  
# 228 cells  
  dim(wolv2022.non.spatial$visit.2022.data.unmarked)  
  dim(wolv2022.non.spatial$site.covs.2022)  
  
# Remove cells/sites in states Wolverine do not occur  
  remove.index=!wolv2022.non.spatial$site.covs.2022$STATE%in%c("Oregon", "Colorado","Utah")
  wolv2022.non.spatial$site.covs.2022=wolv2022.non.spatial$site.covs.2022[remove.index,]
  wolv2022.non.spatial$visit.2022.data.unmarked=wolv2022.non.spatial$visit.2022.data.unmarked[remove.index,]

# Now at 182
  dim(wolv2022.non.spatial$visit.2022.data.unmarked)  
  dim(wolv2022.non.spatial$site.covs.2022)  
  


# Major difference in where they are- Bait is only in ID and WY    
  table(wolv2022.non.spatial$site.covs.2022$SiteType2022, wolv2022.non.spatial$site.covs.2022$STATE) 
  
# Detection history for 2022
  y.2022 =  data.frame(wolv2022.non.spatial$visit.2022.data.unmarked)
  dim(y.2022)
# Standardize spatial continuous covariates
  std.covs.2022 = wolv2022.non.spatial$site.covs.2022
  std.covs.2022[,-c(12:15)] = data.frame(apply(wolv2022.non.spatial$site.covs.2022[-c(12:15)],
                                 2,
                                 scale
                                 )
                           )
  which(is.na(std.covs.2022),arr.ind = TRUE)
  colnames(std.covs.2022)
  
# Pairwise correlations among continuous variables  
  M = cor(std.covs.2022[,-c(12:15)])
  png("./plots/corrplot.variables.2022.png",units="in",
      width=10,height=10,res=200)
#  corrplot::corrplot(M, method="circle")  
  corrplot::corrplot(M, method="number")  
  dev.off()
  
  
# look at variables  
  hist(std.covs.2022$propBurned2016_2021)
  hist(std.covs.2022$propBurned2001_2021)
  hist(std.covs.2022$meanHumanMod_2021)
  hist(std.covs.2022$meanNDVI_2022)
  hist(std.covs.2022$ClusterCount)
  hist(std.covs.2022$SDMayDepth_2022)
  hist(std.covs.2022$meanMayDepth_2022)
  hist(std.covs.2022$SDMaySWE_2022)
  unique(std.covs.2022$SiteType2022)

# Create unmarked frame for 2022 data  
  umf.2022 = unmarkedFrameOccu(y = y.2022, 
                               siteCovs = std.covs.2022,
                               obsCovs = NULL
                               )

# CSPH (snow adn primary habitat)
# SNW (Snow water equivalent, mean and standard deviation)
# Percent burned in last 20 years  
# Snow depth (mean and standard deviation)  

# propCopelandInman increases leads to increase in occupancy
# meanHumanMod_2022 increases leads to decrease in occupancy
# ClusterCount increase leads to increase in occupancy
# meanNDVI_2022 increase leads to increase in occupancy
# propBurned2016_2021 increase leads to decrease in occupancy
# propBurned2001_2015
# propBurned2001_2021 increase leads to increase in occupancy (PROBABLY NEEDS TO ELIMINATE averaged years 2016-2021)
# meanMaySWE_2022 increases to some degree increases in occupancy (quadtratic?)  
# SDMaySWE_2022 increases might lead to decrease in occupancy  
  

# Fit individual occupancy models    
  
# Null Model  
  fit.m1 <- stan_occu(~1 ~1, data=umf.2022, chains=3, iter=2000, cores=3, seed=123)
  save(fit.m1,file="./outputs/fit.m1")

# Human modification influence that varies by connectivity of cells  
  fit.m2 <- stan_occu(~SiteType2022 ~meanHumanMod_2021*ClusterCount,
                      data=umf.2022, 
                      chains=3, 
                      iter=2000, 
                      cores=3, 
                      seed=123)
  save(fit.m2,file="./outputs/fit.m2")  
  
# Environmental model  
  fit.m3 <- stan_occu(~SiteType2022 ~ meanNDVI_2022+I(meanNDVI_2022^2)+
                           propBurned2016_2021+propBurned2001_2015+
                           meanMayDepth_2022+I(meanMayDepth_2022^2),
                           data=umf.2022, 
                           chains=3, 
                           iter=2000, 
                           cores=3, 
                           seed=123)
  save(fit.m3,file="./outputs/fit.m3")  
  
# Anthro + environmental (simple structure)    
  fit.m4 <- stan_occu(~SiteType2022 ~ meanHumanMod_2021+
                           ClusterCount +
                           meanNDVI_2022+
                           propBurned2016_2021+
                           propBurned2001_2015+
                           meanMayDepth_2022,
                          data=umf.2022, 
                          chains=3, 
                          iter=2000, 
                          cores=3, 
                          seed=123)
  save(fit.m4,file="./outputs/fit.m4")
  

  
  
# Load outputs to consider
  load("./outputs/fit.m1")
  load("./outputs/fit.m2")
  load("./outputs/fit.m3")
  load("./outputs/fit.m4")
  

# Look at convergence - converged
  traceplot(fit.m2, pars=c("beta_state", "beta_det"))
  traceplot(fit.m3, pars=c("beta_state", "beta_det"))
  traceplot(fit.m4, pars=c("beta_state", "beta_det"))

# Compare WAIC  
  ubms::waic(fit.m2)
  ubms::waic(fit.m3)
  ubms::waic(fit.m4)
  

# Compare LOOIC  
  mods <- ubms::fitList(fit.m2,fit.m3,
                        fit.m4)
  round(modSel(mods), 3)
  
  
  plot_effects(fit.m4,"det")
  plot_effects(fit.m4,"state")
  
  fit.m4

# #Goodness of fit test
#   gof.m4 <- gof(fit.m4, draws=1000, quiet=TRUE)
#   gof.m4
#   plot(gof.m4)
#   
 # Goodness of fit - by zeros
   sim_y <- posterior_predict(fit.m4, "y", draws=1000)
#   
 # Rows are simulations and columns are sites by observations 
   dim(sim_y)  
   prop1 <- apply(sim_y, 1, function(x) mean(x==1, na.rm=TRUE))
   actual_prop1 <- mean(getY(fit.m4) == 1, na.rm=TRUE)
#   
   #Compare
   hist(prop1, col='gray')
   abline(v=actual_prop1, col='red', lwd=2)

   length(which(prop1>actual_prop1))/length(prop1)
      
   
#Probability of a positive  effect 
  
  sims=data.frame(fit.m4@stanfit@sim$samples)
  colnames(sims)
  sims= sims[,1:9]
  n.mcmc=nrow(sims)
  prob.positive=apply(sims,2,FUN=function(x){length(which(x>0))/n.mcmc})
  names(prob.positive) = c("Occ.Intercept","Occ.meanHumanMod_2012","Occ.ClusterCount","Occ.meanNDVI2022",
                           "Occ.PropBurned2016-2021","Occ.PropBurned2001-2015","Occ_MeanMayDepth",
                           "Det.Intercept","Det.SiteType")
  prob.positive


# Names of all objects
  names(data.frame((fit.m4@stanfit@sim$samples)))


# Plot marginal effects
  one =  plot_marginal(fit.m4, "state",covariate="meanHumanMod_2021")
  two =  plot_marginal(fit.m4, "state",covariate="ClusterCount")
  three =  plot_marginal(fit.m4, "state",covariate="meanNDVI_2022")
  
  arrange <- ggarrange(one, two, three, ncol = 1, nrow = 3)
  ggsave("./plots/inference.marginal.effect.png", arrange,device="png",
         units="in",width=10,height=10)
  
  
  
  