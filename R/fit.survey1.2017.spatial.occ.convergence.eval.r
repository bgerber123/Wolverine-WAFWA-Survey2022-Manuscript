###########################################
# Goal: Fit spatial occupancy model to the 2017 wolverine survey 
#       (first) data with different starting values to asses convergence
#
# Author: Brian D. Gerber
# Last Modified: 8/18/2025
#
##############################################
# Setup Environment
  rm(list=ls())
  library(stocc)
  library(coda)
#######################
# Load data and visualize
load("./outputs/wolv2017.spatial.data")

# Visualize data  
head(visitData2017)
head(habData2017)

#######################################

# Spatial Occupancy Model Fitting    

# Setup object that links the encounter history (eh), site name,
# and site coordinates in the two dataframes used by stocc

# NOTE: you need to use 'site'. Using GRID_ID leads to major issues obvious
# in the spatial occupancy predictions. Using Alberts vs X-Y may have a 
# small effect difference. Not sure.

names <- list(
  visit = list(site = "site.id.for.occ", obs = "eh"),
  site = list(site = "site.id.for.occ", coords = c("Albers_X","Albers_Y"))
)

# Setup different initial values- important when assessing convergence
  set.seed(543543)
  init.values1 = list(beta = runif(2,-3,3),
                      gamma = runif(2,-3,3),
                      tau = c(0.01))
  
  set.seed(12145)
  init.values2 = list(beta = runif(2,-3,3),
                      gamma = runif(2,-3,3),
                      tau = c(0.3))
  
  set.seed(54311115)
  init.values3 = list(beta = runif(2,-3,3),
                      gamma = runif(2,-3,3),
                      tau = c(0.6))

#This is the final model structure for use in the manuscript
sp.occ2017.3.init1 <- spatial.occupancy(
  detection.model = ~ SiteType2017,
  occupancy.model = ~ propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2017, habData2017, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0), Q.g=0.1),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values1
)    
save(sp.occ2017.3.init1,file="./outputs/sp.occ2017.3.init1")


sp.occ2017.3.init2 <- spatial.occupancy(
  detection.model = ~ SiteType2017,
  occupancy.model = ~ propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2017, habData2017, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0), Q.g=0.1),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values2
)    
save(sp.occ2017.3.init2,file="./outputs/sp.occ2017.3.init2")



sp.occ2017.3.init3 <- spatial.occupancy(
  detection.model = ~ SiteType2017,
  occupancy.model = ~ propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2017, habData2017, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0), Q.g=0.1),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values3
)    
save(sp.occ2017.3.init3,file="./outputs/sp.occ2017.3.init3")


########################################
# Asses convergence via Gelman-Rubin

# load outputs
 load("./outputs/sp.occ2017.3")
 load("./outputs/sp.occ2017.3.init1")
 load("./outputs/sp.occ2017.3.init2")
 load("./outputs/sp.occ2017.3.init3")

  beta.list = mcmc.list(sp.occ2017.3$beta,
                        sp.occ2017.3.init1$beta,
                        sp.occ2017.3.init2$beta,
                        sp.occ2017.3.init3$beta
                       )
#check
  is.mcmc.list(beta.list)
  length(beta.list)


  gamma.list = mcmc.list(sp.occ2017.3$gamma,
                         sp.occ2017.3.init1$gamma,
                         sp.occ2017.3.init2$gamma,
                         sp.occ2017.3.init3$gamma
                        )

  tau.list = mcmc.list(sp.occ2017.3$tau,
                       sp.occ2017.3.init1$tau,
                       sp.occ2017.3.init2$tau,
                       sp.occ2017.3.init3$tau
                       )

# we want upper CI below 1.1 and ideally near 1
# looks good
  coda::gelman.diag(beta.list)  #detection intercept
  coda::gelman.diag(gamma.list) #occupancy intercept and slope
  coda::gelman.diag(tau.list)   #spatial parameters



  
  # Examine traceplots
  png(file="./plots/spatial.convergence.2017.png",res=300,width=12,height=12,units="in")  
  par(mfrow=c(3,2),mar=c(5,5,5,5))
  #Detection probability (probit-scale) - Intercept
  matplot(cbind(sp.occ2017.3$beta[,1],
                sp.occ2017.3.init1$beta[,1],
                sp.occ2017.3.init2$beta[,1],
                sp.occ2017.3.init3$beta[,1]
  )
  ,type="l", ylab="Detection probabilty (probit-scale) \n Intercept [Lure only]")
  #Detection probability (probit-scale) - effect of bait from lure
  matplot(cbind(sp.occ2017.3$beta[,2],
                sp.occ2017.3.init1$beta[,2],
                sp.occ2017.3.init2$beta[,2],
                sp.occ2017.3.init3$beta[,2]
  )
  ,type="l", ylab="Detection probabilty (probit-scale) \n Effect of Bait")
  
  
  matplot(cbind(sp.occ2017.3$tau,
                sp.occ2017.3.init1$tau,
                sp.occ2017.3.init2$tau,
                sp.occ2017.3.init3$tau
  )
  ,type="l", ylab="Spatial Scale Parameter (log-scale)")
  
  
  matplot(cbind(sp.occ2017.3$gamma[,1],
                sp.occ2017.3.init1$gamma[,1],
                sp.occ2017.3.init2$gamma[,1],
                sp.occ2017.3.init3$gamma[,1]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n Intercept")
  
  matplot(cbind(sp.occ2017.3$gamma[,2],
                sp.occ2017.3.init1$gamma[,2],
                sp.occ2017.3.init2$gamma[,2],
                sp.occ2017.3.init3$gamma[,2]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n propCopelandInman Habitat Predictor")
  
  
  dev.off()
  
  
  
  
  