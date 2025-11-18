###########################################
# Goal: Fit a spatial occupancy model to the 2022 wolverine data
#       (second survey) with different starting values to asses convergence

# Author: Brian D. Gerber
# Last Modified: 8/18/2025

##############################################
# Setup Environment
rm(list=ls())
library(stocc)
library(coda)

#######################
# Load data and visualize
load("./outputs/wolv2022.spatial.data")

# Visualize data  
head(visitData2022)
head(habData2022)
###########################################################  
# Setup object that links the encounter history (eh), site name,
# and site coordinates in the two dataframes used by stocc

names <- list(
  visit = list(site = "site.id.for.occ", obs = "eh"),
  site = list(site = "site.id.for.occ", coords = c("Albers_X","Albers_Y"))
)

#Groupings 
habData2022$Grouping1=factor(habData2022$Grouping1)  
levels(habData2022$Grouping1)


#Setup different initial values- important when assessing convergence
set.seed(543543)
init.values1 = list(beta = runif(2,-3,3),
                    gamma = runif(6,-3,3),
                    tau = c(0.01))

set.seed(12145)
init.values2 = list(beta = runif(2,-3,3),
                    gamma = runif(6,-3,3),
                    tau = c(0.3))

set.seed(54311115)
init.values3 = list(beta = runif(2,-3,3),
                    gamma = runif(6,-3,3),
                    tau = c(0.6))


#####################################################  
# Spatial Occupancy Model Fitting

# This model is using RSR (restricted spatial regression); the direct ICAR model parameterization
# leads to very difficult convergence. 

# This model has a general prior on tau and leads to plausible spatial site variations in eta.

# This model will use Groupings1, which allows a prior to be assigned on occupancy coefficents (gamma)  
# so that we can inform the model of our prior knowledge of no occurrences of wolverins in CO, UT, and
# the southern Cascades.



#This is the intended final model structure for use in the 2nd manuscript

sp.occ2022.gr.hab.bait.init1 <- spatial.occupancy(
  detection.model = ~ SiteType2022,
  occupancy.model = ~ Grouping1+propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2022, habData2022, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0,-15,-15,-15,0), Q.g=c(0.1,0.1,5,5,5,0.1)),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values1
)    
save(sp.occ2022.gr.hab.bait.init1,file="./outputs/sp.occ2022.gr.hab.bait.init1")  


sp.occ2022.gr.hab.bait.init2 <- spatial.occupancy(
  detection.model = ~ SiteType2022,
  occupancy.model = ~ Grouping1+propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2022, habData2022, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0,-15,-15,-15,0), Q.g=c(0.1,0.1,5,5,5,0.1)),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values2
)    
save(sp.occ2022.gr.hab.bait.init2,file="./outputs/sp.occ2022.gr.hab.bait.init2")  



sp.occ2022.gr.hab.bait.init3 <- spatial.occupancy(
  detection.model = ~ SiteType2022,
  occupancy.model = ~ Grouping1+propCopelandInman,
  spatial.model = list(model="rsr", threshold=23000, moran.cut = 100), 
  so.data = make.so.data(visitData2022, habData2022, names),
  prior = list(a.tau=5, b.tau=1000, 
               mu.b = c(0,0), Q.b = 0.1, 
               mu.g = c(0,0,-15,-15,-15,0), Q.g=c(0.1,0.1,5,5,5,0.1)),
  control = list(burnin=5000, iter=100000, thin=20),
  initial.values = init.values3
)    
save(sp.occ2022.gr.hab.bait.init3,file="./outputs/sp.occ2022.gr.hab.bait.init3")  


########################################
# Asses convergence via Gelman-Rubin

#Load outputs
  load("./outputs/sp.occ2022.gr.hab.bait")
  load("./outputs/sp.occ2022.gr.hab.bait.init1")
  load("./outputs/sp.occ2022.gr.hab.bait.init2")
  load("./outputs/sp.occ2022.gr.hab.bait.init3")

#Combine posteriors from outputs to asses convergence
#using gelman-rubin diagnostics

beta.list = mcmc.list(sp.occ2022.gr.hab.bait$beta,
                      sp.occ2022.gr.hab.bait.init1$beta,
                      sp.occ2022.gr.hab.bait.init2$beta,
                      sp.occ2022.gr.hab.bait.init3$beta
                      )
#check
is.mcmc.list(beta.list)
length(beta.list)


gamma.list = mcmc.list(sp.occ2022.gr.hab.bait$gamma,
                       sp.occ2022.gr.hab.bait.init1$gamma,
                       sp.occ2022.gr.hab.bait.init2$gamma,
                       sp.occ2022.gr.hab.bait.init3$gamma
                       )

tau.list = mcmc.list(sp.occ2022.gr.hab.bait$tau,
                       sp.occ2022.gr.hab.bait.init1$tau,
                       sp.occ2022.gr.hab.bait.init2$tau,
                       sp.occ2022.gr.hab.bait.init3$tau
                    )

# we want upper CI below 1.1 and ideally near 1
# looks good
  coda::gelman.diag(beta.list)
  coda::gelman.diag(gamma.list)
  coda::gelman.diag(tau.list)


  
# Examine traceplots
png(file="./plots/spatial.convergence.2022.png",res=300,width=12,height=12,units="in")  
par(mfrow=c(3,3),mar=c(5,5,5,5))
  #Detection probability (probit-scale) - Intercept
  matplot(cbind(sp.occ2022.gr.hab.bait$beta[,1],
                sp.occ2022.gr.hab.bait.init1$beta[,1],
                sp.occ2022.gr.hab.bait.init2$beta[,1],
                sp.occ2022.gr.hab.bait.init3$beta[,1]
                )
          ,type="l", ylab="Detection probabilty (probit-scale) \n Intercept [Lure only]")
  #Detection probability (probit-scale) - effect of bait from lure
  matplot(cbind(sp.occ2022.gr.hab.bait$beta[,2],
                sp.occ2022.gr.hab.bait.init1$beta[,2],
                sp.occ2022.gr.hab.bait.init2$beta[,2],
                sp.occ2022.gr.hab.bait.init3$beta[,2]
  )
  ,type="l", ylab="Detection probabilty (probit-scale) \n Effect of Bait")

  
  matplot(cbind(sp.occ2022.gr.hab.bait$tau,
                sp.occ2022.gr.hab.bait.init1$tau,
                sp.occ2022.gr.hab.bait.init2$tau,
                sp.occ2022.gr.hab.bait.init3$tau
  )
  ,type="l", ylab="Spatial Scale Parameter (log-scale)")
  
  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,1],
                sp.occ2022.gr.hab.bait.init1$gamma[,1],
                sp.occ2022.gr.hab.bait.init2$gamma[,1],
                sp.occ2022.gr.hab.bait.init3$gamma[,1]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n Intercept [North Cascades]")
  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,2],
                sp.occ2022.gr.hab.bait.init1$gamma[,2],
                sp.occ2022.gr.hab.bait.init2$gamma[,2],
                sp.occ2022.gr.hab.bait.init3$gamma[,2]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n Northern Rockies")

  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,3],
                sp.occ2022.gr.hab.bait.init1$gamma[,3],
                sp.occ2022.gr.hab.bait.init2$gamma[,3],
                sp.occ2022.gr.hab.bait.init3$gamma[,3]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n South Cascades")
  
  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,4],
                sp.occ2022.gr.hab.bait.init1$gamma[,4],
                sp.occ2022.gr.hab.bait.init2$gamma[,4],
                sp.occ2022.gr.hab.bait.init3$gamma[,4]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n Southern Rockies")

  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,5],
                sp.occ2022.gr.hab.bait.init1$gamma[,5],
                sp.occ2022.gr.hab.bait.init2$gamma[,5],
                sp.occ2022.gr.hab.bait.init3$gamma[,5]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n Uintas")

  
  matplot(cbind(sp.occ2022.gr.hab.bait$gamma[,6],
                sp.occ2022.gr.hab.bait.init1$gamma[,6],
                sp.occ2022.gr.hab.bait.init2$gamma[,6],
                sp.occ2022.gr.hab.bait.init3$gamma[,6]
  )
  ,type="l", ylab="Occupancy probability (probit-scale) \n propCopelandInman Habitat Predictor")
  
    
  
    
dev.off()
  
  

