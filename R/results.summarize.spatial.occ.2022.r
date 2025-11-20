###########################################
# Goal: Use stocc model fit to summarize posterior distributions
#       2022 Wolverine Results
#
# Author: Brian Gerber
# Last Modified: 11/19/2025
#
##############################################

# Setup Environment
  rm(list=ls())
  library(stocc)
  library(ggplot2)
  library(HDInterval)

# Load data objects
  load("./outputs/wolv2022.spatial.data")

  load("./outputs/sp.occ2022.gr.hab.bait")
  fitted = sp.occ2022.gr.hab.bait
  
# realized occupancy
  dim(fitted$real.occ)

# Derive posterior distribution of total occupied sites and proportion of 
# occupied sites for the whole sampliong frame
  total.occ.across.frame =   apply(fitted$real.occ,1,sum)
  
  png(file="./plots/total.occ.2022.png",res=200,units="in",width=10,heigh=5)  
    par(mfrow=c(1,2))  
    hist(total.occ.across.frame,main="Across 2022 Sampling Frame",freq=FALSE,
         xlab="Occupied Sites")
    abline(v=median(total.occ.across.frame),lwd=2,lty=2)
    abline(v=quantile(total.occ.across.frame,probs=c(0.025,0.975)),lwd=2)
    prop=total.occ.across.frame/ncol(fitted$real.occ)
    hist(prop,main="Across 2022 Sampling Frame",freq=FALSE,
         xlab="Overall Probability of Occupancy")
    abline(v=median(prop),lwd=2,lty=2)
    abline(v=quantile(prop,probs=c(0.025,0.975)),lwd=2)
  dev.off()

  quantile(total.occ.across.frame/ncol(fitted$real.occ),probs=c(0.025,0.5,0.975))
  
  
# Calculate total occupied sites and proportion for each State
  
  table(habData2022$STATE)
  
  colnames(fitted$real.occ)=habData2022$STATE 

  states= unique(habData2022$STATE)

  states.mcmc=matrix(NA, nrow=nrow(fitted$real.occ),ncol=length(states))
  colnames(states.mcmc)=states
  sites.per.states=rep(NA,length(states))
  for(i in 1:length(states)){
    index=which(colnames(fitted$real.occ)==states[i])
    temp=fitted$real.occ[,index]
    states.mcmc[,i]=apply(temp,1,sum)
    sites.per.states[i]=length(index)
  }

  dim(states.mcmc)
  head(states.mcmc)      
  sites.per.states

# order of states
  states
  
# Estimate occupancy by state with quantile and HPDI  
  
# Plot Utah 
  plot(density(states.mcmc[,1]),lwd=2,main="Utah")
  plot(density(states.mcmc[,1]/sites.per.states[1]),lwd=2,main="Utah")
  quantile(states.mcmc[,1],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,1]/sites.per.states[1])

# Plot Colorado 
  plot(density(states.mcmc[,2]),lwd=2,main="Colorado")
  plot(density(states.mcmc[,2]/sites.per.states[2]),lwd=2,main="Colorado")
  quantile(states.mcmc[,2],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,2]/sites.per.states[2])


# Plot Wyoming 
  plot(density(states.mcmc[,3]),lwd=2,main="Wyoming")
  plot(density(states.mcmc[,3]/sites.per.states[3]),lwd=2,main="Wyoming")
  quantile(states.mcmc[,3]/sites.per.states[3],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,3]/sites.per.states[3])

# Plot Idaho
  plot(density(states.mcmc[,4]),lwd=2,main="Idaho")
  plot(density(states.mcmc[,4]/sites.per.states[4]),lwd=2,main="Idaho")

  quantile(states.mcmc[,4]/sites.per.states[4],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,4]/sites.per.states[4])

# Plot Oregon
  plot(density(states.mcmc[,5]),lwd=2,main="Oregon")
  plot(density(states.mcmc[,5]/sites.per.states[4]),lwd=2,main="Oregon")
  quantile(states.mcmc[,5]/sites.per.states[5],probs=c(0.025,0.5,0.975))

# Plot Montana
  plot(density(states.mcmc[,6]),lwd=2,main="Montana")
  plot(density(states.mcmc[,6]/sites.per.states[4]),lwd=2,main="Montana")
  quantile(states.mcmc[,6]/sites.per.states[6],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,6]/sites.per.states[6])

# Plot Washington
  plot(density(states.mcmc[,7]),lwd=2,main="Washington")
  plot(density(states.mcmc[,7]/sites.per.states[4]),lwd=2,main="Washington")

  quantile(states.mcmc[,7]/sites.per.states[7],probs=c(0.025,0.5,0.975))
  hdi(states.mcmc[,7]/sites.per.states[7])


#Extract quantiles
  state.total.occ.quant=apply(states.mcmc,2,quantile,probs=c(0.025,0.5,0.975))
  t(state.total.occ.quant)

#Derive Occupancy by state
  state.occ.quant <- t(t(state.total.occ.quant) / sites.per.states)
  t(round(state.occ.quant,digits=2))

#check- should be the same as the first column of state.occ.quant
  state.total.occ.quant[1,]/sites.per.states
  state.occ.quant[1,]
  
################################# 
## Plot geom density  
  states.mcmc2=data.frame(states.mcmc)
  str(states.mcmc2)
  states.mcmc2=utils::stack(states.mcmc2)
  colnames(states.mcmc2)=c("mcmc","State")
  
state.occ.sites =  ggplot(states.mcmc2, aes(x = mcmc, colour = State, fill = State)) +
                   geom_density(bw=1,alpha=0.5)+
                   ylab("Probability Density")+
                   xlab("Sites Occupied")+ 
                   theme(panel.background = element_rect(fill = 'white', colour = 'white'),
                        panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
                        axis.line = element_line(colour = "black"))+
                   ylim(0,0.1)+
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  

  state.occ.sites

  states.mcmc2.prob=data.frame(t(t(states.mcmc) / sites.per.states))
  states.mcmc2.prob=utils::stack(states.mcmc2.prob)
  colnames(states.mcmc2.prob)=c("mcmc","State")


  state.occ.prob =  ggplot(states.mcmc2.prob, aes(x = mcmc, colour = State, fill = State)) +
    geom_density(alpha=0.5,bw=0.009)+
    ylab("Probability Density")+
    xlab("Overall Probability of Occupancy")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ylim(0,20)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 
  
  state.occ.prob

  arrange=ggarrange(state.occ.sites,
                    state.occ.prob,
                    nrow=1, ncol=2)
  ggsave("./plots/plot.wolverine.2022.occupied.states.png", arrange,device="png",dpi=400,
         units="in",width=16,height=8)


