###########################################
# Goal: Get 2017 and 2022 model results from stocc
#
# Author: Brian Gerber
# Last Modified: 8/1/2025

##############################################
# Setup Environment
  rm(list=ls())
  library(ggplot2)
  library(viridisLite)
  library(ggpubr)
  library(HDInterval)

# Load model results from 2017 and 2022
  load("./outputs/sp.occ2017.3")
  fitted.2017 = sp.occ2017.3

  load("./outputs/sp.occ2022.gr.hab.bait")
  fitted.2022 = sp.occ2022.gr.hab.bait

  
################################
# COMPARE WHOLE SAMPLING FRAME  
# Merge to find unique GRID_ID that exist in both dataframes
# This is for the whole sampling frame
  

  merge.2017.2022 = merge(fitted.2017$so.data$site,
                          fitted.2022$so.data$site, by = "GRID_ID"
  )  
  
  dim(merge.2017.2022)  
  
# Here are the GRID_ID's that match up in 2017 and 2022
  comb.GRID_ID =  data.frame(unique(merge.2017.2022$GRID_ID))
  colnames(comb.GRID_ID) = "GRID_ID"

# States lined up to unique comb.GRID_ID
  states=merge.2017.2022$STATE.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]

  #correct one cell labled as Utah
  states[which(states=="Utah")]="Wyoming"
  
    table(states)
  
  
# Albers XY lined up to unique comb.GRID_ID
  Albers_X=merge.2017.2022$Albers_X.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]
  Albers_Y=merge.2017.2022$Albers_Y.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]
  
  
# I want to get the probability estimates for each of these
  dim(fitted.2017$real.occ)
  dim(fitted.2022$real.occ)  
  
  colnames(fitted.2017$real.occ) = fitted.2017$so.data$site$GRID_ID
  colnames(fitted.2022$real.occ) = fitted.2022$so.data$site$GRID_ID
  
  occ.compar.2017 = occ.compar.2022 = matrix(NA, nrow=length(comb.GRID_ID$GRID_ID), 
                                             ncol=nrow(fitted.2022$real.occ)
  )
  ######################
  #Get the GRID_ID matched to the fitted models
  for(i in 1:length(comb.GRID_ID$GRID_ID)){
    index.2017 = which(colnames(fitted.2017$real.occ)==as.character(comb.GRID_ID$GRID_ID[i]))
    index.2022 = which(colnames(fitted.2022$real.occ)==as.character(comb.GRID_ID$GRID_ID[i]))
    
    if(length(index.2017)==0){print("warning")}
    
    occ.compar.2017[i,] = fitted.2017$real.occ[,index.2017]
    occ.compar.2022[i,] = fitted.2022$real.occ[,index.2022]
  }
  
  ###################
  #Sum the z's to get the total occupied sites (for comparable sampling frame)  
  sum.z.2017 = apply(occ.compar.2017,2,sum)
  sum.z.2022 = apply(occ.compar.2022,2,sum)  
  
  # Sum of Z's for the same GRID_IDs in 2017 and 2022 Suryves
  hist(sum.z.2017,xlim=c(100,400),freq=FALSE,ylim=c(0,0.03))
  hist(sum.z.2022,add=TRUE,col=2,freq=FALSE)
  
  ##############
  # Plot probability densities of total number of occupied sites  
  stack.2017.2022 = c(sum.z.2017,
                      sum.z.2022
  )
  stack.2017.2022.df = data.frame(stack.2017.2022)
  colnames(stack.2017.2022.df)="Occupied"
  stack.2017.2022.df$Survey.Year = c(rep("2017",length(sum.z.2017)),
                                     rep("2022",length(sum.z.2022))
  )
  
  total.plot.occ.sites.2017.2022 =  ggplot(stack.2017.2022.df, aes(x = Occupied, colour = Survey.Year, fill = Survey.Year)) +
    geom_density(bw=3,alpha=0.5)+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    xlim(100,400)
  
  total.plot.occ.sites.2017.2022
  
  prob.occ.sites.2017.2022.df = stack.2017.2022.df
  prob.occ.sites.2017.2022.df$Occupied = prob.occ.sites.2017.2022.df$Occupied/length(comb.GRID_ID$GRID_ID)
  
  prob.plot.occ.sites.2017.2022 =  ggplot(prob.occ.sites.2017.2022.df, aes(x = Occupied, colour = Survey.Year, fill = Survey.Year)) +
    geom_density(bw=0.01,alpha=0.5)+
    ylab("Probability Density")+
    xlab("Probability of Occupancy")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    xlim(0.2,0.6)
  
  prob.plot.occ.sites.2017.2022
  
  arrange <- ggarrange(total.plot.occ.sites.2017.2022, prob.plot.occ.sites.2017.2022, ncol = 2, nrow = 1,
                       labels=c("Total Occupied Sites (Sites = 633)","Probability Occupied Sites (Sites = 633)"))
  
  ggsave("./plots/plot.2017.2022.same.sampling.frame.comparison.png", arrange,device="png",
         units="in",width=15,height=5)
  
  
  # Get difference probability
  diff.sum.z = sum.z.2022- sum.z.2017
  
  # Probability of a loss of number of occupied sites 
  length(which(diff.sum.z<0))/length(diff.sum.z)
  
  hist(diff.sum.z)  
  
  # Estimated number of lossed sites
  quantile(diff.sum.z,prob=c(0.025,0.5,0.975))
  hdi(diff.sum.z)
  
  
  #occupancy in quantiles
  aggregate(prob.occ.sites.2017.2022.df$Occupied,by=list(prob.occ.sites.2017.2022.df$Survey.Year),FUN=quantile,probs=c(0.025,0.5,0.975))
  
  aggregate(prob.occ.sites.2017.2022.df$Occupied,by=list(prob.occ.sites.2017.2022.df$Survey.Year),FUN=hdi)
  
  #proportional change
  hist(sum.z.2022/sum.z.2017)
  1-mean(sum.z.2022/sum.z.2017)
  
  ###############################  
  # Plot spatial differences across years
  
  # Differences in mean occupancy by site
  occ.diff.by.site =  apply(occ.compar.2022,1,mean)-apply(occ.compar.2017,1,mean)
  
  occ.diff.by.site = data.frame(occ.diff= occ.diff.by.site,
                                states = states,
                                Albers_X= Albers_X,
                                Albers_Y= Albers_Y)  
  
  plot.occ.site.diff.2017.2022 =  ggplot(occ.diff.by.site, 
                                         aes(x = Albers_X, 
                                             y = Albers_Y,
                                             colour = occ.diff
                                         )
  ) +
    scale_colour_viridis_c()+
    geom_point(shape = 15, size = 2) +
    theme_light()
  
  plot.occ.site.diff.2017.2022
  
  ggsave("./plots/plot.occ.diff.comparison.sampling.frame.2017.2022.png", plot.occ.site.diff.2017.2022,device="png",
         units="in",width=10,height=10)
  
  
  ##############
  # NOTE  
  # These 633 rows
  dim(occ.compar.2017)
  dim(occ.compar.2022)
  
  #Correspond to GRID_ID
  comb.GRID_ID$GRID_ID
  # and correspond to state ID
  states
  #And XY 
  Albers_X
  Albers_Y
  
  

# Next, I want to compare state by state
  

  #2017 model results first
  states.mcmc.2017 = matrix(NA, nrow=ncol(occ.compar.2017),ncol=length(unique(states)))
  colnames(states.mcmc.2017) = unique(states)
  sites.per.states.2017 = rep(NA,length(unique(states)))
  occ.mcmc.2017 = matrix(NA, nrow=ncol(occ.compar.2017),ncol=length(unique(states)))
  colnames(occ.mcmc.2017) = unique(states)
  
  for(i in 1:length(unique(states))){
    index=which(states==colnames(states.mcmc.2017)[i])
    temp=occ.compar.2017[index,]
    states.mcmc.2017[,i]=apply(temp,2,sum)
    sites.per.states.2017[i]=length(index)
    occ.mcmc.2017[,i] = states.mcmc.2017[,i]/sites.per.states.2017[i]
  }

  
  #2022 model results first
  states.mcmc.2022 = matrix(NA, nrow=ncol(occ.compar.2022),ncol=length(unique(states)))
  colnames(states.mcmc.2022) = unique(states)
  sites.per.states.2022 = rep(NA,length(unique(states)))
  occ.mcmc.2022 = matrix(NA, nrow=ncol(occ.compar.2022),ncol=length(unique(states)))
  colnames(occ.mcmc.2022) = unique(states)
  
  for(i in 1:length(unique(states))){
    index=which(states==colnames(states.mcmc.2022)[i])
    temp=occ.compar.2022[index,]
    states.mcmc.2022[,i]=apply(temp,2,sum)
    sites.per.states.2022[i]=length(index)
    occ.mcmc.2022[,i] = states.mcmc.2022[,i]/sites.per.states.2022[i]
  }
  
  dim(states.mcmc.2017)
  dim(states.mcmc.2022)
  
  st.stack.2017 = stack(data.frame(states.mcmc.2017))
  st.stack.2022 = stack(data.frame(states.mcmc.2022))
  st.stack.2017.2022 = rbind(st.stack.2017,st.stack.2022)
  st.stack.2017.2022$survey = c(rep("2017",nrow(st.stack.2017)),
                                rep("2022",nrow(st.stack.2022))
                                )
  colnames(st.stack.2017.2022)= c("Occupied.Sites","State","Survey")
  
  
  WY.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Wyoming"),] 
  ID.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Idaho"),]
  MT.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Montana"),]
  WA.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Washington"),]
  
  
  MT.plot.compare =  ggplot(MT.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability density")+
    xlab("Sites occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Montana")+
    xlim(0,200)
  MT.plot.compare
  
  ID.plot.compare =  ggplot(ID.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability density")+
    xlab("Sites occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Idaho")+
    xlim(0,200)
  
  
  WY.plot.compare =  ggplot(WY.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability density")+
    xlab("Sites occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Wyoming")+
    xlim(0,200)
  
  WA.plot.compare =  ggplot(WA.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability density")+
    xlab("Sites occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Washington")+
    xlim(0,200)
  
  
#output ggplot
  arrange <- ggarrange(MT.plot.compare, ID.plot.compare,WY.plot.compare, WA.plot.compare, ncol = 2, nrow = 2)
  ggsave("./plots/plot.state.comparison.sampling.frame.2017.2022.png", arrange,device="png",
         units="in",width=10,height=10)

save.image("./outputs/compar.2017.2022.samplingframe.RData")  
  

# Probability of a decline
head(occ.mcmc.2017)
head(occ.mcmc.2022)

WY.prob = occ.mcmc.2022[,1]-occ.mcmc.2017[,1]
hist(WY.prob)
length(which(WY.prob<0))/length(WY.prob)


ID.prob = occ.mcmc.2022[,2]-occ.mcmc.2017[,2]
hist(ID.prob)
length(which(ID.prob<0))/length(ID.prob)


MT.prob = occ.mcmc.2022[,3]-occ.mcmc.2017[,3]
hist(MT.prob)
length(which(MT.prob<0))/length(MT.prob)


WA.prob = occ.mcmc.2022[,4]-occ.mcmc.2017[,4]
hist(WA.prob)
length(which(WA.prob<0))/length(WA.prob)



############################
# same plot as above bu the probability of occupancy

st.stack.2017 = stack(data.frame(occ.mcmc.2017))
st.stack.2022 = stack(data.frame(occ.mcmc.2022))
st.stack.2017.2022 = rbind(st.stack.2017,st.stack.2022)
st.stack.2017.2022$survey = c(rep("2017",nrow(st.stack.2017)),
                              rep("2022",nrow(st.stack.2022))
)
colnames(st.stack.2017.2022)= c("Occupancy","State","Survey")


WY.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Wyoming"),] 
ID.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Idaho"),]
MT.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Montana"),]
WA.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Washington"),]


MT.plot.compare =  ggplot(MT.plot, aes(x = Occupancy,  fill = Survey)) +
  geom_density(alpha = 0.5)+
  scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
  ylab("Probability Density")+
  xlab("Sites Occupied")+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggtitle("Montana")+
  xlim(0,1)
MT.plot.compare

ID.plot.compare =  ggplot(ID.plot, aes(x = Occupancy,  fill = Survey)) +
  geom_density(alpha = 0.5)+
  scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
  ylab("Probability Density")+
  xlab("Sites Occupied")+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggtitle("Idaho")+
  xlim(0,1)


WY.plot.compare =  ggplot(WY.plot, aes(x = Occupancy,  fill = Survey)) +
  geom_density(bw=0.008,alpha = 0.5)+
  scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
  ylab("Probability Density")+
  xlab("Sites Occupied")+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggtitle("Wyoming")+
  xlim(0,1)

WA.plot.compare =  ggplot(WA.plot, aes(x = Occupancy,  fill = Survey)) +
  geom_density(bw=0.01,alpha = 0.5)+
  scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
  ylab("Probability Density")+
  xlab("Sites Occupied")+ 
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggtitle("Washington")+
  xlim(0,1)


#output ggplot
arrange <- ggarrange(MT.plot.compare, ID.plot.compare,WY.plot.compare, WA.plot.compare, ncol = 2, nrow = 2)
ggsave("./plots/plot.state.comparison.sampling.frame.ocupancy.2017.2022.manuscript.png", arrange,device="png",
       units="in",width=10,height=10)




save.image("./outputs/compar.2017.2022.samplingframe.RData")  


#################################  
#################################
# COMPARE ONLY SAMPLEED SITES  
# Merge to find unique GRID_ID that exist in both dataframes
# This is only for the visit data  
  merge.2017.2022 = merge(fitted.2017$so.data$visit,
                          fitted.2022$so.data$visit, by = "GRID_ID"
                          )
  # Here are the GRID_ID's that match up in 2017 and 2022
  comb.GRID_ID =  data.frame(unique(merge.2017.2022$GRID_ID))
  colnames(comb.GRID_ID) = "GRID_ID"
  dim(comb.GRID_ID)
  
  # States lined up to unique comb.GRID_ID
  states=merge.2017.2022$STATE.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]
  table(states)
  
  
  # Albers XY lined up to unique comb.GRID_ID
  Albers_X=merge.2017.2022$Albers_X.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]
  Albers_Y=merge.2017.2022$Albers_Y.x[match(comb.GRID_ID$GRID_ID,merge.2017.2022$GRID_ID)]
  
  
  # I want to get the probability estimates for each of these
  dim(fitted.2017$real.occ)
  dim(fitted.2022$real.occ)  
  
  colnames(fitted.2017$real.occ) = fitted.2017$so.data$site$GRID_ID
  colnames(fitted.2022$real.occ) = fitted.2022$so.data$site$GRID_ID
  
  occ.compar.2017 = occ.compar.2022 = matrix(NA, nrow=length(comb.GRID_ID$GRID_ID), 
                                             ncol=nrow(fitted.2022$real.occ)
  )
  ######################
  #Get the GRID_ID matched to the fitted models
  for(i in 1:length(comb.GRID_ID$GRID_ID)){
    index.2017 = which(colnames(fitted.2017$real.occ)==as.character(comb.GRID_ID$GRID_ID[i]))
    index.2022 = which(colnames(fitted.2022$real.occ)==as.character(comb.GRID_ID$GRID_ID[i]))
    
    if(length(index.2017)==0){print("warning")}
    
    occ.compar.2017[i,] = fitted.2017$real.occ[,index.2017]
    occ.compar.2022[i,] = fitted.2022$real.occ[,index.2022]
  }
  
  ###################
  #Sum the z's to get the total occupied sites (for comparable sampling frame)  
  sum.z.2017 = apply(occ.compar.2017,2,sum)
  sum.z.2022 = apply(occ.compar.2022,2,sum)  
  
  # Sum of Z's for the same GRID_IDs in 2017 and 2022 Suryves
  hist(sum.z.2017,xlim=c(0,100),freq=FALSE,ylim=c(0,0.2))
  hist(sum.z.2022,add=TRUE,col=2,freq=FALSE)
  
  ##############
  # Plot probability densities of total number of occupied sites  
  stack.2017.2022 = c(sum.z.2017,
                      sum.z.2022
  )
  stack.2017.2022.df = data.frame(stack.2017.2022)
  colnames(stack.2017.2022.df)="Occupied"
  stack.2017.2022.df$Survey.Year = c(rep("2017",length(sum.z.2017)),
                                     rep("2022",length(sum.z.2022))
  )
  
  total.plot.occ.sites.2017.2022 =  ggplot(stack.2017.2022.df, aes(x = Occupied, colour = Survey.Year, fill = Survey.Year)) +
    geom_density(bw=1)+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    xlim(25,100)
  
  total.plot.occ.sites.2017.2022
  
  prob.occ.sites.2017.2022.df = stack.2017.2022.df
  prob.occ.sites.2017.2022.df$Occupied = prob.occ.sites.2017.2022.df$Occupied/length(comb.GRID_ID$GRID_ID)
  
  prob.plot.occ.sites.2017.2022 =  ggplot(prob.occ.sites.2017.2022.df, aes(x = Occupied, colour = Survey.Year, fill = Survey.Year)) +
    geom_density(bw=0.01)+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    xlim(0.2,0.6)
  
  
  
  prob.plot.occ.sites.2017.2022
  
  arrange <- ggarrange(total.plot.occ.sites.2017.2022, prob.plot.occ.sites.2017.2022, ncol = 2, nrow = 1,
                       labels=c("Total Occupied Sites (Sites = 175)","Probability Occupied Sites (Sites = 175)"))
  
  ggsave("./plots/plot.2017.2022.same.sites.comparison.png", arrange,device="png",
         units="in",width=15,height=5)
  
  
# Probability of occupancy by survey
  aggregate(prob.occ.sites.2017.2022.df$Occupied, by=list(prob.occ.sites.2017.2022.df$Survey.Year), quantile, probs=c(0.025,0.5,0.975))
  
  # Get difference probability
  diff.sum.z = sum.z.2022- sum.z.2017
  
  # Probability of a loss of number of occupied sites 
  length(which(diff.sum.z<0))/length(diff.sum.z)
  
  hist(diff.sum.z)  
  
  # Estimated number of lossed sites
  quantile(diff.sum.z,prob=c(0.025,0.5,0.975))
  
  ###############################  
  # Plot spatial differences across years
  
  # Differences in mean occupancy by site
  occ.diff.by.site =  apply(occ.compar.2022,1,mean)-apply(occ.compar.2017,1,mean)
  
  occ.diff.by.site = data.frame(occ.diff= occ.diff.by.site,
                                states = states,
                                Albers_X= Albers_X,
                                Albers_Y= Albers_Y)  
  
  plot.occ.site.diff.2017.2022 =  ggplot(occ.diff.by.site, 
                                         aes(x = Albers_X, 
                                             y = Albers_Y,
                                             colour = occ.diff
                                         )
  ) +
    scale_colour_viridis_c()+
    geom_point(shape = 15, size = 2) +
    theme_light()
  
  plot.occ.site.diff.2017.2022
  
  ggsave("./plots/plot.occ.diff.comparison.sites.2017.2022.png", plot.occ.site.diff.2017.2022,device="png",
         units="in",width=10,height=10)
  
  
  ##############
  # NOTE  
  # These 175 rows
  dim(occ.compar.2017)
  dim(occ.compar.2022)
  
  #Correspond to GRID_ID
  comb.GRID_ID$GRID_ID
  # and correspond to state ID
  states
  #And XY 
  Albers_X
  Albers_Y
  
  
  
  # Next, I want to compare state by state
  
  #2017 model results first
  states.mcmc.2017 = matrix(NA, nrow=ncol(occ.compar.2017),ncol=length(unique(states)))
  colnames(states.mcmc.2017) = unique(states)
  sites.per.states.2017 = rep(NA,length(unique(states)))
  
  for(i in 1:length(unique(states))){
    index=which(states==colnames(states.mcmc.2017)[i])
    temp=occ.compar.2017[index,]
    states.mcmc.2017[,i]=apply(temp,2,sum)
    sites.per.states.2017[i]=length(index)
  }
  
  
  #2022 model results first
  states.mcmc.2022 = matrix(NA, nrow=ncol(occ.compar.2022),ncol=length(unique(states)))
  colnames(states.mcmc.2022) = unique(states)
  sites.per.states.2022 = rep(NA,length(unique(states)))
  
  for(i in 1:length(unique(states))){
    index=which(states==colnames(states.mcmc.2022)[i])
    temp=occ.compar.2022[index,]
    states.mcmc.2022[,i]=apply(temp,2,sum)
    sites.per.states.2022[i]=length(index)
  }
  
  dim(states.mcmc.2017)
  dim(states.mcmc.2022)
  
  st.stack.2017 = stack(data.frame(states.mcmc.2017))
  st.stack.2022 = stack(data.frame(states.mcmc.2022))
  st.stack.2017.2022 = rbind(st.stack.2017,st.stack.2022)
  st.stack.2017.2022$survey = c(rep("2017",nrow(st.stack.2017)),
                                rep("2022",nrow(st.stack.2022))
  )
  colnames(st.stack.2017.2022)= c("Occupied.Sites","State","Survey")
  
  
  WY.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Wyoming"),] 
  ID.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Idaho"),]
  MT.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Montana"),]
  WA.plot = st.stack.2017.2022[which(st.stack.2017.2022$State=="Washington"),]
  
  
  MT.plot.compare =  ggplot(MT.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Montana")+
    xlim(0,100)
  MT.plot.compare
  
  ID.plot.compare =  ggplot(ID.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Idaho")+
    xlim(0,100)
  
  
  WY.plot.compare =  ggplot(WY.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Wyoming")+
    xlim(0,100)
  
  WA.plot.compare =  ggplot(WA.plot, aes(x = Occupied.Sites,  fill = Survey)) +
    geom_density(bw=1,alpha = 0.5)+
    scale_colour_manual(values = c("#709AE1","#FD7446"),aesthetics=c("fill"))+
    ylab("Probability Density")+
    xlab("Sites Occupied")+ 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey",linetype = "dashed"),#panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))+
    ggtitle("Washington")+
    xlim(0,100)
  
  
  #output ggplot
  arrange <- ggarrange(MT.plot.compare, ID.plot.compare,WY.plot.compare, WA.plot.compare, ncol = 2, nrow = 2)
  ggsave("./plots/plot.state.comparison.sites.2017.2022.png", arrange,device="png",
         units="in",width=10,height=10)
  
  
  save.image("./outputs/compar.2017.2022.surveyed.site.RData")  
  