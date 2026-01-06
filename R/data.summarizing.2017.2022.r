###########################################
# Goal: Compare 2017 and 2022 detections
#
# Author: Brian Gerber
# Last Modified: 11/19/2025

##############################################

# Setup environment
  rm(list=ls())
  library(sf)
  library(USA.state.boundaries)
  library(ggplot2)
  library(ggnewscale)

# load data objects
  load(file="./outputs/wolv2017.visitData.only")
  load(file="./outputs/wolv2022.visitData.only")

# What GRID_IDs are shared?
  merge.2017.2022 = merge(visitData2017,
                        visitData2022, by = "GRID_ID"
                          )  

head(merge.2017.2022)


# Summarize detections by grid id for 2017
  dets.2017 = aggregate(merge.2017.2022$eh.x, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=sum)
  dets.2022 = aggregate(merge.2017.2022$eh.y, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=sum)

  Albers_X = aggregate(merge.2017.2022$Albers_X.x, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=unique)
  Albers_Y = aggregate(merge.2017.2022$Albers_Y.x, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=unique)

  Grouping1 = aggregate(merge.2017.2022$Grouping1.y, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=unique)

  state = aggregate(merge.2017.2022$stabb.x, by=list(GRID_ID=merge.2017.2022$GRID_ID), FUN=unique)

  dets.2017$x[which(dets.2017$x>0)]=1
  dets.2022$x[which(dets.2022$x>0)]=1

  dets.comb=data.frame(dets.2017,dets.2022[,2],Albers_X[,2],Albers_Y[,2],Grouping1$x,state$x)

  head(dets.comb)
  colnames(dets.comb)=c("GRID_ID","dets.2017","dets.2022","Albers_X","Albers_Y","Grouping1","State")
  
  boundary=st_transform(state_boundaries_wgs84,crs=5070)
  these.states=boundary['NAME']$NAME%in%c("Montana","Wyoming","Washington","Idaho")
  lim.states=boundary['NAME'][these.states,]

# plot first    
  state.plot=  ggplot(lim.states) +
    geom_sf(aes(),linewidth=2) #+

  dets.comb$dets.2017=factor(dets.comb$dets.2017)
  dets.comb$dets.2022=factor(dets.comb$dets.2022)


#NOTE THAT THE SPATIAL LOCATIONS ARE NOT ACCURATE AS THEY HAVE BEEN ROUNDED

# plot detections 
plot.locs.dets =  state.plot + new_scale_colour()+
  geom_point(data = dets.comb, 
             aes(x = Albers_X, 
                 y = Albers_Y,
                 colour = dets.2017),
             shape = 15, size = 2) +
  scale_colour_manual(values = c("grey", "black"))+
  ggtitle("Surveyed 2017")

plot.locs.dets


# plot detections
plot.locs.dets =  state.plot + new_scale_colour()+
  geom_point(data = dets.comb, 
             aes(x = Albers_X, 
                 y = Albers_Y,
                 colour = dets.2022),
             shape = 15, size = 2) +
  scale_colour_manual(values = c("grey", "black"))+
  ggtitle("Surveyed 2017")

plot.locs.dets

# PLot change in detections

dets.comb$det.change=factor(as.integer(dets.comb$dets.2017)-1-(as.integer(dets.comb$dets.2022)-1))

# Which sites had a detection in both years
temp=as.integer(dets.comb$dets.2017)-1+(as.integer(dets.comb$dets.2022)-1)
index=which(temp==2)

levels(dets.comb$det.change)=c("-1","0","1","2")

dets.comb$det.change[index]="2"

plot.locs.dets =  state.plot + new_scale_colour()+
  geom_point(data = dets.comb, 
             aes(x = Albers_X, 
                 y = Albers_Y,
                 colour = det.change,
                 fill = det.change),
             shape = 15, size = 2) +
  scale_colour_manual(values = c("white","black","orange","red"))+
  ggtitle("")+ labs(color='Detections') 



plot.locs.dets =plot.locs.dets+  scale_fill_discrete(labels = c("Detection in only 2022", 
                                                "No detection either year",
                                                "Detection in 2017 and not in 2022",
                                                "Detection in both years")
                                     ) 

plot.locs.dets

ggsave("./plots/plot.det.change.2017.2022.png", plot.locs.dets,device="png",
       units="in",width=10,height=10)  

# -1 = detected in only 2022
# 0 = no detections in either year
# 1 = detection in 2017 but not 2022
# 2 = detection in both years

table(dets.comb$det.change, dets.comb$State)

