###########################################
# Goal: Get 2017 and 2022 survey data (formatted for stocc package)
#       and convert to to ubms/unmarked R package frame
#
# Author: Brian Gerber
# Last Modified: 11/19/2025

# FYI, using base R and clear non-efficient coding for longevity/transferability purposes

##############################################
# Setup Environment
  rm(list=ls())

#######################
# Load data and visualize
  load("./outputs/wolv2017.spatial.data")
  load("./outputs/wolv2022.spatial.data")

  
##############################
# unmarked/ubms requires a site by occasion dataframe .
# Need to change from long format to wide format

  # 2017 data 
  colnames(visitData2017)
  
  GRID_ID.2017 = unique(visitData2017$GRID_ID)
  occs.2017 = unique(visitData2017$occ)
  
  visit.2017.data.unmarked=data.frame(matrix(NA, nrow=length(GRID_ID.2017),ncol=length(occs.2017)))
  
  for(i in 1:length(GRID_ID.2017)){
    index=which(visitData2017$GRID_ID==GRID_ID.2017[i])
    visit.2017.data.unmarked[i,visitData2017$occ[index]]=visitData2017$eh[index]
  }
  
  #need to have no NA's
  which(is.na(visit.2017.data.unmarked))
  rownames(visit.2017.data.unmarked)=GRID_ID.2017
  colnames(visit.2017.data.unmarked)=occs.2017

  
  head(visit.2017.data.unmarked)
  
  site.covs.2017 = data.frame(matrix(NA,nrow=nrow(visit.2017.data.unmarked),ncol=9))
  colnames(site.covs.2017) = c("propCopeland",
                               "propCopelandInman",
                               "meanHumanMod_2021",
                               "meanNDVI_2017",
                               "ClusterCount",
                               "STATE",
                               "GRID_ID",
                               "SiteType2017",
                               "Grouping1"
                               )
  

  for(i in 1:nrow(visit.2017.data.unmarked)){
    index=which(rownames(visit.2017.data.unmarked)[i]==habData2017$GRID_ID)
    site.covs.2017$propCopeland[i] = habData2017$propCopeland[index]
    site.covs.2017$propCopelandInman[i] = habData2017$propCopelandInman[index]
    site.covs.2017$meanHumanMod_2021[i] = habData2017$meanHumanMod_2021[index]
    site.covs.2017$meanNDVI_2017[i] = habData2017$meanNDVI_2017[index]
    site.covs.2017$ClusterCount[i] = habData2017$ClusterCount[index]
    site.covs.2017$STATE[i] = habData2017$STATE[index]
    site.covs.2017$GRID_ID[i] = habData2017$GRID_ID[index]
    site.covs.2017$SiteType2017[i] = habData2017$SiteType2017[index]
    site.covs.2017$Grouping1[i] = habData2017$Grouping1[index]

  }
  
head(site.covs.2017)
which(is.na(site.covs.2017))
dim(site.covs.2017)
dim(visit.2017.data.unmarked)  

##############################
# 2022 data 
  colnames(visitData2022)
  
  GRID_ID.2022 = unique(visitData2022$GRID_ID)
  occs.2022 = unique(visitData2022$occ)
  
  visit.2022.data.unmarked=data.frame(matrix(NA, nrow=length(GRID_ID.2022),ncol=length(occs.2022)))
  
  for(i in 1:length(GRID_ID.2022)){
    index=which(visitData2022$GRID_ID==GRID_ID.2022[i])
    visit.2022.data.unmarked[i,visitData2022$occ[index]]=visitData2022$eh[index]
  }
  
  #need to have no NA's
  which(is.na(visit.2022.data.unmarked))
  
  
  rownames(visit.2022.data.unmarked)=GRID_ID.2022
  colnames(visit.2022.data.unmarked)=occs.2022
  
#Now grab site covariates

colnames(habData2022)
  
  site.covs.2022=data.frame(matrix(NA,nrow=nrow(visit.2022.data.unmarked),ncol=13))
  colnames(site.covs.2022)= c("propBurned2016_2021",
                              "propBurned2001_2015",
                              "propBurned2001_2021",
                              "meanHumanMod_2021",
                              "meanNDVI_2022",
                              "propCopelandInman",
                              "ClusterCount",
                              "SDMayDepth_2022",
                              "meanMayDepth_2022",
                              "SDMaySWE_2022",
                              "meanMaySWE_2022",
                              "Grouping1",
                              "SiteType2022"
                              )
  
# Re orgnize covariate into the data frame site.covs.2022  
  for(i in 1:nrow(visit.2022.data.unmarked)){
    index=which(rownames(visit.2022.data.unmarked)[i]==habData2022$GRID_ID)
    site.covs.2022$propBurned2016_2021[i] = habData2022$propBurned2016_2021[index]
    site.covs.2022$propBurned2001_2015[i] = habData2022$propBurned2001_2015[index]
    site.covs.2022$propBurned2001_2021[i] = habData2022$propBurned2001_2021[index]
    
    site.covs.2022$SiteType2022[i] = habData2022$SiteType2022[index]
    
    site.covs.2022$meanHumanMod_2021[i] = habData2022$meanHumanMod_2021[index]
    site.covs.2022$meanNDVI_2022[i] = habData2022$meanNDVI_2022[index]
    site.covs.2022$propCopelandInman[i] = habData2022$propCopelandInman[index]
    site.covs.2022$ClusterCount[i] = habData2022$ClusterCount[index]
    site.covs.2022$SDMayDepth_2022[i] = habData2022$SDMayDepth_2022[index]
    site.covs.2022$meanMayDepth_2022[i] = habData2022$meanMayDepth_2022[index]
    site.covs.2022$SDMaySWE_2022[i] = habData2022$SDMaySWE_2022[index]
    site.covs.2022$meanMaySWE_2022[i] = habData2022$meanMaySWE_2022[index]
    site.covs.2022$STATE[i] = habData2022$STATE[index]
    site.covs.2022$GRID_ID[i] = habData2022$GRID_ID[index]
    site.covs.2022$Grouping1[i] = habData2022$Grouping1[index]
    site.covs.2022$SiteType2022[i] = habData2022$SiteType2022[index]
    
  }
  
head(site.covs.2022)

table(site.covs.2022$SiteType2022)
  
# Need to relabel a few
  index=which(site.covs.2022$SiteType2022=="" |site.covs.2022$SiteType2022=="Bait_Lure" )
  site.covs.2022$SiteType2022[index]="Bait"

#######################
  
# Put outputs together for non-spatial modeling
  wolv2017.non.spatial=list(visit.2017.data.unmarked=visit.2017.data.unmarked,
                            site.covs.2017 = site.covs.2017
                            )
  
  wolv2022.non.spatial = list(visit.2022.data.unmarked = visit.2022.data.unmarked,
                              site.covs.2022 = site.covs.2022
                              )
  save(wolv2017.non.spatial,file="./outputs/wolv2017.non.spatial.data")
  save(wolv2022.non.spatial,file="./outputs/wolv2022.non.spatial.data")

  
