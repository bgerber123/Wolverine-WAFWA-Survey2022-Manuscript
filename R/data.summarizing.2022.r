########################
# Goal: Load the 2022 wolverine survey (second multi-state survey) data to summarize and plot
#
# Author: Brian D. Gerber
# Last Modified 7/17/2025
#
#######################
# Setup Environment
  rm(list=ls())

#######################

# Inputs provided by Jake Ivan
  habData2022 = read.csv("./data/habDataWolv2022_07-16-25.csv")
  visitData2022 = read.csv("./data/visitDataWolv2022_07-31-25.csv")
  
  # Look at dataframes  
  head(visitData2022)
  head(habData2022)
  
  
#Note that habData2022:
  #  - GRID_ID is the sampling frame ID
  #  - GRTS_No is the random draw in the generalized random tessellation sampling
  #  - STATE and stabb state designations by the centroid of the cell
  #  - X-Y location Albers and Lat/Long. XY LOCATIONS HAVE BEEN ROUNDED TO 10 KM; lat/long rounded to 0.1 degrees ~ 11 km
  #  - Sample indicates whether it was randomly drawn using GRTS. 
  #  - Surveyed 2017 and Surveyed 2022 indicate the actual sites that were surveyed
  #  - SiteType2017 and SiteType2022 indicate whether there was bait at the site (e.g., deer carcass) or a lure (scent dispensor)
  #  - Grouping1 and Grouping2 groups site/cell locations; these are important for 2022. For 2017, these are equivalent. 
  #  - Grouping1 and Grouping2 groups site/cell locations. Grouping1 separates out mtn ranges and 
  #    disconnected areas. Grouping2 is more specific to mtn ranges that have no detections and have 
  #    had no detections before
  #  - propBurned2016_2021 : Covarite of proportion burned
  #  - propBurned2001_2021  covaraite of proportion burned
  #  - meanHumanMod_2022: human modification covariate 
  #  - propCopelandInman is a prediction of the proportion of wolverine habitat in a cell. This is
  #    the way the sampling frame was defined. 
  #  - ClusterCount: a covariate of how many cells/sites are connected. This was redone in ArcPro by Jake
  #    after some conversations on the original covaraite not making much sense
  #  - meanNDVI_2022: covariate of NDVI
  #  - meanMayDepth_2022: covariate of snowpack
  #  - SDMayDepth_2022: covariate of snowpack
  #  - meanMaySWE_2022: covariate of snopack
  #  - SDMaySWE_2022: covariate of snowpack
  
  
  #Note that visitData2022:
  #  - occ indicates the sampling occasion (1 month)
  #  - eh indicates the encounter history. 0 = nondetection and 1 = detection of wolverine.
  
  
  # This is the number of cells/sites in the sampling frame - these should agree
  nrow(habData2022)
  length(unique(habData2022$GRID_ID))
  
    # For the spatial.occupancy model to work, we need to have the site labels
  # in visitData match those in habData.
  
  #Check to see if GRID_ID matches in the two dataframes  
  all(visitData2022$GRID_ID%in%habData2022$GRID_ID)

  
  # Create a 'site.id' only for the spatial modeling. In passed iterations,
  # there were issues with using GRID_ID in the function spatial.occupancy. 
  habData2022$site.id.for.occ = 1:nrow(habData2022)
  
  #Now, I need to loop through that visitData to assign the same site.id    
  visitData2022$site.id.for.occ=NA
  GRID_ID.unique=unique(habData2022$GRID_ID)    
  for(i in 1:length(GRID_ID.unique)){
    index1=which(visitData2022$GRID_ID==GRID_ID.unique[i])
    if(length(index1>0)){
      visitData2022$site.id.for.occ[index1]=  habData2022$site.id.for.occ[i]
    }
  }  
  # Check to make sure site ID's in visit data are the same as in habdata    
  all(visitData2022$site.id.for.occ%in%habData2022$site.id.for.occ)
  all(visitData2022$GRID_ID%in%habData2022$GRID_ID)
  
  # The GRID_IDs's and site ID's match, which means we can use either
  # in the spatial occupancy model.
  
  visitData2022$site.id.for.occ  


# Look at the baitlure covariate  
  table(visitData2022$SiteType2022)
  unique(visitData2022$SiteType2022)
    
# Bait_lure and Bait are the same  
  
# Change bait_lure to be bait. and missing values
  visitData2022$SiteType2022[which(visitData2022$SiteType2022=="Bait_Lure" | visitData2022$SiteType2022=="")] = "Bait"
  table(visitData2022$SiteType2022)
  unique(visitData2022$SiteType2022)
  
  
# look at grouping variable
  table(visitData2022$Grouping1,visitData2022$STATE)
  
# Create a new grouping - cascades vs RockiesUintas - this is not ultimately used
  visitData2022$Grouping3=visitData2022$Grouping1
  index=which(visitData2022$Grouping3=="NorthCascades" |
                visitData2022$Grouping3=="SouthCascades")
  visitData2022$Grouping3[index]="Cascades"
  index=which(visitData2022$Grouping3!="Cascades")
  visitData2022$Grouping3[index]="Rockies"
  
  
  habData2022$Grouping3=habData2022$Grouping1
  index=which(habData2022$Grouping3=="NorthCascades" |
                habData2022$Grouping3=="SouthCascades")
  habData2022$Grouping3[index]="Cascades"
  index=which(habData2022$Grouping3!="Cascades")
  habData2022$Grouping3[index]="Rockies"
  
  
  # For modeling, the most important outputs are visitData2022 and habData2022
  # Save these as data objects  
  save(visitData2022,file="./outputs/wolv2022.visitData.only")
  save(habData2022,file="./outputs/wolv2022.habData.only")
  
  
  #####################################################  
  # save data object of the environment
  save.image("./outputs/wolv2022.spatial.data")
  #####################################################
  
  
  