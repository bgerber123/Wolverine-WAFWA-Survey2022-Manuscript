########################
# Goal: Load the 2017 wolverine survey (first multi-state survey) data to summarize and plot
#
# Author: Brian D. Gerber
# Last Modified 8/18/2025
#
#######################
# Setup Environment
rm(list=ls())

#######################
# Load data from csv (provided by Jake Ivan; 7/17/25).

habData2017 = read.csv("./data/habDataWolv2017_07-16-25.csv")
visitData2017 = read.csv("./data/visitDataWolv2017_07-16-25.csv")

# Look at dataframes  
head(visitData2017)
head(habData2017)

#Note that habData2017:
#  - GRID_ID is the ID of the sampling frame
#  - GRTS_No is the random draw in the generalized random tesselation sampling
#  - STATE and stabb  are state designations based on cell/site centroid 
#  - X-Y location Albers and Lat/Long. XY LOCATIONS HAVE BEEN ROUNDED TO 10 KM; lat/long rounded to 0.1 degrees ~ 11 km
#  - Sample indicates whether it was randomly drawn using GRTS. 
#  - Surveyed 2017 and Surveyed 2022 indicate the actual sites that were surveyed
#  - SiteType2017 and SiteType2022 indicate whether there was bait at the site (e.g.,deer carcass) or a lure (scent dispensor)
#  - Grouping1 and Grouping2 groups site/cell locations; these are important for 2022. For 2017, these are equivalent. 
#  - PropCopeland is a covariate of the prediction of proportion in a cell of habitat. This
#    is called 'habitat' in the original 2020 manuscript. 
#  - meanHumanMod_2017 is a covariate of human modification. 
#  - meanNDVI_2017  is a covariate of NDVI. 
#  - propCopelandandInman is a prediction of the proportion of wolverine habitat in a cell. This is
#    the way the sampling frame was defined. 
#  - ClusterCount  is a covariate of how many cells/sites are connected.  


#Note that visitData2017:
#  - occ indicates the sampling occasion (1 month occasions)
#  - eh indicates the encounter history. 0 = nondetection and 1 = detection of wolverine.

# The visit data is longer because the encounter history is in the long format and not the wide format.
# Each row is a detection, so multiple rows (4) are of the same site/cell. Each detection is a sampling
# occasion of 1 month.
dim(visitData2017)
dim(habData2017)

# This is the number of cells/sites in the sampling frame - these should agree
nrow(habData2017)
length(unique(habData2017$GRID_ID))


#Check to see if GRID_ID matches in the two dataframes  
all(visitData2017$GRID_ID%in%habData2017$GRID_ID)


# Make a 'site.id' only for the spatial modeling. 
habData2017$site.id.for.occ = 1:nrow(habData2017)    
#Now, I need to loop through that visitData to assign the same site.id    
visitData2017$site.id.for.occ=NA
GRID_ID.unique=unique(habData2017$GRID_ID)    
for(i in 1:length(GRID_ID.unique)){
  index1=which(visitData2017$GRID_ID==GRID_ID.unique[i])
  if(length(index1>0)){
    visitData2017$site.id.for.occ[index1]=  habData2017$site.id.for.occ[i]
  }
}  
# Check to make sure site ID's in visit data are the same as in habdata    
all(visitData2017$site.id.for.occ%in%habData2017$site.id.for.occ)

# The GRID_IDs's and site ID's match, which means we can use either
# in the spatial occupancy model. 

visitData2017$site.id.for.occ  

# For modeling, the most important outputs are visitData2017 and habData2017
# Save these as data objects  
save(visitData2017,file="./outputs/wolv2017.visitData.only")
save(habData2017,file="./outputs/wolv2017.habData.only")

#####################################################  
# save data object of the environment
save.image("./outputs/wolv2017.spatial.data")
#####################################################

