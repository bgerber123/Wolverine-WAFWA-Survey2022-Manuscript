
####################################################################################################
##                                                                                                ##
##  Rearrange all visit and habitat files for 'stocc analysis so everything is consistent         ##
##               Add covariates for each year to each file                                        ##
##                           Jake Ivan 7-15-25                                                    ##
##  (Uses visit and habitat files from Brian Gerber; covariate files from Jake's arcpy scripts)   ##
##                                                                                                ##
####################################################################################################

library(dplyr)
library(tidyverse)

#setwd("C:/Users/ivanj/Documents/Ivan/PROJECTS/Wolverine_Multistate_Monitoring/FinalAnalysisFiles")

  #Read in raw data
        raw_visitData_2017 <- read.csv("VisitDataWolv2017_5-14-25.csv") #visit data 2017 from Brian Gerber
        raw_visitData_2022 <- read.csv("visitDataWolv2022_5-14-25.csv") #visit data 2022 from Brian Gerber
        raw_habData_2017 <- read.csv("habDataWolv2017_5-14-25.csv")     #habitat data 2017 from Brian Gerber
        raw_habData_2022 <- read.csv("habDataWolv2022_5-14-25.csv")     #habitat data 2022 from Brian Gerber
        
        raw_SamplingFrame2022 <- read.csv("WAFWA_WolverineSamplingFrame_2022_Albers_N770.csv")  #Sampling Frame data from Jake Ivan official wolverine file geodatabase
        raw_SamplingFrame2017 <- read.csv("WAFWA_WolverineSamplingFrame_2017_Albers_N633.csv")  #Sampling Frame data from Jake Ivan official wolverine file geodatabase
            
        CovarData2017 <- readRDS("camscovs.rds")
            names(CovarData2017)[names(CovarData2017)=="prop.cov.snomod"] <- "propCopeland"
            names(CovarData2017)[names(CovarData2017)=="prop.cov.csph"] <- "propCopelandInman_2017"
            names(CovarData2017)[names(CovarData2017)=="mean_human"] <- "meanHumanMod_2017"
            names(CovarData2017)[names(CovarData2017)=="meanNDVI"] <- "meanNDVI_2017"
            names(CovarData2017)[names(CovarData2017)=="clustercells"] <- "ClusterCount_2017"
        CovarData2022 <- read.csv("WolverineProcessedCovars2022.csv")
            names(CovarData2022)[names(CovarData2022)=="meanCSPH_raster"] <- "propCopelandInman"
            names(CovarData2022)[names(CovarData2022)=="meanBurned2016_2021"] <- "propBurned2016_2021"
            names(CovarData2022)[names(CovarData2022)=="meanBurned2001_2021"] <- "propBurned2001_2021"
            names(CovarData2022)[names(CovarData2022)=="meanBurned2001_2015"] <- "propBurned2001_2015"
            names(CovarData2022)[names(CovarData2022)=="meanHM_CONUSv2_2021"] <- "meanHumanMod_2021"
            names(CovarData2022)[names(CovarData2022)=="COUNT_CLUSTER_ID"] <- "ClusterCount"
        CovarData2022 <- CovarData2022 %>% select(-(ends_with("0301"))) %>% #I guess we decided not to look at values in March
            select(-c(Shape_Area, Shape_Length, GRID_ID_Text))
        CovarData2022 <- CovarData2022 %>% rowwise() %>%
            mutate(meanNDVI_2022 = mean(c_across(starts_with("meantin")), na.rm=TRUE),
                 meanNDVI_2017 = mean(c_across(meantin_2010w:meantin2016),na.rm=TRUE),
                 meanMayDepth_2022 = mean(c_across(starts_with("meanDepth")), na.rm=TRUE),
                 SDMayDepth_2022 = sd(c_across(starts_with("meanDepth")), na.rm=TRUE),
                 meanMaySWE_2022 = mean(c_across(starts_with("meanSWE")), na.rm=TRUE),
                 SDMaySWE_2022 = sd(c_across(starts_with("meanSWE")), na.rm=TRUE)
            ) 
        
        #Hard code zeros for 16 cells WA surveyed but didn't have time to get into CPW PhotoWarehouse
            WA_missing_cells <- rep(c(14275,14650,15270,15643,15645,16015,16138,16387,  #cell 14525 was sampled but camera failed so removed from this list
                                          16391,16638,16756,16758,16759,16763,16890),each=4)
            WA_missing_occ <- rep(c(1,2,3,4),15)
            WA_missing_eh <- rep(0,60)
            WA_Fixes_2022 <- data.frame(WA_missing_cells,WA_missing_occ,WA_missing_eh) 
                names(WA_Fixes_2022) <- c("GRID_ID", "occ", "eh")

  #Prepare each year's visit and habitat data for joining
        visitData_2017 <- raw_visitData_2017 %>% select(GRID_ID, occ, eh)
        visitData_2022 <- raw_visitData_2022 %>% select(GRID_ID, occ, eh) %>% filter(GRID_ID!=14524) #According to WA folks, cell 14524 was not surveyed
            visitData_2022 <- rbind(visitData_2022, WA_Fixes_2022)   #add back zeros for the 16 cells WA surveyed but didn't have time to include in CPW PhotoWarehouse
        habData_2017 <- raw_habData_2017 %>% select(site, GRID_ID)
        habData_2022 <- raw_habData_2022 %>% select(GRID_ID)
        SamplingFrame2022 <- raw_SamplingFrame2022 %>% 
                                mutate(stabb = recode(STATE,
                                      "Colorado" = "CO",
                                      "Idaho" = "ID",
                                      "Montana" = "MT",
                                      "Oregon" = "OR",
                                      "Utah" = "UT",
                                      "Washington" = "WA",
                                      "Wyoming" = "WY")) %>%
                                select(GRID_ID, GRTS_No,
                                       STATE, stabb, Albers_X, Albers_Y, 
                                       Latitude, Longitude, 
                                       Sample, 
                                       Surveyed2017, Surveyed2022,
                                       SiteType2017, SiteType2022,
                                       Grouping1, Grouping2)
       
              
    #Make one table that contains all of the same data necessary for both the visit and habitat data to join      
        ThingToJoin_2017 <- left_join(habData_2017, SamplingFrame2022, by="GRID_ID")
            ThingToJoin_2017 <- left_join(ThingToJoin_2017, CovarData2017, join_by(site==LocationName)) %>% select(-c(site, propCopelandInman_2017,ClusterCount_2017,meanHumanMod_2017,meanNDVI_2017))
            UpdatedHabCluster <- CovarData2022 %>% select(c(GRID_ID,propCopelandInman,meanHumanMod_2021,meanNDVI_2017,ClusterCount)) #replace the habitat and cluster variables from original file with these re-calculated ones
            ThingToJoin_2017 <- left_join(ThingToJoin_2017, UpdatedHabCluster)
        ThingToJoin_2022 <- left_join(SamplingFrame2022, CovarData2022) %>%
                            select(-c(starts_with(c("meanDepth", "meanSWE","meantin")))) #%>%  #get rid of interim means, leaving only the grand mean and SD across years
                            #drop_na()   #The drop_na() piece removes 137 lines and takes the file from N=770 (2022 survey) back to only cells surveyed in the 4-state area (N=633)
              
    #Joint that one table to each of the base data files so each has the same data in the same format          
        visitDataWolv2017 <- left_join(visitData_2017, ThingToJoin_2017, by="GRID_ID") 
        visitDataWolv2022 <- left_join(visitData_2022, ThingToJoin_2022, by="GRID_ID") #%>% drop_na()  #Brings this back to only the 4-state survey
        habDataWolv2017 <- left_join(habData_2017, ThingToJoin_2017, by="GRID_ID") %>% select(-site)
        habDataWolv2022 <- left_join(habData_2022, ThingToJoin_2022, by="GRID_ID") #%>% drop_na() #Brings this back to only the 4-state survey
    
    #Write each out to .csv        
        write.csv(visitDataWolv2017, paste("visitDataWolv2017_",format(Sys.Date(), "%m-%d-%y"),".csv", sep=""), row.names = FALSE)
        write.csv(visitDataWolv2022, paste("visitDataWolv2022_",format(Sys.Date(), "%m-%d-%y"),".csv", sep=""), row.names = FALSE)
        write.csv(habDataWolv2017, paste("habDataWolv2017_",format(Sys.Date(), "%m-%d-%y"),".csv", sep=""), row.names = FALSE)
        write.csv(habDataWolv2022, paste("habDataWolv2022_",format(Sys.Date(), "%m-%d-%y"),".csv", sep=""), row.names = FALSE)
        
  