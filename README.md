# A repository for:

### Gerber et al. manuscript titled, "Monitoring wolverine (Gulo gulo) distribution across the western United States for regional and national assessment "

## Author

This repository and code were created by Brian D. Gerber (USGS, CSU; brian.gerber@colostate.edu). Data inputs were curated by Jacob S. Ivan (Colorado Parks and Wildlife; jake.ivan@state.co.us).

---

## Links to different parts of the readme file

1. [What's in this repository?](#whats-in-this-repository)
2. [The working directory](#the-working-directory)
3. [Workflows in this repository](#workflows-in-this-repository)
	1. [Summary of workflows](#Summary-of-workflows)
	2. [Wolverine 2017 Spatial Occupancy Workflow](#Wolverine-2017-Spatial-Occupancy-Workflow)
	3. [Wolverine 2022 Spatial Occupancy Workflow](#Wolverine-2022-Spatial-Occupancy-Workflow)
	4. [Wolverine 2017-2022 Spatial Occupancy Comparison Workflow](#Wolverine-2017-2022-Spatial-Occupancy-Comparison-Workflow)
	5. [Wolverine 2022 Non-Spatial Occuapncy Modeling Workflow](#Wolverine-2022-Non-Spatial-Occuapncy-Modeling-Workflow)
	5. [RMarkdown](#Rmarkdown)


## What's in this repository?

This repository stores the data and code relevant to the manuscript 'Monitoring wolverine (Gulo gulo) distribution across the western United States for regional and national assessment'.

The code fits several occupancy models to wolverine detection / non-detection data from multi-state surveys coordinated by teh Western Association of Fish and Wildlife Agencies (WAFWA) Forest Carnivore Subcommittee. Members of this 
committee are all authors of the manuscript.

Note that the data here does not include the original camera trap images or the original processed outputs. The data is only the detection / non-detection data and site covariate information relevant for spatial and non-spatial occupancy. 
**Importantly, the analyses do not replicate the spatial occupancy results in the manuscript exactly because the spatial locations shared are rounded to the nearest 10 km**.


[Back to table of contents ⤒](#a-repository-for)



## The working directory

---

For all scripts in this repository, we assume you have set the working directory as the folder that houses the entire repository. All files that are read in or scripts that are run are made relative to this central directory.

To start, you can open the R project file `Wolverine-WAFWA-2025.Rproj`.

Overall, this repository contains 8 subfolders:

1) The **data** folder includes detection/non-detection data and site covariate for occupancy analyses; these were curated by Jake Ivan of Colorado Parks and Wildlife. 
5) The **outputs** folder includes R object files and RData files produced from the workflow.
6) The **plots** folder will contain plots outputted from the code scripts.
7) The **R** folder includes R scripts for processing or summarizing data, fitting models, and processing results.

[Back to table of contents ⤒](#a-repository-for)


## Workflows in this repository

---

There are three fundamental workflows. 

### Summary of workflows

There are two wolverine surveys (2017 and 2022) and subsequently two data sets.

There are two spatial occupancy analyses, one for each survey.

There is only one non-spatial occupancy analysis, for the 2022 survey. The 2017 analysis was done by Lukacs et al. 2020 and it not recreated here. 


[Back to table of contents ⤒](#a-repository-for)

### Wolverine 2017 Spatial Occupancy Workflow

1) The file `data.summarizing.2017.r` reads in the appropriate input files and creates an .RData file (`wolv2017.spatial.data`) which is used as input for all analyses of the 2017 survey data.
2) The primary spatial occupancy model fitting using the 2017 data is done via file `fit.survey1.2017.spatial.occ.r`. The model object that contains the 2017 spatial occupancy model is saved in object `sp.occ2017.3` and will be located in the folder, `outputs`.
4) The spatial occupancy model is also fit in file `fit.survey1.2017.spatial.occ.convergence.eval.r` to evaluate parameter convergence (via Gelman-Rubin diagnostic) using multiple chains.
5) Some model results are examined in file `results.summarize.spatial.occ.2017.r`

### Wolverine 2022 Spatial Occupancy Workflow

1) The file `data.summarizing.2022.r` reads in the appropriate input files and  creates an .RData file (`wolv2022.spatial.data`) which is used as input for all analyses of the 2022 survey data.
2) The primary spatial occupancy model fitting is done via file `fit.survey2.2022.spatial.occ.r`. The model object that contains the 2022 spatial occupancy model is save in object `sp.occ2022.gr.hab.bait` and wil be located in the folder, `outputs`.
4) The spatial occupancy model is also fit in file `fit.survey2.2022.spatial.occ.convergence.eval.r` to evaluate parameter convergence (via Gelman-Rubin diagnostic) using multiple chains.
5) Some model results are examined in file `results.summarize.spatial.occ.2022.r`


### Wolverine 2017-2022 Spatial Occupancy Comparison Workflow

1) The file `results.spatial.occ.comparison.2017.2022.r` aligns the results from the two surveys and summarizes results.

### Wolverine 2022 Non-Spatial Occuapncy Modeling Workflow

1) The survey data for both 2017 and 2022 are reorganized for non-spatial occupancy modeling; this is done in file `data.mgmt.spatial.to.nonspatial.frame.r`; note that
the 2017 data are not fit with a non-spatial occupancy model here.
2) Non-spatial occupancy models  (for inference purposes) are fit using in the file `fit.survey2.2022.non.spatial.occ.ubms.r`.


[Back to table of contents ⤒](#a-repository-for)