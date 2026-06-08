# Import system modules and check license
import arcpy
import datetime

if arcpy.CheckExtension("Spatial") == "Available":
  arcpy.CheckOutExtension("Spatial")
else:
  print ("Spatial analyst license unavailable!")

import os
import sys
from arcpy import env
from arcpy.sa import *

errormsg = """Configuring the python environment failed."""
tempobjects = []

# Set environment settings
homefolder = "C:/Users/ivanj/Documents/Ivan/PROJECTS/Wolverine_Multistate_Monitoring/2021 Survey/GIS"
gdb = homefolder + "/WolverineCovars2021.gdb"
env.workspace = gdb
arcpy.env.overwriteOutput = "TRUE"

# Reproject SNODAS rasters (Depth & SWE)
print ("Reprojecting all layers...")
snodass = ["Depth_20170301", "Depth_20170501", "Depth_20180301","Depth_20180501",
               "Depth_20190301", "Depth_20190501", "Depth_20200301", "Depth_20200501",
               "Depth_20210301", "Depth_20210501", 
               "SWE_20170301", "SWE_20170501", "SWE_20180301","SWE_20180501",
               "SWE_20190301", "SWE_20190501", "SWE_20200301", "SWE_20200501",
               "SWE_20210301", "SWE_20210501"]
for snodas in snodass:
    print ("  Reprojecting snodas rasters: " + snodas)
    arcpy.management.ProjectRaster(
    in_raster=snodas,
    out_raster=snodas + "_Albers",
    out_coor_system='PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
    resampling_type="NEAREST",
    cell_size="923.372593080525 923.428954711062",
    geographic_transform="NAD_1983_To_WGS_1984_1",
    Registration_Point=None,
    in_coor_system='GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]',
    vertical="NO_VERTICAL")
print ("Reprojecting snodas rasters: Done!")

# Reproject NDVI rasters
ndvis = ["tin_2010w", "tin_2011w", "tin_2012w", "tin_2013w", "tin_2014w", "tin_2015w",
                "tin2016", "tin2017w", "tin_2018w", "tin_2020w"]
for ndvi in ndvis:
    print ("  Reprojecting ndvi rasters: " + ndvi)
    arcpy.management.ProjectRaster(
    in_raster=ndvi,
    out_raster=ndvi + "_Albers",
    out_coor_system='PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
    resampling_type="NEAREST",
    cell_size="250 250",
    geographic_transform=None,
    Registration_Point=None,
    in_coor_system='PROJCS["Sphere_ARC_INFO_Lambert_Azimuthal_Equal_Area",GEOGCS["GCS_Sphere_ARC_INFO",DATUM["D_Sphere_ARC_INFO",SPHEROID["Sphere_ARC_INFO",6370997.0,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Azimuthal_Equal_Area"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",-100.0],PARAMETER["latitude_of_origin",45.0],UNIT["Meter",1.0]]',
    vertical="NO_VERTICAL")
print ("Reprojecting ndvi rasters: Done!")

# Reproject fire_YLB raster
print ("  Reprojecting fire_YLB raster")
arcpy.management.ProjectRaster(
    in_raster="fire_YLB",
    out_raster="fire_YLB" + "_Albers",
    out_coor_system='PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
    resampling_type="NEAREST",
    cell_size="30 30",
    geographic_transform="WGS_1984_(ITRF00)_To_NAD_1983",
    Registration_Point=None,
    in_coor_system='PROJCS["unknown",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137.0,298.257223563]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
    vertical="NO_VERTICAL")
print ("Reprojecting fire_YLB raster: Done!")

# Reproject HumanMod_US raster
#print ("  Reprojecting HumanMod raster")
#arcpy.management.ProjectRaster(
#    in_raster="HumanMod_US",
#    out_raster="HumanMod_US" + "_Albers",
#    out_coor_system='PROJCS["USA_Contiguous_Albers_Equal_Area_Conic_USGS_version",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",23.0],UNIT["Meter",1.0]]',
#    resampling_type="NEAREST",
#    cell_size="225 225",
#    geographic_transform=None,
#    Registration_Point=None,
#    in_coor_system='PROJCS["Albers_Conic_Equal_Area",GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Albers"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",-96.0],PARAMETER["Standard_Parallel_1",29.5],PARAMETER["Standard_Parallel_2",45.5],PARAMETER["Latitude_Of_Origin",37.5],UNIT["Meter",1.0]]',
#    vertical="NO_VERTICAL")
#print ("Reprojecting HumanMod_US raster: Done!")

# Reclassify fire_YLB (year last burned) into a 0,1 raster for burned in the last 5 years or burned in the last 20 years
print ("Reclassifying fire_YLB (year last burned)")
print ("  Calculating burned 2016-2021")
arcpy.ddd.Reclassify(in_raster="fire_YLB_Albers",reclass_field="VALUE",
    remap="1984 0;1985 0;1986 0;1987 0;1988 0;1989 0;1990 0;1991 0;1992 0;1993 0;1994 0;1995 0;1996 0;1997 0;1998 0;1999 0;2000 0;2001 0;2002 0;2003 0;2004 0;2005 0;2006 0;2007 0;2008 0;2009 0;2010 0;2011 0;2012 0;2013 0;2014 0;2015 0;2016 1;2017 1;2018 1;2019 1;2020 1;2021 1;2022 0;NODATA 0;",
    out_raster="Burned2016_2021_Albers",missing_values=0)
print ("  Calculating burned 2001-2021")
arcpy.ddd.Reclassify(in_raster="fire_YLB_Albers",reclass_field="VALUE",
    remap="1984 0;1985 0;1986 0;1987 0;1988 0;1989 0;1990 0;1991 0;1992 0;1993 0;1994 0;1995 0;1996 0;1997 0;1998 0;1999 0;2000 0;2001 1;2002 1;2003 1;2004 1;2005 1;2006 1;2007 1;2008 1;2009 1;2010 1;2011 1;2012 1;2013 1;2014 1;2015 1;2016 1;2017 1;2018 1;2019 1;2020 1;2021 1;2022 0;NODATA 0;",
    out_raster="Burned2001_2021_Albers",missing_values=0)
print ("  Calculating burned 2001-2015")
arcpy.ddd.Reclassify(in_raster="fire_YLB_Albers",reclass_field="VALUE",
    remap="1984 0;1985 0;1986 0;1987 0;1988 0;1989 0;1990 0;1991 0;1992 0;1993 0;1994 0;1995 0;1996 0;1997 0;1998 0;1999 0;2000 0;2001 1;2002 1;2003 1;2004 1;2005 1;2006 1;2007 1;2008 1;2009 1;2010 1;2011 1;2012 1;2013 1;2014 1;2015 1;2016 0;2017 0;2018 0;2019 0;2020 0;2021 0;2022 0;NODATA 0;",
    out_raster="Burned2001_2015_Albers",missing_values=0)
print ("Reclassifying fire_YLB: Done!")

# Convert CSPH to raster for inclusion in loop below
print ("Converting CSPH polygon to raster")
arcpy.conversion.PolygonToRaster(in_features="Combined_Snow_Primary_Habitatdslv2_Albers", value_field="OBJECTID", out_rasterdataset="CSPH_raster_Albers_temp", cellsize=30, build_rat="BUILD")
arcpy.ddd.Reclassify(in_raster="CSPH_raster_Albers_temp",reclass_field="VALUE",remap="1 1;NODATA 0;",out_raster="CSPH_raster_Albers",missing_values=0)
arcpy.management.Delete(in_data="CSPH_raster_Albers_temp")
print ("Converting CSPH polygon to raster: Done!")

# Set locals for executing zonal statistics
now = datetime.datetime.now()
datestr = now.strftime("%b_%d_%Y")
sr = arcpy.SpatialReference("USA_Contiguous_Albers_Equal_Area_Conic_USGS_version")
ftdouble = "DOUBLE"
ftlong = "LONG"
arcpy.management.CopyFeatures(in_features="C:/Users/ivanj/Documents/Ivan/PROJECTS/Wolverine_Multistate_Monitoring/WolverineSurvey_OfficialSpatialData.gdb\WAFWA_WolverineSamplingFrame_2022_Albers_N770",
                              out_feature_class="WolverineProcessedCovars2022")
arcpy.management.DeleteField(in_table="WolverineProcessedCovars2022",drop_field="GRIDSIZEKM;STATE;AREA_SQKM;Latitude;Longitude;GRTS_No;Sample;Surveyed2017;Surveyed2022;SiteType2017;SiteType2022;Grouping1;Grouping2")
arcpy.management.AddField(in_table="WolverineProcessedCovars2022",field_name="GRID_ID_Text",field_type="TEXT")
arcpy.management.CalculateField(in_table="WolverineProcessedCovars2022",field="GRID_ID_Text",expression="!GRID_ID!",expression_type="PYTHON3")
arcpy.management.MakeFeatureLayer(in_features="WolverineProcessedCovars2022", out_layer="Sample_Frame_lyr")
selectlyr = "Sample_Frame_lyr"

print ("Calculating zonal means...")
errormsg = """An error occurred during the calculations."""
inputrasters = ["Depth_20170301", "Depth_20170501", "Depth_20180301","Depth_20180501",
               "Depth_20190301", "Depth_20190501", "Depth_20200301", "Depth_20200501",
               "Depth_20210301", "Depth_20210501", 
               "SWE_20170301", "SWE_20170501", "SWE_20180301","SWE_20180501",
               "SWE_20190301", "SWE_20190501", "SWE_20200301", "SWE_20200501",
               "SWE_20210301", "SWE_20210501",
               "tin_2010w", "tin_2011w", "tin_2012w", "tin_2013w", "tin_2014w", "tin_2015w",
               "tin2016", "tin2017w", "tin_2018w", "tin_2020w",
               "Burned2016_2021", "Burned2001_2021", "Burned2001_2015", "HM_CONUSv2_2021", "CSPH_raster"]
data = "DATA"
mean = "MEAN"
std = "STD"
jointype = "KEEP_ALL"
exprtype = "PYTHON"
fieldObjectID = "GRID_ID_Text"
expr = "!ID!"

# loop through rasters to calculate zonal stats (mean)
for inputraster in inputrasters:
    print ("  Calculating mean " + inputraster)
    calcfield = "mean" + inputraster
    arcpy.AddField_management(selectlyr, calcfield, ftdouble)
    raster = inputraster + "_Albers"
    stats = raster + "Stats"
    expr = "!" + stats + "." + mean + "!"
    # calculate stats
    arcpy.Delete_management(stats)
    #arcpy.gp.ZonalStatisticsAsTable_sa(selectlyr, fieldObjectID, raster, stats, data, mean)
    arcpy.sa.ZonalStatisticsAsTable(in_zone_data="Sample_Frame_lyr",zone_field=fieldObjectID,in_value_raster=raster,out_table=stats,ignore_nodata="DATA",statistics_type="MEAN")
    #tempobjects.append(stats)
    # transfer calculated value to polygon layer
    arcpy.management.AddJoin(in_layer_or_view="Sample_Frame_lyr",in_field=fieldObjectID,join_table=stats,join_field=fieldObjectID,join_type="KEEP_ALL")
    arcpy.CalculateField_management(selectlyr, calcfield, expr, exprtype)
    arcpy.RemoveJoin_management(selectlyr, stats)
print ("Calculating zonal means: Done!")

print ("Computing clusters...")
#print ("Start: " datetime.datetime.now())
arcpy.stats.SpatiallyConstrainedMultivariateClustering(in_features=selectlyr, output_features="ClusterOutput",
                                                       analysis_fields="meanCSPH_raster", spatial_constraints="CONTIGUITY_EDGES_ONLY", number_of_clusters=50)
print ("  Summarizing number of cells in each cluster")
arcpy.analysis.Statistics(in_table="ClusterOutput",out_table="ClusterOutputTable",statistics_fields="CLUSTER_ID COUNT",case_field="CLUSTER_ID",concatenation_separator="")
arcpy.management.JoinField(in_data="ClusterOutput",in_field="CLUSTER_ID",join_table="ClusterOutputTable",join_field="CLUSTER_ID",fields="COUNT_CLUSTER_ID")
print ("  Joining cluster counts to selectlyr")
arcpy.management.JoinField(in_data=selectlyr,in_field="OBJECTID",join_table="ClusterOutput",join_field="SOURCE_ID",fields="COUNT_CLUSTER_ID")
print ("Cluster computation: Done!")

print ("Exporting Table to .csv")
arcpy.conversion.ExportTable(selectlyr, "C:/Users/ivanj/Documents/Ivan/PROJECTS/Wolverine_Multistate_Monitoring/FinalAnalysisFiles/WolverineProcessedCovars2022.csv")
print ("DONE!!!")
