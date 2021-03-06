library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(sf)


df <- fread("C:/Users/cowl0037/Downloads/EOTW_DataOutput_JCNEWproc26Mar.csv")

setDT(df)
# dfcam <- df[,.(MeanCount=mean(modecountbysp),TotalCount=sum(modecountbysp),MonthlyAmbientTemperature=mean(AmbientTemperature)),by=.(season,site,speciesID,YEAR,MONTH,Cam_num,site_fixed,Easting,Northing)]
dfcam <- df[,.(CAMERAMeanCount=mean(modecountbysp),CAMERATotalCount=sum(modecountbysp)),by=.(Cam_num,site_fixed,Easting,Northing)]
# head(dfcam)

# ggplot(dfcam,aes(Easting,Northing,color=TotalCount))+geom_point()+facet_grid(MONTH~speciesID)

dfcam_xy <- dfcam[,c("Easting","Northing")]
dfcam_sp1 <- SpatialPointsDataFrame(coords=dfcam_xy,data=dfcam[,c("site_fixed","Cam_num","Easting","Northing")],proj4string = CRS("+init=epsg:32615"))
plot(dfcam_sp1)
dfcam_sp <- st_as_sf(dfcam_sp1)
plot(st_geometry(dfcam_sp))
### landuselandcover

# eh--ignore. Same as wolfpts
# camlocs_arc <- readOGR("C:/Users/cowl0037/Documents/EOTW_Shape/CameraLocations_ArcDL.shp")
# camlocs_arc <- spTransform(camlocs_arc,"+init=epsg:32615")
# camlocs_rel <- camlocs_arc[camlocs_arc$CameraNumb!="0",]
# plot(camlocs_rel)
# plot(dfcam_sp1,add=T,col="red")
# tstdf <- as.data.frame(camlocs_arc)

lulc <- readOGR("C:/Users/cowl0037/Documents/EOTW_Shape/landuseandcover.shp")
plot(lulc)
lulc <- spTransform(lulc,"+init=epsg:32615")
lulc <- st_as_sf(lulc)
dfcam_sp_lulc <- st_intersection(dfcam_sp,lulc)
plot(st_geometry(dfcam_sp_lulc),add=T,col="red")
camlulc_df <- as.data.frame(dfcam_sp_lulc)
camlulc_dfREL <- camlulc_df[,c("site_fixed","Cam_num","Easting","Northing","C_NUM","C_ALPHA","C_TEXT")]

pcom <- readOGR("C:/Users/cowl0037/Documents/EOTW_Shape/PlantCommunities.shp")
plot(pcom)
pcom <- spTransform(pcom,"+init=epsg:32615")
pcom <- st_as_sf(pcom)

dfcam_sp_pcom <- st_intersection(dfcam_sp,pcom)
plot(st_geometry(dfcam_sp_pcom),add=T,col="blue")

campcom_df <- as.data.frame(dfcam_sp_pcom)
campcom_dfREL <- campcom_df[,c("site_fixed","Cam_num","Easting","Northing","NPC","ENAME","LABEL")]

#### NLCD DATA FOR THREE POINTS WITH NO LULC
bufflocs <- gBuffer(dfcam_sp1,width=1000)
nlcd_mn <- raster("C:/Users/cowl0037/Downloads/tif_biota_landcover_nlcd_mn_2016/NLCD_2016_Land_Cover.tif")
nlcd_class <- fread("C:/Users/cowl0037/Downloads/NLCD_Class.csv")
nlcd_class$NLCD_NUM <- as.character(nlcd_class$NLCD_NUM)

nlcd_nearby <- crop(nlcd_mn,bufflocs)

dfcam_sp$NLCD <- as.character(raster::extract(nlcd_mn,dfcam_xy))
dfcam_nlcd<- as.data.frame(dfcam_sp)
dfcam_nlcd <- merge(dfcam_nlcd,nlcd_class,by.x="NLCD",by.y="NLCD_NUM",all.x=T)


### df cam  -- camera info

cammerge0 <- merge(dfcam,camlulc_dfREL,by=c("site_fixed","Cam_num","Easting","Northing"),all.x=T)
cammerge1 <- merge(cammerge0,campcom_dfREL,by=c("site_fixed","Cam_num","Easting","Northing"),all.x=T)
dfcam_fin <- merge(cammerge1,dfcam_nlcd,by=c("site_fixed","Cam_num","Easting","Northing"),all.x=T)
dfcam_fin$NLCD_DESCRIPTION[is.na(dfcam_fin$C_TEXT)]
table(dfcam_fin$C_TEXT,dfcam_fin$NLCD_DESCRIPTION)

dfcam_fin$biome <- ifelse(grepl("orest",dfcam_fin$C_TEXT),"forest",ifelse(grepl("rass",dfcam_fin$C_TEXT)|grepl("rairie",dfcam_fin$C_TEXT),"grassland",ifelse(grepl("swamp",dfcam_fin$C_TEXT)|grepl("marsh",dfcam_fin$C_TEXT)|grepl("fen",dfcam_fin$C_TEXT)|grepl("et meadow",dfcam_fin$C_TEXT)|grepl("pland deciduous shrubland",dfcam_fin$C_TEXT),"wetlands",ifelse(grepl("oodland",dfcam_fin$C_TEXT)|grepl("avanna",dfcam_fin$C_TEXT),"savanna",ifelse(grepl("avement",dfcam_fin$C_TEXT)|grepl("impervious",dfcam_fin$C_TEXT),"developed",ifelse(dfcam_fin$Cam_num%in%c(39,46),"wetlands",ifelse(dfcam_fin$Cam_num%in%c(31),"grassland",NA)))))))

fwrite(dfcam_fin,"C:/Users/cowl0037/Downloads/EOTW_CameraHabitat_JCproc26Mar.csv")
dfcam_fin <- fread("C:/Users/cowl0037/Downloads/EOTW_CameraHabitat_JCproc246Mar.csv")

tapply(dfcam_fin$C_TEXT,dfcam_fin$NLCD_DESCRIPTION,unique)
tapply(dfcam_fin$ENAME,dfcam_fin$NLCD_DESCRIPTION,unique)

df_fin <- merge(df,dfcam_fin,by=c("site_fixed","Cam_num","Easting","Northing"),all.x=T)
df_fin$season <- gsub("0","",df_fin$season)
df_fin$season <- gsub("S","S0",df_fin$season)
df_fin <- df_fin[,-c("site","CAMERAMeanCount","CAMERATotalCount","geometry","Matching_path","old_path","Origold_path","OrigDirectory.x", "Directory.x", "Orig_filename", "SourceFile", "FileName", "Directory.y","OrigDirectory.y", "FileModifyDate", "FileAccessDate","FileCreateDate", "DateTimeOriginal","Cam_num_pad","Cam_letters")]
fwrite(df_fin,"C:/Users/cowl0037/Downloads/EOTW_DataOutputwHabitat_JCproc26Mar.csv")

