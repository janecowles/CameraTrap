library(sp)
library(raster)
library(rgeos)

#this was 67590 -- see what it is now?
df <- fread("C:/Users/cowl0037/Downloads/EOTW_DataOutput_JCproc21Mar.csv")
str(df)
setDT(df)
# dfcam <- df[,.(MeanCount=mean(modecountbysp),TotalCount=sum(modecountbysp),MonthlyAmbientTemperature=mean(AmbientTemperature)),by=.(season,site,speciesID,YEAR,MONTH,Cam_num,site_fixed,Easting,Northing)]
dfcam <- df[,.(CAMERAMeanCount=mean(modecountbysp),CAMERATotalCount=sum(modecountbysp)),by=.(site,Cam_num,site_fixed,Easting,Northing)]
# head(dfcam)

# ggplot(dfcam,aes(Easting,Northing,color=TotalCount))+geom_point()+facet_grid(MONTH~speciesID)

dfcam_xy <- dfcam[,c("Easting","Northing")]
dfcam_sp <- SpatialPointsDataFrame(coords=dfcam_xy,data=dfcam[,-c("Easting","Northing")],proj4string = CRS("+init=epsg:32615"))
plot(dfcam_sp)
bufflocs <- gBuffer(dfcam_sp,width=1000)


# df_xy <- df[,c("Easting","Northing")]
# df_sp <- SpatialPointsDataFrame(coords=df_xy,data=df,proj4string = CRS("+init=epsg:32615"))
# plot(df_sp)
#####NLCD data

nlcd_mn <- raster("C:/Users/cowl0037/Downloads/tif_biota_landcover_nlcd_mn_2016/NLCD_2016_Land_Cover.tif")
nlcd_class <- fread("C:/Users/cowl0037/Downloads/NLCD_Class.csv")
nlcd_class$NLCD_NUM <- as.character(nlcd_class$NLCD_NUM)

nlcd_nearby <- crop(nlcd_mn,bufflocs)
plot(nlcd_mn)
plot(nlcd_nearby)
plot(dfcam_sp,add=T,pch=0)

nlcd_pts <- as.data.frame(rasterToPoints(nlcd_nearby))
nlcd_pts$NLCD_2016_Land_Cover<-as.character(nlcd_pts$NLCD_2016_Land_Cover)
nlcd_pts <- merge(nlcd_class,nlcd_pts,by.x="NLCD_NUM",by.y="NLCD_2016_Land_Cover",all.y=T)

dfcam_sp$NLCD <- as.character(raster::extract(nlcd_mn,dfcam_xy))
dfcam_nlcd<- as.data.frame(dfcam_sp)
dfcam_nlcd <- merge(dfcam_nlcd,nlcd_class,by.x="NLCD",by.y="NLCD_NUM",all.x=T)

ggplot(nlcd_pts,aes(x,y,color=NLCD_DESCRIPTION))+geom_point()+geom_point(data=dfcam_nlcd,aes(Easting,Northing),color="black",size=3)+coord_fixed()

ggplot(dfcam_nlcd,aes(Easting,Northing,color=NLCD_DESCRIPTION))+geom_point(size=4)+geom_text(aes(label=Cam_num),nudge_x = 150)+scale_color_manual(values=c("red","brown","black","grey","orange","forestgreen","purple","cyan","lightgreen","blue"))

df_fin <- merge(df,dfcam_nlcd,by=c("Easting","Northing","site","Cam_num","site_fixed"),all.x=T)
fwrite(df_fin,"C:/Users/cowl0037/Downloads/EOTW_DataOutputwHabitat_JCproc21Mar.csv")
