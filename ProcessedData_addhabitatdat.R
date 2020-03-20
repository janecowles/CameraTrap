library(sp)
df <- fread("C:/Users/cowl0037/Downloads/EOTW_DataOutput_ThruS4_JCproc18Mar.csv")
df <- df[,c("season","site","speciesID","mediancountbysp","modecountbysp","sum","DATE","YEAR","MONTH","subject_id","date_taken","MoonPhase","AmbientTemperature","Cam_num","site_fixed","SiteID","GRID/ROAD","GPS_X_PRE","GPS_Y_PRE","GPS_X_ACT","GPS_Y_ACT","new_path","old_path","FileName","img123","ImageName")]
df$LAT <- df$GPS_X_PRE
df$LON <- df$GPS_Y_PRE
str(df)
setDT(df)
dfcam <- df[,.(MeanCount=mean(modecountbysp),TotalCount=sum(modecountbysp),MonthlyAmbientTemperature=mean(AmbientTemperature)),by=.(season,site,speciesID,YEAR,MONTH,Cam_num,site_fixed,GPS_X_PRE,GPS_Y_PRE,GPS_X_ACT,GPS_Y_ACT,LAT,LON)]
head(dfcam)

ggplot(dfcam,aes(LON,LAT,color=TotalCount))+geom_point()+facet_grid(MONTH~speciesID)
ggplot(dfcam[dfcam$speciesID=="bison"],aes(LON,LAT,color=TotalCount))+geom_point()+theme_linedraw()

length(df$ImageName[df$speciesID=="bison"&df$LAT<45.43])


df_xy <- dfcam[,c("GPS_Y_PRE","GPS_X_PRE")]
df_sp <- SpatialPointsDataFrame(coords=df_xy,data=dfcam,proj4string = CRS("+init=epsg:4326"))
df_sp <- spTransform(df_sp,"+init=epsg:32615")

#### THIS SAYS BISON BUT IS CLEARLY DEER _ S3_C012_B1_IMG_1258.JPG DID I DO THIS WRONG??

#i don't understand why these are broken up into twos when there are clearly 3 of each??? (418-420 is a series)
zoo_dl$subject_data[grepl("S3_C050_B1_IMG_0418.JPG",zoo_dl$subject_data)][1]
zoo_dl$subject_data[grepl("S3_C050_B1_IMG_0420.JPG",zoo_dl$subject_data)][1]
