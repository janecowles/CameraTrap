library(data.table)
library(ggplot2)

df_fin <- fread("C:/Users/cowl0037/Downloads/EOTW_DataOutputwHabitat_JCproc26Mar.csv")
summary(df_fin)
table(df_fin$site_fixed,df_fin$season)
tapply(df_fin$NumberofClassifications,df_fin$season,mean)
df_fin$date_taken <- as.POSIXct(df_fin$date_taken,format= "%Y-%m-%dT%H:%M:%SZ")

ggplot(df_fin[df_fin$Cam_num<50,],aes(date_taken,site_fixed,color=season))+geom_line(size=4)
ggplot(df_fin[df_fin$Cam_num>49,],aes(date_taken,site_fixed,color=season))+geom_line(size=4)

speciesbyhabitat <- as.data.frame(table(df_fin$biome,df_fin$season,df_fin$speciesID))
str(speciesbyhabitat)
colnames(speciesbyhabitat) <- c("Habitat","Season","Species","Freq")

ggplot(speciesbyhabitat,aes(Habitat,Species,fill=log(Freq)))+geom_tile()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap("Season",nrow=1)

speciesbyhabitat <- as.data.frame(table(df_fin$biome,df_fin$MONTH,df_fin$speciesID))
str(speciesbyhabitat)
colnames(speciesbyhabitat) <- c("Habitat","Month","Species","Freq")

ggplot(speciesbyhabitat,aes(Habitat,Species,fill=log(Freq)))+geom_tile()+theme(axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap("Month",nrow=1)


ggplot(speciesbyhabitat[speciesbyhabitat$Var2%in%c("deer","bison","turkey","otherbird"),],aes(Var1,Var2,fill=log(Freq)))+geom_tile()+theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggplot(speciesbyhabitat[!speciesbyhabitat$Var2%in%c("deer","bison","turkey","otherbird"),],aes(Var1,Var2,fill=log(Freq)))+geom_tile()+theme(axis.text.x = element_text(angle = 30, hjust = 1))


sort(table(df_fin$speciesID))

ggplot(df_fin[df_fin$speciesID=="bison"],aes(Easting,Northing))+geom_point()
ggplot(df_fin,aes(x=MONTH))+geom_bar()+facet_wrap("speciesID",ncol=3,scales="free_y",strip.position = "right")

ggplot(df_fin[df_fin$speciesID=="bison"],aes(Easting,Northing,color=NLCD_DESCRIPTION))+geom_point()+facet_wrap("MONTH")
ggplot(df_fin[df_fin$speciesID=="sandhillcrane"],aes(Easting,Northing,color=NLCD_DESCRIPTION))+geom_point()+facet_wrap("MONTH")
ggplot(df_fin[df_fin$speciesID=="blackbear"],aes(Easting,Northing,color=NLCD_DESCRIPTION))+geom_point()+facet_wrap("MONTH")


ggplot(df_fin,aes(x=NLCD_DESCRIPTION))+geom_bar()+facet_wrap("speciesID")
