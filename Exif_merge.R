#jane cowles
#merging together subjects, classifications, and exif data. Exif data downloaded by FI, image_rename files downloaded from MSI (waiting for season 4 permissions from vlad), subjects and classifications downloaded from zooniverse.
library(data.table)
library(ggplot2)
library(maptools)

tmp <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects_19Mar.csv")
# tmp <- fread("~/Downloads/cedar-creek-eyes-on-the-wild-subjects.csv")
trans <- tmp
metadatasep <- strsplit(trans$metadata,":")
trans$Images <- sapply(metadatasep,`[`,9)
trans<-trans[!is.na(trans$Images),]
trans$Images  <- gsub("[^[:alnum:][:blank:]+./_-]", "", trans$Images )
trans$Images  <- gsub(".JPG",".JPG:",trans$Images )
imglist <- strsplit(trans$Images,":")
trans$IMG1WLOCATION <- sapply(imglist,`[`,1)
trans$IMG2WLOCATION <- sapply(imglist,`[`,2)
trans$IMG3WLOCATION <- sapply(imglist,`[`,3)

s_c_info <- strsplit(trans$IMG1WLOCATION,"/")
trans$IMG1NAME <- sapply(strsplit(trans$IMG1WLOCATION,"/"),`[`,4)
trans$IMG2NAME <- sapply(strsplit(trans$IMG2WLOCATION,"/"),`[`,4)
trans$IMG3NAME <- sapply(strsplit(trans$IMG3WLOCATION,"/"),`[`,4)

s_c_infosplit <- strsplit(trans$IMG1NAME,"_")
trans$season <- sapply(s_c_infosplit,`[`,1)
trans$site <- sapply(s_c_infosplit,`[`,2)

rm(metadatasep,imglist,s_c_info,s_c_infosplit,tmp)

zoo_dl <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-classifications_19Mar.csv")

zoo_dl$annotations2 <- gsub("\"", "", zoo_dl$annotations )
annot_split0 <- strsplit(zoo_dl$annotations2,",filters:")
annot_split2 <- strsplit(sapply(annot_split0,`[`,1),",answers:")
annot_split <- strsplit(sapply(annot_split2,`[`,1),"choice:")
zoo_dl$speciesID <- tolower(sapply(annot_split,`[`,2))

annot_split3 <- strsplit(sapply(annot_split2,`[`,2),"HOWMANY:")
annot_split4 <- strsplit(sapply(annot_split3,`[`,2),",WHATBEHAVIORSDOYOUSEE:")
zoo_dl$Antlers <- sapply(strsplit(sapply(annot_split4,`[`,1),"ANTLERS:"),`[`,2)
zoo_dl$HowMany <- as.numeric(gsub("[^0-9.]", "",sapply(annot_split4,`[`,1)))
annot_split5 <- strsplit(sapply(annot_split4,`[`,2),",ARETHEREANYYOUNGPRESENT:")
zoo_dl$Young <- gsub("}","",sapply(strsplit(sapply(annot_split5,`[`,2),","),`[`,1))

annot_split6 <- strsplit(sapply(annot_split5,`[`,1),",IFYOUCHOSEEATINGHOWMANYBISONAREEATING:")
zoo_dl$BisonNumberEating <- as.numeric(gsub("[^0-9.]", "",sapply(annot_split6,`[`,2)))

zoo_dl$Activities <- gsub("}","",sapply(annot_split6,`[`,1))
zoo_dl$LyingDown <- ifelse(grepl("LYINGDOWN",zoo_dl$Activities),"Y",ifelse(is.na(zoo_dl$Activities),NA,"N"))
zoo_dl$Standing <- ifelse(grepl("STANDING",zoo_dl$Activities),"Y",ifelse(is.na(zoo_dl$Activities),NA,"N"))
zoo_dl$Moving <- ifelse(grepl("MOVING",zoo_dl$Activities),"Y",ifelse(is.na(zoo_dl$Activities),NA,"N"))
zoo_dl$Eating <- ifelse(grepl("EATING",zoo_dl$Activities),"Y",ifelse(is.na(zoo_dl$Activities),NA,"N"))
zoo_dl$Interacting <- ifelse(grepl("INTERACTING",zoo_dl$Activities),"Y",ifelse(is.na(zoo_dl$Activities),NA,"N"))

summary(zoo_dl)
head(zoo_dl)
# is.na.table <- function(x){table(is.na(x))}
# sapply(zoo_dl[,c("speciesID","Antlers","HowMany","Young","BisonNumberEating","Activities")],is.na.table)
###### RUNNING THIS RUINS STACKS SOMEHOW? DON'T DO IT-- sapply(zoo_dl[,c("speciesID","Antlers","HowMany","Young","BisonNumberEating","Activities")],table)

# ISSUE: some annotations contain more  than one set of answers?? - cut off anything more than the first one, for now!
# multipleann <- zoo_dl[nchar(zoo_dl$annotations2)>300,]
# head(multipleann)
rm(annot_split,annot_split0,annot_split2,annot_split3,annot_split4,annot_split5,annot_split6)

#WARNING-- now that animal or not workflow is in here, need to cut that out for current analyses
zoo_dl <- zoo_dl[zoo_dl$workflow_id==5702,]

#this is for later when we want to maybe cut out images that only have 1 classification, or something of that sort.... re-merged in below
zoo_dlTOT <-zoo_dl[, .(NumberofClassifications=length(speciesID)), by=.(subject_ids)]

#take the top classification for each subject
zoo_dl_SUMMARY <- zoo_dl[, .N, by=.(subject_ids,speciesID)][order(-N), .(speciesID=speciesID[1L]), keyby=subject_ids]

myfunnum <- function(x) as.numeric(names(table(x))[which.max(table(x))])
myfun <- function(x) as.character(names(table(x))[which.max(table(x))])

# zoo_dl_sp <- zoo_dl[paste(zoo_dl$subject_ids,zoo_dl$speciesID)%in%paste(zoo_dl_SUMMARY$subject_ids,zoo_dl_SUMMARY$speciesID),]
zoo_dl_COUNTBYSP <- zoo_dl[, .(mediancountbysp=median(HowMany),modecountbysp=myfunnum(HowMany), Antlers=myfun(Antlers),Young=myfun(Young), BisonNumberEating=myfun(BisonNumberEating), LyingDown=myfun(LyingDown), Standing=myfun(Standing), Moving=myfun(Moving), Eating=myfun(Eating), Interacting=myfun(Interacting)), by=.(subject_ids,speciesID)]
head(zoo_dl_COUNTBYSP)


zoo_dl_sum2 <- merge(zoo_dl_SUMMARY,zoo_dl_COUNTBYSP,by=c("subject_ids","speciesID"),all.x=T,all.y=F)

rm(zoo_dl_SUMMARY,zoo_dl_COUNTBYSP)


#merging subject info with classification info
info <- merge(trans,zoo_dl_sum2,by.x=c("subject_id"),by.y="subject_ids")
#that was by subject, now make long by image so we can merge with other files
infolong <- melt(info,id.vars=c("season","site","subject_id","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting"),measure.vars=c("IMG1NAME","IMG2NAME","IMG3NAME"),variable.name = "img123",value.name = "ImageName")
#i always forget how to make two variables long at the same time... so doing it an ugly way
infolong2 <- melt(info,id.vars=c("season","site","subject_id","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting"),measure.vars=c("IMG1WLOCATION","IMG2WLOCATION","IMG3WLOCATION"),variable.name = "imgnumloc",value.name = "ImageNameWLOCATION")
infolong$ImageNameWLOCATION <- infolong2$ImageNameWLOCATION
#data.tables are better
setDT(infolong)



rm(infolong2)

exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/Merged_EXIF_MatchingPicNames.csv")
names(exifdat)
combdat <- merge(exifdat,infolong,by.x="new_path",by.y="ImageNameWLOCATION")

#you could use zoo_dlTOT to subset subject_ids out that have a classification count of less than a certain value. I'll just add that in right here and you can decide later ... (merge 3)

combdat_wCOUNTS <- merge(combdat,zoo_dlTOT,by.x="subject_id",by.y="subject_ids",all.x=T)

combdat_clean <-combdat_wCOUNTS[combdat_wCOUNTS$img123=="IMG1NAME"]

combdat_clean$date_taken <- as.POSIXct(combdat_clean$date_taken,format= "%Y:%m:%d %H:%M:%S")
combdat_clean$DATE <- as.Date(combdat_clean$date_taken, tz = "")
combdat_clean$YEAR <- as.numeric(format(combdat_clean$DATE,"%Y"))
combdat_clean$MONTH <- as.numeric(format(combdat_clean$DATE,"%m"))

library(stringr)
combdat_clean$Cam_num <- as.numeric(gsub("\\D", "", combdat_clean$site))
combdat_clean$Cam_num_pad <- str_pad(combdat_clean$Cam_num,width=3,side=c("left"),pad="0")
combdat_clean$Cam_letters <- gsub("[^a-zA-Z]", "", combdat_clean$site)

combdat_clean$site_fixed <- ifelse(combdat_clean$Cam_letters=="CB",paste0("C",combdat_clean$Cam_num_pad,"B"),paste0("C",combdat_clean$Cam_num_pad))

df <- combdat_clean[!combdat_clean$speciesID%in%c("humanorvehicle","nothingthere")]
df$modecountbysp <- as.numeric(ifelse(df$modecountbysp==1120,11,df$modecountbysp))
# fwrite(df,"C:/Users/cowl0037/Downloads/Exif_Merge/OUTPUT_EXIFandSPID_update20Mar.csv")
# 
# ggplot(df[df$Antlers%in%c("YES","NO"),],aes(x=Antlers))+geom_bar()+facet_wrap("MONTH")
# ggplot(df[df$speciesID%in%c("deer")&df$Antlers%in%c("YES","NO"),],aes(date_taken,modecountbysp))+geom_jitter()+geom_smooth()+facet_wrap("Antlers")


wolfpts <- fread("C:/Users/cowl0037/Downloads/WolfPts_UTM.csv")
wolfpts$SiteID_num <- as.numeric(gsub("\\D", "", wolfpts$SiteID))
ggplot(wolfpts,aes(Easting,Northing,color=SiteID_num))+geom_point()

ALLDAT <- merge(df,wolfpts,by.x="Cam_num",by.y="SiteID_num",all.x=T,all.y=F)
# ggplot(ALLDAT,aes(Easting,Northing,color=Cam_num))+geom_point()+geom_point(data=ALLDAT[ALLDAT$speciesID=="bison"],aes(Easting,Northing),size=4,color="red")+labs(title="bison")
# ggplot(ALLDAT,aes(Easting,Northing,color=Cam_num))+geom_point()+geom_point(data=ALLDAT[ALLDAT$speciesID=="blackbear"],aes(Easting,Northing),size=4,color="green")+labs(title="black bear")

# table(ALLDAT$site[ALLDAT$speciesID=="bison"])
# names(ALLDAT)
# paste(colnames(ALLDAT),collapse="\",\"")
ALLDAT_NEC <- ALLDAT[,c("season","site","Easting","Northing","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting","NumberofClassifications","DATE","YEAR","MONTH","subject_id","date_taken","MoonPhase","AmbientTemperature","Cam_num","site_fixed","SiteID","new_path","old_path","FileName","img123","ImageName","AmbientTemperatureFahrenheit")]
  
fwrite(ALLDAT_NEC,"C:/Users/cowl0037/Downloads/EOTW_DataOutput_JCproc21Mar.csv")
