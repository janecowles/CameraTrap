#jane cowles
#merging together subjects, classifications, and exif data. Exif data downloaded by FI, image_rename files downloaded from MSI (waiting for season 4 permissions from vlad), subjects and classifications downloaded from zooniverse.
library(data.table)
library(ggplot2)
library(maptools)

# subj_info <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects_19Mar.csv")
# table(subj_info$workflow_id)
# subj_BD <- subj_info[is.na(subj_info$workflow_id)|subj_info$workflow_id==5702,]
# metadatasep <- strsplit(subj_BD$metadata,":")
# subj_BD$Images <- sapply(metadatasep,`[`,9)
# subj_BD<-subj_BD[!is.na(subj_BD$Images),]
# subj_BD$Images  <- gsub("[^[:alnum:][:blank:]+./_-]", "", subj_BD$Images )
# subj_BD$Images  <- gsub(".JPG",".JPG:",subj_BD$Images )
# imglist <- strsplit(subj_BD$Images,":")
# subj_BD$IMG1WLOCATION <- sapply(imglist,`[`,1)
# subj_BD$IMG2WLOCATION <- sapply(imglist,`[`,2)
# subj_BD$IMG3WLOCATION <- sapply(imglist,`[`,3)
# 
# s_c_info <- strsplit(subj_BD$IMG1WLOCATION,"/")
# subj_BD$IMG1NAME <- sapply(strsplit(subj_BD$IMG1WLOCATION,"/"),`[`,4)
# subj_BD$IMG2NAME <- sapply(strsplit(subj_BD$IMG2WLOCATION,"/"),`[`,4)
# subj_BD$IMG3NAME <- sapply(strsplit(subj_BD$IMG3WLOCATION,"/"),`[`,4)
# 
# s_c_infosplit <- strsplit(subj_BD$IMG1NAME,"_")
# subj_BD$season <- sapply(s_c_infosplit,`[`,1)
# subj_BD$site <- sapply(s_c_infosplit,`[`,2)
# 
# rm(metadatasep,imglist,s_c_info,s_c_infosplit)

zoo_dl <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-classifications_19Mar.csv")

zoo_dl$subject_data2 <- gsub("\"", "", zoo_dl$subject_data)


subjsplit <- strsplit(zoo_dl$subject_data2,"#original_images:\\[")
imgsplit <- strsplit(sapply(subjsplit,`[`,2),",")
zoo_dl$IMG1WLOCATION <- gsub("]}}","",sapply(imgsplit,`[`,1))
zoo_dl$IMG1NAME <- sapply(strsplit(zoo_dl$IMG1WLOCATION,"/"),`[`,4)
zoo_dl$IMG2WLOCATION <- gsub("]}}","",sapply(imgsplit,`[`,2))
zoo_dl$IMG2NAME <- sapply(strsplit(zoo_dl$IMG2WLOCATION,"/"),`[`,4)
zoo_dl$IMG3WLOCATION <- gsub("]}}","",sapply(imgsplit,`[`,3))
zoo_dl$IMG3NAME <- sapply(strsplit(zoo_dl$IMG3WLOCATION,"/"),`[`,4)

siteseason_info <- strsplit(zoo_dl$IMG1NAME,"_")
zoo_dl$season <- sapply(siteseason_info,`[`,1)
zoo_dl$site <- sapply(siteseason_info,`[`,2)

subjsplit2 <- strsplit(sapply(subjsplit,`[`,1),",")
zoo_dl$retirement_reason <- gsub("}","",sapply(strsplit(sapply(subjsplit2,`[`,8),"retirement_reason:"),`[`,2))

#### in the process of making subject dataset unnecessary

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
rm(annot_split,annot_split0,annot_split2,annot_split3,annot_split4,annot_split5,annot_split6,subjsplit,subjsplit2)

#WARNING-- now that animal or not workflow is in here, need to cut that out for current analyses
zoo_dl <- zoo_dl[zoo_dl$workflow_id==5702,]
zoo_dl <- zoo_dl[!is.na(zoo_dl$IMG1NAME),]

#this is for later when we want to maybe cut out images that only have 1 classification, or something of that sort.... re-merged in below
# zoo_dlTOT2 <-zoo_dl[, .(NumberofClassifications=length(speciesID)), by=.(subject_ids)]
zoo_dlTOT <-zoo_dl[, .(NumberofClassifications=length(speciesID)), by=.(subject_ids,IMG1WLOCATION,IMG1NAME, IMG2WLOCATION, IMG2NAME, IMG3WLOCATION, IMG3NAME, season, site)]

#take the top classification for each subject
zoo_dl_SUMMARY <- zoo_dl[, .N, by=.(subject_ids,speciesID)][order(-N), .(speciesID=speciesID[1L]), keyby=subject_ids]

myfunnum <- function(x) as.numeric(names(table(x))[which.max(table(x))])
myfun <- function(x) as.character(names(table(x))[which.max(table(x))])

# zoo_dl_sp <- zoo_dl[paste(zoo_dl$subject_ids,zoo_dl$speciesID)%in%paste(zoo_dl_SUMMARY$subject_ids,zoo_dl_SUMMARY$speciesID),]
system.time(zoo_dl_COUNTBYSP <- zoo_dl[, .(mediancountbysp=median(HowMany),modecountbysp=myfunnum(HowMany), Antlers=myfun(Antlers),Young=myfun(Young), BisonNumberEating=myfun(BisonNumberEating), LyingDown=myfun(LyingDown), Standing=myfun(Standing), Moving=myfun(Moving), Eating=myfun(Eating), Interacting=myfun(Interacting)), by=.(subject_ids,speciesID)])
head(zoo_dl_COUNTBYSP)


zoo_dl_sum2 <- merge(zoo_dl_SUMMARY,zoo_dl_COUNTBYSP,by=c("subject_ids","speciesID"),all.x=T,all.y=F)

rm(zoo_dl_SUMMARY,zoo_dl_COUNTBYSP)

class_df <- merge(zoo_dl_sum2,zoo_dlTOT,by="subject_ids",all.x=T)
# #merging subject info with classification info
# info <- merge(subj_BD,zoo_dl_sum2,by.x=c("subject_id"),by.y="subject_ids")
# #that was by subject, now make long by image so we can merge with other files
# infolong <- melt(info,id.vars=c("season","site","subject_id","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting"),measure.vars=c("IMG1NAME","IMG2NAME","IMG3NAME"),variable.name = "img123",value.name = "ImageName")
# #i always forget how to make two variables long at the same time... so doing it an ugly way
# infolong2 <- melt(info,id.vars=c("season","site","subject_id","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting"),measure.vars=c("IMG1WLOCATION","IMG2WLOCATION","IMG3WLOCATION"),variable.name = "imgnumloc",value.name = "ImageNameWLOCATION")
# infolong$ImageNameWLOCATION <- infolong2$ImageNameWLOCATION
# #data.tables are better
# setDT(infolong)
# 
# # fwrite(infolong,"C:/Users/cowl0037/Downloads/Exif_Merge/DataProcessing_midpoint_23Mar.csv")
# # infolong <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/DataProcessing_midpoint_23Mar.csv")
# table(duplicated(infolong$ImageNameWLOCATION))
# rm(infolong2)

exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/Merged_EXIF_MatchingPicNames.csv")
names(exifdat)

combdat <- merge(class_df,exifdat,by.x="IMG1WLOCATION",by.y="new_path",all.x=T)
names(combdat)
#you could use zoo_dlTOT to subset subject_ids out that have a classification count of less than a certain value. I'll just add that in right here and you can decide later ... (merge 3)


combdat$date_taken <- as.POSIXct(combdat$date_taken,format= "%Y:%m:%d %H:%M:%S")
combdat$DATE <- as.Date(combdat$date_taken, tz = "")
combdat$YEAR <- as.numeric(format(combdat$DATE,"%Y"))
combdat$MONTH <- as.numeric(format(combdat$DATE,"%m"))

library(stringr)
combdat$Cam_num <- as.numeric(gsub("\\D", "", combdat$site))
combdat$Cam_num_pad <- str_pad(combdat$Cam_num,width=3,side=c("left"),pad="0")
combdat$Cam_letters <- gsub("[^a-zA-Z]", "", combdat$site)

combdat$site_fixed <- ifelse(combdat$Cam_letters=="CB",paste0("C",combdat$Cam_num_pad,"B"),paste0("C",combdat$Cam_num_pad))

df <- combdat[!combdat$speciesID%in%c("humanorvehicle","nothingthere")]
df$modecountbysp <- as.numeric(ifelse(df$modecountbysp==1120,11,df$modecountbysp))
# fwrite(df,"C:/Users/cowl0037/Downloads/Exif_Merge/OUTPUT_EXIFandSPID_update20Mar.csv")
# 
# ggplot(df[df$Antlers%in%c("YES","NO"),],aes(x=Antlers))+geom_bar()+facet_wrap("MONTH")
# ggplot(df[df$speciesID%in%c("deer")&df$Antlers%in%c("YES","NO"),],aes(date_taken,modecountbysp))+geom_jitter()+geom_smooth()+facet_wrap("Antlers")

unique(df$Cam_num)
wolfpts <- fread("C:/Users/cowl0037/Downloads/WolfPts_UTM.csv")
wolfpts$SiteID_num <- as.numeric(gsub("\\D", "", wolfpts$SiteID))
ggplot(wolfpts,aes(Easting,Northing,color=SiteID_num))+geom_point()

ALLDAT <- merge(df,wolfpts,by.x="Cam_num",by.y="SiteID_num",all.x=T,all.y=F)
# ggplot(ALLDAT,aes(Easting,Northing,color=Cam_num))+geom_point()+geom_point(data=ALLDAT[ALLDAT$speciesID=="bison"],aes(Easting,Northing),size=4,color="red")+labs(title="bison")
# ggplot(ALLDAT,aes(Easting,Northing,color=Cam_num))+geom_point()+geom_point(data=ALLDAT[ALLDAT$speciesID=="blackbear"],aes(Easting,Northing),size=4,color="green")+labs(title="black bear")

# table(ALLDAT$site[ALLDAT$speciesID=="bison"])
# names(ALLDAT)
# paste(colnames(ALLDAT),collapse="\",\"")
ALLDAT_NEC <- ALLDAT[,c("season","site_fixed","Easting","Northing","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting","NumberofClassifications","DATE","YEAR","MONTH","subject_id","date_taken","MoonPhase","AmbientTemperature","Cam_num","SiteID","new_path","old_path","FileName","img123","ImageName","AmbientTemperatureFahrenheit")]
  
fwrite(ALLDAT,"C:/Users/cowl0037/Downloads/EOTW_DataOutput_JCproc24Mar.csv")
