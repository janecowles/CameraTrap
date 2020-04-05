#jane cowles
#merging together subjects, classifications, and exif data. Exif data downloaded by FI, image_rename files downloaded from MSI (waiting for season 4 permissions from vlad), subjects and classifications downloaded from zooniverse.
library(data.table)
library(ggplot2)
library(maptools)
library(stringr)

#### FUNCTIONS FOR LATER
myfunnum <- function(x) as.numeric(names(table(x))[which.max(table(x))])
myfun <- function(x) as.character(names(table(x))[which.max(table(x))])
PASTETOGETHER <- function(x) as.character(paste(unique(x),collapse="_"))



zoo_dl <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-classifications_2April.csv")
orig_n <- nrow(zoo_dl)
biodetect_n <- nrow(zoo_dl[zoo_dl$workflow_id==5702,])
#WARNING-- now that animal or not workflow is in here, need to cut that out for current analyses
###FOR MEREDITH: Did not cut out the other workflows, check how well that works in getting all images at the end.
# zoo_dl <- zoo_dl[zoo_dl$workflow_id==5702,]

#get subject information, then delete anything without image names (they are from a test set)
zoo_dl$subject_data2 <- gsub("\"", "", zoo_dl$subject_data)

subjsplit <- strsplit(zoo_dl$subject_data2,"#original_images:\\[")
imgsplit <- strsplit(sapply(subjsplit,`[`,2),",")
zoo_dl$IMG1WLOCATION <- gsub("]","",gsub("]}}","",sapply(imgsplit,`[`,1)))
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


table(is.na(zoo_dl$IMG1NAME))



#cut out anything from the test set -- can find these by deleting those with no image names
zoo_dl <- zoo_dl[!is.na(zoo_dl$IMG1NAME),]


### now to get the classification information
zoo_dl$annotations2 <- gsub("\"", "", zoo_dl$annotations )

zoo_dl$NumberOfChoices <- str_count(zoo_dl$annotations2,"choice:")
zoo_dl$IsOneNothingOrHuman <- str_count(zoo_dl$annotations2,"HUMAN")+str_count(zoo_dl$annotations2,"NOTHING")
zoo_dl$IsOneNothingOrHuman <- ifelse(zoo_dl$IsOneNothingOrHuman>0,1,0)

zoo_dl_choiceMode <- zoo_dl[, .(ModeNumberChoices=myfunnum(NumberOfChoices),SumNothingOrHuman=sum(IsOneNothingOrHuman),NumberofClassifications=length(NumberOfChoices)), by=.(IMG1WLOCATION,IMG1NAME, IMG2WLOCATION, IMG2NAME, IMG3WLOCATION, IMG3NAME, season, site)]

zoo_dl_choiceMode$ProportionContainingHumanOrNothing<-(zoo_dl_choiceMode$SumNothingOrHuman/zoo_dl_choiceMode$NumberofClassifications)

NonHumanDoubles <- zoo_dl[zoo_dl$IMG1NAME%in%zoo_dl_choiceMode$IMG1NAME[zoo_dl_choiceMode$ModeNumberChoices==2&zoo_dl_choiceMode$ProportionContainingHumanOrNothing<0.5],]

secondannot_split0 <- strsplit(NonHumanDoubles$annotations2,",filters:")
secondannot_split2 <- strsplit(sapply(secondannot_split0,`[`,2),",answers:")
secondannot_split <- strsplit(sapply(secondannot_split2,`[`,1),"choice:")
NonHumanDoubles$speciesID <- tolower(sapply(secondannot_split,`[`,2))
secondannot_split3 <- strsplit(sapply(secondannot_split2,`[`,2),"HOWMANY:")
secondannot_split4 <- strsplit(sapply(secondannot_split3,`[`,2),",WHATBEHAVIORSDOYOUSEE:")
NonHumanDoubles$Antlers <- sapply(strsplit(sapply(secondannot_split4,`[`,1),"ANTLERS:"),`[`,2)
NonHumanDoubles$HowMany <- as.numeric(gsub("[^0-9.]", "",sapply(secondannot_split4,`[`,1)))
secondannot_split5 <- strsplit(sapply(secondannot_split4,`[`,2),",ARETHEREANYYOUNGPRESENT:")
NonHumanDoubles$Young <- gsub("}","",sapply(strsplit(sapply(secondannot_split5,`[`,2),","),`[`,1))

secondannot_split6 <- strsplit(sapply(secondannot_split5,`[`,1),",IFYOUCHOSEEATINGHOWMANYBISONAREEATING:")
NonHumanDoubles$BisonNumberEating <- as.numeric(gsub("[^0-9.]", "",sapply(secondannot_split6,`[`,2)))

NonHumanDoubles$Activities <- gsub("}","",sapply(secondannot_split6,`[`,1))
NonHumanDoubles$LyingDown <- ifelse(grepl("LYINGDOWN",NonHumanDoubles$Activities),"Y",ifelse(is.na(NonHumanDoubles$Activities),NA,"N"))
NonHumanDoubles$Standing <- ifelse(grepl("STANDING",NonHumanDoubles$Activities),"Y",ifelse(is.na(NonHumanDoubles$Activities),NA,"N"))
NonHumanDoubles$Moving <- ifelse(grepl("MOVING",NonHumanDoubles$Activities),"Y",ifelse(is.na(NonHumanDoubles$Activities),NA,"N"))
NonHumanDoubles$Eating <- ifelse(grepl("EATING",NonHumanDoubles$Activities),"Y",ifelse(is.na(NonHumanDoubles$Activities),NA,"N"))
NonHumanDoubles$Interacting <- ifelse(grepl("INTERACTING",NonHumanDoubles$Activities),"Y",ifelse(is.na(NonHumanDoubles$Activities),NA,"N"))
head(NonHumanDoubles)
NonHumanDoubles$MainorDouble <-"Double"

####### back to full dataframe, extract species info
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
zoo_dl$MainorDouble <- "Main"
summary(zoo_dl)
head(zoo_dl)
zoo_end_n <- nrow(zoo_dl)
zoo_wdoubles <- rbind(zoo_dl,NonHumanDoubles)
zoo_wdoubles_n <- nrow(zoo_wdoubles)

zoo_img2sp <- zoo_wdoubles[zoo_wdoubles$IMG1NAME%in%c(zoo_wdoubles$IMG1NAME[zoo_wdoubles$MainorDouble=="Double"]),]
# zoo_img1sp <- zoo_wdoubles[!zoo_wdoubles$IMG1NAME%in%c(zoo_wdoubles$IMG1NAME[zoo_wdoubles$MainorDouble=="Double"]),]


rm(annot_split,annot_split0,annot_split2,annot_split3,annot_split4,annot_split5,annot_split6,subjsplit,subjsplit2)


#this is for later when we want to maybe cut out images that only have 1 classification, or something of that sort.... re-merged in below

zoo_wdoublesTOT <-zoo_wdoubles[, .(NumberofClassifications=length(speciesID)), by=.(IMG1WLOCATION,IMG1NAME, IMG2WLOCATION, IMG2NAME, IMG3WLOCATION, IMG3NAME, season, site)]

#take the top classification for each subject
zoo_wdoubles_SUMMARY <- zoo_wdoubles[, .N, by=.(IMG1WLOCATION,speciesID)][order(-N), .(speciesID=speciesID[1L]), keyby=IMG1WLOCATION]


system.time(zoo_wdoubles_COUNTBYSP <- zoo_wdoubles[, .(modecountbysp=myfunnum(HowMany), Antlers=myfun(Antlers),Young=myfun(Young), BisonNumberEating=myfun(BisonNumberEating), LyingDown=myfun(LyingDown), Standing=myfun(Standing), Moving=myfun(Moving), Eating=myfun(Eating), Interacting=myfun(Interacting),SUBJIDLIST=PASTETOGETHER(subject_ids)), by=.(IMG1WLOCATION,speciesID)])
head(zoo_wdoubles_COUNTBYSP)


zoo_wdoubles_COUNTBYSP$SUBJIDLIST[nchar(zoo_wdoubles_COUNTBYSP$SUBJIDLIST)!=8]
zoo_wdoubles_COUNTBYSP$NCHAR_SUBJIDLIST <- nchar(zoo_wdoubles_COUNTBYSP$SUBJIDLIST)

zoo_wdoubles_sum2 <- merge(zoo_wdoubles_SUMMARY,zoo_wdoubles_COUNTBYSP,by=c("IMG1WLOCATION","speciesID"),all.x=T,all.y=F)

##### now for jut doubles, make secondary classifications 
zoo_img2sp_SUMMARY <- zoo_img2sp[, .N, by=.(IMG1WLOCATION,speciesID)][order(-N), .(speciesID_2ndSp=speciesID[2L]), keyby=IMG1WLOCATION]


system.time(zoo_img2sp_COUNTBYSP <- zoo_img2sp[, .(modecountbysp_2ndSp=myfunnum(HowMany), Antlers_2ndSp=myfun(Antlers),Young_2ndSp=myfun(Young), BisonNumberEating_2ndSp=myfun(BisonNumberEating), LyingDown_2ndSp=myfun(LyingDown), Standing_2ndSp=myfun(Standing), Moving_2ndSp=myfun(Moving), Eating_2ndSp=myfun(Eating), Interacting_2ndSp=myfun(Interacting),SUBJIDLIST_2ndSp=PASTETOGETHER(subject_ids)), by=.(IMG1WLOCATION,speciesID)])
head(zoo_img2sp_COUNTBYSP)



zoo_img2sp_sum2 <- merge(zoo_img2sp_SUMMARY,zoo_img2sp_COUNTBYSP,by.x=c("IMG1WLOCATION","speciesID_2ndSp"),by.y=c("IMG1WLOCATION","speciesID"),all.x=T,all.y=F)
head(zoo_img2sp_sum2)

zoo_comb <- merge(zoo_wdoubles_sum2,zoo_img2sp_sum2,by="IMG1WLOCATION",all=T)

class_df <- merge(zoo_comb,zoo_wdoublesTOT,by="IMG1WLOCATION",all.x=T)


###check for weird with animal or not... maybe I should sep that out before doing these summaries?
#### CHECK FOR ROAD CAMERAS? THOSE EXIST?

table(class_df$season[class_df$NCHAR_SUBJIDLIST!=8])

##### EXIF DATA! Processed in Exif_Process_CombinewithRenameFiles.R
exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/Merged_EXIF_MatchingPicNames.csv")
names(exifdat)

combdat <- merge(class_df,exifdat,by.x="IMG1WLOCATION",by.y="new_path",all.x=T)
names(combdat)




combdat$date_taken <- as.POSIXct(combdat$date_taken,format= "%Y:%m:%d %H:%M:%S")
combdat$DATE <- as.Date(combdat$date_taken, tz = "")
combdat$YEAR <- as.numeric(format(combdat$DATE,"%Y"))
combdat$MONTH <- as.numeric(format(combdat$DATE,"%m"))

library(stringr)
combdat$Cam_num <- as.numeric(gsub("\\D", "", combdat$site))
combdat$Cam_num_pad <- str_pad(combdat$Cam_num,width=3,side=c("left"),pad="0")
combdat$Cam_letters <- gsub("[^a-zA-Z]", "", combdat$site)

combdat$site_fixed <- ifelse(combdat$Cam_letters=="CB",paste0("C",combdat$Cam_num_pad,"B"),paste0("C",combdat$Cam_num_pad))
 # df <- combdat
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
# ALLDAT_NEC <- ALLDAT[,c("season","site_fixed","Easting","Northing","speciesID","mediancountbysp","modecountbysp","Antlers","Young","BisonNumberEating","LyingDown","Standing","Moving","Eating","Interacting","NumberofClassifications","DATE","YEAR","MONTH","subject_id","date_taken","MoonPhase","AmbientTemperature","Cam_num","SiteID","new_path","old_path","FileName","img123","ImageName","AmbientTemperatureFahrenheit")]
  
fwrite(ALLDAT,"C:/Users/cowl0037/Downloads/EOTW_DataOutput_ALL_JCproc4April.csv")


### habitat
dfcam_fin <- fread("C:/Users/cowl0037/Downloads/EOTW_CameraHabitat_JCproc26Mar.csv")

tapply(dfcam_fin$C_TEXT,dfcam_fin$NLCD_DESCRIPTION,unique)
tapply(dfcam_fin$ENAME,dfcam_fin$NLCD_DESCRIPTION,unique)

df_fin <- merge(ALLDAT,dfcam_fin,by=c("site_fixed","Cam_num","Easting","Northing"),all.x=T)
df_fin$season <- gsub("0","",df_fin$season)
df_fin$season <- gsub("S","S0",df_fin$season)
df_fin <- df_fin[,-c("site","CAMERAMeanCount","CAMERATotalCount","geometry","Matching_path","old_path","Origold_path","OrigDirectory.x", "Directory.x", "Orig_filename", "SourceFile", "FileName", "Directory.y","OrigDirectory.y", "FileModifyDate", "FileAccessDate","FileCreateDate", "DateTimeOriginal","Cam_num_pad","Cam_letters")]
fwrite(df_fin,"C:/Users/cowl0037/Downloads/EOTW_DataOutputwHabitat_JCproc2April.csv")

table(df_fin$speciesID,df_fin$speciesID_2ndSp)

