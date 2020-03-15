#jane cowles
#merging together subjects, classifications, and exif data. Exif data downloaded by FI, image_rename files downloaded from MSI (waiting for season 4 permissions from vlad), subjects and classifications downloaded from zooniverse.
library(data.table)
library(ggplot2)
library(maptools)

tmp <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects_3Mar.csv")
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

rm(metadatasep,imglist,s_c_info,s_c_infosplit)

zoo_dl <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-classifications_3Mar.csv")

zoo_dl$annotations2 <- gsub("\"", "", zoo_dl$annotations )

# annot_split <- strsplit(zoo_dl$annotations3,",")

annot_split2 <- strsplit(zoo_dl$annotations2,",answers:")
annot_split <- strsplit(sapply(annot_split2,`[`,1),"choice:")
zoo_dl$speciesID <- sapply(annot_split,`[`,2)
zoo_dl$speciesID <- tolower(zoo_dl$speciesID)


#WARNING-- now that animal or not workflow is in here, need to cut that out for current analyses
zoo_dl <- zoo_dl[zoo_dl$workflow_id==5702,]


#this is for later when we want to maybe cut out images that only have 1 classification, or something of that sort.... re-merged in below
zoo_dlTOT <-zoo_dl[, .(sum=length(speciesID)), by=.(subject_ids)] 
#take the top classification for each subject
zoo_dl_SUMMARY <- zoo_dl[, .N, by=.(subject_ids,speciesID)][order(-N), .(speciesID=speciesID[1L]), keyby=subject_ids]

#merging subject info with classification info
info <- merge(trans,zoo_dl_SUMMARY,by.x=c("subject_id"),by.y="subject_ids")
#that was by subject, now make long by image so we can merge with other files
infolong <- melt(info,id.vars=c("season","site","subject_id","speciesID"),measure.vars=c("IMG1NAME","IMG2NAME","IMG3NAME"),variable.name = "img123",value.name = "ImageName")
#i always forget how to make two variables long at the same time... so doing it an ugly way
infolong2 <- melt(info,id.vars=c("season","site","subject_id","speciesID"),measure.vars=c("IMG1WLOCATION","IMG2WLOCATION","IMG3WLOCATION"),variable.name = "imgnumloc",value.name = "ImageNameWLOCATION")
infolong$ImageNameWLOCATION <- infolong2$ImageNameWLOCATION
#data.tables are better
setDT(infolong)


exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/img_meta.csv")
#so much info! this is what I decided was important. Includes a ton of time variables and temperature and image name info
exifsub <- exifdat[,c(1,3,4,7:9,25:37)]
#these subdirectories must've been changed since the upload, so fixing that here.
# exifsub$SourceFile <- sub("/IMG","/100RECNX/IMG",exifsub$SourceFile)
# exifsub$SourceFile <- sub("100RECNX/100RECNX","100RECNX",exifsub$SourceFile)
exifsub$Directory <- sub("2018-1-DJFMA/2018-1-Winter-DJF","2018-1-DJFMA",exifsub$Directory)

#### TESTING FOR THE SPRING ONE.
unique(exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)])
unique(exifsub$Directory[grep("2018-1-DJFMA/Gr",exifsub$Directory)])
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C00","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C0","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)

exifsub$Directory <- sub("2018-1-DJFMA/GridNetwork/C64/20180328","2018-1-DJFMA/GridNetwork/C64/20180328/100RECNX",exifsub$Directory)

exifsub$Directory <- ifelse(exifsub$Directory%in%exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)],paste(exifsub$Directory,"100RECNX",sep="/"),exifsub$Directory)
exifsub$Directory <- sub("100RECNX/100RECNX","100RECNX",exifsub$Directory)


# rename_mapping$old_path <- sub("2018-1-DJFMA/GridNetwork/C64/20180328/100RECNX","2018-1-DJFMA/GridNetwork/C64/20180328",rename_mapping$old_path)


exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C","2018-1-DJFMA/GridNetwork/C",exifsub$Directory)
exifsub$SourceFile <- paste(exifsub$Directory,exifsub$FileName,sep="/")



# exifsub$SourceFile <- sub("2018-1-DJFMA/2018-2-Spring-MAM","2018-1-DJFMA",exifsub$SourceFile) #this just isn't in the rename_mapping - must not have been uploaded???

# read in the conversion files that vlad uploaded to MSI. I put them in a folder called exif_merge, and call them by the name pattern, lapply fread and bind them together... i think it works.
rename_files <-list.files(path="C:/Users/cowl0037/Downloads/Exif_Merge",pattern="image_rename_mapping")
rename_mapping <- rbindlist(lapply(paste0("C:/Users/cowl0037/Downloads/Exif_Merge/",rename_files),fread))
rename_mapping$old_path <- sub("2018-1-DJFMAA","2018-1-DJFMA",rename_mapping$old_path)
rename_mapping$old_path <- sub("2018-1-DJFMA/GridNetwork/C15/20180327/101RECNX","2018-1-DJFMA/GridNetwork/C15/20180327/100RECNX",rename_mapping$old_path)
rename_mapping$old_path <- sub("2018-2-MJJA/GridNetwork/C100/20810603/100RECNX","2018-2-MJJA/GridNetwork/C100/20810603",rename_mapping$old_path)


old_path_path <- strsplit(rename_mapping$old_path,"/IMG")
old_path_path2 <- strsplit(sapply(old_path_path,`[`,1),"/PICT")
rename_mapping$Directory <- sapply(old_path_path2,`[`,1)

# unique(exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/",exifsub$Directory)])
# unique(rename_mapping$Directory[grep("2018-1-DJFMA/GridNetwork/",rename_mapping$Directory)])

list.exif <- unique(exifsub$Directory[grep("SOND/Grid",exifsub$Directory)])
list.map <- unique(rename_mapping$Directory[grep("SOND/Grid",rename_mapping$Directory)])
intersect(list.map,list.exif)
mapnames <- list.map[!list.map%in%list.exif]
exifnames <- list.exif[!list.exif%in%list.map]
for(i in 1:length(mapnames)){
  rename_mapping$old_path <- sub(mapnames[i],exifnames[i],rename_mapping$old_path)
}

old_path_path <- strsplit(rename_mapping$old_path,"/IMG")
old_path_path2 <- strsplit(sapply(old_path_path,`[`,1),"/PICT")
rename_mapping$Directory <- sapply(old_path_path2,`[`,1)


unique(rename_mapping$Directory[!rename_mapping$Directory%in%exifsub$Directory])
unique(exifsub$Directory[!exifsub$Directory%in%rename_mapping$Directory])


#yay data.tables
setDT(rename_mapping)
setDT(exifsub)

#merge 1 - combine the mapping files with exif info by the original path names
merge1 <- merge(rename_mapping,exifsub,by.x="old_path",by.y="SourceFile")

tail(unique(rename_mapping$old_path[!rename_mapping$old_path%in%merge1$old_path]))
(unique(exifsub$Directory[!exifsub$Directory%in%merge1$Directory.y]))

# (rename_mapping$old_path[grep("2018-1-DJFMA/GridNetwork/C14",rename_mapping$old_path)])
# rename_mapping$old_path[grep("C014",rename_mapping$old_path)]
# THIS MERGE IS MISSING WHAT IS LABELED SPRING within 2018-1-DJFMA FOLDER AND IT WILL NOT BE EASY TO CORRECT.### 11,000 pictures missing because of this.
# merge 2 - combine that file with the species info (etc) by the new path
FINAL <- merge(merge1,infolong,by.x="new_path",by.y="ImageNameWLOCATION")

#you could use zoo_dlTOT to subset subject_ids out that have a classification count of less than a certain value. I'll just add that in right here and you can decide later ... (merge 3)

FINAL_wCOUNTS <- merge(FINAL,zoo_dlTOT,by.x="subject_id",by.y="subject_ids",all.x=T)
#RN this is 860888 - uploading new file on 6 feb -- what changes?
#write to a file
fwrite(FINAL_wCOUNTS,"C:/Users/cowl0037/Downloads/Exif_Merge/OUTPUT_EXIFandSPID_update15Mar.csv")
names(FINAL_wCOUNTS)
#actually not that many of these have just 1 classification. That'll change once we get Season 4 in here too? maybe? Waiting for Vlad to change the permissions
sort(FINAL_wCOUNTS$sum,decreasing = T)

# #a little plotting
ggplot(FINAL_wCOUNTS,aes(speciesID,AmbientTemperature))+geom_boxplot()
FINAL_wCOUNTS$date_taken <- as.POSIXct(FINAL_wCOUNTS$date_taken,format= "%Y:%m:%d %H:%M:%S")
FINAL_wCOUNTS$DATE <- as.Date(FINAL_wCOUNTS$date_taken, tz = "")

library(stringr)
FINAL_wCOUNTS$Cam_num <- as.numeric(gsub("\\D", "", FINAL_wCOUNTS$site))
FINAL_wCOUNTS$Cam_num_pad <- str_pad(FINAL_wCOUNTS$Cam_num,width=3,side=c("left"),pad="0")
FINAL_wCOUNTS$Cam_letters <- gsub("[^a-zA-Z]", "", FINAL_wCOUNTS$site)

FINAL_wCOUNTS$site_fixed <- ifelse(FINAL_wCOUNTS$Cam_letters=="CB",paste0("C",FINAL_wCOUNTS$Cam_num_pad,"B"),paste0("C",FINAL_wCOUNTS$Cam_num_pad))

solarpos(FINAL_wCOUNTS$date_taken[1:4])
sunriset(c(-93.194718,45.396570), FINAL_wCOUNTS$date_taken[1:4], proj4string=CRS("+proj=longlat +datum=WGS84"), direction=c("sunrise"), POSIXct.out=FALSE)

FINAL_WCOUNTS$DAYNIGHT <- ifelse(FINAL_wCOUNTS$date_taken>  as.Date(sunrise.set(45.396570, -93.194718, FINAL_wCOUNTS$DATE, timezone = "", num.days = 1)[1,1],tz="")&FINAL_wCOUNTS$date_taken<  as.Date(sunrise.set(45.396570,-93.194718, FINAL_wCOUNTS$DATE, timezone = "", num.days = 1)[1,2],tz=""),"day","night")


ggplot(FINAL_wCOUNTS,aes(speciesID,date_taken))+geom_jitter()
ggplot(FINAL_wCOUNTS[FINAL_wCOUNTS$season=="S4",],aes(speciesID,date_taken))+geom_jitter()

ggplot(FINAL_wCOUNTS[FINAL_wCOUNTS$speciesID=="deer",],aes(date_taken,site_fixed,color=season))+geom_point()
ggplot(FINAL_wCOUNTS[FINAL_wCOUNTS$speciesID=="wolforcoyote",],aes(date_taken,site_fixed,color=season))+geom_point()

FINAL_wCOUNTS$ImageName[FINAL_wCOUNTS$season=="S4"&FINAL_wCOUNTS$speciesID=="nothingthere"]

# 

cam_locs <- fread("C:/Users/cowl0037/Downloads/")
