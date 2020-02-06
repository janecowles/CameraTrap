#jane cowles
#merging together subjects, classifications, and exif data. Exif data downloaded by FI, image_rename files downloaded from MSI (waiting for season 4 permissions from vlad), subjects and classifications downloaded from zooniverse.
library(data.table)
library(ggplot2)


tmp <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects_6Feb.csv")
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

zoo_dl <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-classifications_6Feb.csv")

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
exifsub$SourceFile <- sub("2018-1-DJFMA/2018-1-Winter-DJF","2018-1-DJFMA",exifsub$SourceFile)
exifsub$SourceFile <- sub("2018-1-DJFMA/2018-2-Spring-MAM","2018-1-DJFMA",exifsub$SourceFile)

# read in the conversion files that vlad uploaded to MSI. I put them in a folder called exif_merge, and call them by the name pattern, lapply fread and bind them together... i think it works.
rename_files <-list.files(path="C:/Users/cowl0037/Downloads/Exif_Merge",pattern="image_rename_mapping")
rename_mapping <- rbindlist(lapply(paste0("C:/Users/cowl0037/Downloads/Exif_Merge/",rename_files),fread))

#yay data.tables
setDT(rename_mapping)
setDT(exifsub)

#merge 1 - combine the mapping files with exif info by the original path names
merge1 <- merge(rename_mapping,exifsub,by.x="old_path",by.y="SourceFile")

# merge 2 - combine that file with the species info (etc) by the new path
FINAL <- merge(merge1,infolong,by.x="new_path",by.y="ImageNameWLOCATION")

#you could use zoo_dlTOT to subset subject_ids out that have a classification count of less than a certain value. I'll just add that in right here and you can decide later ... (merge 3)

FINAL_wCOUNTS <- merge(FINAL,zoo_dlTOT,by.x="subject_id",by.y="subject_ids",all.x=T)
#RN this is 860888 - uploading new file on 6 feb -- what changes?
#write to a file
fwrite(FINAL_wCOUNTS,"C:/Users/cowl0037/Downloads/Exif_Merge/OUTPUT_EXIFandSPID.csv")

#actually not that many of these have just 1 classification. That'll change once we get Season 4 in here too? maybe? Waiting for Vlad to change the permissions
sort(FINAL_wCOUNTS$sum,decreasing = T)

# #a little plotting
ggplot(FINAL_wCOUNTS,aes(speciesID,AmbientTemperature))+geom_boxplot()
FINAL_wCOUNTS$date_taken <- as.POSIXct(FINAL_wCOUNTS$date_taken,format= "%Y:%m:%d %H:%M:%S")
ggplot(FINAL_wCOUNTS,aes(speciesID,date_taken))+geom_boxplot()
# 
