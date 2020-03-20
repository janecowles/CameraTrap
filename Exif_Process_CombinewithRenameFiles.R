# exif -- making "clean" file

exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/img_meta.csv")
#so much info! this is what I decided was important. Includes a ton of time variables and temperature and image name info
exifsub <- exifdat[,c(1,3,4,7:9,25,33:37)]
#these subdirectories must've been changed since the upload, so fixing that here.
# exifsub$SourceFile <- sub("/IMG","/100RECNX/IMG",exifsub$SourceFile)
# exifsub$SourceFile <- sub("100RECNX/100RECNX","100RECNX",exifsub$SourceFile)
exifsub$OrigDirectory <- exifsub$Directory
exifsub$Directory <- sub("2018-1-DJFMA/2018-1-Winter-DJF","2018-1-DJFMA",exifsub$Directory)

#### TESTING FOR THE SPRING ONE.
unique(exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)])
unique(exifsub$Directory[grep("2018-1-DJFMA/Gr",exifsub$Directory)])
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C00","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C0","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)

exifsub$Directory <- sub("2018-1-DJFMA/GridNetwork/C64/20180328","2018-1-DJFMA/GridNetwork/C64/20180328/100RECNX",exifsub$Directory)

exifsub$Directory <- ifelse(exifsub$Directory%in%exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)],paste(exifsub$Directory,"100RECNX",sep="/"),exifsub$Directory)
exifsub$Directory <- sub("100RECNX/100RECNX","100RECNX",exifsub$Directory)



exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C","2018-1-DJFMA/GridNetwork/C",exifsub$Directory)
exifsub$SourceFile <- paste(exifsub$Directory,exifsub$FileName,sep="/")


# read in the conversion files that vlad uploaded to MSI. I put them in a folder called exif_merge, and call them by the name pattern, lapply fread and bind them together... i think it works.
rename_files <-list.files(path="C:/Users/cowl0037/Downloads/Exif_Merge",pattern="image_rename_mapping")
rename_mapping <- rbindlist(lapply(paste0("C:/Users/cowl0037/Downloads/Exif_Merge/",rename_files),fread))
rename_mapping$Origold_path <- rename_mapping$old_path
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
merge1 <- merge(rename_mapping,exifsub,by.x="old_path",by.y="SourceFile")

fwrite(merge1,"C:/Users/cowl0037/Downloads/Exif_Merge/Merged_EXIF_MatchingPicNames.csv")
