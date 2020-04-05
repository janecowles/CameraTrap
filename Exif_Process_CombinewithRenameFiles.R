#THIS FILE SHOULD NO LONGER BE NECESSARY. EXIF DATA NOW USED FROM PROCESSING PIPELINE (BETTER MATCHING)

# exif -- making "clean" file

exifdat <- fread("C:/Users/cowl0037/Downloads/Exif_Merge/img_meta.csv")
#so much info! this is what I decided was important. Includes a ton of time variables and temperature and image name info
exifsub <- exifdat[,c(1,3,4,7:9,25,33:37)]
exifsub <- exifsub[!grepl("Time Lapse",exifsub$Directory),]
exifsub <- exifsub[!grepl("MeredithUnknownCameraSavanna",exifsub$Directory),]

exifsub$OrigDirectory <- exifsub$Directory# make new thing to alter, no alterations to origdirectory anymore

# read in the conversion files that vlad uploaded to MSI. I put them in a folder called exif_merge, and call them by the name pattern, lapply fread and bind them together... i think it works.
rename_files <-list.files(path="C:/Users/cowl0037/Downloads/Exif_Merge",pattern="image_rename_mapping")
rename_mapping <- rbindlist(lapply(paste0("C:/Users/cowl0037/Downloads/Exif_Merge/",rename_files),fread))
rename_mapping$Origold_path <- rename_mapping$old_path
rename_mapping$Orig_filename <- gsub("/","",substr(rename_mapping$old_path,(nchar(rename_mapping$old_path)-12),nchar(rename_mapping$old_path)))
rename_mapping$OrigDirectory <- sapply(strsplit(rename_mapping$Origold_path,"/IMG"),`[`,1)
rename_mapping$OrigDirectory <- sapply(strsplit(rename_mapping$OrigDirectory,"/PICT"),`[`,1)
rename_mapping$Directory <- rename_mapping$OrigDirectory # make new thing to alter, no alterations to origdirectory anymore
### add in season, site, roll, new file name info
# pathsplit <- strsplit(rename_mapping$new_path,"/")
# rename_mapping$season <- sapply(pathsplit,`[`,1)
# unique(rename_mapping$season)
# table(duplicated(rename_mapping$new_path))

#original comparison
intersect(unique(exifsub$OrigDirectory),unique(rename_mapping$OrigDirectory))

setdiff(unique(exifsub$OrigDirectory),unique(rename_mapping$OrigDirectory))
setdiff(unique(rename_mapping$OrigDirectory),unique(exifsub$OrigDirectory))


### start edits.

rename_mapping$Directory <- gsub("/home/isbell/shared/albums/","",rename_mapping$Directory)

setdiff(unique(exifsub$Directory),unique(rename_mapping$Directory))
setdiff(unique(rename_mapping$Directory),unique(exifsub$Directory))
intersect(unique(exifsub$Directory),unique(rename_mapping$Directory))


#2018-1-DJFMA structure is different than should be. Fix winter one
exifsub$Directory <- sub("2018-1-DJFMA/2018-1-Winter-DJF","2018-1-DJFMA",exifsub$Directory)


#### Fix SPRING ONE. This one has leading zeros while rename version does not. Remove those.
unique(exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)])
unique(exifsub$Directory[grep("2018-1-DJFMA/Gr",exifsub$Directory)])
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C00","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C0","2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C",exifsub$Directory)

#specific folder error
exifsub$Directory <- sub("2018-1-DJFMA/GridNetwork/C64/20180328","2018-1-DJFMA/GridNetwork/C64/20180328/100RECNX",exifsub$Directory)

#more general folder error. Many are missing 100RECNX, but not all. Add in for all, then remove the duplicate.
exifsub$Directory <- ifelse(exifsub$Directory%in%exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM",exifsub$Directory)],paste(exifsub$Directory,"100RECNX",sep="/"),exifsub$Directory)
exifsub$Directory <- sub("100RECNX/100RECNX","100RECNX",exifsub$Directory)

# Now we can do the same to the winter one.
exifsub$Directory <- sub("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/C","2018-1-DJFMA/GridNetwork/C",exifsub$Directory)



# changes to rename_mapping
# how are there typos like this? Fix double A, 101 to 100, extra 100RECNX.
rename_mapping$Directory <- sub("2018-1-DJFMAA","2018-1-DJFMA",rename_mapping$Directory)
rename_mapping$Directory <- sub("2018-1-DJFMA/GridNetwork/C15/20180327/101RECNX","2018-1-DJFMA/GridNetwork/C15/20180327/100RECNX",rename_mapping$Directory)
rename_mapping$Directory <- sub("2018-2-MJJA/GridNetwork/C100/20810603/100RECNX","2018-2-MJJA/GridNetwork/C100/20810603",rename_mapping$Directory)

# unique(exifsub$Directory[grep("2018-1-DJFMA/2018-2-Spring-MAM/GridNetwork/",exifsub$Directory)])
# unique(rename_mapping$Directory[grep("2018-1-DJFMA/GridNetwork/",rename_mapping$Directory)])

list.exif <- sort(unique(exifsub$Directory[grep("SOND/Grid",exifsub$Directory)]))
list.map <- sort(unique(rename_mapping$Directory[grep("SOND/Grid",rename_mapping$Directory)]))

mapnames <- list.map[!list.map%in%list.exif]
exifnames <- list.exif[!list.exif%in%list.map]
for(i in 1:length(mapnames)){
  rename_mapping$Directory <- sub(mapnames[i],exifnames[i],rename_mapping$Directory)
}



unique(rename_mapping$Directory[!rename_mapping$Directory%in%exifsub$Directory])
unique(exifsub$Directory[!exifsub$Directory%in%rename_mapping$Directory])
###  THERE ARE STILL A FEW!!!!!

rename_mapping$Matching_path <- paste(rename_mapping$Directory,rename_mapping$Orig_filename,sep="/")
exifsub$Matching_path <- paste(exifsub$Directory,exifsub$FileName,sep="/")

#yay data.tables
setDT(rename_mapping)
setDT(exifsub)
merge1 <- merge(rename_mapping,exifsub,by="Matching_path") # with old stuff was: 1245860, with matching path:

fwrite(merge1,"C:/Users/cowl0037/Downloads/Exif_Merge/Merged_EXIF_MatchingPicNames.csv")
