#jane cowles
#image classification of camera trap data
#NOTES TO DO ETC:
###if any image of 3 is not blank/human, keep the whole set.


# install.packages("BiocManager")
# BiocManager::install("EBImage")
# install.packages("keras")

library(EBImage)
library(data.table)
library(keras)
library(tensorflow)
# library(reticulate)
# use_python("/Users/cowl0037/opt/anaconda3/bin/python")
# 
# conda_list()
# use_virtualenv("py3-virtualenv")
# reticulate::py_config()
# py_install("Pillow",envname='py3-virtualenv')


tmp <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects.csv")
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

# trans <- trans[trans$retired_at!=""&trans$retirement_reason=="consensus",!c("metadata","locations")]
# trans <- trans[trans$retired_at!="",!c("metadata","locations")]


ANSWERS <- fread("C:/Users/cowl0037/Downloads/CC_non_aggregated.csv")
merge.ans <- ANSWERS[,c("season","site","roll","subject_id","question__species")]
sp.sum <- merge.ans[,.(sum=length(subject_id)),.(question__species)]
sp.sumord <- sp.sum[order(-sum)]
sp.sumord$SPID <- -0:(nrow(sp.sumord)-1)
merge.ans <- merge(merge.ans,sp.sumord[,-c("sum")],by=c("question__species"))


info <- merge(trans,merge.ans,by=c("season","site","subject_id"))
info$BlankorNot <- ifelse(info$question__species=="blank",0,1)
info$SimpleID <- ifelse(info$question__species=="blank",0,ifelse(info$question__species=="humanorvehicle",1,2))
table(info$question__species)
table(info$BlankorNot)


length(unique(info$subject_id))
length(info$subject_id)

# info$IMG1WLOCATION <- paste(info$season,info$site,info$IMG1NAME,sep="/")
# info$IMG2WLOCATION <- paste(info$season,info$site,info$IMG2NAME,sep="/")
# info$IMG3WLOCATION <- paste(info$season,info$site,info$IMG3NAME,sep="/")

names(info)
# infolong <- melt(info,id.vars=c("season","site","subject_id","question__species","SPID","SimpleID","BlankorNot"),measure.vars=c("IMG1NAME","IMG2NAME","IMG3NAME"),variable.name = "img123",value.name = "ImageName")
# infolong2 <- melt(info,id.vars=c("season","site","subject_id","question__species","SPID","SimpleID","BlankorNot"),measure.vars=c("IMG1WLOCATION","IMG2WLOCATION","IMG3WLOCATION"),variable.name = "imgnumloc",value.name = "ImageWithLocation")
# infolong$ImageWithLocation <- infolong2$ImageWithLocation

# infolong$ImageWithLocation <- paste(infolong$season,infolong$site,infolong$ImageName,sep="/")

rm(ANSWERS,merge.ans,sp.sum,sp.sumord,trans,tmp,infolong2)

######################
# read in images and add together to make 1 x 3 channel image of all three!
# subj_id <- info$subject_id[info$IMG1WLOCATION==imagelist2[1]][1]
# readingroup <- function(subj_id,info,infolong){
readingroup <- function(subj_id){
if(!is.na(info$IMG1NAME[info$subject_id==subj_id])&!is.na(info$IMG2NAME[info$subject_id==subj_id])&!is.na(info$IMG3NAME[info$subject_id==subj_id])){
setwd("C:/Users/cowl0037/Documents/CameraTrapImages")
subj_id <- list.files()

image_1 <- readImage(info$IMG1WLOCATION[info$subject_id==subj_id])
image_1a <- image_1[,35:(dim(image_1)[2]-35),1]#cut off bottom of image
colorMode(image_1a)<-Grayscale
imgresize_1a <- resize(image_1a,100,100)
imgarray_1a <- as.array(imageData(imgresize_1a))

image_2 <- readImage(info$IMG2WLOCATION[info$subject_id==subj_id])
image_2a <- image_2[,35:(dim(image_2)[2]-35),1]#cut off bottom of image
colorMode(image_2a)<-Grayscale
imgresize_2a <- resize(image_2a,100,100)
imgarray_2a <- as.array(imageData(imgresize_2a))

image_3 <- readImage(info$IMG3WLOCATION[info$subject_id==subj_id])
image_3a <- image_3[,35:(dim(image_3)[2]-35),1]#cut off bottom of image
colorMode(image_3a)<-Grayscale
imgresize_3a <- resize(image_3a,100,100)
imgarray_3a <- as.array(imageData(imgresize_3a))


imgvect <- as.vector(c(imgarray_1a,imgarray_2a,imgarray_3a))

# outlist <- list(imgvect,infolong$BlankorNot[infolong$subject_id==paste(subj_id)][1])
# return(outlist)

x_train <<- rbind(x_train,c(imgvect))
y_train <<- append(y_train,info$BlankorNot[info$subject_id==paste(subj_id)][1])

}
  }

subj_list <- info$subject_id[info$IMG1WLOCATION%in%imagelist2]#[sample(1:3000,3)]

# info[info$IMG1WLOCATION%in%imagelist2][1:15]
### the parallel maybe takes time to read in all the info/info long stuff? maybe?
# library(parallel)
# no_cores <- detectCores()-1
# cl<-makeCluster(no_cores)
# clusterExport(cl,c("readImage","resize","colorMode<-","Grayscale","imageData"),envir=environment())
# system.time(output <- data.table::rbindlist(parLapply(cl,subj_list,readingroup,info=info,infolong=infolong)))
# # system.time(output <- data.table::rbindlist(parLapply(cl,subj_list,readingroup,info=info,infolong=infolong)))
# stopCluster(cl)
# output <- lapply(subj_list,readingroup)

x_train <- array(data=NA,dim=c(0));y_train <- array(data=NA,dim=c(0));system.time(lapply(subj_list,readingroup))

table(y_train)
y_train <- to_categorical(y_train,num_classes = 2) #change to 30 when using full sp (SPID instead of SimpleID)



model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(30000)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")#change units to # of levels in Y

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
model %>% predict_classes(x_train)
y_train[,2]
y_t_2 <- ifelse(y_train[,2]==0,1,2)
table(y_t_2,model %>% predict_classes(x_train))








######################
######################
#### read in images
setwd("~/Desktop/CameraTrap")

imagelist <- list.files(pattern=".JPG",recursive=T)
imagelist2 <- intersect(imagelist,infolong$ImageWithLocation)
imagesamp <- sample(intersect(imagelist2,infolong$ImageWithLocation),2000)


readin <- function(image){

img <- readImage(image)

# display(img)
#cut off bottom of image
img1 <- img[,35:(dim(img)[2]-35),1]
colorMode(img1)<-Grayscale
imgresize <- resize(img1,256,256)
# display(img1)
# dim(img1)
imgarray <- as.array(imageData(imgresize))
imgvect <- as.vector(imgarray)
x_train <<- rbind(x_train,c(imgvect))
# y_train <<- append(y_train,infolong$SPID[infolong$ImageWithLocation==paste(image)][1])
# y_train <<- append(y_train,infolong$SimpleID[infolong$ImageWithLocation==paste(image)][1])
y_train <<- append(y_train,infolong$BlankorNot[infolong$ImageWithLocation==paste(image)][1])

return(infolong$SimpleID[infolong$ImageWithLocation==paste(image)][1])
# return(x_train)
# return(y_train)

}

x_train <- array(data=NA,dim=c(0));y_train <- array(data=NA,dim=c(0));lapply(imagesamp,readin)


# dim(x_train)
table(y_train)
y_train <- to_categorical(y_train,num_classes = 2) #change to 30 when using full sp (SPID instead of SimpleID)



model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(65536)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")#change units to # of levels in Y

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)
model %>% predict_classes(x_train)
y_train[,2]
y_t_2 <- ifelse(y_train[,2]==0,1,2)
table(y_t_2,model %>% predict_classes(x_train))
