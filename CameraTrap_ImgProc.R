#jane cowles
#image classification of camera trap data
#NOTES TO DO ETC:
###if any image of 3 is not blank/human, keep the whole set.


# install.packages("BiocManager")
# BiocManager::install("EBImage")

# remove.packages("tensorflow")
# remove.packages("keras")
# library(devtools)
# devtools::install_github("rstudio/reticulate")
# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/keras")

# install.packages("tensorflow")
# install.packages("keras")

library(EBImage)
library(data.table)


library(reticulate)
# use_python("C:/Users/cowl0037/anaconda3/envs/r-reticulate/python.exe")
# 
# conda_list()
# use_virtualenv("py3-virtualenv")
# reticulate::py_config()
# py_install("Pillow",envname='py3-virtualenv')

library(tensorflow)
# install_tensorflow(version = "2.0.0-gpu")
library(keras)
# install_keras()


tmp <- fread("C:/Users/cowl0037/Downloads/cedar-creek-eyes-on-the-wild-subjects.csv")
tmp <- fread("~/Downloads/cedar-creek-eyes-on-the-wild-subjects.csv")
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
ANSWERS <- fread("~/Downloads/CC_non_aggregated.csv")
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




# length(unique(info$subject_id))
# length(info$subject_id)

# info$IMG1WLOCATION <- paste(info$season,info$site,info$IMG1NAME,sep="/")
# info$IMG2WLOCATION <- paste(info$season,info$site,info$IMG2NAME,sep="/")
# info$IMG3WLOCATION <- paste(info$season,info$site,info$IMG3NAME,sep="/")

names(info)
infolong <- melt(info,id.vars=c("season","site","subject_id","question__species","SPID","SimpleID","BlankorNot"),measure.vars=c("IMG1NAME","IMG2NAME","IMG3NAME"),variable.name = "img123",value.name = "ImageName")
infolong2 <- melt(info,id.vars=c("season","site","subject_id","question__species","SPID","SimpleID","BlankorNot"),measure.vars=c("IMG1WLOCATION","IMG2WLOCATION","IMG3WLOCATION"),variable.name = "imgnumloc",value.name = "ImageNameWLOCATION")
infolong$ImageNameWLOCATION <- infolong2$ImageNameWLOCATION

# infolong$ImageWithLocation <- paste(infolong$season,infolong$site,infolong$ImageName,sep="/")

rm(ANSWERS,merge.ans,sp.sum,sp.sumord,trans,tmp,infolong2)






######################
# read in images and add together to make 1 x 3 channel image of all three!
# subj_id <- info$subject_id[info$IMG1WLOCATION==imagelist2[1]][1]
# readingroup <- function(subj_id,info,infolong){


setwd("C:/Users/cowl0037/Documents/CameraTrapImages")
setwd("~/Desktop/CameraTrapImages")
system.time(imagelist <- list.files(pattern = ".JPG",recursive = T))
imagelist2 <- intersect(imagelist,infolong$ImageNameWLOCATION)
# imagesamp <- sample(intersect(imagelist2,infolong$ImageNameWLOCATION),20)
#So i can dynamically set image resize here and in model
pixelresize = 256

# subj_id = subj_list[100] #29319149 is some turkeys
# subj_id <- 29319149

readingroup <- function(subj_id){
if(!is.na(info$IMG1NAME[info$subject_id==subj_id])&!is.na(info$IMG2NAME[info$subject_id==subj_id])&!is.na(info$IMG3NAME[info$subject_id==subj_id])){

image_1 <- readImage(info$IMG1WLOCATION[info$subject_id==subj_id])
image_1a <- image_1[,35:(dim(image_1)[2]-35),1]#cut off bottom of image
colorMode(image_1a)<-Grayscale
imgresize_1a <- resize(image_1a,pixelresize,pixelresize)
imgarray_1a <- as.array(imageData(imgresize_1a))

image_2 <- readImage(info$IMG2WLOCATION[info$subject_id==subj_id])
image_2a <- image_2[,35:(dim(image_2)[2]-35),1]#cut off bottom of image
colorMode(image_2a)<-Grayscale
imgresize_2a <- resize(image_2a,pixelresize,pixelresize)
imgarray_2a <- as.array(imageData(imgresize_2a))

image_3 <- readImage(info$IMG3WLOCATION[info$subject_id==subj_id])
image_3a <- image_3[,35:(dim(image_3)[2]-35),1]#cut off bottom of image
colorMode(image_3a)<-Grayscale
imgresize_3a <- resize(image_3a,pixelresize,pixelresize)
imgarray_3a <- as.array(imageData(imgresize_3a))

##### 3images

imgvect <- as.vector(c(imgarray_1a,imgarray_2a,imgarray_3a))

#### motion detection (xdiff)
oneminustwo <- image_1a-image_2a

thr_1min2 <- oneminustwo > .1

twominusthree <- image_2a-image_3a

thr_2min3 <- twominusthree > .1

tot <- EBImage::normalize(thr_1min2+thr_2min3)
totresize <- resize(tot,pixelresize,pixelresize)
# dim(tot)
# display(tot)

totdiffvect <- as.vector(as.array(imageData(totresize)))



return(list(totdiffvect,info$BlankorNot[info$subject_id==paste(subj_id)][1],subj_id,imgvect))

}
  }


percentfortraining <- 0.8
totsubj_list0 <- sample(info$subject_id[info$IMG1WLOCATION%in%imagelist2&info$BlankorNot==0],5000)
totsubj_list1 <- sample(info$subject_id[info$IMG1WLOCATION%in%imagelist2&info$BlankorNot==1],5000)
totsubj_list <- c(totsubj_list0,totsubj_list1)
# totsubj_list <- info$subject_id[info$IMG1WLOCATION%in%imagelist2]

trainsubj_list <- sample(totsubj_list,round(percentfortraining*length(totsubj_list))) ###using 60% of images to train,
testsubj_list <- setdiff(totsubj_list,trainsubj_list)

# infotrain <- 


# info[info$IMG1WLOCATION%in%imagelist2][1:15]
### the parallel maybe takes time to read in all the info/info long stuff? maybe?


# trainsubj_used <- NULL;xdiff_train <- array(data=NA,dim=c(0));x_train <- array(data=NA,dim=c(0));y_train <- array(data=NA,dim=c(0))
library(doParallel)
registerDoParallel(cores=detectCores()-1)
system.time(train_out <- foreach(i=trainsubj_list,.packages = c("EBImage")) %dopar% readingroup(i))
xdiff_train <- do.call(rbind,lapply(train_out,`[[`,1))
y_train <- unlist(lapply(train_out,`[[`,2))
trainsubj_used <- unlist(lapply(train_out,`[[`,3))
x_train <- do.call(rbind,lapply(train_out,`[[`,4))

system.time(test_out <- foreach(i=testsubj_list[1:4],.packages = c("EBImage")) %dopar% readingroup(i))
xdiff_test <- do.call(rbind,lapply(test_out,`[[`,1))
y_test <- unlist(lapply(test_out,`[[`,2))
testsubj_used <- unlist(lapply(test_out,`[[`,3))
x_test <- do.call(rbind,lapply(test_out,`[[`,4))



#for use when not doParallel!
# trainsubj_used <- NULL;xdiff_train <- array(data=NA,dim=c(0));x_train <- array(data=NA,dim=c(0));y_train <- array(data=NA,dim=c(0));system.time(lapply(trainsubj_list,readingroup,"train"))

fwrite(data.frame(trainsubj_used,y_train),"train_info_25Jan2020.csv")
saveRDS(xdiff_train,"xdiff_train_25Jan2020.Rds")
saveRDS(x_train,"x_train_25Jan2020.Rds")

# testsubj_used <- NULL;xdiff_test <- array(data=NA,dim=c(0));x_test <- array(data=NA,dim=c(0));y_test <- array(data=NA,dim=c(0));system.time(lapply(testsubj_list,readingroup,"test"))
# 
fwrite(data.frame(testsubj_used,y_test),"test_info_25Jan2020.csv")
saveRDS(xdiff_test,"xdiff_test_25Jan2020.Rds")
saveRDS(x_test,"x_test_25Jan2020.Rds")

# y_train <- c(rep(0,12),rep(1,13))
table(y_train)
y_train <- to_categorical(y_train,num_classes = 2) #change to 30 when using full sp (SPID instead of SimpleID)
y_test <- to_categorical(y_test,num_classes = 2)

#####model xdiff
model_xdiff <- keras_model_sequential() 
model_xdiff %>% 
  layer_dense(units = 256, activation = "relu", input_shape = c(pixelresize*pixelresize)) %>%
  # layer_dense(units = 256, activation = "relu", input_shape = c(30000)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = "softmax")#change units to # of levels in Y -- 2 is blank or not, 3 for simple id, 30 for all sp

model_xdiff %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model_xdiff %>% fit(
  xdiff_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

model_xdiff %>% evaluate(xdiff_test, y_test,verbose = 0)
preds<- model_xdiff %>% predict_classes(xdiff_test)
table(y_test[,2],preds)
confusion <- cbind(y_test[,2],preds)
whicharewrong <- as.data.frame(cbind(confusion,testsubj_used))
whicharewrong$testsubj_used[whicharewrong$V1!=whicharewrong$preds]

info$IMG1NAME[info$subject_id%in%whicharewrong$testsubj_used[whicharewrong$V1!=whicharewrong$preds]]
table(info$site[info$subject_id%in%totsubj_list])
totsubj_list






# y_train[,2]
# y_t_2 <- ifelse(y_train[,2]==0,1,2)
# table(y_t_2,model_xdiff %>% predict_classes(xdiff_train))



#### below is original function that doesn't use triplicates
######################
######################
######################
#### read in images
# setwd("~/Desktop/CameraTrap")

system.time(imagelist <- list.files(pattern = ".JPG",recursive = T))
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
