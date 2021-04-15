rm(list = ls()) # clear workspace


library(magick)
size<-100

# mountain images
img_list<-list.files("./data/mountains",pattern = ".png$",full.names = T) #list all files
all_images<-lapply(img_list, image_read) # read them into r
#hold<-all_images[[1]] # for testing purposes
for(i in 1:length(all_images)){ # for each image
  dems<-image_info(all_images[[i]]) # get dimensions
  if(dems$height>size && dems$width>size){ # if thier big enough the crop to middle size by size
    image_write(image_crop(all_images[[i]],paste0(size,"x",size,"+",(dems$width-size)/2,"+",(dems$height-size)/2)),paste0("./squared/mountain/mountain",i,".png"))
  }
}

# forest images
img_list<-list.files("./data/forests",pattern = ".png$",full.names = T)
all_images<-lapply(img_list, image_read)
#hold<-all_images[[1]]
for(i in 1:length(all_images)){
  dems<-image_info(all_images[[i]])
  if(dems$height>size && dems$width>size){
    image_write(image_crop(all_images[[i]],paste0(size,"x",size,"+",(dems$width-size)/2,"+",(dems$height-size)/2)),paste0("./squared/forest/forest",i,".png"))
  }
}
