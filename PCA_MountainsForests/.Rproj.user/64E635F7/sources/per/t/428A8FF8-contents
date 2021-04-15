rm(list = ls()) # clear workspace
library(jpeg)

img_list<-list.files("./squared/mountain",pattern = ".png$",full.names = T)# get square images
img_list2<-list.files("./squared/forest",pattern = ".png$",full.names = T)
mini<-min(length(img_list2),length(img_list))


n_train_items<-200 # number of training items per image type
n_test_items<-100 # number of testing items per image type
sample<-sample(x = 1:mini,
               size = n_train_items+n_test_items,
               replace = F) # sample out total used items
train_items<-sample[1:n_train_items] # decide train items
test_items<-sample[(n_train_items+1):length(sample)] # decide test items

sample_img<-readJPEG(img_list[1]) # get one image to get dimensions 
features<-prod(dim(sample_img)) # get number of features per image
vec_representation<-matrix(data = 0,nrow = n_train_items*2,ncol = features) #make empty stim matrix

notGrey<-function(img_list, i){ #function to check if image is grey (won't work)
  img<-readJPEG(img_list[i]) # read in the image
  if(length(dim(img))<3){ # if less than 3 dimensions then repeat with new item
    notGrey(img_list,(i+1))
  }else{ # if good then return image
    return(img)
  }
}

# put in the mountain images
for(i in 1:length(train_items)){ # for each train item
  img<-notGrey(img_list, train_items[i]) # check for grey image and read it in
  #img<-readJPEG(img_list[train_items[i]]) # read in the image
  r <- img[,,1] #split into components
  g <- img[,,2]
  b <- img[,,3]
  vec_representation[i,]<-c(as.vector(r),as.vector(g),as.vector(b)) #flatten into vector
}

# put in the forest images
for(i in 1:length(train_items)){ # for each train item
  img<-notGrey(img_list, train_items[i]) # check for grey image and read it in
#  img<-readJPEG(img_list2[train_items[i]]) # read in the image
  r <- img[,,1] #split into components
  g <- img[,,2]
  b <- img[,,3]
  vec_representation[(i+n_train_items),]<-c(as.vector(r),as.vector(g),as.vector(b)) #flatten into vector
}

# make test items
test_vec_representation<-matrix(data = 0,nrow = n_test_items*2,ncol = features) #make empty stim matrix

# for mountains
for(i in 1:length(test_items)){ # for each test item
  img<-notGrey(img_list, test_items[i]) # check for grey image and read it in
#  img<-readJPEG(img_list[test_items[i]]) # read in the image
  r <- img[,,1] #split into components
  g <- img[,,2]
  b <- img[,,3]
  test_vec_representation[i,]<-c(as.vector(r),as.vector(g),as.vector(b)) #flatten into vector
}

# for forests
for(i in 1:length(test_items)){ # for each test item
  img<-notGrey(img_list, test_items[i]) # check for grey image and read it in
#  img<-readJPEG(img_list2[test_items[i]]) # read in the image
  r <- img[,,1] #split into components
  g <- img[,,2]
  b <- img[,,3]
  test_vec_representation[(i+n_test_items),]<-c(as.vector(r),as.vector(g),as.vector(b)) #flatten into vector
}

save(vec_representation,test_vec_representation, n_train_items, n_test_items, file = "stim_matrix.RData")
