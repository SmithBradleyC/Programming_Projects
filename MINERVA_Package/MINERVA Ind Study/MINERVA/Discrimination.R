##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
# make sure number of items is always even so that we can always evenly split it in half
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################

#Libraries Needed
library(LaplacesDemon)

##################################################################################################################################################################

# Make each instance by randomly generating a string of items that are either -1 or 1
make_instance<-function(num_items=20){
  return(c((2*rbern(num_items,0.5)-1)))
}

##################################################################################################################################################################

# add a row to memory
add_to_memory<-function(instance,
                        memory,
                        L=1,
                        times = 1){
  
  for (i in 1:times){
    
    
    # add the instance to memory
    memory<-rbind(memory,instance)
    
    # Forget each feature in the added instance with probability 1-L
    count<-1
    for (i in instance){
      memory[nrow(memory),count]<-i*(rbern(1,L))
      count<-count+1
    }
  }
  # avoid naming rows
  row.names(memory)<-NULL
  return(memory)
}

##################################################################################################################################################################

forget_in_memory<-function(memory,
                           L=.25){
  for (i in 1:length(memory)){
    memory[i]<-memory[i]*(rbern(1,L))
  }
  return(memory)
}

##################################################################################################################################################################

# make an empty matrix for memory to be added in
initiate_memory<-function(num_items=20){
  return(matrix(ncol = num_items,nrow = 0))
}

##################################################################################################################################################################

# make a probe matrix from 
make_probe_matrix<-function(test_instances,
                            
                            # assume just probing one but option to test several
                            num_instances_to_probe = 1){
  return(matrix(test_instances,ncol = num_instances_to_probe))
}

##################################################################################################################################################################

get_divide_matrix<-function(memory,
                            probe){
  
  # make a matrix as large as the resulting intensities matrix and fill it with the max relevant n's
  divide<-matrix(data = c(rep(ncol(memory),(nrow(memory)*ncol(probe)))),byrow = TRUE,ncol = ncol(probe))
  
  # for each probe
  count_probe<-0
  while (count_probe < ncol(probe)){
    count_probe<-count_probe+1
    
    # for each row in memory
    count_row<-0
    while (count_row < nrow(memory)){
      count_row<-count_row+1
      
      # for each item in that row in memory
      count_col<-0
      while (count_col < ncol(memory)){
        count_col<-count_col+1
        
        # if both the item in memory and the item in the probe are 0 then subtract 1 from the proper spot in the divide matrix
        if (memory[count_row,count_col]==0){
          if (probe[count_col,count_probe]==0){
            divide[count_row,count_probe]<-divide[count_row,count_probe]-1
          }
        }
      }
      
    }
  }
  
  # returns a matrix of values that the make up the denominator for the similarity of the traces
  return(divide)
}

##################################################################################################################################################################

get_divide_matrix_cosine<-function(memory,
                                   probe){
  probe2<-(probe^2)
  memory2<-(memory^2)
  
  # this might screw things up ###############
  sum_memory_rows<-sum(memory2)
  try(sum_memory_rows<-rowSums(memory2))
  
  # this might make problems if probing 2 things at once
  sum_probe_columns<-sum(probe2)
  divide<-c()
  for (i in sum_memory_rows){
    for (j in sum_probe_columns){
      divide<-c(divide,sqrt(i)*sqrt(j))
    }
  }
  if (nrow(memory) == 0) {return(matrix(c(1),nrow = 1))}
  return(matrix(divide,nrow = length(sum_memory_rows),byrow = TRUE))
}

##################################################################################################################################################################

get_similarity<-function(memory,
                         probe,
                         cosine = FALSE,
                         divide = NULL){
  if (is.null(divide)){
    if (cosine == TRUE){
      divide = get_divide_matrix_cosine(memory,probe)
    }
    else {
      divide = get_divide_matrix(memory,probe)
    }
  }
  # get the numerator of the similarity
  try(similarity<-memory%*%probe,silent = F)
  
  if (nrow(memory)==0){similarity<-matrix(c(0),nrow = 1)}
  
  # divide each similarity numerator by the denominator calculated in divide_matrix()
  count<-0
  while (count < nrow(similarity)*ncol(similarity)){
    count<-count+1
    similarity[count]<-similarity[count]/divide[count]
  }
  return(similarity)
}

##################################################################################################################################################################

# cube each iten the the similarity matrix so that relevant items have more influence than less relevant items
get_activation<-function(similarity){
  return(similarity^3)
}

##################################################################################################################################################################

# sum the columns of the activation matrix to get a measure of intensity (how "familiar" is the probe i)
get_intensity<-function(activation){
  return(colSums(activation))
}

##################################################################################################################################################################

# get the content of the echo
get_echo<-function(activation,
                   memory,
                   type = "normed",
                   rounding_digits = 0){
  
  good_type_input<-type%in%c("raw","normed","rounded")
  if (good_type_input == F){stop("Improper echo type specified")}
  
  if (nrow(memory)==0){return(matrix(rep(0,ncol(memory)),nrow = 1))}
  
  
  # get the echo by multiplying the activation matrix by the memory
  echo<-t(activation)%*%memory
  
  # if the user asks for the raw echo then return it
  if (type == "raw"){return(echo)}
  
  # normalize the echo by dividing each number in an instance by the largest number in that instance
  normed_echo<-echo
  nrow<-0
  while (nrow < nrow(normed_echo)){
    nrow<-nrow+1
    normed_echo[nrow,]<-normed_echo[nrow,]/max(abs(normed_echo[nrow,]))
  }
  
  # if the user asks for the normed echo (default setting) then return it
  if (type == "normed"){return(normed_echo)}
  
  # if the user asks for the rounded echo then return the rounded normed echo (default is to 0 sig digits for whole numbers comparison with probe)
  if(type == "rounded"){return(round(normed_echo,digits = rounding_digits))}
  
}

##################################################################################################################################################################

add_noise<-function(echo){
  for (i in 1:length(echo)){
    echo[i]<-echo[i]+runif(1,-0.001,0.001)
  }
  return(echo)
}

##################################################################################################################################################################

add_distortion<-function(instance,
                         proportion = 0.3,
                         name_first = T){
  len<-round(length(instance)/2,0)
  number_to_distort<-round(len*proportion,0)
  distort_list<-c()
  repeat{
    new_num<-round(runif(1,1,len),0)
    if (new_num %in% distort_list){} # do nothing
    else{
      distort_list<-c(distort_list,new_num)
    }
    if (length(distort_list)==number_to_distort){
      break
    }
  }
  
  if (name_first == T){
    distort_list<-distort_list+len
  }
  
  for (i in distort_list){
    instance[i]<-(-1)*instance[i]
  }
  
  return(instance)
}

##################################################################################################################################################################

correlate_echo_names<-function(echo,
                               category,
                               name_first = T){
  len<-round(length(echo)/2,0)
  if (name_first == F){return(cor(echo[len+1:length(echo)],category[len+1:length(echo)]))}
  else{return(cor(echo[1:len],category[1:len]))}
}

##################################################################################################################################################################
##################################################################################################################################################################

schema_abstraction<-function(n_participants = 50,
                             num_items =40,
                             distortion_proportion = 0.3,
                             name_first = T,
                             number_of_categories = 3,
                             # list specifying number in each category
                             frequency_of_categories = c(10,50,100),
                             number_of_distractors = 0,
                             forget_proportion = 0.75,
                             present_names = T,
                             type = "normed",
                             sig_digits = 2,
                             ...){
  
  # for when you want to do the follow up test with forgetting
  L = 1-forget_proportion
  
  # if the parameters don't match up flag error
  if (number_of_categories != length(frequency_of_categories)){stop("Must specify how many instances in each category")}
  
  # make a matrix to store the prototypes in to remember later
  prototype_matrix<-matrix(ncol = num_items,nrow = 0)
  for (i in 1:number_of_categories){
    prototype<-make_instance(num_items = num_items)
    prototype_matrix<-rbind(prototype_matrix,prototype)
  }
  
  # set up items to be able to average intensity and echo over participants
  t_intensity<-0
  t_echo<-0
  
  # repeat experiment with same prototypes over several participants
  for (par in 1:n_participants){
    
    # initiate memory
    memory<-initiate_memory(num_items = num_items)
    
    for (i in 1:number_of_categories){
      prototype<-prototype_matrix[i,]
      for (y in 1:frequency_of_categories[i]){
        memory<-add_to_memory(add_distortion(instance = prototype,
                                             proportion = distortion_proportion,
                                             name_first = name_first),
                              memory = memory)
      }
    }
    #distractor<-make_instance(num_items = num_items)
    #prototype_matrix<-rbind(prototype_matrix,distractor)
    rownames(prototype_matrix)<-NULL
    
    # make a probe matrix for testing
    probe<-c()
    
    if (present_names == T){
      distractor<-make_instance(num_items = num_items)
      #prototype_matrix<-rbind(prototype_matrix,distractor)
      #rownames(prototype_matrix)<-NULL
      probe<-prototype_matrix
      probe<-rbind(probe,distractor)
      rownames(probe)<-NULL
      
      
      for (i in (num_items/2+1):num_items){
        probe[,i]<-c(rep(0,nrow(probe)))
      }
      
      names<-c()
      for (i in 1:number_of_categories){
        names<-c(names,paste("Prototype ",i))
      }
      names_col<-c(names,"Distractor")
      names_row<-c("Prototype", "Probe", "Echo", "Intensity")
      number_of_rows<-number_of_categories+1
      
      
      
    }
    if (present_names == F){
      
      # get an exemplar that had been stored for each prototype for the probe matrix
      for (i in 1:number_of_categories){
        exemplar<-memory[sum(frequency_of_categories[1:i]),]
        probe<-c(probe,exemplar,prototype_matrix[i,],add_distortion(prototype_matrix[i,],proportion = distortion_proportion,name_first = name_first))
      }
      probe<-c(probe,make_instance(num_items = num_items))
      probe<-matrix(probe,ncol=num_items,byrow = TRUE)
      prototype_matrix<-probe
      #prototype_matrix<-rbind(probe,make_instance(num_items = num_items))
      #print(prototype_matrix)
      
      
      for (i in 1:(num_items/2)){
        probe[,i]<-c(rep(0,nrow(probe)))
      }
      
      names<-c()
      for (i in 1:number_of_categories){
        names<-c(names,paste("Prototype ",i," (Exemplary in memory)"),"(Prototype)","(New Exemplar)")
      }
      names_col<-c(names,"Distractor")
      names_row<-c("Prototype ", "Probe", "Echo", "Intensity")
      number_of_rows<-nrow(prototype_matrix)
      
      
      
    }
    
    #print(memory)
    #print(t(probe))
    similarity<-get_similarity(memory,t(probe))
    activation<-get_activation(similarity)
    #print(activation)
    
    intensity<-get_intensity(activation)
    echo<-get_echo(activation, memory,type = type)
    
    # sum up total intensity and echo
    t_intensity<-t_intensity+intensity
    t_echo<-t_echo+echo
    
  }
  
  # average out intensity and echo
  a_intensity<-t_intensity/n_participants
  a_echo<-t_echo/n_participants
  
  # format data for output
  if (present_names == T){prototype_text<-apply(format(rbind(prototype_matrix,distractor)),1,paste,collapse=",")}
  else{prototype_text<-apply(format(prototype_matrix),1,paste,collapse=",")}
  probe_text<-apply(format(probe), 1, paste, collapse=",")
  # round_normed_echo_text<-apply(format(round(normed_echo,digits = 0)), 1, paste, collapse=",")
  # normed_echo_text<-apply(format(round(normed_echo,digits = 2)), 1, paste, collapse=",")
  echo_text<-apply(format(round(a_echo,digits = sig_digits)), 1, paste, collapse=",")
  
  
  return(as.table(matrix(c(prototype_text,probe_text,echo_text, round(a_intensity,digits = 3)), 
                         nrow = number_of_rows, byrow = FALSE, 
                         dimnames = list(names_col,names_row))))
  # 
  
  
  
  # print(echo)
  # print(intensity)
  # similarity
  # activation
  # intensity
  # echo
}

# sum(x[1,]==memory[30,]) == 20
# sum(x[2,]==prototype_matrix[1,]) == 20
# sum(x[3,]==prototype_matrix[1,])== 17
# (x[4,])
# sum(x[5,]==memory[70,])== 20
# sum(x[6,]==prototype_matrix[2,])== 20
# sum(x[7,]==prototype_matrix[2,])== 17
# (x[8,])
# sum(x[9,]==memory[130,])== 20
# sum(x[10,]==prototype_matrix[3,])== 20
# (sum(x[11,]==prototype_matrix[3,]))== 17
# x[12,]



##################################################################################################################################################################
# # Libraries needed
# library(LaplacesDemon)
# 
# # The number of items that each instance will have
# num_items<-20
# 
# # The number of distractor items
# num_distractors<-500
# 
# # The learning rate variable. Each individaul item in each instance has an L chance of being remembered. Otherwise =0
# L<-0.75
# 
# # How many instances of each target item will be made
# item1_rep<-9
# item2_rep<-6
# item3_rep<-3
##################################################################################################################################################################


# intensity<-0
# L<-1
# for (i in 1:100){
# #intensity<-0
# memory<-initiate_memory()
# memory<-add_to_memory(make_instance(),memory,L = L,times = 9)
# memory<-add_to_memory(make_instance(),memory,L = L,times = 6)
# memory<-add_to_memory(make_instance(),memory,L = L,times = 3)
# for (i in 1:100){
#   memory<-add_to_memory(make_instance(),memory,L = L)
# }
# 
# probe<-make_probe_matrix(c(memory[1,],memory[10,],memory[16,],make_instance()),4)
# similarity<-get_similarity(memory,probe)
# activation<-get_activation(similarity)
# intensity<-intensity+get_intensity(activation)
# intensity
# #get_echo(activation,memory,"rounded")
# #t(probe)
# #intensity
# }
# intensity/100

##################################################################################################################################################################



##################################################################################################################################################################

sim_x<-function(event,
                n_echo){
  num<-0
  p<-0
  m<-0
  for (i in 101:120){
    num<-num+(event[i]*n_echo[i])
    p<-p+(event[i]^2)
    m<-m+(n_echo[i]^2)
  }
  #return(num/20)
  return(num/(sqrt(p)*sqrt(m)))
}


sim_a<-function(event,
                n_echo){
  num<-0
  p<-0
  m<-0
  for (i in 1:20){
    num<-num+(event[i]*n_echo[i])
    #p<-p+(event[i]^2)
    #m<-m+(n_echo[i]^2)
  }
  return(num/20)
  #return(num/(sqrt(p)*sqrt(m)))
}


##################################################################################################################################################################
# test accuisition

A<-matrix(c(rep(1,20),rep(0,100)),nrow = 1)
B<-matrix(c(rep(0,20),rep(1,20),rep(0,80)),nrow = 1)
X<-matrix(c(rep(0,100),rep(1,20)),nrow = 1)
C<-matrix(c(rep(0,80),rep(1,20),rep(0,20)),nrow = 1)
AX<-A+X+C
BX<-(-B)+X+C
A<-A+C
X<-X+C
recall_list<-c()
learn<-2/3
sub<-1
for (k in 1:sub){
  memory<-initiate_memory(120)
  for (i in 1:50){
    s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),AX[,1:100],cosine = TRUE)
    a<-get_activation(s)
    e<-get_echo(a,memory)
    e<-add_noise(e)
    memory<-add_to_memory(AX-e,memory,L=learn)
    s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),AX[,1:100],cosine = TRUE)
    a<-get_activation(s)
    e<-get_echo(a,memory)
    recall_list<-c(recall_list,sim_a(X[101:120],e[101:120]))
  }
  for (i in 1:50){
    s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),A[,1:100],cosine = TRUE)
    a<-get_activation(s)
    e<-get_echo(a,memory)
    e<-add_noise(e)
    memory<-add_to_memory(A-e,memory,L=learn)
    s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),A[,1:100],cosine = TRUE)
    a<-get_activation(s)
    e<-get_echo(a,memory)
    recall_list<-c(recall_list,sim_a(X[101:120],e[101:120]))
  }
    for (i in 1:50){
      s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),AX[,1:100],cosine = TRUE)
      a<-get_activation(s)
      e<-get_echo(a,memory)
      e<-add_noise(e)
      memory<-add_to_memory(AX-e,memory,L=learn)
      s<-get_similarity(matrix(memory[,1:100],nrow = nrow(memory)),AX[,1:100],cosine = TRUE)
      a<-get_activation(s)
      e<-get_echo(a,memory)
      recall_list<-c(recall_list,sim_a(X[101:120],e[101:120]))
    }
}

recall<-matrix(recall_list,nrow = sub,byrow = TRUE)
recall2<-colSums(recall)/sub

plot(1:length(recall2),recall2,ylim = c(0,1))

##################################################################################################################################################################

new_similarity<-function(new_memory,probe){
  num_sum<-0
  den_sum1<-0
  den_sum2<-0
  for (i in 1:length(probe)){
    num_sum<-num_sum+(new_memory[i]*probe[i])
    den_sum1<-den_sum1+(new_memory[i]^2)
    den_sum2<-den_sum2+(probe[i]^2)
  }
  den<-sqrt(den_sum1)*sqrt(den_sum2)
  return(num_sum/den)
}

##################################################################################################################################################################
# test accuisition for robot

# set up 4 types of stimuli
A<-matrix(c(rep(1,20),rep(0,40)),nrow = 1)
X<-matrix(c(rep(0,40),rep(1,20)),nrow = 1)
C<-matrix(c(rep(0,20),rep(1,20),rep(0,20)),nrow = 1)
# light on turn right
AR<-A+C
# light on turn left
AL<-A-C
# light off turn right
OR<-C
# light off turn left
OL<-(-C)
learn<-.8
sub<-1

# light is always true for now
light<- TRUE
light_list<-c(1)

# make a list of turns and empty new_memory and recall
turns<-c() # 1 means right, -1 means left choice
recall_list<-c()
new_memory<-NULL
sim_R<-0
sim_L<-0
sim_r_list<-c()
sim_l_list<-c()


for (k in 1:sub){
  
  # empty the echo lists
  #r_echo<-matrix(c(rep(0,60)),nrow = 1)
  AR_echo<-matrix(c(rep(0,60)),nrow = 1)
  AL_echo<-matrix(c(rep(0,60)),nrow = 1)
  OR_echo<-matrix(c(rep(0,60)),nrow = 1)
  OL_echo<-matrix(c(rep(0,60)),nrow = 1)
  
  # make the first memory
  r_echo<-matrix(c(rep(0,60)),nrow = 1)
  r_echo<-add_noise(r_echo)
  
  # comment out if desired. just to normalize original echo
  r_echo<-r_echo/max(r_echo)
  
  # choose right or left randomly to begin
  turn<-sample(c(-1,1),1)
  turns<-c(turns,turn)
  
  # make the new_memory according to choice
  if (turn == -1){new_memory<-AL-r_echo
  }else{new_memory<-(AR+X)-r_echo}
  #new_memory<-ARX-r_echo
  
  # encode the memory with learning parameter
  for (i in 1:length(new_memory)){
    new_memory[i]<-rbern(1,learn)*new_memory[i]
  }
  new_memory
  
  for (j in 1:100){
    #n_s<-new_similarity(new_memory[,1:40],AX[,1:40])
    #sum_echo<-sum_echo+(n_s^3)*new_memory
    
    # fill echo lists with the raw echo from the new_memory
    AR_echo<-AR_echo+(((new_similarity(new_memory[,1:40],AR[,1:40]))^3)*new_memory)
    AL_echo<-AL_echo+(((new_similarity(new_memory[,1:40],AL[,1:40]))^3)*new_memory)
    OR_echo<-OR_echo+(((new_similarity(new_memory[,1:40],OR[,1:40]))^3)*new_memory)
    OL_echo<-OL_echo+(((new_similarity(new_memory[,1:40],OL[,1:40]))^3)*new_memory)
    
    light<-sample(c(-1,1),1)
    light_list<-c(light_list,light)
    
    # check if the light is present and normalize echos in the correct lists
    if (light == 1){
      norm_echoR<-AR_echo/max(abs(AR_echo))
      norm_echoL<-AL_echo/max(abs(AL_echo))
    } else{
      norm_echoR<-OR_echo/max(abs(OR_echo))
      norm_echoL<-OL_echo/max(abs(OL_echo))
    }
    
    sim_R<-sim_a(X[,41:60],norm_echoR[41:60])
    sim_L<-sim_a(X[,41:60],norm_echoL[41:60])
    
    sim_r_list<-c(sim_r_list,sim_R)
    sim_l_list<-c(sim_l_list,sim_L)
    
    # make a decision on right or left and go that way
    if (abs(sim_R) < 0.1 && abs(sim_L) < 0.1){
      turn<-sample(c(-1,1),1)
    }else{
      if (sim_R*1000 > sim_L*1000){
        turn<-(1)
      } else{turn<-(-1)}
    }
    
    turns<-c(turns,turn)
    
    
    #sim_a(X[,41:60],norm_echoARX[41:60])
    
    
    
    ## recall_listAR<-c(recall_listAR,sim_a(X[,41:60],norm_echoAR[41:60]))
    ## recall_listAR<-c(recall_listAR,sim_a(X[,41:60],norm_echoAL[41:60]))
    ## recall_listOR<-c(recall_listOR,sim_a(X[,41:60],norm_echoOR[41:60]))
    ## recall_listOL<-c(recall_listOL,sim_a(X[,41:60],norm_echoOL[41:60]))
    
    
    # make new memory
    if (turn == 1 && light == 1){
      new_memory<-(AR+X)-(add_noise(norm_echoR))
    } else if (turn == -1 && light == 1) {
      new_memory<-(AL)-(add_noise(norm_echoL))
    } else if (turn == 1 && light == -1){
      new_memory<-(OR)-(add_noise(norm_echoR))
    } else if (turn == -1 && light == -1){
      new_memory<-(OL+X)-(add_noise(norm_echoR))
    }
    
    for (i in 1:length(new_memory)){
      new_memory[i]<-rbern(1,learn)*new_memory[i]
    }
  }
}

turns[1:100]
turns[101:200]
mean(turns[149:200])
sim_r_list*100
sim_l_list*100
plot(1:100,sim_l_list,ylim = c(0,1))
plot(1:100,sim_r_list,ylim = c(0,1))

# length(recall_list)
recall_list2<-matrix(sim_r_list,ncol = 100)
recall_list2<-rowSums(recall_list2)
plot(1:100,recall_list2/100,ylim = c(0,1))
recall_list3<-matrix(sim_l_list,ncol = 100)
recall_list3<-rowSums(recall_list3)
plot(1:100,recall_list3/100,ylim = c(0,1))
recall_list2/100
recall_list3/100
turns
light_list
frame<-data.frame(turns, light_list, c(0,sim_r_list), c(0,sim_l_list))
frame
