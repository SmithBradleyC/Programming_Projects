# Libraries needed
library(LaplacesDemon)

# The number of items that each instance will have
num_items<-20

# The number of distractor items
num_distractors<-500

# The learning rate variable. Each individaul item in each instance has an L chance of being remembered. Otherwise =0
L<-1

# How many instances of each target item will be made
item1_rep<-9
item2_rep<-6
item3_rep<-3

# Make each instance by randomly generating a string of items that are either -1 or 1
item1<-c((2*rbern(num_items,0.5)-1))
item2<-c((2*rbern(num_items,0.5)-1))
item3<-c((2*rbern(num_items,0.5)-1))

# Make the distractor instances by randomly generating strings of items in a similar way to the items
distractors<-c()
count<-1
repeat {
  distractors<-c(distractors,(2*rbern(num_items,0.5)-1))
  if (count == num_distractors) {
    break}
  count<-count+1
}

# Put everything that will be in the memory into a list
l_memory<-c(rep(item1,item1_rep),rep(item2,item2_rep),rep(item3,item3_rep),distractors)

# For each item in memory assign items a value of 0 with probability 1-L
count<-1
for (num in l_memory){
  l_memory[count]<-num*(rbern(1,L))
  count<-count+1
}

# Make the memory into a matrix
memory<-matrix(data = l_memory,ncol = num_items, byrow = TRUE)

# Make a probe matrix to test the recall of different items
probe<-matrix(c(item1,item2,item3,memory[item1_rep+item2_rep+item3_rep+1,],(2*rbern(num_items,0.5)-1),c(item1[1:(num_items/2)],rep(0,num_items/2))),ncol = 6)

# get the intensity of each probe item by multiplying the memory matrix by the probe matrix, then dividing by the number of relevent items,
# then raising each number to the third power, then summing across columns or adding up all trace intensities

inten_data<-colSums(((memory%*%probe/num_items)^3))



################################################################################################################################################
#### new way to do dividing without matrix (I would like a simpler matrix way but....)

# make a matrix as large as the resulting intensities matrix and fill it with the max relevant n's
divide<-matrix(data = c(rep(num_items,(nrow(memory)*ncol(probe)))),byrow = TRUE,ncol = ncol(probe))

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

# get the raw intensity matrix by multiplying the memory matrix by the probe matrix
inten<-memory%*%probe

# we now have a "divide" matrix of equal size to the intensity matrix. The "divide" matrix is full of the numbers that the
# corresponding intensity matrix numbers need to be divided by

# for each item in the intensity matrix divide by the corresponding "divide" matrix item
count<-0
while (count < nrow(inten)*ncol(inten)){
  count<-count+1
  inten[count]<-inten[count]/divide[count]
}

# cube each item in the intensity matrix so that relevant items have more influence than less relevant items
inten<-inten^3

# sum the columns of the intensity matrix to get the overall intensity values for each probe
inten_data<-colSums(inten)
############################################################################################################################################

# get the echo by multiplying the intensity matrix by the 
echo<-t(inten)%*%memory

# normalize the echo by dividing each row (probe result) by the largest number in that row
normed_echo<-echo
nrow<-0
while (nrow < nrow(normed_echo)){
  nrow<-nrow+1
  normed_echo[nrow,]<-normed_echo[nrow,]/max(abs(normed_echo[nrow,]))
}

# format data for output
probe_text<-apply(format(probe), 2, paste, collapse=",")
round_normed_echo_text<-apply(format(round(normed_echo,digits = 0)), 1, paste, collapse=",")
normed_echo_text<-apply(format(round(normed_echo,digits = 2)), 1, paste, collapse=",")
echo_text<-apply(format(round(echo,digits = 2)), 1, paste, collapse=",")

# output a table with probe, Rounded Echo, Normed Echo, Echo and Intensity for each probe.
as.table(matrix(c(probe_text,round_normed_echo_text,normed_echo_text,echo_text, round(inten_data,digits = 3)), 
       nrow = 6, byrow = FALSE, 
       dimnames = list(c("Item 1","Item 2","Item 3","Distractor","Random","Half Item 1"),c("Probe","Rounded Normed Echo","Normed Echo","Echo","Intensity"))))

# sim<-(memory%*%probe2)/23
# acti<-sim^3
# inten<-sum(acti)
# echo<-t(acti)%*%memory
# normed<-echo/max(abs(echo))
# sim
# acti
# inten
# echo
# normed