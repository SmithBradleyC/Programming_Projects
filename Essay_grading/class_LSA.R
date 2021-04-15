# Clear the work environment
rm(list = ls())

library(LSAfun)

library(hunspell)
cleantext = function(x){
  
  sapply(1:length(x),function(y){
    bad = hunspell(x[y])[[1]]
    good = unlist(lapply(hunspell_suggest(bad),`[[`,1))
    #print("bad:")
    #print(bad)
    #print("good:")
    #print(good)
    
    if (length(bad)){
      for (i in 1:length(bad)){
        x[y] <<- gsub(bad[i],good[i],x[y])
      }}})
  x
}

load("BEAGLE.rda")

library(readxl)
Test2_ThinkingMarks_excel <- read_excel("Test2_ThinkingMarks_excel.xlsx")


Question_to_analyze<-4

q1<-subset(Test2_ThinkingMarks_excel,Test2_ThinkingMarks_excel[,9]==Question_to_analyze)
q1<-subset(q1,q1[,17]>0)

all_text<-paste(q1[,15],sep = " ")
all_text<-gsub("[^[:alnum:]]", " ", all_text)
all_text<-cleantext(all_text)
all_text<-tolower(all_text)
Unique_words <- unique(scan(text=all_text, what="char", sep=""))
w <- length(Unique_words)
which_words<-Unique_words %in% rownames(BEAGLE)
remove_words<-Unique_words[!which_words]
student_average_word<-matrix(data = 0,nrow = nrow(q1),ncol = ncol(BEAGLE))

for(i in 1:nrow(q1)){
  wordaverage<-c(rep(0,ncol(BEAGLE)))
  count<-0
  answer_text<-q1[i,15]
  answer_words<-(strsplit(paste0(answer_text),split = " "))[[1]]
  for(j in 1:length(answer_words)){
    if(answer_words[j]%in%rownames(BEAGLE)){
      wordaverage<-wordaverage+BEAGLE[answer_words[j],]
      count<-count+1
    }
  }
  student_average_word[i,]<-wordaverage/count
}


possible_marks<-unique(as.numeric(q1[,17][[1]]))
average_vec_for_grades<-matrix(data = 0,nrow = length(possible_marks),ncol = ncol(BEAGLE))
for(i in 1:length(possible_marks)){
  if(possible_marks[i]%in%as.numeric(q1[,17][[1]])){
    if(sum(possible_marks[i]==as.numeric(q1[,17][[1]]))==1){
      average_vec_for_grades[i,]<-student_average_word[as.numeric(q1[,17][[1]])==possible_marks[i],]
    }else{
      average_vec_for_grades[i,]<-colMeans(student_average_word[as.numeric(q1[,17][[1]])==possible_marks[i],],na.rm = T)
    }
  }
}
rownames(average_vec_for_grades)<-c(possible_marks)

Cosine <- function (x, y) {
  z <- sum(normalize(x) * normalize(y))
  return(z)
}

prediction<-c(rep(0,nrow(student_average_word)))
for(i in 1:nrow(student_average_word)){
  cosines<-c(rep(0,nrow(student_average_word)))
  for(j in 1:nrow(average_vec_for_grades)){
    cosines[j]<-Cosine(student_average_word[i,],average_vec_for_grades[j,])
  }
  prediction[i]<-rownames(average_vec_for_grades)[which(cosines==max(cosines))]
}

matrix(c(as.numeric(q1[,17][[1]]),as.numeric(prediction)),ncol = 2,byrow = F)
Cosine(as.numeric(q1[,17][[1]]),as.numeric(prediction))
cor(as.numeric(q1[,17][[1]]),as.numeric(prediction))
summary(lm(as.numeric(q1[,17][[1]])~as.numeric(prediction)))
