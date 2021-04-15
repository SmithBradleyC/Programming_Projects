rm(list = ls()) # clear workspace

# Standard cosine function
Cosine <- function (x, y) {
  z <- sum(normalize(x) * normalize(y))
  return(z)
}
library(LSAfun)

load("stim_matrix.RData")

SVD<-svd(vec_representation)
#U<-SVD$u
W<-SVD$v%*%t(SVD$v)


x<-test_vec_representation[1,]
x_hat<-(SVD$v[,1:40])%*%(t(SVD$v[,1:40])%*%x)
#x_hat2<-W%*%x

#Cosine(x,x_hat)
#eig<-eigen(cor(vec_representation))

library(neuralnet)
mount<-c(rep(1,n_train_items),rep(0,n_train_items)) # correct answers (1 if mountain)
n_eigen_values<-10
predictors<-matrix(data = 0,nrow = n_train_items*2, ncol = n_eigen_values)
for(i in 1:(n_train_items*2)){
  predictors[i,]<-t(SVD$v[,1:n_eigen_values])%*%vec_representation[i,]
}
df<-data.frame(mount,predictors)

nn<-neuralnet(mount~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10
              ,data=df,
              hidden = 0)
plot(nn)


# predict the new images
predictors<-matrix(0,nrow = nrow(test_vec_representation),ncol = n_eigen_values)
for(i in 1:nrow(test_vec_representation)){
  predictors[i,]<-t(SVD$v[,1:n_eigen_values])%*%test_vec_representation[i,]
}
answer<-c(rep(1,n_test_items),rep(0,n_test_items))
test<-data.frame(predictors)

## Prediction using neural network
Predict<-compute(nn,test)
Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

sum(pred==answer)/(n_test_items*2)*100
