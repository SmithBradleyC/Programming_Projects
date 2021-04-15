rm(list = ls()) # clear workspace

# Standard cosine function
Cosine <- function (x, y) {
  z <- sum(normalize(x) * normalize(y))
  return(z)
}
library(LSAfun)


load("stim_matrix.RData")
pca<-prcomp(vec_representation, center = FALSE)
summary(pca)


