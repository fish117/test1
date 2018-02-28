install.packages("scales")
install.packages("tidyverse") 


  
install.packages('tidyverse', dependencies=TRUE, type="source", repos="https://cloud.r-project.org")
install.packages("ISLR") 
install.packages("knitr")

library(tibble)
library(tidyverse)
library(dplyr)
#8.1
#Using both the values of X1 and X2

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)# tibble means table frame
}
mydata1<-genreg(1000)
dat<-mutate(mydata1,yhat=5,yhat1=5-x1,yhat2=5+2*x2,yhat12=5-x1+2*x2)
mse<-mean((dat$yhat-dat$y)^2)
mse1<-mean((dat$yhat1-dat$y)^2)
mse2<-mean((dat$yhat2-dat$y)^2)
mse12<-mean((dat$yhat12-dat$y)^2)
#when we add the parameters, the error decrases.



#8.2  Oracle classification

# when x=2
gencla <- function(n) {
  x <- 2 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x)))
  tibble(x=x, y=y)
}
mydata1<-gencla(1000)  



