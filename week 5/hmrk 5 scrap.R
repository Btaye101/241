library(tidyverse)
library(GGally)
#2
cancer_DGP <- function(n) {
  tibble(
    age = runif(n,0,100),
    smoke = as.numeric(rbernoulli(n, p = case_when(
                              age %in% 25:64  ~ .145,
                              age %in% 0:10 ~ .00001,
                              TRUE ~ .08)
                       )),
    gene = runif(n,0,100),
    lung = as.numeric(rbernoulli(n, p = (gene+age+smoke*100)/300)),
    skin = as.numeric(rbernoulli(n, p = (gene+age)/200))
  )
}

test <- cancer_DGP(10000) %>% 
  ggpairs()

test

test_prob <- cancer_DGP(10000) %>% 
  filter(smoke == TRUE) %>% 
  count(lung) %>% 
  mutate(p = n/sum(n))

#3

logistic <- function(x){
  
  1/(1+exp(-x))
  
}

logistic(2)

DGP_A <- function(n){
  tibble(
  X = runif(n),
  W = rbernoulli(n),
  YgX,W = rnorm(n,X+W,1+X*W) 
)
}

 
DGP_B <- function(n){
  tibble(X = runif(n),
    WgX = rbernoulli(n,p= logistic(X)),
    YgX,W = rnorm(n,X+W,1+X*W) 
  )
} 


