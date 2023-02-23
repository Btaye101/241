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
  Y = rnorm(n,X+W,1+X*W) 
)
}

 
DGP_B <- function(n){
  tibble(
    X = runif(n),
    W = rbernoulli(n,p= logistic(X)),
    Y = rnorm(n,X+W,1+X*W) 
  )
} 

DGP_C <- function(n){
  tibble(L = runif(n,-1,0),
         U = runif(n),
         M = runif(n,L,U),
         X = rnorm(n, L),
         Y = rnorm(n, U)
  )
} 

DGP_D <- function(n) {
  tibble(
    X = runif(n),
    W = rbernoulli(n, logistic(X)),
    Y = rnorm(n, W, 1+W)
  )
  
}
tibble(x = rlnorm(100000, meanlog = 3.22)) %>% ggplot() + geom_density(aes(x=x))

