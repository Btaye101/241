library(tidyverse)
library(GGally)
#2
cancer_DGP <- function(n) {
  tibble(
    age = rlnorm(n, 3.584, 0.434),
    smoke = rbernoulli(n, p = case_when(
                              age %in% 25:64  ~ .145,
                              age %in% 0:10 ~ .00000000001,
                              TRUE ~ .08)
                       ),
    gene = runif(n,0,100),
    lung = rbernoulli(n, p = (logistic(gene,.5,50)+logistic(age,.1,60)+smoke)/9),
    skin = rbernoulli(n, p = (logistic(gene, .5,50)+logistic(age,.1,35))/7)
  )
}

test <- cancer_DGP(10000) %>% 
  ggpairs()

test

test_prob <- cancer_DGP(10000) %>%
  filter(age > 60, gene > 50) %>% 
  count(skin == TRUE) %>% 
  mutate(p = n/sum(n))
 

test_prob <- cancer_DGP(10000) %>%
  filter(age > 60, smoke == TRUE, gene > 50) %>% 
  count(lung) %>% 
  mutate(p = n/sum(n))


#3

logistic <- function(x, l=1,e=0){
  
  1/(1+exp(-l*(x-e)))
  
}

logistic(60,l = .005 ,e = 50)

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
tibble(x = seq(0,100,length = 100),
       y = logistic(x,l = .1, e = 30)) %>% ggplot() + geom_line(aes(x=x, y= y))


