#4.1
library(tidyverse)
library(rmutil)
library(pracma)
X = function() {
  c(X = rnorm(1))
}
Y_given_X = function(x) {
  c(Y = rexp(1, rate=abs(x)))
}
Y_and_X = function() {
  x = X()
  y = Y_given_X(x)
  return(list(X=x, Y=y))
}
sample = function(dist, n, ...) {
  map_df(1:n, function(i) dist(...))
}

prob_x <- X %>% 
  sample(10000) %>% 
  count(X<0) %>% 
  mutate(p = n/sum(n))

prob_ygx <- Y_given_X %>% 
  sample(10000, x= 1) %>% 
  count(Y>1) %>% 
  mutate(p = n/sum(n))

prob_yandx <- Y_and_X %>% 
  sample(10000) %>% 
  count(X<0 & Y > 1) %>% 
  mutate(p = n/sum(n))

#4.2

# it's not possible to compute since Y is continuous and we can't get it to exactly equal 1 and thus the probability is zero.

#4.3
prob_xgy1 <- Y_and_X %>% 
  sample(10000) %>% 
  filter(Y > 1) %>%
  count(X<0) %>% 
 mutate(p = n/sum(n))

#5.1
f_X = function(x) {dnorm(x)
}

f_Y_given_X = function(y,x) {
  dexp(y, rate=abs(x))
}

f_Y_and_X <- function(y,x){
  X <-  f_X(x)
  Y <- f_Y_given_X(y,x)
  return(c(X,Y))
  
}
#5.2
f_Y_given_X_is_0 <- function(y){
  dexp(y, rate=abs(0))
}
f_Y_given_X_is_0(.2)


#5.3

p1 <- integrate(f_X,-1,0)

p2 <- integrate(f_Y_given_X_is_0,-1,0)

p3 <- int2(f_Y_and_X, a = c(0,0), b = c(1,1))
    