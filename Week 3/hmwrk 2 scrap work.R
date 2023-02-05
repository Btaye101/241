library(tidyverse)

ya <- tibble(X = seq(-5,5, length = 1000)) %>% #prob 1.1
  mutate(Y = case_when(X < 0 ~ X,
                       X >= 0 ~ X^2)) %>% 
  ggplot() +
  geom_line(aes(x=X,y=Y))

ya

ya_inv <- tibble(X = seq(-5,25, length = 1000)) %>% # prob 1.2
  mutate(Y = case_when(X < 0 ~ X,
                       X >= 0 ~ X^0.5)) %>% 
  ggplot() +
  geom_line(aes(x=X,y=Y))

ya_inv




univ_prob1 <- tibble(w = seq(0,1, length = 1000), #6.1
                     x = w^2) %>%
  mutate(inside = (x >= 0.2 & x <= 0.3) | (x >= 0.9 & x <= 1)) %>% 
  count(inside) %>% 
  mutate(prob = n/sum(n))

univ_prob2 <- tibble(w = seq(0,1, length = 1000), #6.2
                     y = (1-w)^2) %>%
  mutate(inside = (y >= 0.2 & y <= 0.3) | (y >= 0.9 & y <= 1)) %>% 
  count(inside) %>% 
  mutate(prob = n/sum(n))




#6.3
funk <- function(w) {
  w^2 + 1
}

probability <- function(a,b,X) {
  prob <- tibble(w = seq(0,1, length =1000),
                 Fx = X(w)) %>% 
    mutate(inside = Fx >= a & Fx <= b) %>% 
    count(inside) %>% 
    mutate(prob = n/sum(n)) 
    return(pull(prob[2,3]))
    #return(prob)
}

probability(1.33,1.66,funk) 
