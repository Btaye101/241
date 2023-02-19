#4.1
library(tidyverse)
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


