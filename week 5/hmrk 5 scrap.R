library(tidyverse)
library(GGally)
#2####################################
logistic <- function(x, l=1,e=0){
  
  1/(1+exp(-l*(x-e)))
  
}

logistic(60,l = .005 ,e = 50)

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



test_prob <- cancer_DGP(10000) %>%
  filter(age > 60, gene > 50) %>% 
  count(skin == TRUE) %>% 
  mutate(p = n/sum(n))
 

test_prob <- cancer_DGP(10000) %>%
  filter(age > 60, smoke == TRUE, gene > 50) %>% 
  count(lung) %>% 
  mutate(p = n/sum(n))
# age: I chose a logistic normal distribution to eliminate any negative ages while trying to create a right skewed distribution. Age is independent of gene since it is only a function of time and not any of your genes.
# gene: I chose a uniform distribution over the interval [0,100] since I think there is an equal chance of acquiring a random mutation. This is independent of age since overall your genes don't typically change with age.
# smoke: I chose a Bernoulli distribution conditioned on age since it is a binary variable with how it was written that is more likely to occur given certain intervals of age (Interval prevalence data taken from CDC). I have smoking as a conditional distribution. Thus given age smoke is conditionally independent with Lung and Skin.
# Lung: I chose a Bernoulli distribution conditioned on age and gene which are transformed using a logistic function to output probabilities that are weighted towards higher ages and higher gene scores respectively. Lung is also conditioned on smoking status with smokers contributing to a higher probability of lung cancer. Lung and skin are conditionally independent given age and gene.
# skin: I chose a Bernoulli distribution conditioned on age and gene which are transformed using a logistic function to output probabilities that are weighted towards higher ages and higher gene scores respectively. I adjusted the age distribution logistic to scale up at a younger age than that of the lung variable since skin cancer can occur at an earlier age than lung cancer. Skin cancer also is of hgiher prevalence in general so I made sure it occurs more in this DGP than lung cancer.

test <- cancer_DGP(10000) %>% 
  ggpairs()

test

# One surprising thing is that the age distribution of smokers and non-smokers does not differ all that much. The same thing can be said with the age distribution between those with lung cancer and those without. I expected the distribution mass to shift right as I thought I had coded that smokers and people with Lung cancer should be of higher age. Upon closer inspection of the marginal distribution of age this could be due to there not being enough occurrences of older people.I could adjust the logistic normal distribution somehow to spread it out to increase the probability of older ages.

#3##################################

prob <- function(df, ...) {
  final <- df %>% 
  count(...) %>% 
    mutate(p = n/sum(n)) 
    pull(final[2,3])
}

#################A###########
DGP_A <- function(n){
  tibble(
  X = runif(n),
  W = rbernoulli(n),
  Y = rnorm(n,X+W,1+X*W) 
)
}
### Independent: X and W
#P(X)
prob_ax <- prob(DGP_A(10000), X < .5)
prob_ax
#P(X|W)
prob_axgw <- DGP_A(10000)%>% 
  filter(W == 1) %>% 
  count(X < .5) %>% 
  mutate(p = n/sum(n))
  pull(prob_axgw[2,3])
# Probabilities are equal so X and W are independent 
### conditionally independent: none
#P(Y|X)
prob_aygw <- DGP_A(10000)%>% 
    filter(X < .5) %>% 
    count(Y < .5) %>% 
    mutate(p = n/sum(n))
  pull(prob_aygw[2,3])
   
#P(Y|X,W)
  prob_aygwx <- DGP_A(10000)%>% 
    filter(W == 1,X< .5) %>% 
    count(Y < .5) %>% 
    mutate(p = n/sum(n))
  pull(prob_aygwx[2,3])
# The probabilities are not equal so Y and W are not conditionally independent on X.
###############################B##################
DGP_B <- function(n){
  tibble(
    X = runif(n),
    W = rbernoulli(n,p= logistic(X)),
    Y = rnorm(n,X+W,1+X*W) 
  )
} 
### Independent: none
#P(W)
prob_bw <- prob(DGP_B(10000),W == 1)
prob_bw

#P(W|X)
prob_bwgx <- DGP_B(10000)%>% 
  filter(X < .5) %>% 
  count(W == 1) %>% 
  mutate(p = n/sum(n))
pull(prob_bwgx[2,3])
# The probabilities are not equal so X and W are not independent.
### conditionally independent: none
# P(Y|X)
prob_bygx <- DGP_B(10000)%>% 
  filter(X < .5) %>% 
  count(Y < 2) %>% 
  mutate(p = n/sum(n))
pull(prob_bygx[2,3])
# P(Y|X,W)
prob_bygxw <- DGP_B(10000)%>% 
  filter(W == 1 & X < .5) %>% 
  count(Y < 2) %>%
  mutate(p = n/sum(n))
pull(prob_bygxw[2,3])
# Probabilities are not equal so Y and W are not conditionally independent on X

#######################C######################
DGP_C <- function(n){
  tibble(L = runif(n,-1,0),
         U = runif(n),
         M = runif(n,L,U),
         X = rnorm(n, L),
         Y = rnorm(n, U)
  )
} 
### Independent: L and U
#P(L)
prob_cl <- prob(DGP_C(10000), L < -.5)
prob_cl
#P(L|U)
prob_clgu <- DGP_C(10000)%>% 
  filter(U < .5) %>% 
  count(L < -.5) %>% 
  mutate(p = n/sum(n))
pull(prob_clgu[2,3]) 
# The probabilities are the same so  L and U are independent.

### conditionally independent: M and X given L, M and Y given U
#P(M|L)
prob_cmgl <- DGP_C(10000) %>% 
  filter(L < -.5) %>% 
  count(M <.5) %>% 
  mutate(p = n/sum(n))
pull(prob_cmgl[2,3]) 

#P(M|L,X)
prob_cmglx <- DGP_C(10000) %>% 
  filter(L < -.5, X < -.5) %>% 
  count(M <.5) %>% 
  mutate(p = n/sum(n))
pull(prob_cmglx[2,3]) 

# The probabilities are the same proving M and X are conditionally independent on L.
################################D###################
DGP_D <- function(n) {
  tibble(
    X = runif(n),
    W = rbernoulli(n, logistic(X)),
    Y = rnorm(n, W, 1+W)
  )
### Independent: none
#P(Y)
prob_dy <- prob(DGP_D(10000), Y< 1)
prob_dy
#P(Y|W)
prob_dygw <- DGP_D(10000) %>% 
  filter(W == 1) %>% 
  count(Y < 1) %>% 
  mutate(p = n/sum(n))
pull(prob_dygw[2,3]) 

# Probabilities are not equal thus Y and W are not independent.
### Conditionally independent: none

#P(Y|X)
prob_dygx <- DGP_D(10000) %>% 
  filter(X < .5) %>% 
  count(Y < 1) %>% 
  mutate(p = n/sum(n))
pull(prob_dygx[2,3]) 
#P(Y|X,W)
prob_dygxw <- DGP_D(10000) %>% 
  filter(X < .5, W == 1) %>% 
  count(Y < 1) %>% 
  mutate(p = n/sum(n))
pull(prob_dygxw[2,3]) 

# Probabilities are not equal thus Y and W are not conditionally independent on X.
  
  
}
tibble(x = seq(0,100,length = 100),
       y = logistic(x,l = .1, e = 30)) %>% ggplot() + geom_line(aes(x=x, y= y))


