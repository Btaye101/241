library(tidyverse)
library(magrittr)
library(simcausal)
#1.1

# That they are independent. Y⊥D

#1.2
# No since those with higher blood pressure are more likely to be insured to cover potential adverse health outcomes. Healthier individuals are less likely to be motivated to buy health insurance.

#1.3
# E[Y] = E[E[Y∣X]]  This is arrived at through the total expectation identity stating that E[X] = E[E[X|Y]]
# = E[E[Y ∣D = 1, X]] This is arrived at through conditioning on an independent RV where E[X|Y,Z] = E[X|Z] given X and Y are conditionally independent on Z, in this case Y and D are conditionally independent on X
# = E[μ(X)] This is arrived at with through the identity provided in the above problem where E[Y∣D=1,X]=μ(X)

#P2
#2.1
n = 1e6
full_data = tibble(
  X = runif(n),
  D = rbern(n, X),
  Y = X^2 + rnorm(n)
)

EY <- mean(full_data$Y)

observed_data <-  full_data %>%
  mutate(Y = ifelse(D, Y, NA))

yobv <- observed_data %>% 
  filter(D == 1) %>% 
  mutate(X2 = X^2) # used E[Y] = E[mu[X]] = E[X^2] since mu(x) = x^2
eyobv <- mean(yobv$X2)

#2.2
# 1st bullet: correct since Y|X,U is distributed normally with a mean of X^2 + U and the expectation of a normal distribution is just its mean which is X^2 + U in this case
# 2nd bullet: U cannot equal 1 given D is 1 since U and D are disjoint events. If U = 1 then D = (1-1) * Bern(X) which is equal to zero so D can only equal zero when U is 1. The same logic can be applied to the second part of this bullet; U can only equal zero when D is 1.
# 3rd bullet: Since P(U=1|D=1,X) = and P(U=0|D=1,X) = 1 we can substitute these values in resulting in: E[Y|D=1,X] = E[Y|D=1,X,U=1]*0 + E[Y|D=1,X,U=0] * 1 which simplifies to E[Y|D=1,X] = E[Y|D=1,X,U=0]. The wizard knows from the DGP that Y|X,U is normally distributed with mean X^2 + U so with U being 0 and it being normally distrbuted so its mean is the expectation E[Y|D=1,X,U=0] = X^2 + 0  = X^2. So E[Y|D=1,X] = mu(X) = X^2.  

#2.3
mu = function(x) x^2
n = 1e6
full_datau = tibble(
  U = rbern(n, 1/2),
  X = runif(n),
  D = U*rbern(n, X),
  Y = X^2 + U + rnorm(n)
)
observed_datau = full_datau %>%
  mutate(Y = ifelse(D, Y, NA),
         X2 = mu(X)) %>%
  select(-U)

Eyu <- mean(observed_datau$X2)

# Y is not conditionally independent with D on X?

# P3
#3.1
#Since the expectation of a conditioned RV is that function's output times the relative frequency of that RV's output given the condition summed over all possible RV values. Here function g(x,y)'s output is being multiplied by the relative frequency of said output given a particular z, this is summed over all possible values of x and y.

# 3.2


# P4


  
