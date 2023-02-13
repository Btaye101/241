library(tidyverse)
#2.1

  # TALBE
    values <- tibble(w = seq(0,1,length = 1000),
                     X = w^2,
                     Y = (1-w)^2,)
    
  # CDF FUNCTION
    CDF <- function(df, rv) {
      
      rv <- enquo(rv)
      df %>% 
        arrange(!!rv) %>% 
        mutate(p = row_number()/nrow(df)) %>% 
        ggplot() +
        geom_line(aes(x = !!rv, y = p))
      
    }
  # X CDF
    X_CDF <- CDF(values,X)
    X_CDF
  
  # X PDF
    X_PDF <- values %>% 
      ggplot()+
      geom_histogram(aes(x = X), binwidth = .01)
    
    X_PDF
    

  # Y CDF
Y_CDF <- CDF(df=values,rv = Y)

Y_CDF

  # Y PDF

Y_PDF <- values %>% 
  ggplot()+
  geom_histogram(aes(x = Y), binwidth = .01)

Y_PDF

# 2.2 and 2.3

#TABLE
custom <- tibble(w = seq(0,1,length = 1000),
                 X = 2^w,
                 Y = 2^(1-w))
# CUSTOM X CDF
CX_CDF <- CDF(custom,X)

CX_CDF
# CUSTOM X PDF

CX_PDF <- custom %>% 
  ggplot()+
  geom_histogram(aes(x = X))

CX_PDF

# CUSTOM Y CDF
CY_CDF <- CDF(custom,Y)

CY_CDF
# CUSTOM Y PDF

CY_PDF <- custom %>% 
  ggplot()+
  geom_histogram(aes(x = Y))

CY_PDF

# 3.1

#rlaplace <-  function(n, location = 0, scale = 1) {
 # (1/(2*scale))* exp((-(abs(n-location)/scale)))
#}

# a is mu location b is scale

qlap <- function(p, a = 0, b = 1) {
  return(case_when(0 <= p & p < 0.5 ~ a+b*log(2*p),
            p >= 0.5 & p <= 1 ~ a-b*log(2*(1-p)),
        )
  )
}
rlaplace <- function(n,location = 0,scale = 1) {
  tibble(p = runif(n),
         X = qlap(p,location,scale)) %>% 
    pull(X)
  
}

lap_PDF <- ggplot() +
  geom_histogram(aes(x = rlaplace(1000)))
lap_PDF
lap_CDF <- tibble(p = seq(0,1,length = 1000),
              X = qlap(p)) %>% 
  ggplot() +
  geom_line(aes(x = X, y = p))
lap_CDF

#1

# Distributions allow us to better visualize the probability of certain events that random variables assign. CDFs, PDFs, and quantile functions all allow us to directly quantify probabilities of events without us having to get the pre-image of the RV's output to derive the same information.




#5.1

# Height in a given population is probably normally distributed as individual height will symmetrically cluster around the population mean and symmetrically taper off in frequency on both sides as the height measures get more extreme.

#5.2

# A fair coin toss has a Bernoulli distribution as it is a discrete distribution with two possible outcomes: heads or tails, with a probability of 0.5 for either outcome. 

#5.3

# The number of work place accidents at a construction site in a year probably has a poisson distribution. The number of Workplace accidents is a countable event within a given time interval, is relatively rare, and the probability of the occurrence of one accident is independent of the probability of the occurrence of another. 

#5.4

# The probability of rolling a particular value on a six sided die is uniformly distributed as each value as an equal probability of occurrence (1/6).

#5.5

# The amount of time it takes to receive an order of chicken nuggets from a particular McDonalds location is probably exponentially distributed as the wait times will cluster around relatively small values (it's just chicken nuggets) and dramatically fall off from there in an exponential fashion.


