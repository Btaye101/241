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

rlaplace <-  function(n, location = 0, scale = 1) {
  (1/(2*scale))* exp((-(abs(n-location)/scale)))
}

rlaplace(-1)

test <- tibble(w = seq(0,1,length = 1000),
               X = rlaplace(w)) %>% 
  ggplot()+
  geom_histogram(aes(x=X))

test  


