library(tidyverse)
library(magrittr)
library(rapportools)
library(dplyr)


if(is.empty(df_larvae_in_portions_afterCook_join)){
  df_larvae_in_portions <- read.csv(file = "df_larvae_in_portions_afterCook.csv")
} else {
  df_larvae_in_portions <- read.csv(file = df_larvae_in_portions_afterCook_join)
}


dose.response <- function( p_surv, d, r )
{
  1 + exp( d *log(1-p_surv) ) - 
    exp(d*log(1-r*p_surv))- 
    exp(d*log(1-(1-r)*p_surv))
}

p_surv <- read.csv( file = ab, header=F,
                    colClasses=c("numeric","numeric") ) %>% 
  setNames( c("a", "b") ) %>% 
  mutate( p_surv = rbeta( n=dplyr::n(), shape1=a, shape2=b )) %>% 
  pull( p_surv )

############################################
# dose-resp                                #
############################################
df_larvae_in_portions_afterDR <- df_larvae_in_portions %>%
  mutate( p_surv_larva = sample( p_surv, dplyr::n(), replace=TRUE) ) %>% 
  mutate( p_ill = dose.response( p_surv_larva, larvae_after_cooking, r ) )


df_larvae_in_portions_afterDR_file <- "df_larvae_in_portions_DR.csv"
write.csv(df_larvae_in_portions_afterDR, file = df_larvae_in_portions_afterDR_file, row.names = FALSE)

