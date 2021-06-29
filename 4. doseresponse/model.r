#----------------------------------------------
# Reference and info
#----------------------------------------------
# The mathematical model is published in Frits Franssen, Arno Swart, Joke van der Giessen, Arie Havelaar, Katsuhisa Takumi, 
# Parasite to patient: A quantitative risk model for Trichinella spp. in pork and wild boar meat, 
# International Journal of Food Microbiology, Volume 241, 2017, Pages 262-275, ISSN 0168-1605.

# Trichinella muscle larvae (ML)

#----------------------------------------------
# library
#----------------------------------------------
library( tidyverse )

dose.response <- function( p_surv, d, r )
{
 1 + exp( d *log(1-p_surv) ) - 
                exp(d*log(1-r*p_surv))- 
                exp(d*log(1-(1-r)*p_surv))
}

p_surv <- read.csv( file = ab_filename, header=F,
                colClasses=c("numeric","numeric") ) %>% 
  setNames( c("a", "b") ) %>% 
  mutate( p_surv = rbeta( n=n(), shape1=a, shape2=b )) %>% 
  pull( p_surv )

############################################
# dose-resp                                #
############################################
df_larvae_in_portions <- df_larvae_in_portions %>%
  mutate( p_surv_larva = sample( p_surv, n(), replace=TRUE) ) %>% 
  mutate( p_ill = dose.response( p_surv_larva, larvae_after_cooking, r ) )

rm( list=c("ab_filename", "p_surv", "r", "dose.response"))
