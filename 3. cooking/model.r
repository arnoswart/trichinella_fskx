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

source("functions.R" )

df_larvae_in_portions <- df_larvae_in_portions %>% 
  mutate( larvae_rare = round( inactivation( k=inactivation.parameter, 
                                             alpha.plus=alpha.plus, 
                                             T.star=T.star, 
                                             I0=larvae_per_portion,
                                             T0=rare.start.temp,
                                             T1=rare.end.temp, 
                                             t1=rare.end.time ) ),
          larvae_welldone = round( inactivation( k=inactivation.parameter, 
                                                 alpha.plus=alpha.plus,
                                                 T.star=T.star, 
                                                 I0=larvae_rare,
                                                 T0=welldone.start.temp,
                                                 T1=welldone.end.temp, 
                                                 t1=welldone.end.time ) ),
          larvae_medium   = round( inactivation( k=inactivation.parameter, 
                                                 alpha.plus=alpha.plus, 
                                                 T.star=T.star, 
                                                 I0=larvae_rare, 
                                                 T0=medium.start.temp,
                                                 T1=medium.end.temp,
                                                 t1=medium.end.time ) ),
          larvae_after_cooking = ifelse(is_welldone, larvae_welldone, larvae_medium ) )

# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions <- left_join( 
  df_zero_larvae_in_portions,
  df_larvae_in_portions %>% 
    group_by( part, simulation, carcass ) %>% 
    summarize( n_zeros_after_cooking = sum(larvae_after_cooking==0), .groups="drop"), 
  by=c("part", "carcass", "simulation") )

df_larvae_in_portions <- df_larvae_in_portions %>% 
  filter( larvae_after_cooking != 0 )

# Remove all we don't need
toremove <- grep("^df_larvae_in_portions$|^swine.table$|df_zero_larvae_in_portions", ls(), 
                 invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))