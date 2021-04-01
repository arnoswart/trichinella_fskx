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

#----------------------------------------------
# functions
#----------------------------------------------

inactivation <- function( k, alpha.plus, T.star, I0, T0,T1, t1 ){
  I0*((exp(k*T.star) + exp( k * T1 ))/(exp(k*T.star) +exp( k * T0 )))^(alpha.plus * t1 /( k*(T0-T1)))                                                                                          
}

# New inactivation function
inactivation_new <- function(I0, T0,T1, t1){
  # just using the final temperature for now.
  # later better do a piecewise linear approximation between T0 and T1
  temp = T1
  p_surv = -1.38e3 + 8.56*t1 + 1.09e1*temp - 4.06e-2*t1*temp +8.31e2*log(t1) +
            2.94e2*log(temp) -2.53e2*log(t1)*log(temp)
  return( I0*(1-p_surv) )
}

my_rmultinom <- function( larvae_per_part, portions_per_part,...){
  return( cbind(...,
                larvae_per_part,
                portions_per_part,
                tibble( portion=1:portions_per_part,
                        larvae_per_portion=rmultinom(1, larvae_per_part, 
                                                     rep(1/portions_per_part, portions_per_part))[,1])))
}


#----------------------------------------------
# Run simulations 
#----------------------------------------------


###############################
# Divide larvae over portions #
###############################

df_larvae_in_portions <- df_larvae_in_parts %>% 
  pivot_longer( -c(carcass, simulation), names_to="part", values_to="larvae_per_part" ) %>% 
  left_join( 
    n_portions_per_part %>% 
      pivot_longer( everything(), names_to="part", values_to="portions_per_part" )) %>% 
  pmap_dfr( my_rmultinom )
  
# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions <- df_larvae_in_portions %>% 
  group_by( part, simulation, carcass ) %>% 
  summarize( n_zeros = sum(larvae_per_portion==0))

df_larvae_in_portions <- df_larvae_in_portions %>% 
  filter( larvae_per_portion != 0 )

############################################
# Cook the portions                        #
############################################

# First everything is cooked rare
# Then it continues to either well done or medium
# TODO: make nice scenarios for the cooking styles to replace the fixed numbers below
# TODO: plugin new inactivation model
df_larvae_in_portions <- df_larvae_in_portions %>% 
  mutate( is_welldone = rbinom( n(), 1, p_welldone ),
          larvae_rare = round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, 
                                             I0=larvae_per_portion,T0=20,T1=54, t1=2.5 ) ),
          larvae_welldone = round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, 
                                                 I0=larvae_rare, T0=54,T1=76.7, t1=15 ) ),
          larvae_medium   = round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3, 
                                                 I0=larvae_rare, T0=54,T1=63, t1=1.5 ) ),
          larvae_after_cooking = ifelse(is_welldone, larvae_welldone, larvae_medium ) )

# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions <- left_join( 
  df_zero_larvae_in_portions,
  df_larvae_in_portions %>% 
    group_by( part, simulation, carcass ) %>% 
    summarize( n_zeros_after_cooking = sum(larvae_after_cooking==0), .groups="drop"), by="part")

df_larvae_in_portions <- df_larvae_in_portions %>% 
  filter( larvae_after_cooking != 0 )

# Remove all we don't need
toremove <- grep("^df_larvae_in_portions$|^swine.table$|df_zero_larvae_in_portions", ls(), 
                 invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
