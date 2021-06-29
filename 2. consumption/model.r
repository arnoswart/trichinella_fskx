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
      pivot_longer( everything(), names_to="part", values_to="portions_per_part" ),
    by="part" ) %>% 
  pmap_dfr( my_rmultinom )
  
# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions <- df_larvae_in_portions %>% 
  group_by( part, simulation, carcass ) %>% 
  summarize( n_zeros = sum(larvae_per_portion==0), .groups="drop")

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
  mutate( is_welldone = rbinom( n(), 1, p_welldone ))

# Remove all we don't need
toremove <- grep("^df_larvae_in_portions$|^swine.table$|df_zero_larvae_in_portions", ls(), 
                 invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))
