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

# This is only for swine batches with 'escaped swine'
df_summary <- 
  df_larvae_in_portions %>% 
  left_join( df_zero_larvae_in_portions ) %>% 
  unique() %>% 
  group_by( part, simulation ) %>% 
  summarise( n_portions_total = first( portions_per_part),
             n_portions_zero = first(n_zeros_after_cooking),
             p_ill_given_exposure = mean(p_ill ),
             .groups="drop" ) %>% 
  mutate( p_exposure = 1-n_portions_zero/n_portions_total,
          p_ill = p_ill_given_exposure * p_exposure ) %>% 
  left_join( swine.table )
