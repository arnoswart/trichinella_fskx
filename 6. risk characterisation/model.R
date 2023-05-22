library(tidyverse)
library(magrittr)
library(rapportools)
library(dplyr)


if(is.empty(df_larvae_in_portions_afterDR_join)){
  df_larvae_in_portions <- read.csv(file = "df_larvae_in_portions_afterDR.csv")
} else {
  df_larvae_in_portions <- read.csv(file = df_larvae_in_portions_afterDR_join)
}


if(is.empty(df_zero_larvae_in_portions_afterCook_join)){
  df_zero_larvae_in_portions <- read.csv(file = "df_zero_larvae_in_portions_afterCook.csv")
} else {
  df_zero_larvae_in_portions <- read.csv(file = df_zero_larvae_in_portions_afterCook_join)
}


if(is.empty(swine_table_join)){
  swine_table <- read.csv(file = "swine_table.csv")
} else {
  swine_table <- read.csv(file = swine_table_join)
}


df_larvae_in_portions$part <- as.factor(df_larvae_in_portions$part)

# This is only for swine batches with 'escaped swine'
df_summary <- 
  df_larvae_in_portions %>% 
  left_join( df_zero_larvae_in_portions, by = c("carcass", "simulation", "part") ) %>% 
  unique() %>% 
  group_by( part, simulation ) %>% 
  summarise( n_portions_total = first( portions_per_part),
             n_portions_zero = first(n_zeros_after_cooking),
             p_ill_given_exposure = mean(p_ill ),
             .groups="drop" ) %>% 
  mutate( p_exposure = 1-n_portions_zero/n_portions_total,
          p_ill = p_ill_given_exposure * p_exposure,
          n_portions_nonzero = n_portions_total-n_portions_zero ) %>% 
  left_join( swine_table %>% 
               select( simulation, 
                       p_falseneg_batch=p.falseneg,
                       p_positive_carc=p.carc), by="simulation" )


df_summary_file <- "df_summary.csv"
write.csv(df_summary, file = df_summary_file, row.names = FALSE)