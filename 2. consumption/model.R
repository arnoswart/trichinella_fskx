library(tidyverse)
library(magrittr)
library(rapportools)
library(dplyr)

source("functions_consumption.R")

n_portions_per_part <- tibble(diaphragm = diaphragm,
                              shoulder  = shoulder,
                              belly     = belly, 
                              loin      = loin, 
                              ham       = ham, 
                              other     = other)

if(is.empty(df_larvae_in_parts_join)){
  df_larvae_in_parts <- read.csv(file = "df_larvae_in_parts.csv")
} else {
  df_larvae_in_parts <- read.csv(file = df_larvae_in_parts_join)
}


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
df_zero_larvae_in_portions_afterConsump <- df_larvae_in_portions %>% 
  group_by( part, simulation, carcass ) %>% 
  summarize( n_zeros = sum(larvae_per_portion<=1), .groups="drop")

df_larvae_in_portions_afterConsump <- df_larvae_in_portions %>% 
  filter( larvae_per_portion > 1 )


df_larvae_in_portions_afterConsump_file <- "df_larvae_in_portions_Cons.csv"
write.csv(df_larvae_in_portions_afterConsump, file = df_larvae_in_portions_afterConsump_file, row.names = FALSE)

df_zero_larvae_in_portions_afterConsump_file <- "df_zero_larvae_in_portions_Cons.csv"
write.csv(df_zero_larvae_in_portions_afterConsump, file = df_zero_larvae_in_portions_afterConsump_file, row.names = FALSE)