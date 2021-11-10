library(tidyverse)
library(magrittr)
library(rapportools)
library(dplyr)

source("functions_cookingNew.r")


if(is.empty(df_larvae_in_portions_afterConsump_join)){
  df_larvae_in_portions <- read.csv(file = "df_larvae_in_portions_afterConsump.csv")
} else {
  df_larvae_in_portions <- read.csv(file = df_larvae_in_portions_afterConsump_join)
}


if(is.empty(df_zero_larvae_in_portions_afterConsump_join)){
  df_zero_larvae_in_portions <- read.csv(file = "df_zero_larvae_in_portions_afterConsump.csv")
} else {
  df_zero_larvae_in_portions <- read.csv(file = df_zero_larvae_in_portions_afterConsump_join)
}


############################################
# Cook the portions                        #
############################################

# First everything is cooked rare
# Then it continues to either well done or medium

df_larvae_in_portions <- df_larvae_in_portions %>% 
  mutate( larvae_rare = round(inactivation( I0=larvae_per_portion,T0=rare_start_temp,T1=rare_end_temp, t1=rare_end_time ) ),
          larvae_welldone = round(inactivation( I0=larvae_rare, T0=rare_end_temp,T1=welldone_end_temp, t1=welldone_end_time ) ),
          larvae_medium   = round(inactivation( I0=larvae_rare, T0=rare_end_temp,T1=medium_end_temp, t1=medium_end_time ) ),
          larvae_after_cooking = ifelse(is_welldone, larvae_welldone, larvae_medium ) )

df_larvae_in_portions$part <- as.factor(df_larvae_in_portions$part)

# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions_afterCook <- left_join( 
  df_zero_larvae_in_portions,
  df_larvae_in_portions %>% 
    group_by( part, simulation, carcass ) %>% 
    summarize( n_zeros_after_cooking = sum(larvae_after_cooking<=1), .groups="drop"), 
  by=c("part", "carcass", "simulation") ) %>% 
  mutate( n_zeros_after_cooking = coalesce( n_zeros_after_cooking, n_zeros ) )

df_larvae_in_portions_afterCook <- df_larvae_in_portions %>% 
  filter( larvae_after_cooking > 1 )


df_larvae_in_portions_afterCook_file <- "df_larvae_in_portions_Cook.csv"
write.csv(df_larvae_in_portions_afterCook, file = df_larvae_in_portions_afterCook_file, row.names = FALSE)

df_zero_larvae_in_portions_afterCook_file <- "df_zero_larvae_in_portions_Cook.csv"
write.csv(df_zero_larvae_in_portions_afterCook, file = df_zero_larvae_in_portions_afterCook_file, row.names = FALSE)