library(tidyverse)
library(magrittr)
library(rapportools) # for is.empty
library(dplyr)


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

# This line defines the inactivation function from the character string 'inactivation'
# defined in the inactivation model
eval(parse(text=inactivation ))

# First everything is cooked rare
# Then it continues to either well done or medium

df_larvae_in_portions <- df_larvae_in_portions %>% 
  mutate( is_welldone = rbinom( dplyr::n(), 1, p_welldone ), 
          part = as.factor(part),
          larvae_rare = round(inactivation_func( I0=larvae_per_portion,
                                            T0=rare_start_temp,
                                            T1=rare_end_temp, 
                                            t1=rare_end_time,
                                            inactivation_params ) ),
          larvae_welldone = round(inactivation_func( I0=larvae_rare,
                                                T0=rare_end_temp,
                                                T1=welldone_end_temp,
                                                t1=welldone_end_time,
                                                inactivation_params ) ),
          larvae_medium   = round(inactivation_func( I0=larvae_rare, 
                                                T0=rare_end_temp,
                                                T1=medium_end_temp, 
                                                t1=medium_end_time,
                                                inactivation_params ) ),
          # Extra cooking time after medium or welldone.
          larvae_after_cooking = ifelse(is_welldone, 
                                        round(inactivation_func( I0=larvae_welldone,
                                                                 T0=welldone_end_temp,
                                                                 T1=welldone_end_temp,
                                                                 t1=extra_time,
                                                                 inactivation_params )),
                                        round(inactivation_func( I0=larvae_medium,
                                                                 T0=medium_end_temp,
                                                                 T1=medium_end_temp,
                                                                 t1=extra_time,
                                                                 inactivation_params ))))


# Remove zero larvae rows, but remember how many
df_zero_larvae_in_portions_afterCook <- left_join( 
  df_zero_larvae_in_portions,
  df_larvae_in_portions %>% 
    group_by( part, simulation, carcass ) %>% 
    summarize( n_zeros_after_cooking = sum(larvae_after_cooking<=1), .groups="drop"), 
  by=c("part", "carcass", "simulation") ) %>% 
  mutate( n_zeros_after_cooking =  n_zeros_after_cooking + n_zeros )

df_larvae_in_portions_afterCook <- df_larvae_in_portions %>% 
  filter( larvae_after_cooking > 1 )

df_larvae_in_portions_afterCook_file <- "df_larvae_in_portions_Cook.csv"
write.csv(df_larvae_in_portions_afterCook, file = df_larvae_in_portions_afterCook_file, row.names = FALSE)

df_zero_larvae_in_portions_afterCook_file <- "df_zero_larvae_in_portions_Cook.csv"
write.csv(df_zero_larvae_in_portions_afterCook, file = df_zero_larvae_in_portions_afterCook_file, row.names = FALSE)