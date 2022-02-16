library( patchwork )
library(ggplot2)


((df_larvae_in_portions_afterConsump %>% 
  ggplot() +
  geom_boxplot( aes( x=part, y=larvae_per_portion)))/
(df_larvae_in_portions_afterConsump %>% 
  select( part, portions_per_part) %>% 
  unique() %>% 
  ggplot() +
  geom_col( aes( x=part, y=portions_per_part)))/
(df_zero_larvae_in_portions_afterConsump %>% 
   ggplot() +
   geom_boxplot( aes(x=part, y=n_zeros) ))) %>% 
 print()
