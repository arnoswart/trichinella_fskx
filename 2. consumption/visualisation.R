library( patchwork )


(df_larvae_in_portions %>% 
  ggplot() +
  geom_boxplot( aes( x=part, y=larvae_per_portion)))/
(df_larvae_in_portions %>% 
  select( part, portions_per_part) %>% 
  unique() %>% 
  ggplot() +
  geom_col( aes( x=part, y=portions_per_part)))
