library( patchwork )


(df_larvae_in_portions %>% 
  filter( p_ill > 0 ) %>% 
  ggplot() +
    geom_boxplot( aes(x=part, y=p_ill) ) +
    scale_y_log10())/
(df_larvae_in_portions %>% 
  group_by( part ) %>% 
  summarize( p_ill_zero = sum(p_ill==0)/n() ) %>% 
   ggplot( ) +
   geom_col( aes(x=part,y=p_ill_zero)) )
