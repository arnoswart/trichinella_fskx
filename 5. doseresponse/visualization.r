library( patchwork )
library(ggplot2)
library(dplyr)


((df_larvae_in_portions_afterDR %>% 
  filter( p_ill > 0 ) %>% 
  ggplot() +
    geom_boxplot( aes(x=part, y=p_ill) ) +
    scale_y_log10())/
(df_larvae_in_portions_afterDR %>% 
  group_by( part ) %>% 
  summarise( p_ill_zero = sum(p_ill==0)/dplyr::n() ) %>% 
   ggplot( ) +
   geom_col( aes(x=part,y=p_ill_zero)) )) %>% 
print()