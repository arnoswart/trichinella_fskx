library( patchwork )
library(ggplot2)

((df_larvae_in_portions_afterCook %>% 
  pivot_longer( c(larvae_per_portion, larvae_after_cooking), names_to="cooked", values_to="larvae") %>% 
  ggplot() +
    geom_boxplot( aes(x=part, y=larvae, fill=cooked) ))/
(df_zero_larvae_in_portions_afterCook %>% 
  pivot_longer( starts_with("n_zeros"), names_to="zeros", values_to="value") %>% 
  ggplot() +
    geom_boxplot( aes(x=part, y=value, color=zeros )))) %>% 
print()