df_summary %>%
  select( starts_with("p_"), part ) %>% 
  pivot_longer( -part, names_to="probability", values_to="value") %>% 
  group_by( probability, part ) %>% 
  summarize( value=mean(value)) %>% 
  pivot_wider( names_from=part, values_from=value )
