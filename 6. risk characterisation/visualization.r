library(patchwork)
library(ggplot2)

p <- (df_summary %>%
        select( starts_with("p_"), part ) %>% 
        pivot_longer( -part, names_to="probability", values_to="value") %>% 
        ggplot( ) +
        geom_boxplot( aes( x=probability, y=value, fill=part )) +
        scale_y_log10() +
        coord_flip() )+
  (df_summary %>%
     select( starts_with("n_"), part ) %>% 
     pivot_longer( -part, names_to="count", values_to="value") %>% 
     ggplot( ) +
     geom_boxplot( aes( x=part, y=value, fill=count ))) + 
  plot_annotation(
    title = 'Model output fractions and numbers' )

print(p)