library( ggplot2 )
library( gridExtra )
library( grid )

plot_table <- swine_table %>% 
  select( - simulation ) %>%
  mutate( has_falseneg = (n.falseneg!=0) ) %>% 
  group_by( has_falseneg ) %>% 
  summarize( across( everything(), median, na.rm=T) ) 

colnames( plot_table ) <-c("False negatives present",
                           "Number of swine",
                           "Number swine in \nfalse negative batches",
                           "Probability of swine from\n a false negative batch",
                           "Median number of swine  with\n zero larvae in a false negative batch",
                           "Median number of swine  with\n larvae  in a false negative batch",
                           "Probabiltiy of a positive swine in\n a false negative batch")

plot_table <- plot_table %>% 
  pivot_longer( -contains("False negatives present") ) %>% 
  mutate( `False negatives present` = ifelse( `False negatives present`==TRUE,
                                              "False negatives present",
                                              "No false negatives present")) %>% 
  pivot_wider( names_from=contains("False negatives present") )

grid.arrange(
  plot_table %>% tableGrob( rows=NULL ),
  df_larvae_in_parts %>% 
      pivot_longer(c(- simulation,-carcass), names_to="muscle", values_to="value") %>% 
      group_by( carcass, simulation ) %>% 
      filter( any(value!=0)) %>% 
      ggplot( ) +
        geom_boxplot( aes( x=muscle, y=value) ),
  ncol=1
)
