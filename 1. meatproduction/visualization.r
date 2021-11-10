library( patchwork )
library( flextable )
library( ggplotify )
library( magick )
library(ggplot2)
library(webshot)

wrap_plots(
  swine_table %>% 
    select( - simulation ) %>%
    mutate( has_falseneg = (n.falseneg!=0) ) %>% 
    group_by( has_falseneg ) %>% 
    summarize( across( everything(), median, na.rm=T) ) %>% 
    flextable() %>% 
    add_header_row(values = c("False negatives  present",
      "Number of swine",
      "Number swine in false negative batches",
      "Probability of swine from a false negative batch",
      "Median number of swine  with zero larvae in a false negative batch",
      "Median number of swine  with larvae  in a false negative batch",
      "Probabiltiy of a positive swine in a false negative batch")) %>% 
    as_raster() %>% 
    as.ggplot(),
  df_larvae_in_parts %>% 
      pivot_longer(c(- simulation,-carcass), names_to="muscle", values_to="value") %>% 
      group_by( carcass, simulation ) %>% 
      filter( any(value!=0)) %>% 
      ggplot( ) +
        geom_boxplot( aes( x=muscle, y=value) ),
  ncol=1
)