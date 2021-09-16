library( gridExtra )
library( patchwork )

q25 <- function( x,...) quantile(x, probs=0.025,...)
q975 <- function( x,...) quantile(x, probs=0.975,...)

t1 <- swine.table %>% 
  select(-simulation) %>% 
  summarize( across( .cols = everything(),
                     .fns = list("mean" = mean, "sd" = sd, "q25"=q25, "q975"=q975),
                     na.rm=TRUE) ) %>% 
  pivot_longer( everything(), names_to =c("var", ".value"), names_sep="_" ) %>% 
  tableGrob(theme = ttheme_minimal(), rows = NULL)

p1 <- df_larvae_in_parts %>%
  pivot_longer( -c(carcass, simulation), names_to="part", values_to="larva" ) %>% 
  group_by(carcass, part ) %>% 
  summarize( larva = mean(larva) ) %>% 
  ggplot() +
    geom_boxplot( aes(x=part, y=larva)) +
    ggtitle( "Larva in parts, variation over carcasses")

wrap_plots( t1, p1, ncol = 1 )
