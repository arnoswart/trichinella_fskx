load.larvae.numbers <- function( ){
  lpg.dia <- list()
  df <- data.frame( larva = numeric(0), freq=numeric(0), sim=numeric(0) )
  
  files <- list.files("./Swine infection", pattern = "^.*\\.csv$")
  sim<-0
  for( f in files){
    lpg.dia[[f]] <- read.csv(file = paste0("./Swine infection/",f) )
    lpg.dia[[f]] <- lpg.dia[[f]] %>% filter( larva < 500 )
    df <- rbind( df, data.frame( larva=lpg.dia[[f]]$larva, freq=lpg.dia[[f]]$Freq, sim=sim ))
    sim <- sim +1 
  }
  
  df <- df %>% mutate( sim=as.factor(sim) ) %>% filter( larva !=0 )
  p <- ggplot(df, aes( x=larva, y=freq) )
  p <- p + geom_bar( aes( fill=sim), stat="identity")
  p <- p + scale_x_continuous( "Larva per 100 g", limits=c(0,500))
  p <- p + scale_y_continuous( "Frequency" )
  p <- p + theme(legend.position="none", text = element_text(size=20) )
  p
  ggsave( "./Outcomes/larva_dist.eps")
  ggsave( "./Outcomes/larva_dist.pdf")
  return( lpg.dia )
}