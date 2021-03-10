load.larvae.numbers <- function( type.animal, plot.dist=F ){
  
  
  if( type.animal=="boar" ){
    base.filename <- "sim.boar"
  }else if(type.animal=="pig"){
    base.filename <- "sim.pig"
  } else stop( "Error. Unknown option for animal type")
  
  lpg.dia <- list()
  df <- data.frame( larva = numeric(0), freq=numeric(0), sim=numeric(0) )
  
  files <- list.files("./Swine infection", paste0( base.filename, "[0-9]+.csv") )
  sim<-0
  for( f in files){
    temp <- read.csv(file = paste0("./Swine infection/",f) )
    if( nrow( temp )==0 ){
      lpg.dia[[f]] <- data.frame(larva=NA, Freq=NA)
    }else{
      lpg.dia[[f]] <- temp
      lpg.dia[[f]] <- lpg.dia[[f]] %>% filter( larva < 100 )
    }
    df <- rbind( df, data.frame( larva=lpg.dia[[f]]$larva, freq=lpg.dia[[f]]$Freq, sim=sim ))
    sim <- sim +1 
  }
  
  if( plot.dist ){
    df <- df %>% mutate( sim=as.factor(sim) ) %>% filter( larva !=0, !is.na( larva) )
    df <- df %>% group_by( sim ) %>% mutate( cumfreq = order_by(larva, cumsum(freq))) %>%
      mutate( cumfreq = cumfreq/max(cumfreq)) %>% ungroup %>% droplevels()
    
    p <- ggplot(df, aes( x=larva, y=cumfreq) )
    p <- p + geom_line( aes( color=sim) )
    p <- p + scale_x_continuous( "Larva per 100 g", limits=c(0,500))
    p <- p + scale_y_continuous( "Cummulative Frequency" )
    p <- p + theme(legend.position="none", text = element_text(size=20) )
    p
    
    
    ggsave( paste0("./Outcomes/", base.filename, "larva_dist.eps") )
    ggsave( paste0("./Outcomes/", base.filename, "larva_dist.pdf"))
  }
  return( lpg.dia )
}