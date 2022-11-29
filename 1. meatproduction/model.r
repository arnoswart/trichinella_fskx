library(tidyverse)
library(magrittr)
library(dplyr)

source("functions_meatProduction.r")

n_portions_per_part <- tibble(diaphragm = diaphragm,
                              shoulder  = shoulder,
                              belly     = belly, 
                              loin      = loin, 
                              ham       = ham, 
                              other     = other)
                              
swine_table <- tibble( 
  simulation=numeric(0),
  nSwine=numeric(0),
  n.falseneg=numeric(0), 
  p.falseneg=numeric(0), 
  n.zeros=numeric(0), 
  n.nzeros=numeric(0), 
  p.carc=numeric(0)
)

df_larvae_in_parts <- tibble(
  shoulder=numeric(0),
  belly=numeric(0),
  loin=numeric(0),
  ham=numeric(0),
  other=numeric(0),
  simulation=numeric(0),
  carcass=numeric(0)
)

#----------------------------------------------
# Run simulations 
#----------------------------------------------

larvae.dia <- infec.stat.swine(m=m, k=k, nSwine, swine_per_pool=swine_per_pool, 
                               propDiaphragm= propDiaphragm, sim_max= sim_max, 
                               alpha = alpha, beta=beta)

for( i in 1:sim_max )
{ 
  # How many larvae were found in each simulation
  larvae.sim <- larvae.dia %>% filter( iteration==i )
  
  if( !is.na(larvae.sim[1,"larva"]) )
  {
    n.zeros       <- larvae.sim %>% filter( larva==0 ) %>% pull( Freq )
    larvae.sim    <- larvae.sim %>% slice(-1)              # Row with zeros not needed any more.
    n.nzeros      <- larvae.sim %>% 
      summarize( n=sum(Freq) ) %>% 
      pull(n) # How often were larvae found? => n.nzeros-times
    
    swine_table <- add.to.swine.table( swine_table, i, nSwine=nSwine, 
                                       n.zeros=n.zeros, n.nzeros=n.nzeros )
    
    #################################
    # Distribute larvae over parts  #
    #################################
    
    df_larvae_in_parts <- df_larvae_in_parts %>% 
      rbind(
        larvae.sim %>% 
          slice_sample( n=nCarc, weight_by = Freq, replace=TRUE ) %>% 
          pull( larva ) %>% 
          map_dfr( .f = ~sampleNM( n=1, 
                                   p=p_larvae_in_part, 
                                   m=.x * n_portions_per_part[['diaphragm']] ) ) %>% 
          mutate( simulation=i, carcass = 1:dplyr::n() ))
  } else { #no false negative batches this year
    df_larvae_in_parts <-  df_larvae_in_parts %>% 
      rbind(tibble( shoulder=rep(0,nCarc),
                    belly=rep(0,nCarc),
                    loin=rep(0,nCarc),
                    ham=rep(0,nCarc),
                    other=rep(0,nCarc),
                    carcass=1:nCarc,
                    simulation=i ))
    swine_table <- add.to.swine.table( swine_table, i, nSwine, n.zeros=0, n.nzeros=0 )
  }
}


df_larvae_in_parts_file <- "df_larvae_in_parts_MP.csv"
write.csv(df_larvae_in_parts, file = df_larvae_in_parts_file, row.names = FALSE)

swine_table_file <- "swine_table_MP.csv"
write.csv(swine_table, file = swine_table_file, row.names = FALSE)
