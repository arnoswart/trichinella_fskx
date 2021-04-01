#----------------------------------------------
# Reference and info
#----------------------------------------------
# The mathematical model is published in Frits Franssen, Arno Swart, Joke van der Giessen, Arie Havelaar, Katsuhisa Takumi, 
# Parasite to patient: A quantitative risk model for Trichinella spp. in pork and wild boar meat, 
# International Journal of Food Microbiology, Volume 241, 2017, Pages 262-275, ISSN 0168-1605.

# Trichinella muscle larvae (ML)

#----------------------------------------------
# library
#----------------------------------------------
library( tidyverse )

add.table.row <- function( table.input, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, type )
{
  return( rbind( table.input, data.frame( n.portions = n.portions,
                                          n.zeros = n.zeros,
                                          p.portion = 1 - n.zeros/n.portions,
                                          n.cooked.zeros = n.cooked.zeros,
                                          p.cooked = p.cooked,
                                          p.ill = p.cooked * p.ill,
                                          n.ill = p.cooked * p.ill * n.portions,
                                          n.ill.overall = p.cooked * p.ill * n.portions * p.swine,
                                          per.million = p.cooked * p.ill * p.swine * 10^6,
                                          type = type
  )
  )
  )
}

add.to.table <- function( table.input, nCarc, n_portions_per_part, p.swine, shoulder, belly, loin ){
  
  # Shoulder 
  n.portions     <- nCarc * n_portions_per_part[['shoulder']] # nr. of carcassess * nr of portions per carcass
  n.zeros        <- shoulder$zeros
  n.cooked.zeros <- shoulder$zeros.after.cooking + shoulder$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( shoulder$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros, 
                                p.cooked, p.ill, p.swine, "shoulder" )
  
  # Belly
  n.portions     <- nCarc * n_portions_per_part[['belly']] # nr. of carcassess * nr of portions per carcass
  n.zeros        <- belly$zeros
  n.cooked.zeros <- belly$zeros.after.cooking + belly$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( belly$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros, 
                                p.cooked, p.ill, p.swine, "belly" )
  
  # Loin
  n.portions     <- nCarc * n_portions_per_part[['loin']]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- loin$zeros
  n.cooked.zeros <- loin$zeros.after.cooking + loin$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( loin$ill )
  
  table.input <- add.table.row( table.input, n.portions, n.zeros, n.cooked.zeros,
                                p.cooked, p.ill, p.swine, "loin" )
  
  return( table.input)
}

add.to.swine.table <- function( swine.table, nSwine, n.zeros, n.nzeros ){
  return( rbind( swine.table, data.frame( nSwine=nSwine,
                                          n.falseneg=n.zeros + n.nzeros,
                                          p.falseneg=(n.zeros + n.nzeros)/nSwine,
                                          n.zeros=n.zeros, 
                                          n.nzeros=n.nzeros, 
                                          p.carc=ifelse( n.nzeros!=0, n.nzeros/(n.zeros+n.nzeros), NA) # Prevalence of positive carcasses, over all carcasses from false negative batches
  )
  )
  )
}

# This is only for swine batches with 'escaped swine'
df_summary <- 
  df_larvae_in_portions %>% 
  select( part, simulation, carcass, portions_per_part ) %>% 
  unique() %>% 
  left_join( df_zero_larvae_in_portions ) %>% 
  unique() %>% 
  group_by( part, simulation ) %>% 
  summarise( n_portions = sum( portions_per_part), .groups="drop" ) %>% 
  left_join( df_zero_larvae_in_portions ) %>% 
  group_by( part  ) %>%
  summarise( p.portion.mean        = mean( (n_portions-n_zeros) / n_portions ),
             p.portion.cooked.mean = mean( (n_portions-n_zeros_after_cooking) / n_portions  ))

# TODO: mean for p.portions is over what?


table.res.summary <- table.res %>% group_by( type) %>% summarize( p.portion.mean = mean( p.portion ),
                                                                  p.portion.p2.5 = quantile( p.portion, 0.025, na.rm=T ),
                                                                  p.portion.p97.5 = quantile( p.portion, 0.975 , na.rm=T),
                                                                  p.cooked.mean = mean( p.cooked),
                                                                  p.cooked.p2.5 = quantile( p.cooked, 0.025, na.rm=T),
                                                                  p.cooked.p97.5 = quantile( p.cooked, 0.975, na.rm=T),
                                                                  p.ill.mean = mean( p.ill),
                                                                  p.ill.p2.5 = quantile( p.ill, 0.025, na.rm=T),
                                                                  p.ill.p97.5 = quantile( p.ill, 0.975, na.rm=T),
                                                                  per.million.mean = mean( per.million ),
                                                                  per.million.p2.5 = quantile( per.million, 0.025, na.rm=T),
                                                                  per.million.p97.5 = quantile( per.million, 0.975, na.rm=T )
)

swine.table.summary <- swine.table %>% summarize( p.falseneg.mean = mean( p.falseneg, na.rm=T),
                                                  p.falseneg.p2.5 = quantile( p.falseneg, 0.025, na.rm=T),
                                                  p.falseneg.p97.5 = quantile( p.falseneg, 0.975, na.rm=T ),
                                                  p.carc.mean = mean( p.carc, na.rm=T),
                                                  p.carc.p2.5 = quantile( p.carc, 0.025, na.rm=T),
                                                  p.carc.p97.5 = quantile( p.carc, 0.975, na.rm=T )
)

# name results
names(table.res.summary$p.ill.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.portion.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.cooked.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$p.ill.mean)<-c('shoulder', 'belly', 'loin')
names(table.res.summary$per.million.mean)<-c('shoulder', 'belly', 'loin')


output.mean <- list(A.FlaseNegPool = swine.table.summary$p.falseneg.mean,
           B.PosCarcInPool = swine.table.summary$p.carc.mean,
           C.TrichInfecPortFromPosCarc = table.res.summary$p.portion.mean,
     D.TrichInfecCookPortFromPosCarc = table.res.summary$p.cooked.mean,
     E.PIllFromCookPortFormPosCarc = table.res.summary$p.ill.mean,
       F.NIllPerMilPort = table.res.summary$per.million.mean,
     G.AvgNIllPerMilPort= mean(table.res.summary$per.million.mean),
     K.TotPredHumCasePerYear = mean(table.res.summary$per.million.mean)*n_portions_per_person*pop*1e-6,
     L.PredHumCasePerMilPerYear = mean(table.res.summary$per.million.mean)*n_portions_per_person #mean(table.res.summary$per.million.mean)*1e-6*n_portions_per_person*pop/pop*1e6
)

#make a table out of the lsit
output.mean.df <-as.data.frame(matrix(NA, ncol = 4, nrow = length(output.mean), 
                                      dimnames = list(names(output.mean),paste(c('all.parts', 'shoulder', 'belly', 'loin'),  '- mean'))))
for(i.iter in 1:length(output.mean)){
  temp <- output.mean[[i.iter]]
  #temp <- sapply(temp, function(j.iter) round(j.iter, digits = 2))
  if(length(temp)>1){
    output.mean.df[i.iter,2:4] <-sapply(temp, function(j.iter) round(j.iter, digit =3))
  }else{
    output.mean.df[i.iter,1] <- temp
  }
}

output.CI <- list(A.FlaseNegPool = paste(swine.table.summary$p.falseneg.p2.5, '-', swine.table.summary$p.falseneg.p97.5),
                  B.PosCarcInPool = paste(swine.table.summary$p.carc.p2.5,'-', swine.table.summary$p.carc.p97.5), 
                  C.TrichInfecPortFromPosCarc = paste(table.res.summary$p.portion.p2.5,'-', table.res.summary$p.portion.p97.5),
                  D.TrichInfecCookPortFromPosCarc = paste(table.res.summary$p.cooked.p2.5,'-',table.res.summary$p.cooked.p97.5),
                  E.PIllFromCookPortFormPosCarc = paste(table.res.summary$p.ill.p2.5,'-', table.res.summary$p.ill.p97.5),
                  F.NIllPerMilPort = paste(table.res.summary$per.million.p2.5,'-',table.res.summary$per.million.p97.5 ),
                  G.AvgNIllPerMilPort= paste(mean(table.res.summary$per.million.p2.5),'-', mean(table.res.summary$per.million.p97.5)),
                  K.TotPredHumCasePerYear = paste(mean(table.res.summary$per.million.p2.5)*n_portions_per_person*pop*1e-6,'-', mean(table.res.summary$per.million.p97.5)*n_portions_per_person*pop*1e-6),
                  L.PredHumCasePerMilPerYear = paste(mean(table.res.summary$per.million.p2.5)*n_portions_per_person,'-', mean(table.res.summary$per.million.p97.5)*n_portions_per_person)
)

#make a data.frame out of the lsit
output.CI.df <-as.data.frame(matrix(NA, ncol = 4, nrow = length(output.CI), 
                                    dimnames = list(names(output.CI),paste(c('all.parts', 'shoulder', 'belly', 'loin'), '- CI'))))
for(i.iter in 1:length(output.CI)){
  temp <- output.CI[[i.iter]]
  #temp <- sapply(temp, function(j.iter) round(j.iter, digits = 2))
  if(length(temp)>1){
    output.CI.df[i.iter,2:4] <-temp
  }else{
    output.CI.df[i.iter,1] <- temp
  }
}

output <- cbind(output.mean.df, output.CI.df)
