inactivation <- function( k, alpha.plus, T.star, I0, T0,T1, t1 ){
  I0*((exp(k*T.star) + exp( k * T1 ))/(exp(k*T.star) +exp( k * T0 )))^(alpha.plus * t1 /( k*(T0-T1)))                                                                                          
}
 

cook <- function( part, scenario ){
  
  ### uncooked, everyone uncooked ###
  if( scenario==0){
    cooked.raw   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=20, t1=0 ) )
    
    # Everyone rare
    part$x <- cooked.raw
  }
  
  ### Baseline 90% welldone Chef & 10% Medium Swart
  if( scenario==1 ){
    is.welldone <- rbinom( length( part$x ), 1, 0.9 )
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.medium   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=63, t1=1.7 ) )
    cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=76.7, t1=7.5 ) )
    
    # Proportion welldone, proportion medium
    part$x <- is.welldone * cooked.welldone + (1-is.welldone) * cooked.medium
  }
 
 
  ### Rare, Swart, everyone rare ###
  if( scenario==2){
    cooked.rareSwart   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    
    # Everyone rare
    part$x <- cooked.rareSwart
  }
  
  ### Medium, Swart, everyone medium ###
  if( scenario==3){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.mediumSwart   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=63, t1=1.7 ) )
    
    # Everyone medium
    part$x <- cooked.mediumSwart
  }
  
  
  ### Welldone, Swart, everyone well done ###
  if( scenario==4){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.medium   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=63, t1=1.7 ) )
    cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.medium, T0=63,T1=68, t1=1.1 ) )
    cooked.welldoneSwart <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.medium, T0=68,T1=67.9, t1=2 ) )
    
    # Everyone well done
    part$x <- cooked.welldoneSwart
  }
  
   
  ### Medium, USDA, everyone medium:4 minutes cooking + 3 minutes rest ###
  if( scenario==5){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.medium   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=63, t1=1.5 ) )
    cooked.mediumUSDA <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.medium, T0=63,T1=62.9, t1=3 ) )
    
    # Everyone medium
    part$x <- cooked.mediumUSDA
  }
  
  
  ### Well done USDA ###
  if( scenario==6){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.welldoneUSDA <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=71, t1=6.5 ) )
    
    #Everyone Well done USDA
    part$x <- cooked.welldoneUSDA
  }
  
  
  ### Well done Chef ###
  if( scenario==7){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )  
    cooked.welldoneChef <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=76.7, t1=7.5 ) )
    
    #Everyone Well done Chef
    part$x <- cooked.welldoneChef
  }
  
 
   ### Well done Traditional, lower limit ###
  if( scenario==8){
    cooked.rare   <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=part$x, T0=20,T1=54, t1=2.5 ) )
    cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.rare, T0=54,T1=76.7, t1=7.5 ) )
    cooked.welldoneTraditional <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=76.7,T1=76.9, t1=11 ) )
    
    #Everyone Well done Traditional
    part$x <- cooked.welldoneTraditional
  }
  
  
  ### Well done Traditional, upper limit ###
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=76.7,T1=76.9, t1=21 ) )
  
  ### cooked welldone +2 Swart ####
  # cooked.welldone <- round( inactivation( k=0.17, alpha.plus=0.63, T.star=59.3,I0=cooked.welldone, T0=54,T1=75, t1=2 ) )
  
  
  
  # All medium
  # part$x <- cooked.medium
  # Cooked rare
  # part$x <- cooked.rare
  # With resting
  # part$x <- cooked.medium.rest
  
  part <- remove.zeros( part, cooked=T)
  
  return( part  )
}

remove.zeros <- function( part, cooked ){
  
  index <- part$x==0|part$x==1
  
  if( cooked ){
    part$zeros.after.cooking <- part$zeros.after.cooking + sum( index )
  }else{
    part$zeros <- part$zeros + sum( index )
  }
  
  part$x     <- part$x[!index]
  
  return( part )
}
