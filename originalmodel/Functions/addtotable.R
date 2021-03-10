

add.table.row <- function( table, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, type )
{
  return( rbind( table, data.frame( n.portions = n.portions,
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

add.to.table <- function( table, n.carc, w, p.swine, shoulder, belly, loin ){

  
  # Shoulder 
  n.portions     <- n.carc * w[2]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- shoulder$zeros
  n.cooked.zeros <- shoulder$zeros.after.cooking + shoulder$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( shoulder$ill )
  
  table <- add.table.row( table, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "shoulder" )
  
  # Belly
  n.portions     <- n.carc * w[3]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- belly$zeros
  n.cooked.zeros <- belly$zeros.after.cooking + belly$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( belly$ill )
  
  table <- add.table.row( table, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "belly" )
  
  # Loin
  n.portions     <- n.carc * w[4]               # nr. of carcassess * nr of portions per carcass
  n.zeros        <- loin$zeros
  n.cooked.zeros <- loin$zeros.after.cooking + loin$zeros # Need to add them up!
  p.cooked       <- 1 - n.cooked.zeros/n.portions
  p.ill          <- mean( loin$ill )
  
  table <- add.table.row( table, n.portions, n.zeros, n.cooked.zeros, p.cooked, p.ill, p.swine, "loin" )

  return( table)
}


add.to.swine.table <- function( swine.table, n.swine, n.zeros, n.nzeros ){
  return( rbind( swine.table, data.frame( n.swine=n.swine,
                                               n.falseneg=n.zeros + n.nzeros,
                                               p.falseneg=(n.zeros + n.nzeros)/n.swine,
                                               n.zeros=n.zeros, 
                                               n.nzeros=n.nzeros, 
                                               p.carc=ifelse( n.nzeros!=0, n.nzeros/(n.zeros+n.nzeros), NA) # Prevalence of positive carcasses, over all carcasses from false negative batches
                                        )
              )
          )
}
