#----------------------------------------------
# functions
#----------------------------------------------


my_rmultinom <- function( larvae_per_part, portions_per_part,...){
  return( cbind(...,
                larvae_per_part,
                portions_per_part,
                tibble( portion=1:portions_per_part,
                        larvae_per_portion=rmultinom(1, larvae_per_part, 
                                                     rep(1/portions_per_part, portions_per_part))[,1])))
}