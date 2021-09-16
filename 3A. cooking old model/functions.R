inactivation <- function( k, alpha.plus, T.star, I0, T0,T1, t1 ){
  I0*((exp(k*T.star) + exp( k * T1 ))/(exp(k*T.star) +exp( k * T0 )))^(alpha.plus * t1 /( k*(T0-T1)))                                                                                          
}