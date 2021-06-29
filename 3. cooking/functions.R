inactivation <- function( k, alpha.plus, T.star, I0, T0,T1, t1 ){
  I0*((exp(k*T.star) + exp( k * T1 ))/(exp(k*T.star) +exp( k * T0 )))^(alpha.plus * t1 /( k*(T0-T1)))                                                                                          
}

# New inactivation function
inactivation_new <- function(I0, T0,T1, t1){
  # just using the final temperature for now.
  # later better do a piecewise linear approximation between T0 and T1
  temp = T1
  p_surv = -1.38e3 + 8.56*t1 + 1.09e1*temp - 4.06e-2*t1*temp +8.31e2*log(t1) +
    2.94e2*log(temp) -2.53e2*log(t1)*log(temp)
  return( I0*(1-p_surv) )
}
