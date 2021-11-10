# New inactivation function
inv_logit <- function( x ) 1/(1+exp(-x))

inactivation <- function(I0, T0,T1, t1){
  # just using the final temperature for now.
  # later better do a piecewise linear approximation between T0 and T1
  temp = T1
  
  # found that at low temp, or high time, the model is invalid
  p_inact = 
    inv_logit(
    -12.6 + 0.16*t1 + 0.22*temp 
    -0.0023*t1*temp)
  
  return( I0*(1-p_inact) )
}