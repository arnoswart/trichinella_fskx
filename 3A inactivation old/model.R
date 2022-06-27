
inactivation_params <- c(k=k, alpha_plus=alpha_plus, T_star=T_star )


# We pass inactivation as a function, to be parsed later.
# In this way, we can pass functions to the follow step (consumption)

inactivation <- 'inactivation_func <- function(  I0, T0,T1, t1, inactivation_params ){
    k      <- inactivation_params[["k"]]
    T_star <- inactivation_params[["T_star"]]
    alpha_plus <- inactivation_params[["alpha_plus"]]
    
    return( ifelse( t1=0, I0,
                          I0*((exp(k*T_star) + exp( k * T1 ))/(exp(k*T_star) +
                              exp( k * T0 )))^(alpha_plus * t1 /( k*(T0-T1)))))                                                                                          
}'

