propDiaphragm <- 0.05
k <- 0.00044705143472671417
m <- 5.244308845834319
p_larva_in_part <- c( 0.01344, 0.102788, 0.2776, 0.10103, 0.164062,0.3410 )
nSwine <- 685595
nCarc <- 5000
n_portions_per_part <- tibble(  diaphragm=2,
                                shoulder =35,
                                belly    =54, 
                                loin     =144, 
                                ham      =121, 
                                other    =48)
alpha <- 1.53575 
beta <- 0.289405
sim_max <- 1000
swine_per_pool <- 20