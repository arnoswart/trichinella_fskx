propDiaphragm <- 0.01
k <- 1.3523e-7
m <- 0.00159125
p_larva_in_part <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
nSwine <- 1e6
nCarc <- 50
n_portions_per_part <- tibble(  diaphragm=2,
                                shoulder =35,
                                belly    =54, 
                                loin     =144, 
                                ham      =121, 
                                other    =48)
alpha <- 1.53575 
beta <- 0.289405
sim_max <- 50
swine_per_pool <- 100
