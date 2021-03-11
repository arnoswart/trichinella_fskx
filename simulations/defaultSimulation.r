ab <- read.csv( file = 'trich-ab.csv', header=F,colClasses=c("numeric","numeric") )
nCarc <- 5000
pop <- 38336743
alpha <- 1.53575 
beta <- 0.289405
m <- 0.00159125
k <- 1.3523e-7
propDiaphragm <- 0.01
nSwine <- 114395672
swine_per_pool <- 100
sim_max <- 1000
r <- 0.7
n_portions_per_part <- tibble(  diaphragm=2,
                                shoulder =35,
                                belly    =54, 
                                loin     =144, 
                                ham      =121, 
                                other    =48)
p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
n_portions_per_person <- 246
