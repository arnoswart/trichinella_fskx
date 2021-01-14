ab <- read.csv( file = 'trich-ab.csv', header=F,colClasses=c("numeric","numeric") )
propDiaphragm <- 0.05
k <- 0.00044705143472671417
m <- 5.244308845834319
p <- c( 0.01344, 0.102788, 0.2776, 0.10103, 0.164062,0.3410 )
nSwine <- 685595
pop <- 38336743
r <- 0.7
nCarc <- 5000
n_portions_per_person <- 0.68
w <- c( 2, 50, 76, 103, 120, 26 )
alpha <- 1.53575 
sim_max <- 1000
swine_per_pool <- 20
beta <- 0.289405
