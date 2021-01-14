ab <- read.csv( file = 'trich-ab.csv', header=F,colClasses=c("numeric","numeric") )
propDiaphragm <- 0.01
k <- 1.3523e-7
m <- 0.00159125
p <- c(0.01103, 0.05903, 0.16183, 0.11588, 0.13571, 0.51652 )
nSwine <- 114395672
pop <- 38336743
r <- 0.7
nCarc <- 5000
n_portions_per_person <- 246
w <- c(2, 35, 54, 144, 121, 48 )
alpha <- 1.53575 
sim_max <- 50
swine_per_pool <- 100
beta <- 0.289405
