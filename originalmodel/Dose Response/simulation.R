setwd("R:/Projecten/V092112 Trichinella/AAA - model/Model/Dose Response")

ab <- read.csv( file = "trich-ab.csv", header=F, colClasses=c("numeric","numeric"))

r <-0.7

doses <- 1:2000
p.ill.sim <- vector( "numeric", length=length(doses) )
p.ill.peter <- vector( "numeric", length=length(doses) )
for( d in doses ){
  p.ill.sim[d] <- p.inf.simulate( d )
  p.ill.peter[d] <- p.inf.peter( d )
}

plot( doses, p.ill.sim )
plot( doses, p.ill.peter )

plot( p.ill.sim, p.ill.peter )
abline(0,1)

p.inf.peter <- function( d ){
  # Do m iterations
  m <- 5000
  # Prob survival
  index <- sample.int(1000, size=m, replace=T)
  a <- ab[ index, 1 ]
  b <- ab[ index, 2 ]
  p <- rbeta( n=m, shape1=a, shape2=b )
  
  return( mean(1 + (1-p)^d - (1-r*p)^d- (1-(1-r)*p)^d ) )
}

p.inf.simulate <- function( d ){
  # Do m iterations
  m <- 5000
  
  # Pick males and females
  n.males <- rbinom( m, size=d, prob=r )
  
  # Prob survival
  index <- sample.int(1000, size=m, replace=T)
  a <- ab[ index, 1 ]
  b <- ab[ index, 2 ]
  p.survive <- rbeta( n=m, shape1=a, shape2=b )
  
  # Nr. surviving males, females
  n.males.survive <- rbinom( n=m, size=n.males, prob=p.survive )
  n.females.survive <- rbinom( n=m, size=d-n.males, prob=p.survive )
  
  inf <- sum(n.females.survive * n.males.survive != 0 )/m
  return( inf )
}
