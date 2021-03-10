#setwd( "/s-schijf/franssef/AAA - model/Model" )
setwd( "R:/Projecten/V092112 Trichinella/AAA/Model")

k <- 1.55584E-9
m <-0.0000183094
n.smp <- 2E9

df <- data.frame( k=numeric(0), m=numeric(0), prev=numeric(0), 
                  abund=numeric(0), abund.pos=numeric(0) )

for( k.loop in c( k/10, k, k*10) ){
  for( m.loop in c( m/10, m, m*10) ){
    smp <- rnbinom( n.smp, size=k.loop, prob=k.loop/(k.loop+m.loop) )
    
    df <- rbind( df, data.frame( k=k.loop, m=m.loop,
                                 prev=sum( smp>0 )/length( smp ),
                                 abund=mean(smp),
                                 abund.pos=mean( smp[smp>0]) ) )
  }
}

print( df, digits=2 )

