require( dplyr )

readData <- function(){
  
  data <- read.xlsx( "../Data/inactivation.xlsx", 
                     sheetName="Sheet1")
  
  # transformation of LPG from bio-assay to infection dose
  data <- data %>% mutate( I1 = ifelse( study=="Carlin", dr.rat.inv( LPG.rat), I1))
  # Set died rats to 16000
  data <- data %>% mutate( LPG.rat = ifelse( study=="Carlin" & died=="y", 16000, LPG.rat ) ) %>%
           select( - died )
  # More out than in is not possible, maximize
  data <- data %>% mutate( I1 = ifelse( study=="Carlin", pmax( I1, LPG.rat ), I1 ) ) %>%
                   select( -LPG.rat )
  
  # Convert LPG.pig (diafragm) to loin 
  b <- read.b()
  
  # Times the conversion factor, then times 45 = 3 days * 15 grams
  data <- data %>% mutate( I0 = round( ifelse(  study=="Carlin",LPG.pig *b["pork.chops", "factor"] * 45, I0 )),
                           I1 = round( I1 )) %>%
                    select( - LPG.pig )
                  
  return( data )
}

#Trichinella heat-inactivation
transformData <- function( data, Ts ){
  
  # Transform temperatures
  # Remove data points ending before inactivation starts
  data <- data %>%  mutate( T1=T1-Ts, T0=T0-Ts ) %>%
                    filter( T1 > 0 ) %>%
                    # Case 2
                    mutate( t = ifelse( T0!=T1 & 1 < -T0/(T1-T0), (T1-T0)*t/T1, t )) %>%
                    mutate( T1 = ifelse( T0!=T1 & 1 < -T0/(T1-T0), 2*T1, T1 )) %>%
                    # Case T1=T0 to be handled in calculation of Tbar
                    mutate( Tbar = ifelse( T0==T1, T0, (T0+T1)/2 ))
  return( data )
}

plotData <- function( data ){
  p <- ggplot( data, aes( x=t, y= log(I1)/log(I0) ) )
  p <- p + geom_point( aes( size=Tbar, colour=study )  )
  p <- p + labs( title="Inactivation over time for the two experiments",
                 x="Time [min.]", y="log(P(survival))")
  print(p)
  
  p <- ggplot( data, aes( x=Tbar, y=log(I1)/log(I0) ) )
  p <- p + geom_point( aes( size=t, colour=study)  )
  p <- p + labs( title="Effect of temperature for the two experiments",
                 x="Temp. [deg C]", y="log( prob.survival )")
  print(p)
}

# dose respons
#p.ill <- function(d){
#  r<- 0.7
#  p<-0.01
#  return(1+exp(-d*p) -exp(-d*p*(1-r)) -exp(-d*p*r))
#}
