#dose-response-rat

dr.rat <- function(dose){
  m <- 0.96
  m1 <- 2.3
  v <- 0.16
  v1 <- 5.3
  h1 <- 200
  h2 <- 2000
  h3 <- 9000
  if(dose<h1){response <- m*dose}
  else {
    if(dose < h2){response <- m*h1 }
    else {
      if(dose < h3) { response <- m*h1*(dose^m1)*(h2^-m1)}
      else {response <- m*h1*(h3^m1)*(h2^-m1) }
    }
  }
  return(response)
}

dr.rat.inv <- function( r ){
  m <- 0.96
  m1 <- 2.3
  v <- 0.16
  v1 <- 5.3
  h1 <- 200
  h2 <- 2000
  h3 <- 9000
  
  c1 <- m * h1 
  c2 <- m * h1 *( h3/h2)^m1
  
  d <- (r < c1)* r/m + (r>c1)*(r<c2)*((r/ (m *h1))^(1/m1))*h2 + (r>c2)*h3
  
  return( d )
}