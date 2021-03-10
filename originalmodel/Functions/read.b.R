read.b <- function( ){
  b <- read.xlsx("../Data/b.xlsx", sheetName = "b values")
  b <- b[,-1]
  tmp <- colnames(b)
  b <- as.data.frame( b[, "diaphragm.pillar"] )
  rownames(b) <- tmp
  colnames(b) <- "factor"
  return( b )
}