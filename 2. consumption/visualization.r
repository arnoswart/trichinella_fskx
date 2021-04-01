library(gridExtra)
row.names(output)<-paste("Table 8, testing", c(letters[1:7], 'k', 'l'))
grid.table(output[,1:(ncol(output)/2)])

