rm(list=ls(all.names = TRUE))

dataFill <- as.data.frame(matrix(rnorm(1e6, 100), ncol = 200))
dataFill[,"index"] <- sample(1:10, nrow(dataFill), replace = TRUE)
dataGet <- as.data.frame(matrix(rnorm(1e6, 100), ncol = 200))
dataGet[,"index"] <- sample(1:10, nrow(dataGet), replace = TRUE)

expr <- quote({
  a <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a1 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b1 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c1 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a2 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b2 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c2 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a3 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b3 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c3 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a4 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b4 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c4 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a5 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b5 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c5 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a0 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b0 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c0 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a11 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b11 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c11 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a12 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b12 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c12 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a13 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b13 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c13 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a14 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b14 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c14 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)

  a15 <- V1 + lookup(V3 + V4 + V7 + V9 + V10)
  b15 <- V2 + lookup(V11 + V4 + V7 + V9 + V10)
  c15 <- V3 + lookup(V13 + V4 + V7 + V9 + V10)
})


oldObj <- ls(all.names = TRUE)
source("./R/avgImpute.R")
source("./R/utils.R")

# Approx 210 seconds using lapply.

for(i in 1:3) {
  print(system.time(imputeDomainDriven(
    dataFill = dataFill,
    dataGet = dataGet,
    lookupIndexNames = "index",
    similarityCols = c("V1", "V2", "V3"),
    minObs = 10,
    expr = expr,
    showProgressBar = FALSE
    )))
}

newOjb <- ls(all.names = TRUE)
newOjb <- newOjb[!newOjb %in% oldObj]
rm(newOjb)


# Approx 190 seconds usind data.table.
require(avgImpute)
for(i in 1:3) {
  print(system.time(avgImpute::imputeDomainDriven(
    dataFill = dataFill,
    dataGet = dataGet,
    lookupIndexNames = "index",
    similarityCols = c("V1", "V2", "V3"),
    minObs = 10,
    expr = expr,
    showProgressBar = FALSE
  )))
}
