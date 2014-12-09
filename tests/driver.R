#select da, bui for this job
require(xts)
#source("R/src.R")
require(aabd)
require(aa0)
require(pcalib)
require(aautil)
require(testthat)
source("R/aaxtslib.R")

#x <- as.xts(getbdh("PX_LAST_TTL"))
root.global <- paste0(aabd::bbdir(),"/") #"../BBnew/" 
bui.global <- buiindir(paste0(root.global,"BDH/RAW/BEST_TARGET_PRICE/TFU"))[1:2]
da.global <- commonda(patt="CUR_MKT_CAP_TFU.RData")
daw.global <- as.Date(intersect(as.character(da.global),getca()))

#step 100 - range------------------------------
xn100 <- ls()[grep("^x0100",ls())]
for(i in seq_along(xn100)) {
  print(xn100[i])
  print(i)
  do.call(putstep,args=list(fun=xn100[i]))
}
#test data range, class, has zeros
for(i in seq_along(xn100)) {
  print(xn100[i])
  x <- do.call(getstep,args=list(mnem=xn100[i]))
  expect_true(all(index(x)==da.global))
}


#step 200 - zero to na-------------------------
xn100 <- ls()[grep("^x0100",ls())]
xn200 <- ls()[grep("^x0200",ls())]

for(i in seq_along(xn200)) {
  print(xn200[i])
  print(i)
  do.call(putstep,args=list(fun=xn200[i]))
}
#test data range, class, has zeros
for(i in seq_along(xn100)) {
  print(xn200[i])
  x <- coredata(do.call(getstep,args=list(mnem=xn100[i])))
  x <- x[!is.na(x)]
  if(any(x==0)) {
    cat(xn200[i],"has zeros step 1...")
    x <- coredata(do.call(getstep,args=list(mnem=xn200[i])))
    x <- x[!is.na(x)]
    if(any(x==0)) stop()
    cat(xn200[i],"... no zeros step 2")
  }
}

#step 300 - remove outliers-------------------------
xn300 <- ls()[grep("^x0300",ls())]
for(i in seq_along(xn300)) {
  print(xn300[i])
  print(i)
  do.call(putstep,args=list(fun=xn300[i]))
}

#step 400 - zero to na-------------------------
xnnnn <- ls()[grep("^x04",ls())]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 500 - lags-------------------------
xnnnn <- ls()[grep("^x05",ls())]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 600 - extract wed-------------------------
xnnnn <- ls()[grep("^x06",ls())]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 700 - final custom calcs-------------------------
xnnnn <- ls()[grep("^x07",ls())]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}
