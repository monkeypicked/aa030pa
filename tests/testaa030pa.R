require(aautil)
aatopselect("prod")
require(aapa)
require(testthat)
#.supa.der()

#######
#USEAGE
#######
#if(FALSE) {
  deraapa()  #simples (what about macro?)
#}
######
#TESTS
######
#step 100 - range------------------------------
xn100 <- aapafun[grep("^x0100",aapafun)]
#test data range, class, has zeros
for(i in seq_along(xn100)) {
  print(xn100[i])
  x <- do.call(getstep,args=list(mnem=xn100[i]))
  expect_true(all(getdapa()%in%index(x))) #not identical for macro
}

#step 200 - zero to na-------------------------
xn100 <- aapafun[grep("^x0100",aapafun)]
xn200 <- aapafun[grep("^x0200",aapafun)]

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
xn300 <- aapafun[grep("^x0300",aapafun)]
for(i in seq_along(xn300)) {
  print(xn300[i])
  print(i)
  do.call(putstep,args=list(fun=xn300[i]))
}

#step 400 - zero to na-------------------------
xnnnn <- aapafun[grep("^x04",aapafun)]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 500 - lags-------------------------
xnnnn <- aapafun[grep("^x05",aapafun)]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 600 - extract wed-------------------------
xnnnn <- aapafun[grep("^x06",aapafun)]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}

#step 700 - final custom calcs-------------------------
xnnnn <- aapafun[grep("^x07",aapafun)]
for(i in seq_along(xnnnn)) {
  print(xnnnn[i])
  print(i)
  do.call(putstep,args=list(fun=xnnnn[i]))
}


# #test getpi
# su <- getrdatv('jo','su')
# pa1 <- getpi(su)
# pa2 <- getpi(su,fix=F)
# pa3 <- getpi(su,da='2011-11-30')
# pa4 <- getpi(su,fix=F,da='2011-11-30')
# 
# expect_more_than(mean(is.na(pa2))-mean(is.na(pa1)),0)#has more na because history screened out
# expect_more_than(mean(is.na(pa4))-mean(is.na(pa3)),0)#has more na because history screened out
# 
# if(F) {
# par(mfrow=c(2,2))
# imgzoo(pa1)
# imgzoo(pa2)
# imgzoo(pa3)
# imgzoo(pa4)
# par(mfrow=c(1,1))
# }