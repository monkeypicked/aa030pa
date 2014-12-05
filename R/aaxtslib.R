# da <- as.Date(sort(intersect(index(x),index(x0))))
# bui <- intersect(colnames(x),colnames(x0))
# x <- getbdh("EQY_WEIGHTED_AVG_PX_TFU",ty="z")[da,bui]
# x0 <- getbdh("PX_LAST_TFU",ty="z")[da,bui]
source("c:/users/bloomberg/rsql/00lib.r")


dern <- function(root=root.global,n="001",type=c("BDH","BDP")) {
  type <- match.arg(type)
  paste0(root,type,"/derive-",n,"/")
}
getstep <- function(mnem=dir(mydir)[1],mydir=dern(...),...) {load(paste0(mydir,mnem,".RData"));x}
putstep <- function(fun="x01BTPTselect",mydir=dern(...),x=do.call(fun,args=list()),...) {
  stopifnot(exists(fun))
  save(x,file=paste0(mydir,fun,".RData"))
}

# commonbui <- function(...) {
#   dd <- dern(...)
#   ff <- dir(dd)
#   load(paste0(paste0(dd,ff[1])))
#   bui<-unique(colnames(x))
#   for(i in 2:length(ff)) {
#     load(paste0(paste0(dd,ff[i])))
#     bui <-intersect(bui,unique(colnames(x)))
#   }
#   bui
# }

#commonbui - applies joinfun to raw directory contents
commonbui <- function(BD=c("BDP/key1/","BDH/raw/"),...,joinfun=intersect) {
  BD <- match.arg(BD)
  root <- paste0(root.global,BD)
  ll0 <- list.dirs(root)
  ll <- lapply(as.list(ll0[ll0!=root]),buiindir)
  bui <- Reduce(joinfun,ll[0<lapply(ll,length)])
  bui[nchar(bui)==18]
}

#commonda - applies joinfun to derive-000 directory contents
commonda <- function(joinfun=intersect,nn="000",patt=".?") {
  dd0 <- paste0(root.global,"BDH/derive-",nn,"/")
  dd <- dir(dd0)[grepl(patt,dir(dd0))]
  pp <- paste0(dd0,dd)
  ll <- vector("list",length(dd))
  for(i in seq_along(pp)) {
    load(pp[i])
    ll[[i]] <- index(x)
  }
  #lapply(lapply(lapply(as.list(paste0(dd,dir(dd))),load),assign,value=x),function(x){length(index(x))})
  as.Date(Reduce(joinfun,ll[0<lapply(ll,length)]))
}

#select ranges inplace - zoo
f0100select <- function(x=getbdh(type="zoo"),da=da.global,bui=bui.global) {
  stopifnot(is.zoo(x) && all(da%in%index(x)) && all(bui%in%colnames(x)))
  x[da,bui]
}
#here the idea is that the function ending 00 is the 'generic' and the others are wrappers such that no args need be passed
#these in turn can be called in a generic name so the result is saved with the name and they can thus be re-used symbolically
# x01BTPTselect <- function() {f0100select(getbdh("BEST_TARGET_PRICE_TFU"))}
# x01PLTselect <- function() {f0100select(getbdh("PX_LAST_TFU"))}
#_TTL,TTU,VWAP_TTU,MCAP_TTU,DIPR_TTU,ERPR_TTU,CAPR_TTU,BOPR_TTU,volume
x0100BTP_TFU  <- function() {f0100select(getstep("BEST_TARGET_PRICE_TFU",n="000"))}
x0100PL_TFU   <- function() {f0100select(getstep("PX_LAST_TFU",n="000"))}
x0100PL_TTU   <- function() {f0100select(getstep("PX_LAST_TTU",n="000"))}
x0100PL_TTL   <- function() {f0100select(getstep("PX_LAST_TTL",n="000"))}
x0100EWAP_TTU <- function() {f0100select(getstep("EQY_WEIGHTED_AVG_PX_TTU",n="000"))}
x0100CMC      <- function() {f0100select(getstep("CUR_MKT_CAP_TFU",n="000"))}
x0100EDYI     <- function() {f0100select(getstep("EQY_DVD_YLD_IND_TFU",n="000"))}
x0100PE       <- function() {f0100select(getstep("PE_RATIO_TFU",n="000"))} 
x0100PTCF     <- function() {f0100select(getstep("PX_TO_CASH_FLOW_TFU",n="000"))}
x0100PTBR     <- function() {f0100select(getstep("PX_TO_BOOK_RATIO_TFU",n="000"))}
x0100PV       <- function() {f0100select(getstep("PX_VOLUME_TFU",n="000"))}
x0100EFFP     <- function() {f0100select(getstep("EQY_FREE_FLOAT_PCT_TFU",n="000"))}

#zeros set to na inplace - zoo
f0200zerotona <- function(x){
  coredata(x)[coredata(x)==0] <- NA
  x
}
x0200BTP_TFU  <- function(){f0200zerotona(getstep("x0100BTP_TFU"))}
x0200PL_TFU   <- function(){f0200zerotona(getstep("x0100PL_TFU"))}
x0200PL_TTU   <- function(){f0200zerotona(getstep("x0100PL_TTU"))}
x0200PL_TTL   <- function(){f0200zerotona(getstep("x0100PL_TTL"))}
x0200EWAP_TTU <- function(){f0200zerotona(getstep("x0100EWAP_TTU"))}
x0200CMC     <- function(){f0200zerotona(getstep("x0100CMC"))}
x0200EDYI    <- function(){f0200zerotona(getstep("x0100EDYI"))}
x0200PE      <- function(){f0200zerotona(getstep("x0100PE"))}
x0200PTCF    <- function(){f0200zerotona(getstep("x0100PTCF"))}
x0200PTBR    <- function(){f0200zerotona(getstep("x0100PTBR"))}
x0200PV      <- function(){f0200zerotona(getstep("x0100PV"))}
x0200EFFP    <- function(){f0200zerotona(getstep("x0100EFFP"))}

#outlier detect and correct x by comparison with x0 [THIS NOT PART OF BEST]
f0300outlier <- function(x,x0,tol=.1){
  stopifnot(identical(dim(x),dim(x0)) & is.zoo(x) & is.zoo(x0))
  xx <- coredata(x/x0)
  xx[xx<(1-tol)] <- 1-tol
  xx[(1+tol)<xx] <- 1+tol
  coredata(x) <- coredata(x0)*xx
  x
}
x0300EWAP_TTU  <- function(){f0300outlier(x=getstep("x0200EWAP_TTU"),x0=getstep("x0200PL_TTU"))}

#apply rolling op to xts
f0400rollop <- function(x){
  rollxts(x,what="median",n=5)
}
f0401rollop <- function(x){
  rollxts(x,what="median",n=260)
}
x0400BTP_TFU  <- function(){f0400rollop(getstep("x0200BTP_TFU"))}
x0400PL_TFU   <- function(){f0400rollop(getstep("x0200PL_TFU"))}
x0401PV   <- function(){f0401rollop(getstep("x0200PV"))}

#lag xts
f0501lag <- function(x){
  lag.xts(x=x,k=1)
}
f0502lag <- function(x){
  lag.xts(x=x,k=2)
}
x0501BTP_TFU  <- function(){f0501lag(getstep("x0400BTP_TFU"))}
x0501PL_TFU   <- function(){f0501lag(getstep("x0400PL_TFU"))}
x0501PV      <- function(){f0501lag(getstep("x0401PV"))}
x0501CMC     <- function(){f0501lag(getstep("x0200CMC"))}
x0501EDYI    <- function(){f0501lag(getstep("x0200EDYI"))}
x0501PE      <- function(){f0501lag(getstep("x0200PE"))}
x0501PTCF    <- function(){f0501lag(getstep("x0200PTCF"))}
x0501PTBR    <- function(){f0501lag(getstep("x0200PTBR"))}
x0501EFFP    <- function(){f0501lag(getstep("x0200EFFP"))}

x0502BTP_TFU  <- function(){f0502lag(getstep("x0400BTP_TFU"))}
x0502PL_TFU   <- function(){f0502lag(getstep("x0400PL_TFU"))}
x0502PV      <- function(){f0502lag(getstep("x0401PV"))}
x0502CMC     <- function(){f0502lag(getstep("x0200CMC"))}
x0502EDYI    <- function(){f0502lag(getstep("x0200EDYI"))}
x0502PE      <- function(){f0502lag(getstep("x0200PE"))}
x0502PTCF    <- function(){f0502lag(getstep("x0200PTCF"))}
x0502PTBR    <- function(){f0502lag(getstep("x0200PTBR"))}
x0502EFFP    <- function(){f0502lag(getstep("x0200EFFP"))}

#extract wed using locf in dt
f0600wed <- function(x){  #locf
  dtlocf(x,wd=3,roll=4)
}
x0601BTP_TFU  <- function(){f0600wed(getstep("x0501BTP_TFU"))} #lag 1
x0601PL_TFU   <- function(){f0600wed(getstep("x0501PL_TFU"))}
x0601PV      <- function(){f0600wed(getstep("x0501PV"))}
x0601CMC     <- function(){f0600wed(getstep("x0501CMC"))}
x0601EDYI    <- function(){f0600wed(getstep("x0501EDYI"))}
x0601PE      <- function(){f0600wed(getstep("x0501PE"))}
x0601PTCF    <- function(){f0600wed(getstep("x0501PTCF"))}
x0601PTBR    <- function(){f0600wed(getstep("x0501PTBR"))}
x0601EFFP    <- function(){f0600wed(getstep("x0501EFFP"))}

x0602BTP_TFU  <- function(){f0600wed(getstep("x0502BTP_TFU"))} #lag 2
x0602PL_TFU   <- function(){f0600wed(getstep("x0502PL_TFU"))}
x0602PV      <- function(){f0600wed(getstep("x0502PV"))}
x0602CMC     <- function(){f0600wed(getstep("x0502CMC"))}
x0602EDYI    <- function(){f0600wed(getstep("x0502EDYI"))}
x0602PE      <- function(){f0600wed(getstep("x0502PE"))}
x0602PTCF    <- function(){f0600wed(getstep("x0502PTCF"))}
x0602PTBR    <- function(){f0600wed(getstep("x0502PTBR"))}
x0602EFFP    <- function(){f0600wed(getstep("x0502EFFP"))}

f0603wed <- function(x){  #focb
  dtlocf(x,wd=3,roll=-4)
}
x0603PL_TFU   <- function(){f0603wed(getstep("x0200PL_TFU"))}
x0603PL_TTU   <- function(){f0603wed(getstep("x0200PL_TTU"))}
x0603PL_TTL   <- function(){f0603wed(getstep("x0200PL_TTL"))}
x0603EWAP_TTU <- function(){f0603wed(getstep("x0300EWAP_TTU"))}

#calcs
f0700return <- function(x){
  as.zoo(retxts(x))[daw.global,,drop=FALSE]
}
f0700premium <- function(x){
  x1 <- as.zoo(retxts(x))[daw.global,,drop=FALSE]
  x2 <- getusst()[daw.global,,drop=FALSE]
  i <- as.Date(intersect(index(x1),index(x2)))
  sweep(x1[i],FUN="-",STAT=x2[i],MAR=1)
}
x0700prdo     <- function(...){getstep("x0603PL_TTU")}
x0700prdovw   <- function(...){getstep("x0603EWAP_TTU")}
x0700prlo     <- function(...){getstep("x0603PL_TTL")}
x0700reloto   <- function(...){f0700return(getstep("x0603PL_TTL"))}
#x0700rrloto   <- function(...){f0700premium(getstep("x0602PL_TTL"))} #phase 2 - match up stir in local
x0700redoto   <- function(...){f0700return(getstep("x0603PL_TTU"))}
x0700rrdoto   <- function(...){f0700premium(getstep("x0603PL_TTU"))} 
x0700rrdotovw <- function(...){f0700premium(getstep("x0603EWAP_TTU"))} 
x0700turn     <- function(...){getstep("x0601PL_TFU")*getstep("x0601PV")} 

x0701best     <- function(...){getstep("x0601BTP_TFU")/getstep("x0601PL_TFU")}
x0701prdo     <-  function(...){getstep("x0601PL_TFU")}
x0701mcap     <-  function(...){getstep("x0601CMC")}
x0701dipr     <-  function(...){getstep("x0601EDYI")}
x0701erpr     <-  function(...){getstep("x0601PE")}
x0701capr     <-  function(...){getstep("x0601PTCF")}
x0701bopr     <-  function(...){getstep("x0601PTBR")}

x0702best     <- function(...){getstep("x0602BTP_TFU")/getstep("x0602PL_TFU")}
x0702prdo     <-  function(...){getstep("x0602PL_TFU")}
x0702mcap     <-  function(...){getstep("x0602CMC")}
x0702dipr     <-  function(...){getstep("x0602EDYI")}
x0702erpr     <-  function(...){getstep("x0602PE")}
x0702capr     <-  function(...){getstep("x0602PTCF")}
x0702bopr     <-  function(...){getstep("x0602PTBR")}

#getusst
getusst <- function()
{
  load(paste0(root.global,"/macro/derive-000/US0001m.RData"))
  x <- dtlocf(x)[daw.global,drop=FALSE]
  n <- nrow(x)
  xx <- coredata(x)
  xx[-1,] <- xx[-n, ] * diff(as.numeric(as.Date(index(x))))/365
  xx[1,] <- NA
  0.01*zoo(focb.mat(xx),index(x))
}

