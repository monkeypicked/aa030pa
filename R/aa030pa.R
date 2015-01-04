.onLoad <- function(libname, pkgname) {
  root.global <<- paste0(aabd::bbdir(),"/") #"../BBnew/" 
  bui.global <<- buiindir(paste0(root.global,"BDH/RAW/BEST_TARGET_PRICE/TFU")) #somewhat arbitrary?
  da.global <<- commonda(patt="CUR_MKT_CAP_TFU.RData")
  daw.global <<- as.Date(intersect(as.character(da.global),getca()))
  aapaenv <<- environment(x0100BTP_TFU) #this is the package environment - could be done better?
  aapafun <<- ls(aapaenv)
  options("stringsAsFactors"=FALSE)
}

#' Derive all panels
#'
#' Top-level function that calls the others
#' @param token character vector of strings match in function names
#' @export
deraapa <- function(token=c("^x0100","^x0200","^x0300","^x04","^x05","^x06","^x07","^x08")) {
  for(j in seq_along(token)) {
    xn <- aapafun[grep(token[j],aapafun)] 
    print(xn)
    for(i in seq_along(xn)) {
      print(xn[i])
      do.call(putstep,args=list(fun=xn[i]))
    }
  }
}

#' @export
dern <- function(root=root.global,n="001",type=c("BDH","BDP","macro")) {
  type <- match.arg(type)
  paste0(root,type,"/derive-",n,"/")
}

#' Get panel
#'
#' get a timeseries/cross-section panel or cross-section of reference data
#' @param mnem filename without extension
#' @param mydir directory
#' @param ... passed to dern to construct mydir
#' @examples getstep('NAME',n='000',typ='BDP')
#' @export
getstep <- function(mnem=strsplit(dir(mydir)[1],split="\\.")[[1]][1],mydir=dern(...),...) {load(paste0(mydir,mnem,".RData"));x}

#' @export
putstep <- function(fun="x01BTPTselect",mydir=dern(...),x=do.call(fun,args=list()),...) {
  stopifnot(exists(fun))
  save(x,file=paste0(mydir,fun,".RData"))
}

#commonbui - applies joinfun to raw directory contents
#' @export
commonbui <- function(BD=c("BDP/key1/","BDH/raw/"),...,joinfun=intersect) {
  BD <- match.arg(BD)
  root <- paste0(root.global,BD)
  ll0 <- list.dirs(root)
  ll <- lapply(as.list(ll0[ll0!=root]),buiindir)
  bui <- Reduce(joinfun,ll[0<lapply(ll,length)])
  bui[nchar(bui)==18]
}

#commonda - applies joinfun to derive-000 directory contents
#' @export
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

#####0100
#' Select identifiers and dates
#'
#' subset timeseries
#' @name f0100
NULL
#' @export
#' @rdname f0100 
x0100BTP_TFU  <- function() {f0100select(getstep("BEST_TARGET_PRICE_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100PL_TFU   <- function() {f0100select(getstep("PX_LAST_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100PL_TTU   <- function() {f0100select(getstep("PX_LAST_TTU",n="000"))}
#' @export
#' @rdname f0100 
x0100PL_TTL   <- function() {f0100select(getstep("PX_LAST_TTL",n="000"))}
#' @export
#' @rdname f0100 
x0100EWAP_TTU <- function() {f0100select(getstep("EQY_WEIGHTED_AVG_PX_TTU",n="000"))}
#' @export
#' @rdname f0100 
x0100CMC      <- function() {f0100select(getstep("CUR_MKT_CAP_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100EDYI     <- function() {f0100select(getstep("EQY_DVD_YLD_IND_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100PE       <- function() {f0100select(getstep("PE_RATIO_TFU",n="000"))} 
#' @export
#' @rdname f0100 
x0100PTCF     <- function() {f0100select(getstep("PX_TO_CASH_FLOW_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100PTBR     <- function() {f0100select(getstep("PX_TO_BOOK_RATIO_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100PV       <- function() {f0100select(getstep("PX_VOLUME_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100EFFP     <- function() {f0100select(getstep("EQY_FREE_FLOAT_PCT_TFU",n="000"))}
#' @export
#' @rdname f0100 
x0100VIX     <- function() {getstep("VIX",n="000",typ="macro")}
#' @export
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

#####0200
#' convert zero to NA
#' @name f0200
NULL
#' @export
f0200zerotona <- function(x){
  coredata(x)[coredata(x)==0] <- NA
  x
}
#' @export
#' @rdname f0200 
x0200BTP_TFU  <- function(){f0200zerotona(getstep("x0100BTP_TFU"))}
#' @export
#' @rdname f0200 
x0200PL_TFU   <- function(){f0200zerotona(getstep("x0100PL_TFU"))}
#' @export
#' @rdname f0200 
x0200PL_TTU   <- function(){f0200zerotona(getstep("x0100PL_TTU"))}
#' @export
#' @rdname f0200 
x0200PL_TTL   <- function(){f0200zerotona(getstep("x0100PL_TTL"))}
#' @export
#' @rdname f0200 
x0200EWAP_TTU <- function(){f0200zerotona(getstep("x0100EWAP_TTU"))}
#' @export
#' @rdname f0200 
x0200CMC     <- function(){f0200zerotona(getstep("x0100CMC"))}
#' @export
#' @rdname f0200 
x0200EDYI    <- function(){f0200zerotona(getstep("x0100EDYI"))}
#' @export
#' @rdname f0200 
x0200PE      <- function(){f0200zerotona(getstep("x0100PE"))}
#' @export
#' @rdname f0200 
x0200PTCF    <- function(){f0200zerotona(getstep("x0100PTCF"))}
#' @export
#' @rdname f0200 
x0200PTBR    <- function(){f0200zerotona(getstep("x0100PTBR"))}
#' @export
#' @rdname f0200 
x0200PV      <- function(){f0200zerotona(getstep("x0100PV"))}
#' @export
#' @rdname f0200 
x0200EFFP    <- function(){f0200zerotona(getstep("x0100EFFP"))}
#' @export
#' @rdname f0200 
x0200VIX    <- function(){f0200zerotona(getstep("x0100VIX"))}

#####0300
#' outlier detect and correct x by comparison with x0
#' @name f0300
NULL
#' @export
f0300outlier <- function(x,x0,tol=.1){
  stopifnot(identical(dim(x),dim(x0)) & is.zoo(x) & is.zoo(x0))
  xx <- coredata(x/x0)
  xx[xx<(1-tol)] <- 1-tol
  xx[(1+tol)<xx] <- 1+tol
  coredata(x) <- coredata(x0)*xx
  x
}
#' @export
#' @rdname f0300 
x0300EWAP_TTU  <- function(){f0300outlier(x=getstep("x0200EWAP_TTU"),x0=getstep("x0200PL_TTU"))}

#####0400
#' apply rolling op to xts
#' @name f0400
NULL
#' @export
f0400rollop <- function(x){
  rollxts(x,what="median",n=5)  #works correctly on both xts and zoo
}
f0401rollop <- function(x){
  rollxts(x,what="median",n=260)
}
f0402rollop <- function(x){
  rollxts(x = x, 
          what = "meantri", 
          n = 20
  )
}
#' @export
#' @rdname f0400 
x0400BTP_TFU  <- function(){f0400rollop(getstep("x0200BTP_TFU"))}
#' @export
#' @rdname f0400 
x0400PL_TFU   <- function(){f0400rollop(getstep("x0200PL_TFU"))}
#' @export
#' @rdname f0400 
x0401PV   <- function(){f0401rollop(getstep("x0200PV"))}
#' @export
#' @rdname f0400 
x0402VIX   <- function(){f0402rollop(getstep("x0200VIX"))}

#####0500
#' lag xts
#' @name f0500
NULL
#' @export
f0501lag <- function(x){
  lag.xts(x=x,k=1)  #+ve lag is feasible ie past -> future
}
f0502lag <- function(x){
  lag.xts(x=x,k=2)
}
#' @export
#' @rdname f0500 
x0501BTP_TFU  <- function(){f0501lag(getstep("x0400BTP_TFU"))}
#' @export
#' @rdname f0500 
x0501PL_TFU   <- function(){f0501lag(getstep("x0400PL_TFU"))}
#' @export
#' @rdname f0500 
x0501PV      <- function(){f0501lag(getstep("x0401PV"))}
#' @export
#' @rdname f0500 
x0501CMC     <- function(){f0501lag(getstep("x0200CMC"))}
#' @export
#' @rdname f0500 
x0501EDYI    <- function(){f0501lag(getstep("x0200EDYI"))}
#' @export
#' @rdname f0500 
x0501PE      <- function(){f0501lag(getstep("x0200PE"))}
#' @export
#' @rdname f0500 
x0501PTCF    <- function(){f0501lag(getstep("x0200PTCF"))}
#' @export
#' @rdname f0500 
x0501PTBR    <- function(){f0501lag(getstep("x0200PTBR"))}
#' @export
#' @rdname f0500 
x0501EFFP    <- function(){f0501lag(getstep("x0200EFFP"))}

#' @export
#' @rdname f0500 
x0502BTP_TFU  <- function(){f0502lag(getstep("x0400BTP_TFU"))}
#' @export
#' @rdname f0500 
x0502PL_TFU   <- function(){f0502lag(getstep("x0400PL_TFU"))}
#' @export
#' @rdname f0500 
x0502PV      <- function(){f0502lag(getstep("x0401PV"))}
#' @export
#' @rdname f0500 
x0502CMC     <- function(){f0502lag(getstep("x0200CMC"))}
#' @export
#' @rdname f0500 
x0502EDYI    <- function(){f0502lag(getstep("x0200EDYI"))}
#' @export
#' @rdname f0500 
x0502PE      <- function(){f0502lag(getstep("x0200PE"))}
#' @export
#' @rdname f0500 
x0502PTCF    <- function(){f0502lag(getstep("x0200PTCF"))}
#' @export
#' @rdname f0500 
x0502PTBR    <- function(){f0502lag(getstep("x0200PTBR"))}
#' @export
#' @rdname f0500 
x0502EFFP    <- function(){f0502lag(getstep("x0200EFFP"))}
#' @export
#' @rdname f0500 
x0502VIX    <- function(){f0502lag(getstep("x0402VIX"))}

#####0600
#' last observation carry forward
#' @name f0600
NULL
#' @export
f0600wed <- function(x){  #locf
  dtlocf(x,wd=3,roll=5)
}
#' @export
#' @rdname f0600 
x0601BTP_TFU  <- function(){f0600wed(getstep("x0501BTP_TFU"))} #lag 1
#' @export
#' @rdname f0600 
x0601PL_TFU   <- function(){f0600wed(getstep("x0501PL_TFU"))}
#' @export
#' @rdname f0600 
x0601PV      <- function(){f0600wed(getstep("x0501PV"))}
#' @export
#' @rdname f0600 
x0601CMC     <- function(){f0600wed(getstep("x0501CMC"))}
#' @export
#' @rdname f0600 
x0601EDYI    <- function(){f0600wed(getstep("x0501EDYI"))}
#' @export
#' @rdname f0600 
x0601PE      <- function(){f0600wed(getstep("x0501PE"))}
#' @export
#' @rdname f0600 
x0601PTCF    <- function(){f0600wed(getstep("x0501PTCF"))}
#' @export
#' @rdname f0600 
x0601PTBR    <- function(){f0600wed(getstep("x0501PTBR"))}
#' @export
#' @rdname f0600 
x0601EFFP    <- function(){f0600wed(getstep("x0501EFFP"))}

#' @export
#' @rdname f0600 
x0602BTP_TFU  <- function(){f0600wed(getstep("x0502BTP_TFU"))} #lag 2
#' @export
#' @rdname f0600 
x0602PL_TFU   <- function(){f0600wed(getstep("x0502PL_TFU"))}
#' @export
#' @rdname f0600 
x0602PV      <- function(){f0600wed(getstep("x0502PV"))}
#' @export
#' @rdname f0600 
x0602CMC     <- function(){f0600wed(getstep("x0502CMC"))}
#' @export
#' @rdname f0600 
x0602EDYI    <- function(){f0600wed(getstep("x0502EDYI"))}
#' @export
#' @rdname f0600 
x0602PE      <- function(){f0600wed(getstep("x0502PE"))}
#' @export
#' @rdname f0600 
x0602PTCF    <- function(){f0600wed(getstep("x0502PTCF"))}
#' @export
#' @rdname f0600 
x0602PTBR    <- function(){f0600wed(getstep("x0502PTBR"))}
#' @export
#' @rdname f0600 
x0602EFFP    <- function(){f0600wed(getstep("x0502EFFP"))}

f0603wed <- function(x){  #focb
  dtlocf(x,wd=3,roll=-5)
}
#' @export
#' @rdname f0600 
x0603PL_TFU   <- function(){f0603wed(getstep("x0200PL_TFU"))}
#' @export
#' @rdname f0600 
x0603PL_TTU   <- function(){f0603wed(getstep("x0200PL_TTU"))}
#' @export
#' @rdname f0600 
x0603PL_TTL   <- function(){f0603wed(getstep("x0200PL_TTL"))}
#' @export
#' @rdname f0600 
x0603EWAP_TTU <- function(){f0603wed(getstep("x0300EWAP_TTU"))}

#####0700
#' custom calculations
#' @name f0700
NULL
#' @export
#calcs
#' @export
f0700return <- function(x){ #this is safe for both zoo and xts
  as.zoo(retxts(as.xts(x)))[daw.global,,drop=FALSE]
}
#' @export
f0700premium <- function(x){
  x1 <- as.zoo(retxts(as.xts(x)))[daw.global,,drop=FALSE]
  x2 <- getusst()[daw.global,,drop=FALSE]
  i <- as.Date(intersect(index(x1),index(x2)),origin="1970-01-01")
  sweep(x1[i],FUN="-",STAT=x2[i],MAR=1)
}
#' @export
#' @rdname f0700 
x0700prdo     <- function(...){getstep("x0603PL_TTU")}
#' @export
#' @rdname f0700 
x0700prdovw   <- function(...){getstep("x0603EWAP_TTU")}
#' @export
#' @rdname f0700 
x0700prlo     <- function(...){getstep("x0603PL_TTL")}
#' @export
#' @rdname f0700 
x0700reloto   <- function(...){f0700return(getstep("x0603PL_TTL"))}
#' @export
#' @rdname f0700 
x0700rrloto   <- function(...){f0700premium(getstep("x0603PL_TTL"))} #phase 2 - match up stir in local
#' @export
#' @rdname f0700 
x0700redoto   <- function(...){f0700return(getstep("x0603PL_TTU"))}
#' @export
#' @rdname f0700 
x0700rrdoto   <- function(...){f0700premium(getstep("x0603PL_TTU"))} 
#' @export
#' @rdname f0700 
x0700rrdotovw <- function(...){f0700premium(getstep("x0603EWAP_TTU"))} 
#' @export
#' @rdname f0700 
x0700turn     <- function(...){getstep("x0601PL_TFU")*getstep("x0601PV")} 

x0701best     <- function(...){getstep("x0601BTP_TFU")/getstep("x0601PL_TFU")}
#' @export
#' @rdname f0700 
x0701prdo     <-  function(...){getstep("x0601PL_TFU")}
#' @export
#' @rdname f0700 
x0701mcap     <-  function(...){getstep("x0601CMC")}
#' @export
#' @rdname f0700 
x0701dipr     <-  function(...){getstep("x0601EDYI")}
#' @export
#' @rdname f0700 
x0701erpr     <-  function(...){getstep("x0601PE")}
#' @export
#' @rdname f0700 
x0701capr     <-  function(...){getstep("x0601PTCF")}
#' @export
#' @rdname f0700 
x0701bopr     <-  function(...){getstep("x0601PTBR")}

#' @export
#' @rdname f0700 
x0702best     <- function(...){getstep("x0602BTP_TFU")/getstep("x0602PL_TFU")}
#' @export
#' @rdname f0700 
x0702prdo     <-  function(...){getstep("x0602PL_TFU")}
#' @export
#' @rdname f0700 
x0702mcap     <-  function(...){getstep("x0602CMC")}
#' @export
#' @rdname f0700 
x0702dipr     <-  function(...){getstep("x0602EDYI")}
#' @export
#' @rdname f0700 
x0702erpr     <-  function(...){getstep("x0602PE")}
#' @export
#' @rdname f0700 
x0702capr     <-  function(...){getstep("x0602PTCF")}
#' @export
#' @rdname f0700 
x0702bopr     <-  function(...){getstep("x0602PTBR")}

#getusst
#' @export
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

#' @export
x0802best     <- function(...){
  x <- getstep("x0702best")
  coredata(x)[which(coredata(x)<0.1)] <- 0.1
  coredata(x)[which(coredata(x)>3)] <- 3
  dtlocf(x,roll=TRUE,rollends=c(FALSE,TRUE))
}

#' @export
x0802beta     <- function(...){
  mz(tabtomat(getrd(max(greprd("ce")))[,list(date,bui,betamcp)]))
}

#' @export
x0802mcap     <- function(...){
  dtlocf(rollxts(log(getstep("x0702mcap")),"mean",5),roll=TRUE,rollends=c(FALSE,TRUE))
}