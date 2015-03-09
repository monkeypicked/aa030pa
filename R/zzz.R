.onLoad <- function(libname, pkgname) {
    bui.local <- buiindir(paste0(root.global, "BDH/RAW/BEST_TARGET_PRICE/TFU"))
    da.local <<- commonda(patt = "CUR_MKT_CAP_TFU.RData")
    dd <- data.frame(derca())[, "date"]
    daw.local <<- as.Date(intersect(da.local, dd), origin = as.Date("1970-01-01"))
    supad.g <<- cart(bui = data.table(bui = bui.local), da = data.table(date = da.local))
    supaw.g <<- cart(bui = data.table(bui = bui.local), da = data.table(date = daw.local))
    aapaenv <<- environment(x0100BTP_TFU)  #this is the package environment - could be done better?
    aapafun <<- ls(aapaenv)
} 
