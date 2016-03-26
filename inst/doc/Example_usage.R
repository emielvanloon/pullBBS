## ----message=FALSE,warning=F---------------------------------------------
require(pullBBS)
pullBBSmeta()

## ----message=FALSE,warning=FALSE-----------------------------------------
routes <- pullBBS(year = c(2011,2012,2013),country = c(124),region = c(11,04,79,45,68),
                  AOU = c(06740,06470,07280),useCache = T)
head(routes)

## ----warning=F,message=F-------------------------------------------------
routes.long <- WideToLongBBS(routes = routes)
head(routes.long)

## ----warning=FALSE,message=F,fig.width=7,fig.height=5--------------------
plotBBS(routes = routes.long,year = 2011,markerSize = 4)

## ----message=FALSE,warning=F---------------------------------------------
routes <- pullBBS(year = c(2011),country = c(840),
                  AOU = c(06740,05600),useCache = T)
routes.long <- WideToLongBBS(routes = routes)

## ----warning=FALSE,message=F,fig.width=7,fig.height=5--------------------
plotBBS(routes = routes.long,markerSize = 2)

## ----message=FALSE,warning=F---------------------------------------------
routes <- pullBBS(year = c(2011),country = c(124),
                  region = 04, useCache = F)
routes.long <- WideToLongBBS(routes = routes)

## ------------------------------------------------------------------------
AOUs <-unique(routes.long$AOU)
message(length(AOUs),' species found within this region.')

## ----warning=FALSE,message=F,fig.width=7,fig.height=5--------------------
plotBBS(routes = routes.long,year = 2011,markerSize = 4,AOU = AOUs[1:8])

