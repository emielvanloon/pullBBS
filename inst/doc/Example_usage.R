## ----message=FALSE-------------------------------------------------------
require(pullBBS)
pullBBSmeta()

## ----message=FALSE-------------------------------------------------------
routes <- pullBBS(year = c(2011,2012,2013),country = c(124),region = c(04,79),
                  AOU = c(06740))

## ------------------------------------------------------------------------
routes.long <- WideToLongBBS(routes = routes)
str(routes.long)

temp <- routes.long[,c('Route','year','AOU')]
temp2 <- unique(temp)

length(subset(temp2,AOU == 6470)[,1])
length(subset(temp2,AOU == 6740)[,1])

hist(temp$Route)



## ----fig.width=7,fig.height=5--------------------------------------------
## Load packages for GIS
require(leaflet)
require(sp)

## Calculate averaged stop observations per route
stops.OVEN.average <- aggregate(x = routes.long$Observations,
                 by = list(Lati = routes.long$Lati, Longi = routes.long$Longi), FUN = mean)

## Count the number of stops per route and year (all route-years should have 50 stops)
stops.OVEN.count <- aggregate(x = routes.long$Observations,
                 by = list(year = routes.long$year, Lati = routes.long$Lati,
                 Longi = routes.long$Longi), FUN = length)
range(stops.OVEN.count$x)

## Count number of years surveyed for each route
stops.OVEN.surveyYears <- aggregate(x = stops.OVEN.count$x,
                 by = list(Lati = stops.OVEN.count$Lati, Longi = stops.OVEN.count$Longi),
                 FUN = length)

## Create spatial points dataframe
proj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
route.mean.spdf <- SpatialPointsDataFrame(coords = stops.OVEN.average[,c('Longi','Lati')],
                 data.frame(obs = stops.OVEN.average$x,years = stops.OVEN.surveyYears$x),
                 proj4string = proj)

## Plot results in Leaflet
pal.1 <- colorNumeric(palette = c('#FF5555','#FFFF55','#55FF55'),route.mean.spdf@data$obs)
pal.2 <- colorFactor(palette = c('#FF5555','#FFFF55','#55FF55'),route.mean.spdf@data$years)
marker.rad <- 0.6 
leaflet() %>% addTiles() %>%
  addCircleMarkers(data = route.mean.spdf,fillColor = ~pal.1(obs),
                   color = '#444444',stroke = T,fillOpacity = marker.rad,
                   weight = 0.5,group = 'Observations',radius = 7) %>%
  addLegend(position = 'topright',values = route.mean.spdf@data$obs,pal = pal.1,
                   title = 'Mean Obs. Dens.') %>%
  addCircleMarkers(data = route.mean.spdf,fillColor = ~pal.2(years),color = '#444444',
                   stroke = T,fillOpacity = marker.rad,
                   weight = 0.5,group = 'Sample Years',radius = 7) %>%
  addLegend(position = 'topright',values = route.mean.spdf@data$years,pal = pal.2,
                   title = 'Survey Years') %>%
  addLayersControl(baseGroups = c("Observations", "Sample Years"))

## ------------------------------------------------------------------------
message('Total number of routes: ',nrow(unique(subset(routes.long,select = c('Route')))))
message('Total number of Stops: ',nrow(unique(subset(routes.long,select = c('Route','Stop')))))
message('Total Stops / Total Routes: ',nrow(unique(subset(routes.long,
            select = c('Route','Stop'))))/nrow(unique(subset(routes.long,select = c('Route')))))
message('Total number of Ovenbird observations: ',
            sum(subset(routes.long,select = c('Route','Stop','Observations'))$Observations))
message('Mean Ovenbird observations per stop: ',
            mean(subset(routes.long,select = c('Route','Stop','Observations'))$Observations))

## ------------------------------------------------------------------------
## Import query results from text file
routes2.wide <- read.csv('Routes_wide.txt')

## Convert from wide to long dataset
require(tidyr)
routes2.long <- gather(routes2.wide,Stop,Observations,Stop1:Stop50)

## replace NA's with 0's
idx <- which(is.na(routes2.long$Observations))
routes2.long$Observations[idx] <- 0

## Convert Stop Column to numeric
routes2.long$Stop <- as.matrix(lapply(routes2.long$Stop,
              FUN = function(x){return(as.numeric(substr(x,5,nchar(x))))}))

message('Total number of routes: ',nrow(unique(subset(routes2.long,select = c('Route')))))
message('Total number of Stops: ',nrow(unique(subset(routes2.long,select = c('Route',
       'Stop')))))
message('Total Stops / Total Routes: ',nrow(unique(subset(routes2.long,
        select = c('Route','Stop'))))/nrow(unique(subset(routes2.long,select = c('Route')))))
message('Total number of Ovenbird observations: ',sum(subset(routes2.long,
        select = c('Route','Stop','Observations'))$Observations))
message('Mean Ovenbird observations per stop: ',mean(subset(routes2.long,
        select = c('Route','Stop','Observations'))$Observations))

