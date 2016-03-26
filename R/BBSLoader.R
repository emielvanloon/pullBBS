#' Pull data from the North American Breeding Bird Survey ftp server
#'
#' This will automatically download all the BBS data from the FTP server.  Only those routes within your specified regions and sightings from your specified species will be saved in the output data.frame.  This function will also use the weather data from the BBS server to create missing absence records for routes in which your species was not observed.
#'
#' @param year A vector containing a list of the years for the surveys
#' @param country ISO country code for the region to pull observations from.  Use the function pullBBSmeta to get a list of country and region ISO codes.
#' @param region A BBS region code representing a country or state.  Use the function pullBBSmeta to get a list of country and region codes.  Can be in the form of a list for selecting multiple regions.  If no region is specified, all regions within the country will be pulled.
#' @param AOU Vector of AOU species codes to pull.  AOU = 0 will pull all species within the sepified region.  If no AOU codes are specified, all species observed within the regions for the given period will be pulled.
#' @param useCache When set to TRUE, downloaded data will not be erased after the function is complete.  In subsequent runs, data will be loaded taken from the cache instead of being downloaded.  This is usefull if you are running the function multiple times in a session and you don't want to redownload the BBS data on each run.
#' @return A data.frame containing all the selected routes and observations for the specified species.
#' @export

pullBBS<- function(year,country,region,AOU,useCache = F){

  ## Check that only a single country code was entered
  if(length(country) > 1){
    message('You may only enter a single country code.  Exiting function...')
    return()
  }

  if(missing(region)){region = 0}
  if(missing(AOU)){AOU = 0}

  ## Pull observation data
  routes <- pullBBSObservations(year = year,country = country,region = region,AOU = AOU,useCache = useCache)
  #summary(routes)

  ## Set species and region codes if user did not specify
  if(AOU == 0){AOU = unique(routes$AOU)}
  if(region == 0){region = unique(routes$statenum)}

  ## Pull lat lon coords for each route (filtered by country and region)
  routeInfo <- pullBBSRouteLocations(country = country,region = region,useCache = useCache)


  message('Get missing absence data')
  ## Add missing absence data
  routes.Meta <- pullBBSRouteData(year = year,country = country,region = region,useCache = useCache)



  ## Loop through each species code to add absence data
  for(i in AOU){
    ## Subset observations for current species
    routes.species <- subset(routes,AOU == i)

    ## Create unique keys for the routes and routes.Meta tables for comparing records
    routes.sample <- paste(routes.species$year,routes.species$countrynum,routes.species$statenum,routes.species$Route,sep = '-')
    routes.Meta.sample <- paste(routes.Meta$Year,routes.Meta$countrynum,routes.Meta$statenum,routes.Meta$Route,sep = '-')

    ## Find which year-routes are not included in routes data.frame
    idx <- which(!routes.Meta.sample %in% routes.sample)

    ## Prepare data.frame containing missing route info and absence values
    temp <- routes.Meta[idx,c('RouteDataId','countrynum','statenum','Route','RPID','Year')]
    temp.stops <- cbind(i,matrix(data = 0,nrow = length(temp$RouteDataId),ncol = 50))
    temp.append <- cbind(temp,temp.stops)
    names(temp.append) <- names(routes)

    ## Append the correctly formatted missing absence data
    if(i == AOU[1]){
      routes.appended <- rbind(routes,temp.append)
    }else{
      routes.appended <- rbind(routes.appended,temp.append)
    }

    ## Clear old variables from memory
    rm(routes.sample,routes.Meta.sample,idx,temp,temp.stops,temp.append)
  }
  rm(routes)

  ## Double check the the resulting table has the correct number of rows (Compare observation data to meta data)
  message('Row number check: ',nrow(routes.appended) == nrow(routes.Meta)*length(AOU))

  message('Append covariates from sampling instances (weather, noise, etc...)')
  ## Append covariate info from sampling round
  routes.appended.sample <- paste(routes.appended$year,routes.appended$countrynum,routes.appended$statenum,routes.appended$Route,sep = '-')
  routes.Meta.sample <- paste(routes.Meta$Year,routes.Meta$countrynum,routes.Meta$statenum,routes.Meta$Route,sep = '-')

  ## Find matching indexes
  idx <- match(routes.appended.sample,routes.Meta.sample)
  # idx holds the indexes of the matching vectors from the routes.appended table

  ## Create data.frame to be appended
  temp.covariates <- routes.Meta[idx,c('Month','Day','ObsN',"StartTemp","EndTemp","TempScale","StartWind","EndWind","StartSky","EndSky","StartTime","EndTime","Assistant","RunType")]

  ## Append covariates
  routes.appended.2 <- cbind(routes.appended,temp.covariates)
  rm(routes.appended,temp.covariates)

  message('Append route info (lat/long, habitat type, etc...)')
  ## Append route info
  routes.appended.2.sample <- paste(routes.appended.2$countrynum,routes.appended.2$statenum,routes.appended.2$Route,sep = '-')
  routeInfo.sample <- paste(routeInfo$countrynum,routeInfo$statenum,routeInfo$Route,sep = '-')

  ## Find matching indexes
  idx <- match(routes.appended.2.sample,routeInfo.sample)
  # idx holds the indexes of the matching vectors from the routes.appended table

  ## Create data.frame to be appended
  temp.routeInfo <- routeInfo[idx,c('Lati','Longi','Stratum',"BCR","LandTypeID","RouteTypeID","RouteTypeDetailId")]

  ## Append covariates
  routes.appended.3 <- cbind(routes.appended.2,temp.routeInfo)
  rm(routes.appended.2,temp.routeInfo,routes.Meta,routeInfo)

  message('Dataset is ready!!!')
  return(routes.appended.3)
}

pullBBSObservations <- function(year,country,region,AOU,useCache = F){
  require(RCurl)

  ### Set parameters
  ftp <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide/'

  tempdirectory <- 'BBS_temp'

  ### Get list of files from server
  fileList <- getURL(ftp,verbose=TRUE,dirlistonly = TRUE)
  filenames <- strsplit(fileList, "\r*\n")[[1]]

  ### Create directory for holding temporary data
  if(!dir.exists(tempdirectory)){
    dir.create(tempdirectory)
  }

  ### Loop through files on server
  for(i in seq(from = 1,by = 1, to = length(filenames))){
    ### Download and unzip data
    if(file.exists(paste0(tempdirectory,'/',filenames[i])) == F | useCache == F){
      download.file(paste0(ftp,filenames[i]),destfile = paste0(tempdirectory,'/',filenames[i]))
    }
    message(paste0('Unzipping...(',filenames[i],')'))
    unzip(paste0(tempdirectory,'/',filenames[i]),exdir = paste0(tempdirectory,'/unzipped'))
  }

  ### Initialize output table
  results <- data.frame()

  ### Load zipped data into memory
  file <- dir(paste0(tempdirectory,'/unzipped/'))
  for(j in seq(from = 1,by = 1, to = length(file))){
    message(paste0('Importing downloaded data...(',file[j],')'))
    temp <- read.csv(paste0(tempdirectory,'/unzipped/',file[j]))

    message('Filtering dataset...')
    ## Filter data according to user selection
    idx.year = which(temp$year %in% year)
    idx.country = which(temp$countrynum %in% country)
    if(region == 0){
      ##  If region is set to 0, do not filter species
      idx.region = seq(1:nrow(temp))
    }else{
      idx.region = which(temp$statenum %in% region )
    }
    if(AOU == 0){
      ##  If AOU is set to 0, do not filter species
      idx.AOU = seq(1:nrow(temp))
    }else{
      idx.AOU = which(temp$AOU %in% AOU )
    }

    idx <- intersect(intersect(intersect(idx.year,idx.country),idx.region),idx.AOU)

    message(paste0(length(idx),' matching records saved...'))
    results <- rbind(temp[idx,],results)
  }

  if(nrow(results) == 0){
    message('No matching obseravtions found.  Are you sure you entered the correct AOU, region, and country codes?')
  }else{
    message('Pull complete!!!')
  }

  ## Delete temporary files
  if(useCache == F){
    unlink(tempdirectory,recursive = T)
    message('Temporary files deleted.')
  }else{
    message('Temporary files stored in cache (',tempdirectory,').')
  }

  return(results)
}

#' Pull data from the North American Breeding Bird Survey ftp server
#'
#' country: A BBS country code.  Use the function pullBBSmeta to get a list of country and region codes.
#' region: A BBS region code representing a country or state.  Use the function pullBBSmeta to get a list of country and region codes.  Can be in the form of a list for selecting multiple regions.
#' ignoreCache: Set to TRUE to overwrite previously downloaded data.  This is usefull if a prevously cached file has been corrupted due to a download error.
#' returns a data.frame containing all the selected routes and their lat/long coordinates.

pullBBSRouteLocations <- function(country,region,useCache = F){
  require(RCurl)

  ## Set parameters
  ftp <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/'
  tempdirectory <- 'BBS_temp'
  file <- 'Routes.zip'

  ## Create directory for holding temporary data
  if(!dir.exists(tempdirectory)){
    dir.create(tempdirectory)
  }

  ## Download and unzip data
  if(file.exists(paste0(tempdirectory,'/',file)) == F | useCache == F){
    download.file(paste0(ftp,file),destfile = paste0(tempdirectory,'/',file))
  }
  message('Unzipping...')
  unzip(paste0(tempdirectory,'/',file),exdir = paste0(tempdirectory,'/unzippedRoutes'))

  ## Initialize output table
  results <- data.frame()

  ## Load table into memory
  file <- 'routes.csv'
  message(paste0('Importing downloaded data...(',file,')'))
  temp <- read.csv(paste0(tempdirectory,'/unzippedRoutes/',file))

  message('Filtering dataset...')
  ## Filter data according to user selection
  idx.country = which(temp$countrynum %in% country)
  idx.region = which(temp$statenum %in% region)

  idx <- intersect(idx.country,idx.region)

  message(paste0(length(idx),' matching records saved...'))
  results <- rbind(temp[idx,],results)
  message('Pull complete!!!')

  ## Delete temporary files
  if(useCache == F){
    unlink(tempdirectory,recursive = T)
    message('Temporary files deleted.')
  }else{
    message('Temporary files stored in cache (',tempdirectory,').')
  }

  return(results)
}

pullBBSRouteData <- function(year,country,region,useCache = F){
  require(RCurl)

  ### Set parameters
  ftp <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/'
  tempdirectory <- 'BBS_weather'
  file <- 'Weather.zip'

  ### Create directory for holding temporary data
  if(!dir.exists(tempdirectory)){
    dir.create(tempdirectory)
  }

  ### Download and unzip data
  if(file.exists(paste0(tempdirectory,'/',file)) == F | useCache == F){
    download.file(paste0(ftp,file),destfile = paste0(tempdirectory,'/',file))
  }
  message('Unzipping...')
  unzip(paste0(tempdirectory,'/',file),exdir = paste0(tempdirectory,'/unzipped'))

  ### Load zipped data into memory
  results <- data.frame() # initialize empty results table
  file <- dir(paste0(tempdirectory,'/unzipped/'))
  for(j in seq(from = 1,by = 1, to = length(file))){
    message(paste0('Importing downloaded data...(',file[j],')'))
    temp <- read.csv(paste0(tempdirectory,'/unzipped/',file[j]))

    message('Filtering dataset...')
    ## Filter data according to user selection
    idx.year = which(temp$Year %in% year)
    idx.country = which(temp$countrynum %in% country)
    idx.region = which(temp$statenum %in% region)

    idx <- intersect(intersect(idx.year,idx.country),idx.region)

    message(paste0(length(idx),' matching records saved...'))
    results <- rbind(temp[idx,],results)
  }
  message('Pull complete!!!')

  ## Delete temporary files
  if(useCache == F){
    unlink(tempdirectory,recursive = T)
    message('Temporary files deleted.')
  }else{
    message('Temporary files stored in cache (',tempdirectory,').')
  }
  return(results)
}

#' Pull data from the North American Breeding Bird Survey ftp server
#'
#' This will create a folder called BBS_Meta in your working directory where the metadata files from the BBS server will be saved to.  You can use this metadata to find out mode information about the sampling protocol and species and country/region codes.
#'
#' @export

pullBBSmeta <- function(){
  require(RCurl)

  ### Set parameters
  ftp <- 'ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/'

  tempdirectory <- 'BBS_Meta'

  ### Get list of files from server
  fileList <- getURL(ftp,verbose=TRUE,dirlistonly = TRUE)
  filenames <- strsplit(fileList, "\r*\n")[[1]]

  metalist <- c('SpeciesList.txt','RegionCodes.txt','RunProtocolID.txt','weathercodes.txt','WeatherInf.txt','RouteInf.txt','BCR.txt','FiftySt.txt')

  ### Create directory for holding temporary data
  if(!dir.exists(tempdirectory)){
    dir.create(tempdirectory)
  }

  ### Loop through files on server
  for(i in seq(from = 1,by = 1, to = length(metalist))){
    ### Download and unzip data
    if(file.exists(paste0(metalist[i])) == F){
      download.file(paste0(ftp,metalist[i]),destfile = paste0(tempdirectory,'/',metalist[i]))
    }
  }
  message(paste0('Meta datafies have been downloaded and saved in the folder ',tempdirectory))
}

#' Convert BBS data into a long format
#'
#'When used on the data.frame returned by pullBBS, the 50 columns holding observations for each of the 50 stops will be condensed into two columns: 'Stop' and 'Observations'.
#'
#' @param routes A data.frame returned form the pullBBS function.
#' @export

WideToLongBBS <- function(routes){
  ## Create vector containing column names to transpose
  stopcols <- c()
  for(i in seq(from = 1, by = 1, to = 50)){
    stopcols[i] <- paste0('Stop',i)
  }
  ## Reshape to a wide fromat
  routes.long <- reshape(routes,varying = stopcols,v.names = 'Observations',timevar = 'Stop',times=1:50,direction = 'long')

  ## Create unique key for identifying spatially separated independant locations
  routes.long$siteKey <- paste(routes.long$countrynum,routes.long$statenum,routes.long$Route,routes.long$Stop,sep = '-')

  ## Delete id column (not necessary)
  columnFilter <- names(routes.long)[which(names(routes.long)!= 'id')]
  routes.long <- routes.long[,columnFilter]

  return(routes.long)
}

#' Plot data from the North American Breeding Bird Survey
#'
#' @details Each site in the North American Breeding Bird Survey consists of 50 stops along a route.  This function will return the summed observations for each species accross all 50 stops for each site.
#'
#' @param routes A data.frame returned from the function WideToLongBBS.
#' @param covariates An optional raster or raster sobject to be used as a layer containing covariate information.
#' @param colorScheme An optional argument to specify the color map of the covariate raster layers.
#' @param markerSize An optional argument specifyig the radius of the plotted circle markers.
#' @param AOU Optional vector of AOU codes to plot  If unused, all species will be plotted.
#' @return A interactive map which can be used to compare observation data between species.
#' @export

plotBBS <- function(routes,year,covariates,colorScheme = c('#FF5555','#FFFF55','#55FF55'),markerSize = 7,AOU){
  ## Load packages for GIS
  require(leaflet)
  require(sp)

  ## Filter out correct year
  # Some routes have missing years, so restricting the plot to a single year avoides issues realted to this.
  if(length(unique(routes$year)) == 1){
    year = unique(routes$year)
  }else if(missing(year)){
    stop('Data spans multiple years.  You must specify a year to plot with the \'year\' argument')
  }
  routes <- subset(routes,year == year)

  ## Extract unique AOU species codes
  if(missing(AOU)){
    AOUs <- unique(routes$AOU)
  }else{
    AOUs <- AOU
  }


  if(length(AOUs) > 10){
    warning('WARNING: More than 10 species in dataset.  Legend may not display corrrectly.')
  }

  ## Calculate summed stop observations per route per species
  stops.summed <- aggregate(x = routes.long$Observations,
                            by = list(Lati = routes$Lati, Longi = routes$Longi, AOU = routes$AOU), FUN = sum)

  ## Create raster layers
  #  pal.covariates <- colorFactor(palette = c('#FF5555','#FFFF55','#55FF55'),route.mean.spdf@data$years)

  ## Color pallete for obsercations (common scale shared by all species)
  max.observations <- max(subset(stops.summed,AOU %in% AOUs)$x)
  pal.observations <- colorNumeric(palette = c('#FF5555','#FFFF55','#55FF55'),seq(from = 1,by = 1, to = max.observations))

  ## Function for passing multiple addCircleMarkers objects in leaflet
  speciesLayers <- function(map){
    for(i in 1:length(AOUs)){      ## Subset single species
      temp <- subset(stops.summed,AOU == AOUs[i])
      observations <- SpatialPointsDataFrame(coords = temp[,c('Longi','Lati')],
                                              data.frame(obs = temp$x),
                                              proj4string = proj)
      ## Update map object with new layer
      map <- addCircleMarkers(map,data = observations,
                              fillColor = ~pal.observations(obs),
                              color = '#444444',
                              stroke = T,fillOpacity = 0.6,
                              weight = 0.5,group = as.character(AOUs[i]),radius = markerSize)
    }
    return(map)
  }

  ## Plot results in Leaflet
  leaflet() %>% addTiles() %>%
    speciesLayers %>%
    addLegend(position = 'topright',
              values = seq(from = 1,by = 1, to = max.observations),
              pal = pal.observations,
              title = 'Route Observations') %>%
    addLayersControl(baseGroups = as.character(AOUs))
}
