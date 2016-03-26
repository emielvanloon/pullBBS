### About pullBBS

pullBBS is a R package designed to download and reorganize data from the [North American Breeding Bird Survey](https://www.pwrc.usgs.gov/bbs/) (here after referred to as BBS).  Data stored on the ftp server is saved in a series of tables designed to be accessed within a relational database environment.  The pullBBS package will automatically query this data once downloaded, appending the tables containing species observations, survey dates and locations, and weather data into a single table.  pullBBS can then further reorganize this data table into a long format which allows it to be easily used used within species distribution modelling functions within R.

**\*Note: pullBBS is not officially supported by the North American Breeding Bird Survey**, therefore the North American Breeding Bird Survey cannot be held responsible for any mistakes in the package or resulting data which is downloaded using this package.

### Installation and use

By executing the following code in R, you can install `pullBBS` using the `devtools` package.  Once installed, you can view the `Example_usage` vignette to see example workflows.

```r
<<<<<<< HEAD
## Install devtools package
install.packages('devtools')
require(devtools)

## Install and load pullBBS package
install_github(repo = 'RTbecard/pullBBS',build_vignettes=F,force = T)
require(pullBBS)

## Load example
vignette(package = 'pullBBS',topic = 'Example_usage')
```
### To do list

A short list of features we're currently working on implementing into the package:

- Function to quickly calculate species richness metrics for pulled data (Simpsons and Chao indices).
