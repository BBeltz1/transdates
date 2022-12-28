library(raster)
library(tidyverse)
library(sf)
# Read in netcfs
raw.dir <- here::here("data-raw", "gridded")
seasonal_oisst_anom_nc <-"sst.day.mean.2022.v2.nc"

dat <- raster::stack(file.path(raw.dir,seasonal_oisst_anom_nc))
# Get Daily Mean and SD
## Crop and Rotate
crs(dat) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
dat <- raster::crop(dat, extent(280,300,30,50))
dat <- raster::rotate(dat)

## Dat too big - cut in half because Raster
dat2<- dat[[181:360]]

## Loop for calculating mean and SD
### Mask to EPU
epu <- ecodata::epu_sf %>%filter(EPU != "SS")
gb <- NULL
gom <- NULL
mab <- NULL
## GB
for(i in 1:nlayers(dat2)){
  outgb <- raster::mask(dat2, epu[epu$EPU == "GB",])
  sst.mean <- mean(outgb[[i]]@data@values, na.rm = T)
  sst.sd <- sd(outgb[[i]]@data@values, na.rm = T)
  sst.date <- outgb[[i]]@data@names

  df<- data.frame(DATE = sst.date, EPU = "GBK",
                  MEAN = sst.mean, SD = sst.sd)
  gb<- gb %>% rbind(df)
}

## GOM
for(i in 1:nlayers(dat2)){
  outgom <- raster::mask(dat2, epu[epu$EPU == "GOM",])
  sst.mean <- mean(outgom[[i]]@data@values, na.rm = T)
  sst.sd <- sd(outgom[[i]]@data@values, na.rm = T)
  sst.date <- outgom[[i]]@data@names

  df<- data.frame(DATE = sst.date, EPU = "GOM",
                  MEAN = sst.mean, SD = sst.sd)
  gom<- gom %>% rbind(df)
}

## MAB
for(i in 1:nlayers(dat2)){
  outmab <- raster::mask(dat2, epu[epu$EPU == "MAB",])
  sst.mean <- mean(outmab[[i]]@data@values, na.rm = T)
  sst.sd <- sd(outmab[[i]]@data@values, na.rm = T)
  sst.date <- outmab[[i]]@data@names

  df<- data.frame(DATE = sst.date, EPU = "MAB",
                  MEAN = sst.mean, SD = sst.sd)
  mab<- mab %>% rbind(df)
}

# Build CSV and attach to original file from Kevin
raw <- rbind(gb, gom, mab) %>%
  dplyr::mutate(DATE = gsub("X","",as.character(DATE)),
                DATE = lubridate::as_date(DATE),
                Y = lubridate::year(DATE),
                M = lubridate::month(DATE),
                D = lubridate::day(DATE),
                Date = lubridate::yday(DATE),
                X = c("NA"),
                DayStart = c("NA"),
                Percent = c(1),
                Summary.Type = c("normal"),
                Mission = c("AV"),
                n.pixels = c("NA")) %>%
  dplyr::rename(Mean = MEAN,
                SE = SD,
                Location = EPU) %>%
  dplyr::select(!DATE) %>%
  dplyr::filter(!M == 6)
regdata<- read.csv(file.path(here::here("data-raw/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv")))
regdata<- regdata %>% rbind(raw)

write.csv(regdata, file = here::here("data-raw/2022_final_TS_EPU.CSV" ))


