## Get transition dates for the EPUs
## Code from Kevin Friedland
##



# trans dates from sst already extracted by grid file areas
library(raster)

# where the extracted data is located
# TS_SHP_adv rep MAB GOM GBK NES SCSPoly.RData
# TS_SHP_nes five areasPoly.csv
# TS_SHP_DFO_GBKPoly.csv
# TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv
#regdata=read.csv(file.choose(),header=T)
regdata<- read.csv(file.path(here::here("data-raw/TS_SHP_adv rep MAB GOM GBK NES SCSPoly.csv"))) %>%
  dplyr::filter(Location %in% c("GBK", "GOM", "MAB"))

# set lastyear
lastyear = max(regdata$Y)
#lastyear = max(1985)
table(regdata$Location)

eregs=c("GBK","GOM","MAB")
#eregs=c("GBK","GOM","MAB")
# or just get them
ereg = unique(regdata$Location)

sprtrans=NA
falltrans=NA
sprtrans10=NA
falltrans10=NA
sprtrans20=NA
falltrans20=NA
sprtrans30=NA
falltrans30=NA
maxday=NA
sumlen=NA
sumlen10=NA
sumlen20=NA
sumlen30=NA

trans_dat = data.frame("ereg" = c(NA), "yy" = c(NA),
                       "sprtrans" = c(NA), "falltrans" = c(NA),
                       "sprtrans10" = c(NA), "falltrans10" = c(NA),
                       "sprtrans20" = c(NA), "falltrans20" = c(NA),
                       "sprtrans30" = c(NA), "falltrans30" = c(NA),
                       "maxday" = c(NA), "sumlen"= c(NA),
                       "sumlen10"= c(NA), "sumlen20"= c(NA), "sumlen30"= c(NA)
                       )

for (yy in 1982:lastyear){
  for (ereg in eregs){
    print(c(yy,ereg))

    # long term mean based on 1982 to 2011, ie 30 years
    erdata=regdata$Mean[regdata$Y>1981 & regdata$Y<2012 & regdata$Location==ereg]
    ltmean=mean(erdata, na.rm=T)
    #    minld=min(tyr)
    #    maxld=max(tyr)

    tyr=regdata$Mean[regdata$Y==yy & regdata$Location==ereg]
    styr=raster::movingFun(tyr, 5, fun=mean, type='around', circular=FALSE, na.rm=T)

    maxsstyr=max(styr)

    ## mean
    for (dayc in 105:195){
      if (styr[dayc] > ltmean){
        sprtrans=dayc
        break
      }}
    for (dayc in 105:195){
      if (styr[dayc] > (ltmean*.9)){
        sprtrans10=dayc
        break
      }}
    for (dayc in 105:195){
      if (styr[dayc] > (ltmean*.8)){
        sprtrans20=dayc
        break
      }}
    for (dayc in 105:195){
      if (styr[dayc] > (ltmean*.7)){
        sprtrans30=dayc
        break
      }}
    #}

    if(length(styr)>364){

      if(length(styr)>182){
        for (dayc in 275:365){
          if (styr[dayc] < ltmean){
            falltrans=dayc
            break
          }}
        for (dayc in 275:365){
          if (styr[dayc] < (ltmean*1.1)){
            falltrans10=dayc
            break
          }}
        for (dayc in 275:365){
          if (styr[dayc] < (ltmean*1.2)){
            falltrans20=dayc
            break
          }}
        for (dayc in 275:365){
          if (styr[dayc] < (ltmean*1.3)){
            falltrans30=dayc
            break
          }}
        }
        for (dayc in 1:365){
          if (styr[dayc] == maxsstyr){
            maxday=dayc
            break
          }
        }
      }

    #} # loop to guide aginst incomplete last year

    sumlen=falltrans-sprtrans
    sumlen10 = falltrans10-sprtrans10
    sumlen20 = falltrans20-sprtrans20
    sumlen30 = falltrans30-sprtrans30

    trans_out<-  cbind(ereg, yy, sprtrans, sprtrans10,
                       sprtrans20, sprtrans30,
                       falltrans, falltrans10,
                       falltrans20, falltrans30,
                       maxday, sumlen, sumlen10,
                       sumlen20, sumlen30
                       ) %>%
      as.tibble()

    trans_dat<- trans_dat %>% rbind(trans_out)

    trans_dates <- trans_dat %>%
      dplyr::rename(EPU = ereg,
                    Time = yy)
  }  # ecoregion
}#year

write.csv(trans_dat, "EPU_transition_dates.csv",col.names=T)


test<-read.csv(here::here("EPU_transition_dates.csv"))
trans<- test %>% filter(yy %in% 1982:2022)

### Test Plot



