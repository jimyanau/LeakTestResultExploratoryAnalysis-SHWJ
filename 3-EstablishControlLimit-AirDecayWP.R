rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS")

Install_And_Load(Required_Packages)


## Load data processed in 0-ExtractCleanData-LeakTestStation.R
dt.AirDecay.WP.NoMaster <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")

## As we found out form data, there was huge process variation between 16/FEB ~ 12/APR 2017.
## Data form this period of time will be removed during establishing control limits.
dt.AirDecay.WP.NoMaster.temp <- dt.AirDecay.WP.NoMaster[dt.AirDecay.WP.NoMaster$LeakTestDateTime >= as.Date("2017-04-13", tz = "Australia/Melbourne"),]
rm(dt.AirDecay.WP.NoMaster)

## Check the descriptive statics of dataset on daily basis
dt.DailyStat.AirDecay.WP.NoMaster.temp <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.NoMaster.temp, -3, 2.1)

## randomly subset 30 days for analysis
set.seed(77)
List.Date <- as.data.frame(sample(dt.DailyStat.AirDecay.WP.NoMaster.temp$Date, 30, replace = FALSE))
dt.AirDecay.WP.NoMaster.sample <- dt.AirDecay.WP.NoMaster.temp[ as.Date(dt.AirDecay.WP.NoMaster.temp$LeakTestDateTime) %in% List.Date[,1] , ]

## Check the descriptive statics of sampled dataset on daily basis
dt.DailyStat.AirDecay.WP.NoMaster.sample <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.NoMaster.sample, -3, 2.1)

## Transform leak rate into 1/sqrt(x) as per the suggestion from Carolyn
# Added offset of 3.001 into leak rate considering the lsl is -3 cc/min
dt.AirDecay.WP.NoMaster.temp$LeakRateOffseted=dt.AirDecay.WP.NoMaster.temp$air_decay_wp+3.001
# Transform leak rate into 1/sqrt(x)
dt.AirDecay.WP.NoMaster.temp$transform_AirWP = 1/sqrt(dt.AirDecay.WP.NoMaster.temp$LeakRateOffseted)



















