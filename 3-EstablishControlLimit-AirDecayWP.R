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
set.seed(8)
List.Date <- as.data.frame(sample(dt.DailyStat.AirDecay.WP.NoMaster.temp$Date, 30, replace = FALSE))
dt.AirDecay.WP.NoMaster.sample <- dt.AirDecay.WP.NoMaster.temp[ as.Date(dt.AirDecay.WP.NoMaster.temp$LeakTestDateTime) %in% List.Date[,1] , ]


## Check the descriptive statics of sampled dataset on daily basis
dt.DailyStat.AirDecay.WP.NoMaster.sample <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.NoMaster.sample, -3, 2.1)

## Qty of each day was > 30
## Now Need to remove outliners
dt.AirDecay.WP.NoMaster.sample <- dt.AirDecay.WP.NoMaster.sample[ dt.AirDecay.WP.NoMaster.sample$air_decay_wp<900, ]


## run Quantile-Quantile Plot on dataset to access the distribution
qqnorm(dt.AirDecay.WP.NoMaster.sample$air_decay_wp)
qqline(dt.AirDecay.WP.NoMaster.sample$air_decay_wp, col = "red")

## Plot distribution of dataset
g.Sample <- ggplot(dt.AirDecay.WP.NoMaster.sample, aes(x = dt.AirDecay.WP.NoMaster.sample$air_decay_wp)) +
            geom_histogram(binwidth=0.1, alpha = 0.9, position = "dodge") +
            # geom_density(alpha=.2, fill="#FF6666")+
            scale_x_continuous(limits = c(-3, 3)) +
            xlab("Leak Rate") +
            ylab("Counts") +
            ggtitle(paste("QUK2 SH WJ Leak Rate Distribution" )) +
            theme(text = element_text(size=10))


## Transform leak rate into 1/sqrt(x) as per the suggestion from Carolyn
# Added offset of 3.001 into leak rate considering the lsl is -3 cc/min
dt.AirDecay.WP.NoMaster.sample$LeakRateOffseted=dt.AirDecay.WP.NoMaster.sample$air_decay_wp+3.001
# Transform leak rate into 1/sqrt(x)
dt.AirDecay.WP.NoMaster.sample$transform_AirWP = 1/sqrt(dt.AirDecay.WP.NoMaster.sample$LeakRateOffseted)


## Now do the hourly statics of the dataset as per original leak rate
dt.HourlyStat.AirDecay.WP.NoMaster.sample <- Hourly.Statics.AirDecay.WP(dt.AirDecay.WP.NoMaster.sample, -3, 2.1)

## Set control limit as per origianl leak rate
Mean.Ave.LeakRate <- mean(dt.HourlyStat.AirDecay.WP.NoMaster.sample$Avg.LeakRate)
SD.Ave.LeakRate <- sd(dt.HourlyStat.AirDecay.WP.NoMaster.sample$Avg.LeakRate)
Mean.SD.LeakRate <- mean(dt.HourlyStat.AirDecay.WP.NoMaster.sample$Stdev.LeakRate)
SD.SD.LeakRate <- sd(dt.HourlyStat.AirDecay.WP.NoMaster.sample$Stdev.LeakRate)



## Now do the hourly statics of the dataset as per transformed leak rate
dt.HourlyStat.AirDecay.WP.NoMaster.sample.Trans <- Hourly.Statics.AirDecay.Transform.WP(dt.AirDecay.WP.NoMaster.sample, -3, 2.1)

## Set control limit as per transformed leak rate
Mean.Ave.LeakRate.Trans <- mean(dt.HourlyStat.AirDecay.WP.NoMaster.sample.Trans$Avg.LeakRate)
SD.Ave.LeakRate.Trans <- sd(dt.HourlyStat.AirDecay.WP.NoMaster.sample.Trans$Avg.LeakRate)
Mean.SD.LeakRate.Trans <- mean(dt.HourlyStat.AirDecay.WP.NoMaster.sample.Trans$'Stdev.LeakRate')
SD.SD.LeakRate.Trans <- sd(dt.HourlyStat.AirDecay.WP.NoMaster.sample.Trans$'Stdev.LeakRate')


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################
## Below is an example to plot contorl chart with new setup contorl limits

## Subset data in continuous date to plot contorl chart
## Plot control chart of original leak test value with set control limits
dt.SmallGroup <- dt.AirDecay.WP.NoMaster.temp[dt.AirDecay.WP.NoMaster.temp$LeakTestDateTime >= as.Date("2017-11-01", tz = "Australia/Melbourne") & 
                                                dt.AirDecay.WP.NoMaster.temp$LeakTestDateTime <= as.Date("2017-11-10", tz = "Australia/Melbourne"), ]

dt.HourlyStat.AirDecay.WP.SmallGroup <- Hourly.Statics.AirDecay.WP(dt.SmallGroup, -3, 2.1)

Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.HourlyStat.AirDecay.WP.SmallGroup, 0, -3, 2.1, 
                                          "SH WJ Leak Test Control Chart - WP (Original Leak Rate Value)", Mean.Ave.LeakRate, SD.Ave.LeakRate,
                                          Mean.SD.LeakRate, SD.SD.LeakRate )



## Plot control chart of transformed leak test value with set control limits

## Transform leak rate into 1/sqrt(x) as per the suggestion from Carolyn
# Added offset of 3.001 into leak rate considering the lsl is -3 cc/min
dt.SmallGroup$LeakRateOffseted=dt.SmallGroup$air_decay_wp+3.001
# Transform leak rate into 1/sqrt(x)
dt.SmallGroup$transform_AirWP = 1/sqrt(dt.SmallGroup$LeakRateOffseted)

# Calculate hourlt statics on transformed data
dt.HourlyStat.AirDecay.WP.SmallGroup.Trans <- Hourly.Statics.AirDecay.Transform.WP(dt.SmallGroup, -3, 2.1)


### Remember to verse the order of LSL & USL due to the trasformation
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.HourlyStat.AirDecay.WP.SmallGroup.Trans, 1/sqrt(0.001), 1/sqrt(2.1+3.001), 1/sqrt(-3+3.001), 
                          "SH WJ Leak Test Control Chart - WP (Tranformed Leak Rate Value)", Mean.Ave.LeakRate.Trans, SD.Ave.LeakRate.Trans,
                          Mean.SD.LeakRate.Trans, SD.SD.LeakRate.Trans )





