## This code is to investigate the data of time between failures

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts2")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "7-PrepareData-XbarSChart.R"
## duplicaes will be included becasue we want to observe all process variation
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")


## Rename the date/time & targeted data of dataset as per function requirement
dt.AirDecay.WP.NoMaster.TBM$UseTime <- dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime
dt.AirDecay.WP.NoMaster.TBM$UseData <- dt.AirDecay.WP.NoMaster.TBM$air_decay_wp

dt.DailyStatics.AirWP <- Daily.Statics.General(dt.AirDecay.WP.NoMaster.TBM)

# Plot line chart for the whole year
Plot.LineChart.Date(dt.DailyStatics.AirWP,"Period", "RejectPercent", 0, 0, 0, mean(dt.DailyStatics.AirWP$RejectPercent), 
                    sd(dt.DailyStatics.AirWP$RejectPercent), "Air Decay WP Reject Rate run Chart")

###############################################################################################
# subset one period of time to observe
dt.DailyStatics.period1 <- dt.DailyStatics.AirWP[dt.DailyStatics.AirWP$Period >= as.Date("2017-08-01") & dt.DailyStatics.AirWP$Period < as.Date("2017-10-01") , ]
Plot.LineChart.Date(dt.DailyStatics.period1,"Period", "RejectPercent", 0, 0, 0, mean(dt.DailyStatics.AirWP$RejectPercent), 
                    sd(dt.DailyStatics.AirWP$RejectPercent), "Air Decay WP Reject Rate run Chart")


#############################################################################################
# subset individual data from 5/Sep/17 for investigation
dt.period2 <- dt.AirDecay.WP.NoMaster.TBM[dt.AirDecay.WP.NoMaster.TBM$UseTime >= as.Date("2017-09-05") & dt.AirDecay.WP.NoMaster.TBM$UseTime < as.Date("2017-09-06") , ]
Plot.SinglePoint.WP.ControlChart(dt.period2, 0, -3, 2.1, "Air Decay WP Leak Rate run Chart (05/Sep/2017")

# Plot histogram of time between failures in testing time
ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
              aes(x=TBF_TestTime_Min)) +
              geom_histogram(binwidth=10, alpha=.5, position="identity", colour='red', fill = 'red') +
              # geom_density() +
              scale_x_continuous(limits = c(0, 150)) +
              xlab("Minutess btw Failures - Testing Time") +
              ylab("Counts") +
              ggtitle(paste("QUK2 SH WJ Test Time Between Failures - Air Decay WP Failures (05/Sep/2017)")) +
              theme(text = element_text(size=10),legend.position="bottom")

# Plot histogram of time between failures in testing time
ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
             aes(x=TBF_CastTime_Min)) +
              geom_histogram(binwidth=10, alpha=.5, position="identity", colour='red', fill = 'red') +
              # geom_density() +
              scale_x_continuous(limits = c(0, 150)) +
              xlab("Minutes btw Failures - Casting Time") +
              ylab("Counts") +
              ggtitle(paste("QUK2 SH WJ Cast Time Between Failures - Air Decay WP Failures (05/Sep/2017)")) +
              theme(text = element_text(size=10),legend.position="bottom")

# control chart of Ok Master of the same period
dt.AirDecay.WP.Master <- readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")
dt.period2.Master <- dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id == "XBA1601290101A23" &
                                            dt.AirDecay.WP.Master$LeakTestDateTime >= as.Date("2017-09-01") & 
                                             dt.AirDecay.WP.Master$LeakTestDateTime < as.Date("2017-09-20") , ]

qic(y= dt.period2.Master$air_decay_wp, x=dt.period2.Master$LeakTestDateTime, chart = 'xbar', standardised=TRUE)
## can qic generate control limit automatically?

