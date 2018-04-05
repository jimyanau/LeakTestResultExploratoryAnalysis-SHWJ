## Here is to establish the control limit of OK Master part
## OK Master ID is "XBA1601290101A23"

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS")

Install_And_Load(Required_Packages)


## Load data processed in 0-ExtractCleanData-LeakTestStation.R
dt.AirDecay.WP.Master <- readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")
dt.AirDecay.MC.Master <- readRDS("DataOutput/dt.AirDecay.MC.Master.RDS")
dt.AirDecay.He.Master <- readRDS("DataOutput/dt.AirDecay.He.Master.RDS")


### Establish control limit for Air Decay WP Test
# Subset OK Master data
dt.AirDecay.WP.OKMaster <- dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id=="XBA1601290101A23" & Result=="PASS", ]

# Random pick up samples
set.seed(777)
dt.AirDecay.WP.Master.Sample <- dt.AirDecay.WP.OKMaster[sample(nrow(dt.AirDecay.WP.OKMaster),60),]

# Assess dataset
qqnorm(dt.AirDecay.WP.Master.Sample$air_decay_wp)
qqline(dt.AirDecay.WP.Master.Sample$air_decay_wp, col = "red")

g.Master.WP <- ggplot(dt.AirDecay.WP.Master.Sample, aes(x = dt.AirDecay.WP.Master.Sample$air_decay_wp)) +
                      geom_histogram(binwidth=0.1, alpha = 0.9, position = "dodge") +
                      scale_x_continuous(limits = c(-3, 3)) +
                      xlab("Leak Rate") +
                      ylab("Counts") +
                      ggtitle(paste("QUK2 SH WJ Air Decay WP Leak Rate Distribution - OK Master" )) +
                      theme(text = element_text(size=10))

## Setup control limit for Air Decay - WP
Mean.Ave.LeakRate.Air.OKMaster.WP <- mean(dt.AirDecay.WP.Master.Sample$air_decay_wp)
SD.Ave.LeakRate.Air.OKMaster.WP <- sd(dt.AirDecay.WP.Master.Sample$air_decay_wp)

##??? Need to calculate the control limit of Stdev of Stdev of Leak Rate???


## Plot Control Chart of Master over a period of time
Plot.SinglePoint.WP.FixedLimit.FixedPeriod(dt.AirDecay.WP.OKMaster, Mean.Ave.LeakRate.Air.OKMaster.WP, SD.Ave.LeakRate.Air.OKMaster.WP, 
                                           "SH WJ Air Decay Leak Rate - OK Master", as.Date("2018-01-10", tz = "Australia/Melbourne"),
                                           as.Date("2018-01-30", tz = "Australia/Melbourne"))











