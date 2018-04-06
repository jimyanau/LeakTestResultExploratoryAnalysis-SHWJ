## Here is to establish the control limit of OK Master part by using X-bar Range Chart
## OK Master ID is "XBA1601290101A23"

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


## Load data processed in 0-ExtractCleanData-LeakTestStation.R
dt.AirDecay.WP.Master <- readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")
dt.AirDecay.MC.Master <- readRDS("DataOutput/dt.AirDecay.MC.Master.RDS")
dt.AirDecay.He.Master <- readRDS("DataOutput/dt.AirDecay.He.Master.RDS")





############################# Establish control limit for Air Decay WP Test  ###########################
# Subset OK Master data
dt.AirDecay.WP.OKMaster <- dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id=="XBA1601290101A23" & Result=="PASS", ]

# Random pick up samples
## Qty. >=30 is ok. 30 ~ 200???
set.seed(777)
dt.AirDecay.WP.Master.Sample <- dt.AirDecay.WP.OKMaster[sample(nrow(dt.AirDecay.WP.OKMaster),60),]

# Assess dataset. The OK master data was closed to normal distribution. It means the leak tester result itself was a normal distribution data.
qqnorm(dt.AirDecay.WP.Master.Sample$air_decay_wp)
qqline(dt.AirDecay.WP.Master.Sample$air_decay_wp, col = "red")

# g.Master.WP <- ggplot(dt.AirDecay.WP.Master.Sample, aes(x = dt.AirDecay.WP.Master.Sample$air_decay_wp)) +
#                       geom_histogram(binwidth=0.1, alpha = 0.9, position = "dodge") +
#                       scale_x_continuous(limits = c(-3, 3)) +
#                       xlab("Leak Rate") +
#                       ylab("Counts") +
#                       ggtitle(paste("QUK2 SH WJ Air Decay WP Leak Rate Distribution - OK Master" )) +
#                       theme(text = element_text(size=10))

## Check Master Part Data Distribution
hist(dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id=="XBA1601290101A23" &
                             dt.AirDecay.WP.Master$air_decay_wp < max(dt.AirDecay.WP.Master$air_decay_wp), ]$air_decay_wp, breaks=50,col=2)

## Get the daily statics of OK Master Part for X-bar R Chart. Rejected records were dropped
dt.DailyStat.AirDecay.WP.Master <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.OKMaster, -3, 2.1)


## Setup control limit for Air Decay - WP
Mean.Ave.LeakRate.Air.OKMaster.WP <- mean(dt.AirDecay.WP.Master.Sample$air_decay_wp)
SD.Ave.LeakRate.Air.OKMaster.WP <- sd(dt.AirDecay.WP.Master.Sample$air_decay_wp)


## For Example, plot Control Chart of OK Master over a period of time
Plot.SinglePoint.WP.FixedLimit.FixedPeriod(dt.AirDecay.WP.OKMaster, Mean.Ave.LeakRate.Air.OKMaster.WP, SD.Ave.LeakRate.Air.OKMaster.WP, 
                                           "SH WJ Air Decay Leak Rate - WP (OK Master)", as.Date("2018-01-10", tz = "Australia/Melbourne"),
                                           as.Date("2018-01-30", tz = "Australia/Melbourne"))

###########################################################################################################################
###########################################################################################################################


############################# Establish control limit for Air Decay MC Test  ###########################
# Subset OK Master data
dt.AirDecay.MC.OKMaster <- dt.AirDecay.MC.Master[dt.AirDecay.MC.Master$part_id=="XBA1601290101A23" & Result=="PASS", ]

# Random pick up samples
set.seed(777)
dt.AirDecay.MC.Master.Sample <- dt.AirDecay.MC.OKMaster[sample(nrow(dt.AirDecay.MC.OKMaster),60),]

# Assess dataset
qqnorm(dt.AirDecay.MC.Master.Sample$air_decay_mc)
qqline(dt.AirDecay.MC.Master.Sample$air_decay_mc, col = "red")

g.Master.MC <- ggplot(dt.AirDecay.MC.Master.Sample, aes(x = dt.AirDecay.MC.Master.Sample$air_decay_mc)) +
                geom_histogram(binwidth=0.1, alpha = 0.9, position = "dodge") +
                scale_x_continuous(limits = c(-3, 3)) +
                xlab("Leak Rate") +
                ylab("Counts") +
                ggtitle(paste("QUK2 SH WJ Air Decay MC Leak Rate Distribution - OK Master" )) +
                theme(text = element_text(size=10))

## Setup control limit for Air Decay - WP
Mean.Ave.LeakRate.Air.OKMaster.MC <- mean(dt.AirDecay.MC.Master.Sample$air_decay_mc)
SD.Ave.LeakRate.Air.OKMaster.MC <- sd(dt.AirDecay.MC.Master.Sample$air_decay_mc)

##??? Need to calculate the control limit of Stdev of Stdev of Leak Rate???

## Plot Control Chart of OK Master over a period of time
Plot.SinglePoint.MC.FixedLimit.FixedPeriod(dt.AirDecay.MC.OKMaster, Mean.Ave.LeakRate.Air.OKMaster.MC, SD.Ave.LeakRate.Air.OKMaster.MC, 
                                           "SH WJ Air Decay Leak Rate - MC (OK Master)", as.Date("2018-01-10", tz = "Australia/Melbourne"),
                                           as.Date("2018-01-30", tz = "Australia/Melbourne"))

###########################################################################################################################
###########################################################################################################################




############################# Establish control limit for Helium Leak Test  ###########################
# Subset OK Master data
dt.AirDecay.He.OKMaster <- dt.AirDecay.He.Master[dt.AirDecay.He.Master$part_id=="XBA1601290101A23" & Result=="PASS", ]

# Random pick up samples
set.seed(777)
dt.AirDecay.He.Master.Sample <- dt.AirDecay.He.OKMaster[sample(nrow(dt.AirDecay.He.OKMaster),60),]

# Assess dataset
qqnorm(dt.AirDecay.He.Master.Sample$helium_test)
qqline(dt.AirDecay.He.Master.Sample$helium_test, col = "red")

g.Master.He <- ggplot(dt.AirDecay.He.Master.Sample, aes(x = dt.AirDecay.He.Master.Sample$helium_test)) +
                  geom_histogram(binwidth=0.1, alpha = 0.9, position = "dodge") +
                  scale_x_continuous(limits = c(-3, 3)) +
                  xlab("Leak Rate") +
                  ylab("Counts") +
                  ggtitle(paste("QUK2 SH WJ Helium Leak Rate Distribution - OK Master" )) +
                  theme(text = element_text(size=10))

## Setup control limit for Air Decay - He

## Estimate control limit by using quantile of 1,2,3 sigma of distribution. The He leak rate looks like Exp. Distributed.
qmean=mean(dt.AirDecay.He.Master.Sample$helium_test)

## This is based on distribution of theory. Ue it in small sample size
qexp(c(0.68,0.95,0.99),rate=1/qmean)


## This is based on actual dataset
quantile(dt.AirDecay.He.Master.Sample$helium_test,c(0.68,0.95,0.99))


Mean.Ave.LeakRate.Air.OKMaster.He <- mean(dt.AirDecay.He.Master.Sample$helium_test)
SD.Ave.LeakRate.Air.OKMaster.He <- sd(dt.AirDecay.He.Master.Sample$helium_test)




##??? Need to calculate the control limit of Stdev of Stdev of Leak Rate???

## Plot Control Chart of OK Master over a period of time
Plot.SinglePoint.He.FixedLimit.FixedPeriod(dt.AirDecay.He.OKMaster, Mean.Ave.LeakRate.Air.OKMaster.He, SD.Ave.LeakRate.Air.OKMaster.He, 
                                           "SH WJ Helium Leak Rate (OK Master)", as.Date("2018-01-10", tz = "Australia/Melbourne"),
                                           as.Date("2018-01-30", tz = "Australia/Melbourne"))


## helium_test > min(helium_test)  is to remove the min value of He test from dataset. Becsue there were so many values at the min level due to min. tester sensitivity level
hist(dt.AirDecay.He.Master[dt.AirDecay.He.Master$part_id=="XBA1601290101A23" & dt.AirDecay.He.Master$helium_test>min(dt.AirDecay.He.Master$helium_test), ]$helium_test,breaks=20,col=2)
###########################################################################################################################
###########################################################################################################################


