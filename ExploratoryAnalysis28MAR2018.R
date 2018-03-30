rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly")

Install_And_Load(Required_Packages)

File.LeakTestStation <- c("DataSource/QUK2SH_WJ_Leak_Rate.tsv")
File.Inspection <- c("DataSource/Barcode_Reporting.tsv")

Sys.setenv("R_ZIPCMD" = paste0(getwd(),"/zip.exe"))


######################################################################################################################################################################
## Extract raw data from tsv file. 
## All processed data include duplicates and was sorted in order of time/part_id. Row contained NA was removed.
Extract.LeakTestStation.Data(File.LeakTestStation)

dt.AirDecay.WP.Full <- readRDS("DataOutput/dt.AirDecay.WP.Full.RDS")
dt.AirDecay.MC.Full <- readRDS("DataOutput/dt.AirDecay.MC.Full.RDS")
dt.AirDecay.He.Full <- readRDS("DataOutput/dt.AirDecay.He.Full.RDS")

## Process leak test data. 
## Assemble casting date / time based on barcode. Records with incorrect barcode will be dropped.
## Convert some columns into factor
## Master Part Data will be excluded in the dataset
dt.AirDecay.WP.Full <- Process.LeakTest.Data(dt.AirDecay.WP.Full)
dt.AirDecay.MC.Full <- Process.LeakTest.Data(dt.AirDecay.MC.Full)
dt.AirDecay.He.Full <- Process.LeakTest.Data(dt.AirDecay.He.Full)

## Remove duplicates, calculate leak test result based on supplied spec.
## Run statics on data sets
dt.Daily.Statics.AirDecay.WP <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.Full, -3, 2.1 )
dt.Daily.Statics.AirDecay.MC <- Daily.Statics.AirDecay.MC(dt.AirDecay.MC.Full, -6, 6 )
dt.Daily.Statics.AirDecay.He <- Daily.Statics.AirDecay.He(dt.AirDecay.He.Full, 0, 3.0E-6 )



######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
## Exploratory Analysis Starts of Water Passage Air Decay Test

## Plot Contorl Chart from Jan2017 till MAR2018
Plot.Daily.Mean.SD.Control.MovingLimit(dt.Daily.Statics.AirDecay.WP, 0, -3, 2.1, "WP Daily Statics")


## Select data from 2018 for investigation
dt.Daily.Statics.AirDecay.WP.2018 <- dt.Daily.Statics.AirDecay.WP[dt.Daily.Statics.AirDecay.WP$Date >= as.Date("2018-01-01"),]

## Within the 2018 dataset, the mean of daily average leak rate was 0.1059684 while sigma was 0.1298548
mean(dt.Daily.Statics.AirDecay.WP.2018$Avg.LeakRate)
sd(dt.Daily.Statics.AirDecay.WP.2018$Avg.LeakRate)

## Plot contorl charts of 2018
Plot.Daily.Mean.SD.Control.MovingLimit(dt.Daily.Statics.AirDecay.WP.2018, 0, -3, 2.1, "WP Daily Statics")

## Now we select some dates according to the 8 rules 

## Start with Average Leak Rate Chart
## Rule1: One Point outside 3 sigma zone - 27/FEB/2018
## Rule2: 9 points on the same side of mean - 12/FEB/2018 to 20/FEB/2018
## Rule3: 6 points in a row steadily increasing or decreasing - 18/FEB/2018 to 23/FEB/2018
## Rule4: 14 points in a row alternating up and down - N/A
## Rule5: 2 out of 3 points in a row beyong outside 2 sigma range - 27/FEB/2018 to 1/MAR/2018
## Rule6: 4 out of 5 points in a row outside 1 sigma range - 24/FEB/2018 to 28/FEB/2018
## Rule7: 15 points in a row within 1 sigma range - N/A (max. case was 14 points within 1 sigma zone)
## Rule8: 8 points in a row on both side of centerline within 2 sigma zone - 4/FEB/2018 to 11/FEB/2018
## Good Period: 10/JAN/2018 to 16/JAN/2018

## Subset data from groups
dt.good <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-01-10") 
                               & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-01-16") ,]

dt.ng.Rule1 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) == as.Date("2018-02-27"),]

dt.ng.Rule2 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-02-12") 
                               & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-02-20") ,]

dt.ng.Rule3 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-02-18") 
                                & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-02-23") ,]

dt.ng.Rule5 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-02-27") 
                               & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-03-01") ,]

dt.ng.Rule6 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-02-24") 
                               & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-02-28") ,]

dt.ng.Rule8 <- dt.AirDecay.WP.Full[date(dt.AirDecay.WP.Full$LeakTestDateTime) >= as.Date("2018-02-04") 
                                   & date(dt.AirDecay.WP.Full$LeakTestDateTime) <= as.Date("2018-02-11") ,]


######################################################################################################################################################################
## Try to identify contorl limits from "good" sample group

dt.Hourly.Statics.AirDecay.WP.good <- Hourly.Statics.AirDecay.WP(dt.good, -3, 2.1)

## Contorl Charts with moving limits of "good" sample group
Plot.Hourly.WP.Mean.SD.Control.MovingLimit(dt.Hourly.Statics.AirDecay.WP.good, 0, -3, 2.1, "WP Hourly Statics")

## Set contorl limits based on the statics of "good" sample group
Mean.Ave.WP <- mean(dt.Hourly.Statics.AirDecay.WP.good$Avg.LeakRate)  #set the mean target of coltrol limit of average leak rate as 0.09256552
SD.Ave.WP <- sd(dt.Hourly.Statics.AirDecay.WP.good$Avg.LeakRate)  #set the sigma target of coltrol limit of average leak rate as 0.1169584
Mean.SD.WP <- mean(dt.Hourly.Statics.AirDecay.WP.good$Stdev.LeakRate)  #set the sigma target of coltrol limit of the sigma of leak rate as 0.1446988
SD.SD.WP <- sd(dt.Hourly.Statics.AirDecay.WP.good$Stdev.LeakRate)  #set the sigma target of coltrol limit of the sigma of leak rate as 0.04539666

Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.good, 0, -3, 2.1, "WP Hourly Statics - Stable Group", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)

## Observe the trend chart of every data points
## Compare the actual situation with data after removing duplicates
Plot.SinglePoint.WP.ControlChart(dt.good, 0, -3, 2.1, "Air Decay WP - Good Group")
dt.good.cleaned <- RemoveDuplicates.KeepLatest(dt.good)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.good.cleaned, 0, -3, 2.1, "Air Decay WP - Good Group")



######################################################################################################################################################################
## Check control charts of abnormal groups with contorl limits established from "Good" samples

## Rule1: One Point outside 3 sigma zone - 27/FEB/2018
dt.Hourly.Statics.AirDecay.WP.Rule1 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule1, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule1, 0, -3, 2.1, "WP Hourly Statics - NG Rule1", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule1, 0, -3, 2.1, "Air Decay WP - Rule1 NG Group")
dt.ng.Rule1.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule1)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule1.cleaned, 0, -3, 2.1, "Air Decay WP - Rule1 NG Group")

## Rule2: 9 points on the same side of mean - 12/FEB/2018 to 20/FEB/2018
dt.Hourly.Statics.AirDecay.WP.Rule2 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule2, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule2, 0, -3, 2.1, "WP Hourly Statics - NG Rule2", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule2, 0, -3, 2.1, "Air Decay WP - Rule2 NG Group")
dt.ng.Rule2.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule2)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule2.cleaned, 0, -3, 2.1, "Air Decay WP - Rule2 NG Group")


## Rule3: 6 points in a row steadily increasing or decreasing - 18/FEB/2018 to 23/FEB/2018
dt.Hourly.Statics.AirDecay.WP.Rule3 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule3, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule3, 0, -3, 2.1, "WP Hourly Statics - NG Rule3", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule3, 0, -3, 2.1, "Air Decay WP - Rule3 NG Group")
dt.ng.Rule3.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule3)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule3.cleaned, 0, -3, 2.1, "Air Decay WP - Rule3 NG Group")
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule3.cleaned, 0, -3, 2.1, "Air Decay WP - Rule3 NG Group")

## Rule5: 2 out of 3 points in a row beyong outside 2 sigma range - 27/FEB/2018 to 1/MAR/2018
dt.Hourly.Statics.AirDecay.WP.Rule5 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule5, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule5, 0, -3, 2.1, "WP Hourly Statics - NG Rule5", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule5, 0, -3, 2.1, "Air Decay WP - Rule5 NG Group")
dt.ng.Rule5.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule5)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule5.cleaned, 0, -3, 2.1, "Air Decay WP - Rule5 NG Group")

## Rule6: 4 out of 5 points in a row outside 1 sigma range - 24/FEB/2018 to 28/FEB/2018
dt.Hourly.Statics.AirDecay.WP.Rule6 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule6, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule6, 0, -3, 2.1, "WP Hourly Statics - NG Rule6", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule6, 0, -3, 2.1, "Air Decay WP - Rule6 NG Group")
dt.ng.Rule6.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule6)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule6.cleaned, 0, -3, 2.1, "Air Decay WP - Rule6 NG Group")

## Rule8: 8 points in a row on both side of centerline with none in 3 sigma zone - 4/FEB/2018 to 11/FEB/2018
dt.Hourly.Statics.AirDecay.WP.Rule8 <- Hourly.Statics.AirDecay.WP(dt.ng.Rule8, -3, 2.1)
Plot.Hourly.WP.Mean.SD.Control.FixedLimit(dt.Hourly.Statics.AirDecay.WP.Rule8, 0, -3, 2.1, "WP Hourly Statics - NG Rule8", Mean.Ave.WP, SD.Ave.WP, Mean.SD.WP, SD.SD.WP)
Plot.SinglePoint.WP.ControlChart(dt.ng.Rule8, 0, -3, 2.1, "Air Decay WP - Rule8 NG Group")
dt.ng.Rule8.cleaned <- RemoveDuplicates.KeepLatest(dt.ng.Rule8)
Plot.SinglePoint.WP.LeakRate.Dynamic(dt.ng.Rule8.cleaned, 0, -3, 2.1, "Air Decay WP - Rule8 NG Group")
Plot.SinglePoint.WP.Histogram(dt.ng.Rule8.cleaned, 0, -3, 2.1, "Air Decay WP - Rule8 NG Group")


######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################
## Exploratory Analysis Starts of Main Case Air Decay Test

## Plot Contorl Chart from Jan2017 till MAR2018
Plot.Daily.Mean.SD.Control.MovingLimit(dt.Daily.Statics.AirDecay.MC, 0, -6, 6, "MC Daily Statics")

## Select data from 2018 for investigation
dt.Daily.Statics.AirDecay.MC.2018 <- dt.Daily.Statics.AirDecay.MC[dt.Daily.Statics.AirDecay.MC$Date >= as.Date("2018-01-01"),]

## Within the 2018 dataset, the mean of daily average leak rate was 0.7344318 while sigma was 1.055863
mean(dt.Daily.Statics.AirDecay.MC.2018$Avg.LeakRate)
sd(dt.Daily.Statics.AirDecay.MC.2018$Avg.LeakRate)

## Plot contorl charts of 2018
Plot.Daily.Mean.SD.Control.MovingLimit(dt.Daily.Statics.AirDecay.MC.2018, 0, -6, 6, "MC Daily Statics")
