## This code is to investigate the data of time between failures in MAR-18

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts", "ggpubr")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "2-ExploratoryAnalysis-EffectsOfTimeRelationBetweenStations.R"
dt.CompleteProcessTiming.1stRecord <- readRDS("DataOutput/dt.CompleteProcessTiming.1stRecord.RDS")

# Subset Data of 2018 for investigation
dt.CompleteProcessTiming.1stRecord <- dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$"1st_AirDecay_DateTime" >= as.Date("2017-06-01"),]
dt.CompleteProcessTiming.1stRecord <- dt.CompleteProcessTiming.1stRecord[!is.na(dt.CompleteProcessTiming.1stRecord$`1st_LeakRate_He` ), ]

## Investigate distribution
dt.use <- dt.CompleteProcessTiming.1stRecord[ dt.CompleteProcessTiming.1stRecord$`1st_LeakRate_He` < max(dt.CompleteProcessTiming.1stRecord$`1st_LeakRate_He`), ]

# Histogram at original He reading
hist(dt.use$`1st_LeakRate_He`*1000000, main="He Leak Rate Dist.")

# Histogram at He reading - Transformation 1/x
hist(1/(dt.use$`1st_LeakRate_He`*1000000), main="He Leak Rate Dist. - 1/x Transformation")

# Histogram at He reading - Transformation log(x)
hist(log(dt.use$`1st_LeakRate_He`*1000000), main="He Leak Rate Dist. - 1/x Transformation")

# Histogram at He reading - Transformation squrt(x)
hist(sqrt(dt.use$`1st_LeakRate_He`*1000000), main="He Leak Rate Dist. - sqrt(x) Transformation")














