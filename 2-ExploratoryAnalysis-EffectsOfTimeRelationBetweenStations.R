rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych")

Install_And_Load(Required_Packages)

File.LeakTestStation <- c("DataSource/QUK2SH_WJ_Leak_Rate.tsv")
File.Inspection <- c("DataSource/Barcode_Reporting.tsv")



## Load data from Pinning Station processed in 0-ExtractCleanData-LeakTestStation.R
dt.Pinning.Full <- readRDS("DataOutput/dt.Pinning.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.Pinning.1stRecord <- KeepOldestRecord.PinningStation(dt.Pinning.Full)



## Load data from FIPg Station processed in 0-ExtractCleanData-LeakTestStation.R
dt.FIPG.Full <- readRDS("DataOutput/dt.FIPG.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.FIPG.1stRecord <- KeepOldestRecord.FIPGStation(dt.FIPG.Full)
