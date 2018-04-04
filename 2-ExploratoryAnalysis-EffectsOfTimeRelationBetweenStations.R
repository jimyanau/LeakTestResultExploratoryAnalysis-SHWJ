rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych")

Install_And_Load(Required_Packages)

## Load data from Pinning Station processed in 0-ExtractCleanData-LeakTestStation.R
## Data included duplicates
dt.Pinning.Full <- readRDS("DataOutput/dt.Pinning.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.Pinning.1stRecord <- KeepOldestRecord.PinningStation(dt.Pinning.Full)
rm(dt.Pinning.Full)



## Load data from FIPg Station processed in 0-ExtractCleanData-LeakTestStation.R
## Data included duplicates
dt.FIPG.Full <- readRDS("DataOutput/dt.FIPG.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.FIPG.1stRecord <- KeepOldestRecord.FIPGStation(dt.FIPG.Full)
rm(dt.FIPG.Full)

## Load data of Air Decay Stations
## Data included duplicates
dt.AirDecay.WP.NoMaster <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
dt.AirDecay.MC.NoMaster <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.RDS")
dt.AirDecay.He.NoMaster <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.RDS")

## Remove duplicates by keeping only the oldest record of the same part id
dt.AirDecay.WP.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.WP.NoMaster)
dt.AirDecay.MC.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.MC.NoMaster)
dt.AirDecay.He.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.He.NoMaster)
rm(dt.AirDecay.WP.NoMaster)
rm(dt.AirDecay.MC.NoMaster)
rm(dt.AirDecay.He.NoMaster)

## Load Inspection Data
dt.Inspection.Full <- readRDS("DataOutput/dt.Inspection.Full.RDS")
dt.Inspection.Gate1 <- dt.Inspection.Full[dt.Inspection.Full$gate=="100%",]
dt.Inspection.Gate2 <- dt.Inspection.Full[dt.Inspection.Full$gate=="200%",]
dt.Inspection.Gate3 <- dt.Inspection.Full[dt.Inspection.Full$gate=="300%",]
rm(dt.Inspection.Full)

## Keep only the oldest data of the same part ID at each gate
dt.Inspection.Gate1.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate1)


## Assemble Date/Time of different Station as per individual part id

dt.CompleteProcessTiming <- NULL
dt.CompleteProcessTiming <- merge(dt.Pinning, dt.FIPG, by.x = "part_id", by.y = "part_id", all.x = TRUE)