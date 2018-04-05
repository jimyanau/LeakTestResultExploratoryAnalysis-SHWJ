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
dt.Inspection.Gate2.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate2)
dt.Inspection.Gate3.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate3)


## Assemble Date/Time of different Station as per individual part id

dt.CompleteProcessTiming.1stRecord <- NULL
dt.CompleteProcessTiming.1stRecord <- dt.Inspection.Gate1.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code","defect_location")]

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "100%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "100%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "100%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "100%DefectLocation")

# Combine 100% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Pinning.1stRecord[, c( "part_id" , "PinningDateTime")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Add FIPG Station data into dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.FIPG.1stRecord[, c( "part_id" , "FIPGDateTime")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Add Air Decay WP Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.WP.NoMaster.1stRecord[, c( "part_id" , 
                                 "CastMC", "CastDie","CastMC_Die","CastDate","CastDateTime","LeakTestDateTime", "air_decay_wp" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_AirDecay_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "air_decay_wp", "1st_LeakRate_WP")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_LeakTestResult_WP")

# Add Air Decay MC Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.MC.NoMaster.1stRecord[, c( "part_id" , "LeakTestDateTime", "air_decay_mc" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_AirDecay_DateTime_MC")
setnames(dt.CompleteProcessTiming.1stRecord, "air_decay_mc", "1st_LeakRate_MC")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_LeakTestResult_MC")

### Remove row with missing data on Pinning Station Time & FIPG Station Time & Air Decay Test  ###
dt.CompleteProcessTiming.1stRecord <- dt.CompleteProcessTiming.1stRecord[ complete.cases(dt.CompleteProcessTiming.1stRecord), ]


# Add Helium Leak Test Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.He.NoMaster.1stRecord[, c( "part_id" , "LeakTestDateTime", "helium_test" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_Helium_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "helium_test", "1st_LeakRate_He")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_HeTestResult")

# Combine 200% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Inspection.Gate2.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code", "defect_location")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "200%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "200%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "200%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "200%DefectLocation")


# Combine 300% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Inspection.Gate3.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code", "defect_location")], 
                                            by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "300%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "300%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "300%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "300%DefectLocation")


### Pls notice that the data with missing value in 100% Inspection, Pinning Station, FIPG Station & Air Decay Leak 
### Test Station had been removed for investigation purpose. All records were the 1st. record of the same part ID.

# Calculate leak time between stations
dt.CompleteProcessTiming.1stRecord$Mins_IncomingInsp_FIPG <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$FIPGDateTime, dt.CompleteProcessTiming.1stRecord$"100%_Insp_DateTime"), units="mins")
dt.CompleteProcessTiming.1stRecord$Mins_FIPG_AirDecay <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$`1st_AirDecay_DateTime`, dt.CompleteProcessTiming.1stRecord$FIPGDateTime), units="mins")
dt.CompleteProcessTiming.1stRecord$Mins_IncomingInsp_AirDecay <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$`1st_AirDecay_DateTime`, dt.CompleteProcessTiming.1stRecord$"100%_Insp_DateTime"), units="mins")

### Noticed that there was time difference between stations. Working with NCAP to get the difference between machine time and actual time.
### Will add the offset into dataset.





