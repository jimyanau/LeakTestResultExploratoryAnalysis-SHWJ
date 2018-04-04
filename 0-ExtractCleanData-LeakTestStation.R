rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly", "mlr")

Install_And_Load(Required_Packages)

#Setup File Names
File.LeakTestStation <- c("DataSource/QUK2SH_WJ_Leak_Rate.tsv")
File.TempHumidity <- c("DataSource/TempRecord.tsv")
File.Inspection <- c("DataSource/GateInspection2017-18.tsv")
File.Pinning <- c("DataSource/QUK2SH_WJ_Pinning.tsv")
File.FIPG <- c("DataSource/QUK2SH_WJ_FIPG_Bolting.tsv")

##################################################################################################################
## Extract raw data from tsv file. 
## Duplicates was not removed
## All processed data include duplicates and was sorted in order of time/part_id. 
## Data from the same file will be subset into 3 different data set with similar structure.
## Within the same table, the Rows contained NA was removed.
## File will be saved into separated RDS files.
Extract.LeakTestStation.Data(File.LeakTestStation)

## Extract data from Temp & Humidity logger
## Duplicates was not removed
## data was saved as "DataOutput/dt.TempHumidity.RDS"
Extract.TempHumidity(File.TempHumidity)

## Extract data from inspection dataset
## data was saved as "DataOutput/dt.Inspection.Full.RDS"
## Duplicates was not removed
Extract.InspectionData(File.Inspection)


## Extract data from Pinning Station
## Duplicates was not removed
## data was saved as "DataOutput/dt.Pinning.RDS"
Extract.PinningStation(File.Pinning)


## Extract data from FIPG Station
## Duplicates was not removed
## data was saved as "DataOutput/dt.FIPG.RDS"
Extract.FIPGStation(File.FIPG)


## Process leak test data. 
## Convert part ID into upper cases
## Assemble casting date / time based on barcode. Records with incorrect barcode format will be dropped.
## Convert some columns into factor
## Calculate leak test result
## subset Master Part Data into separated datasets
dt.AirDecay.WP.Full <- readRDS("DataOutput/dt.AirDecay.WP.Full.RDS")
dt.AirDecay.MC.Full <- readRDS("DataOutput/dt.AirDecay.MC.Full.RDS")
dt.AirDecay.He.Full <- readRDS("DataOutput/dt.AirDecay.He.Full.RDS")

dt.AirDecay.WP.NoMaster <- Process.LeakTest.Data(dt.AirDecay.WP.Full)
dt.AirDecay.MC.NoMaster <- Process.LeakTest.Data(dt.AirDecay.MC.Full)
dt.AirDecay.He.NoMaster <- Process.LeakTest.Data(dt.AirDecay.He.Full)


## Calculate leak test result
dt.AirDecay.WP.NoMaster <- Process.LeakTest.Result.WP(dt.AirDecay.WP.NoMaster)
dt.AirDecay.MC.NoMaster <- Process.LeakTest.Result.MC(dt.AirDecay.MC.NoMaster)
dt.AirDecay.He.NoMaster <- Process.LeakTest.Result.He(dt.AirDecay.He.NoMaster)
  
saveRDS(dt.AirDecay.WP.NoMaster, "DataOutput/dt.AirDecay.WP.NoMaster.RDS")
saveRDS(dt.AirDecay.MC.NoMaster, "DataOutput/dt.AirDecay.MC.NoMaster.RDS")
saveRDS(dt.AirDecay.He.NoMaster, "DataOutput/dt.AirDecay.He.NoMaster.RDS")


## Extract data of master parts for comparison
## Assemble casting date / time based on barcode. Records with incorrect barcode format will be dropped.
## Convert some columns into factor
## Only record with part id in the master part list were kept
dt.AirDecay.WP.Master <- Process.LeakTest.Master.Data(dt.AirDecay.WP.Full)
dt.AirDecay.MC.Master <- Process.LeakTest.Master.Data(dt.AirDecay.MC.Full)
dt.AirDecay.He.Master <- Process.LeakTest.Master.Data(dt.AirDecay.He.Full)

## Calculate leak test result
dt.AirDecay.WP.Master <- Process.LeakTest.Result.WP(dt.AirDecay.WP.Master)
dt.AirDecay.MC.Master <- Process.LeakTest.Result.MC(dt.AirDecay.MC.Master)
dt.AirDecay.He.Master <- Process.LeakTest.Result.He(dt.AirDecay.He.Master)

saveRDS(dt.AirDecay.WP.Master, "DataOutput/dt.AirDecay.WP.Master.RDS")
saveRDS(dt.AirDecay.MC.Master, "DataOutput/dt.AirDecay.MC.Master.RDS")
saveRDS(dt.AirDecay.He.Master, "DataOutput/dt.AirDecay.He.Master.RDS")



