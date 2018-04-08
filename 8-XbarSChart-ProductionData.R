## This code is to calculate the X-bar Sigma chart as per SPC recommendation

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "7-PrepareData-XbarSChart.R"
## duplicaes will be included becasue we want to observe all process variation
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")

# Load the control chart constants
dt.ControlChartConstants <- read.table("ControlChartsConstants.tsv", sep = '\t', header = TRUE, stringsAsFactors = FALSE)

## Rename the date/time & targeted data of dataset as per function requirement
dt.AirDecay.WP.NoMaster.XSChart <- dt.AirDecay.WP.NoMaster.TBM
dt.AirDecay.WP.NoMaster.XSChart$UseTime <- dt.AirDecay.WP.NoMaster.XSChart$LeakTestDateTime
dt.AirDecay.WP.NoMaster.XSChart$UseData <- dt.AirDecay.WP.NoMaster.XSChart$air_decay_wp

# ## Process contorl limit row by row
# dt.AirDecay.WP.NoMaster.MonthlyXSChart <- Process.Monthly.Xbar_SChart(dt.AirDecay.WP.NoMaster.TBM)









