## This code is to investigate the data of time between failures in Jan-18

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts", "ggpubr")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "7-PrepareData-XbarSChart.R"
## duplicaes will be included becasue we want to observe all process variation
# dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
# dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")


## Rename the date/time & targeted data of dataset as per function requirement
dt.AirDecay.MC.NoMaster.TBM$UseTime <- dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime
dt.AirDecay.MC.NoMaster.TBM$UseData <- dt.AirDecay.MC.NoMaster.TBM$air_decay_mc

dt.DailyStatics.AirMC <- Daily.Statics.General(dt.AirDecay.MC.NoMaster.TBM)

# Plot line chart for the whole year
Plot.LineChart.Date(dt.DailyStatics.AirMC,"Period", "RejectPercent", 0, 0, 0, mean(dt.DailyStatics.AirMC$RejectPercent), 
                    sd(dt.DailyStatics.AirMC$RejectPercent), "Air Decay MC Reject Rate run Chart")

###############################################################################################
# subset one period of time to observe
dt.DailyStatics.period1 <- dt.DailyStatics.AirMC[dt.DailyStatics.AirMC$Period >= as.Date("2018-03-01") & dt.DailyStatics.AirMC$Period <= as.Date("2018-03-20") , ]
Plot.LineChart.Date(dt.DailyStatics.period1,"Period", "RejectPercent", 0, 0, 0, mean(dt.DailyStatics.AirMC$RejectPercent), 
                    sd(dt.DailyStatics.AirMC$RejectPercent), "Air Decay MC Reject Rate run Chart")
## Found high reject rate on 8/MAR/2018

#############################################################################################
# subset individual data from 8/Mar/18 for investigation
dt.period2 <- dt.AirDecay.MC.NoMaster.TBM[dt.AirDecay.MC.NoMaster.TBM$UseTime >= as.Date("2018-03-08") & dt.AirDecay.MC.NoMaster.TBM$UseTime < as.Date("2018-03-09") , ]
Plot.SinglePoint.MC.ControlChart(dt.period2, 0, -6, 6, "Air Decay MC Leak Rate run Chart (08/Mar/2018")

## Remove row with TBF between casting was 0. They are retest parts
# dt.period2 <- dt.period2[ dt.period2$TBF_CastTime_Min != 0, ]

# Plot histogram of time between failures in testing time
g.TBF.Test <- ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
              aes(x=TBF_TestTime_Min)) +
              geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
              # geom_density() +
              # geom_text(aes(label=dt.period2$TBF_TestTime_Min,vjust=1.5)) +
              scale_x_continuous(breaks=c(0,10,20,30,40,50,100,150),limits = c(0, 150)) +
              xlab("Minutes btw Failures - Testing Time") +
              ylab("Counts") +
              ggtitle(paste("QUK2 SH Leak Test Qty. Between Failures - Air Decay MC Failures (08/Mar/2018)")) +
              theme(text = element_text(size=10),legend.position="bottom")

# Plot histogram of parts count  between failures in testing time
g.CBF.Test <- ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
               aes(x=CBF_TestTime)) +
                geom_histogram(binwidth=2, alpha=.5, position="identity", colour='red', fill = 'red') +
                # geom_density() +
                scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,30,40,50),limits = c(0, 50)) +
                xlab("Part Qty. btw Failures - Testing Time") +
                ylab("Counts") +
                ggtitle(paste("QUK2 SH Leak Test - Test Qty. Between Failures - Air Decay MC Failures (08/Mar/2018)")) +
                theme(text = element_text(size=10),legend.position="bottom")


# Plot histogram of time between failures in cast time
g.TBF.Cast <- ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
             aes(x=TBF_CastTime_Min)) +
              geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
              # geom_density() +
              scale_x_continuous(breaks=c(0,10,20,30,40,50,100,150),limits = c(0, 150)) +
              xlab("Minutes btw Failures - Casting Time") +
              ylab("Counts") +
              ggtitle(paste("QUK2 SH Leak Test Cast Time Between Failures - Air Decay MC Failures (08/Mar/2018)")) +
              theme(text = element_text(size=10),legend.position="bottom")

# Plot histogram of parts count  between failures in Castinging time
g.CBF.Cast <- ggplot(dt.period2[dt.period2$Result == "FAIL", ], 
               aes(x=CBF_CastTime)) +
              geom_histogram(binwidth=2, alpha=.5, position="identity", colour='red', fill = 'red') +
              # geom_density() +
              scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16,18,20,30,40,50),limits = c(0, 50)) +
              xlab("Part Qty. btw Failures - Casting Time") +
              ylab("Counts") +
              ggtitle(paste("QUK2 SH Leak Test - Cast Qty. Between Failures - Air Decay MC Failures (08/Mar/2018)")) +
              theme(text = element_text(size=10),legend.position="bottom")

multiplot(g.TBF.Test, g.CBF.Test, g.TBF.Cast, g.CBF.Cast, cols=2)

# Histogram combined TBF of Testing Time & TBF of Casting Time
ggplot(dt.period2[dt.period2$Result == "FAIL", ] ) +
        geom_histogram(aes(x=TBF_TestTime_Min, fill="TBF-Test Time"), binwidth=5, alpha=.5, position="identity", colour='red') +
        geom_histogram(aes(x=TBF_CastTime_Min, fill="TBF-Cast Time"), binwidth=5, alpha=.5, position="identity", colour='red') +
        # geom_density() +
        scale_x_continuous(limits = c(0, 150)) +
        xlab("Minutess btw Failures") +
        ylab("Counts") +
        ggtitle(paste("QUK2 SH WJ Test Time Between Failures - Air Decay MC Failures (08/Mar/2018)")) +
        theme(text = element_text(size=10),legend.position="bottom")






# control chart of Ok Master of the same period
dt.AirDecay.MC.Master <- readRDS("DataOutput/dt.AirDecay.MC.Master.RDS")
dt.period2.Master <- dt.AirDecay.MC.Master[dt.AirDecay.MC.Master$part_id == "XBA1601290101A23" &
                                             dt.AirDecay.MC.Master$LeakTestDateTime >= as.Date("2018-03-01") & 
                                             dt.AirDecay.MC.Master$LeakTestDateTime < as.Date("2018-03-16") , ]

# Plot run cahrt of OK Master
ggplot(dt.period2.Master, aes(x = dt.period2.Master$LeakTestDateTime, y=dt.period2.Master$air_decay_mc)) +
            geom_line()+
            geom_point()+
            geom_hline(aes(yintercept = mean(dt.period2.Master$air_decay_mc), colour="Mean"),  size=1) +
            geom_hline(aes(yintercept = mean(dt.period2.Master$air_decay_mc) + 3*sd(dt.period2.Master$air_decay_mc), colour="UCL"),  size=1) +
            geom_hline(aes(yintercept = mean(dt.period2.Master$air_decay_mc) - 3*sd(dt.period2.Master$air_decay_mc), colour="LCL"),  size=1) +
            xlab("Date/Time") +
            ylab("Leak Rate") +
            ggtitle(paste("QUK2 SH WJ OK Master Leak Rate - Air Decay MC")) +
            scale_x_datetime(date_breaks = "12 hour", labels = date_format("%d/%b %H:00")) +
            theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))

# I-MR Chart of OK Master
qic(y= dt.period2.Master$air_decay_mc, x=dt.period2.Master$LeakTestDateTime, chart = 'i')
## can qic generate control limit automatically?

# Plot comboined chart with multiple information
dt.TempHumidity <- readRDS("DataOutput/dt.TempHumidity.RDS")

Plot.SinglePoint.MC.Type3(dt.AirDecay.MC.NoMaster.TBM, 0, -6, 6, 
                          " WC - 08/MAR/2018", dt.AirDecay.MC.Master, "XBA1601290101A23", 
                          as.Date("2018-03-07"), as.Date("2018-03-09") )

#############################################################################################



