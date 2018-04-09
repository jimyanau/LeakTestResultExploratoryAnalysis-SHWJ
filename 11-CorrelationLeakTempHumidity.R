## This code is to investigate the correlation between leak rate and temp/humidity

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts", "ggpubr")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")

# Load dataset processed at "7-PrepareData-XbarSChart.R"
## duplicaes will be included becasue we want to observe all process variation
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")

# Load temp/humidity data
dt.TempHumidity <- readRDS("DataOutput/dt.TempHumidity.RDS")


## Subset 3 days from 2018
dt.AirDecay.WP.NoMaster.Sample <- dt.AirDecay.WP.NoMaster.TBM[dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime>= as.Date("2018-03-05") & dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime<= as.Date("2018-03-16"), ]
dt.AirDecay.MC.NoMaster.Sample <- dt.AirDecay.MC.NoMaster.TBM[dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime>= as.Date("2018-03-05") & dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime<= as.Date("2018-03-16"), ]
dt.AirDecay.He.NoMaster.Sample <- dt.AirDecay.He.NoMaster.TBM[dt.AirDecay.He.NoMaster.TBM$LeakTestDateTime>= as.Date("2018-03-05") & dt.AirDecay.He.NoMaster.TBM$LeakTestDateTime<= as.Date("2018-03-16"), ]

#####################################################################################################
## Do summary of leak rate - 30 mins
dt.30minStat.AirDecay.WP.NoMaster.Sample <- dt.AirDecay.WP.NoMaster.Sample %>%
                                              group_by(Time=floor_date(LeakTestDateTime, "30 mins")) %>%
                                              summarize(Qty = n(),
                                                        RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                                        Avg.LeakRate = mean(air_decay_wp), 
                                                        Stdev.LeakRate = sd(air_decay_wp),
                                                        Max.LeakRate = max(air_decay_wp), 
                                                        Min.LeakRate = min(air_decay_wp),
                                                        Range.LeakRate = (Max.LeakRate - Min.LeakRate))

dt.30minStat.AirDecay.MC.NoMaster.Sample <- dt.AirDecay.MC.NoMaster.Sample %>%
                                                        group_by(Time=floor_date(LeakTestDateTime, "30 mins")) %>%
                                                        summarize(Qty = n(),
                                                                  RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                                                  Avg.LeakRate = mean(air_decay_mc), 
                                                                  Stdev.LeakRate = sd(air_decay_mc),
                                                                  Max.LeakRate = max(air_decay_mc), 
                                                                  Min.LeakRate = min(air_decay_mc),
                                                                  Range.LeakRate = (Max.LeakRate - Min.LeakRate))

dt.30minStat.AirDecay.He.ppm.NoMaster.Sample <- dt.AirDecay.He.NoMaster.Sample %>%
                                                          group_by(Time=floor_date(LeakTestDateTime, "30 mins")) %>%
                                                          summarize(Qty = n(),
                                                                    RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                                                    Avg.LeakRate = mean(helium_test*1000000), 
                                                                    Stdev.LeakRate = sd(helium_test*1000000),
                                                                    Max.LeakRate = max(helium_test*1000000), 
                                                                    Min.LeakRate = min(helium_test*1000000),
                                                                    Range.LeakRate = (Max.LeakRate - Min.LeakRate))




dt.30min.Stat.TempHumidity <- dt.TempHumidity %>%
                                    group_by(Time=floor_date(Date.Time, "30 mins")) %>%
                                    summarize(Qty = n(),
                                              Ave.Temp = mean(Temperature),
                                              Ave.Humidity = mean(RelativeHumidity))

## merge datasets together
dt.30min.Stat.Combined.WP <- merge(dt.30minStat.AirDecay.WP.NoMaster.Sample, dt.30min.Stat.TempHumidity, 
                                     by.x = "Time", by.y = "Time", all.x = TRUE)
dt.30min.Stat.Combined.MC <- merge(dt.30minStat.AirDecay.MC.NoMaster.Sample, dt.30min.Stat.TempHumidity, 
                                   by.x = "Time", by.y = "Time", all.x = TRUE)
dt.30min.Stat.Combined.He <- merge(dt.30minStat.AirDecay.He.ppm.NoMaster.Sample, dt.30min.Stat.TempHumidity, 
                                   by.x = "Time", by.y = "Time", all.x = TRUE)


# Remove NAs
dt.30min.Stat.Combined.WP <- dt.30min.Stat.Combined.WP[complete.cases(dt.30min.Stat.Combined.WP),]
dt.30min.Stat.Combined.MC <- dt.30min.Stat.Combined.MC[complete.cases(dt.30min.Stat.Combined.MC),]
dt.30min.Stat.Combined.He <- dt.30min.Stat.Combined.He[complete.cases(dt.30min.Stat.Combined.He),]

# Clean outliners
dt.30min.Stat.Combined.WP <- dt.30min.Stat.Combined.WP[dt.30min.Stat.Combined.WP$Avg.LeakRate<3,]
dt.30min.Stat.Combined.MC <- dt.30min.Stat.Combined.MC[dt.30min.Stat.Combined.MC$Avg.LeakRate<8,]
dt.30min.Stat.Combined.He <- dt.30min.Stat.Combined.He[dt.30min.Stat.Combined.He$Avg.LeakRate<10,]

# Do correlation
ggscatter(dt.30min.Stat.Combined.WP, x = "Ave.Temp", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Air Decay WP")

ggscatter(dt.30min.Stat.Combined.WP, x = "Ave.Humidity", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Air Decay WP")

ggscatter(dt.30min.Stat.Combined.MC, x = "Ave.Temp", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Air Decay MC")

ggscatter(dt.30min.Stat.Combined.MC, x = "Ave.Humidity", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Air Decay MC")

ggscatter(dt.30min.Stat.Combined.He, x = "Ave.Temp", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Helium Test")

ggscatter(dt.30min.Stat.Combined.He, x = "Ave.Humidity", y = "Avg.LeakRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Helium Test")


#########################################################################################################
#####################################################################################################
## Do summary of leak rate - 1 hour
dt.HourlyStat.AirDecay.WP.NoMaster.Sample <- dt.AirDecay.WP.NoMaster.Sample %>%
                                    group_by(Time=floor_date(LeakTestDateTime, "1 hour")) %>%
                                    summarize(Qty = n(),
                                              RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                              Avg.LeakRate = mean(air_decay_wp), 
                                              Stdev.LeakRate = sd(air_decay_wp),
                                              Max.LeakRate = max(air_decay_wp), 
                                              Min.LeakRate = min(air_decay_wp),
                                              Range.LeakRate = (Max.LeakRate - Min.LeakRate))

dt.HourlyStat.AirDecay.MC.NoMaster.Sample <- dt.AirDecay.MC.NoMaster.Sample %>%
                                          group_by(Time=floor_date(LeakTestDateTime, "1 hour")) %>%
                                          summarize(Qty = n(),
                                                    RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                                    Avg.LeakRate = mean(air_decay_mc), 
                                                    Stdev.LeakRate = sd(air_decay_mc),
                                                    Max.LeakRate = max(air_decay_mc), 
                                                    Min.LeakRate = min(air_decay_mc),
                                                    Range.LeakRate = (Max.LeakRate - Min.LeakRate))

dt.HourlyStat.AirDecay.He.ppm.NoMaster.Sample <- dt.AirDecay.He.NoMaster.Sample %>%
                                            group_by(Time=floor_date(LeakTestDateTime, "1 hour")) %>%
                                            summarize(Qty = n(),
                                                      RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                                      Avg.LeakRate = mean(helium_test*1000000), 
                                                      Stdev.LeakRate = sd(helium_test*1000000),
                                                      Max.LeakRate = max(helium_test*1000000), 
                                                      Min.LeakRate = min(helium_test*1000000),
                                                      Range.LeakRate = (Max.LeakRate - Min.LeakRate))




dt.HourlyStat.TempHumidity <- dt.TempHumidity %>%
                              group_by(Time=floor_date(Date.Time, "1 hour")) %>%
                              summarize(Qty = n(),
                                        Ave.Temp = mean(Temperature),
                                        Ave.Humidity = mean(RelativeHumidity))

## merge datasets together
dt.HourlyStat.Combined.WP <- merge(dt.HourlyStat.AirDecay.WP.NoMaster.Sample, dt.HourlyStat.TempHumidity, 
                                   by.x = "Time", by.y = "Time", all.x = TRUE)
dt.HourlyStat.Combined.MC <- merge(dt.HourlyStat.AirDecay.MC.NoMaster.Sample, dt.HourlyStat.TempHumidity, 
                                   by.x = "Time", by.y = "Time", all.x = TRUE)
dt.HourlyStat.Combined.He <- merge(dt.HourlyStat.AirDecay.He.ppm.NoMaster.Sample, dt.HourlyStat.TempHumidity, 
                                   by.x = "Time", by.y = "Time", all.x = TRUE)


# Remove NAs
dt.HourlyStat.Combined.WP <- dt.HourlyStat.Combined.WP[complete.cases(dt.HourlyStat.Combined.WP),]
dt.HourlyStat.Combined.MC <- dt.HourlyStat.Combined.MC[complete.cases(dt.HourlyStat.Combined.MC),]
dt.HourlyStat.Combined.He <- dt.HourlyStat.Combined.He[complete.cases(dt.HourlyStat.Combined.He),]

# Clean outliners
dt.30min.Stat.Combined.WP <- dt.30min.Stat.Combined.WP[dt.30min.Stat.Combined.WP$Avg.LeakRate<3,]
dt.HourlyStat.Combined.MC <- dt.HourlyStat.Combined.MC[dt.HourlyStat.Combined.MC$Avg.LeakRate<8,]
dt.HourlyStat.Combined.He <- dt.HourlyStat.Combined.He[dt.HourlyStat.Combined.He$Avg.LeakRate<10,]

# Do correlation
g.Hourly.Cor.WP.Temp <- ggscatter(dt.HourlyStat.Combined.WP, x = "Ave.Temp", y = "Avg.LeakRate", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",
                              title = "Correlation Leak Rate Air Decay WP vs Temp (Hourly Ave. Data)",
                              xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Air Decay WP")

g.Hourly.Cor.WP.Humidity <- ggscatter(dt.HourlyStat.Combined.WP, x = "Ave.Humidity", y = "Avg.LeakRate", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                title = "Correlation Leak Rate Air Decay WP vs Humidity (Hourly Ave. Data)",
                                xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Air Decay WP")

multiplot(g.Hourly.Cor.WP.Temp, g.Hourly.Cor.WP.Humidity, cols=1)

g.Hourly.Cor.MC.Temp <- ggscatter(dt.HourlyStat.Combined.MC, x = "Ave.Temp", y = "Avg.LeakRate", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                title = "Correlation Leak Rate Air Decay MC vs Temp (Hourly Ave. Data)",
                                xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Air Decay MC")

g.Hourly.Cor.MC.Humidity <- ggscatter(dt.HourlyStat.Combined.MC, x = "Ave.Humidity", y = "Avg.LeakRate", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                title = "Correlation Leak Rate Air Decay MC vs Humidity (Hourly Ave. Data)",
                                xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Air Decay MC")

multiplot(g.Hourly.Cor.MC.Temp, g.Hourly.Cor.MC.Humidity, cols=1)


g.Hourly.Cor.He.Temp <- ggscatter(dt.HourlyStat.Combined.He, x = "Ave.Temp", y = "Avg.LeakRate", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                title = "Correlation Leak Rate Helium vs Temp (Hourly Ave. Data)",
                                xlab = "Ave. Temp", ylab = "Ave. Leak Rate - Helium Test")

g.Hourly.Cor.He.Humidity <- ggscatter(dt.HourlyStat.Combined.He, x = "Ave.Humidity", y = "Avg.LeakRate", 
                                add = "reg.line", conf.int = TRUE, 
                                cor.coef = TRUE, cor.method = "pearson",
                                title = "Correlation Leak Rate Helium vs Humidity (Hourly Ave. Data)",
                                xlab = "Ave.Humidity", ylab = "Ave. Leak Rate - Helium Test")

multiplot(g.Hourly.Cor.He.Temp, g.Hourly.Cor.He.Humidity, cols=1)
