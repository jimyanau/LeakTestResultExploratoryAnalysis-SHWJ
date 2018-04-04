Install_And_Load <- function(Required_Packages)
{
      Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])]
      
      if(length(Remaining_Packages)) 
      {
        install.packages(Remaining_Packages)
      }
      for(package_name in Required_Packages)
      {
        library(package_name,character.only=TRUE,quietly=TRUE)
      }
}




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
        print(plots[[1]])
        
      } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
          # Get the i,j matrix positions of the regions that contain this subplot
          matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
          
          print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                          layout.pos.col = matchidx$col))
    }
  }
}


Extract.LeakTestStation.Data = function(InputFile) {

        ## Extract raw data from tsv file. 
        ## All processed data include duplicates and was sorted in order of time/part_id. Row contained NA was removed.  
  
        # #test variable
        # InputFile <- "DataSource/QUK2SH_WJ_Leak_Rate.tsv"
        
        #Read TSV file
        dt <- read.table(InputFile, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
        
        #Convert to data table
        dt <- data.table(dt)
        
        #Keep only XBA parts
        dt <- dt[grepl("XBA",dt$part_id, ignore.case=TRUE), ]
        
        #clean spacer at barcode
        dt[ , part_id := as.character(gsub("    ", "", part_id))]
        
        #Assemble Date/Time
        dt$LeakTestDateTime <- ymd_hms(paste0(dt$time_year,"-",dt$time_month,"-", dt$time_day," ", dt$time_hour,":", dt$time_minute,":",dt$time_second))
        
        #subset air decay test data of water passage
        dt.AirDecay.WP.Full <- dt[dt$chamber_id=="1"]
        dt.AirDecay.WP.Full <- dt.AirDecay.WP.Full[,c("part_id","LeakTestDateTime","chamber_id","air_decay_wp","ad_wp_pass_fail","j32u","drying_time_bypass")]
        dt.AirDecay.WP.Full <- dt.AirDecay.WP.Full[order(dt.AirDecay.WP.Full[,2],dt.AirDecay.WP.Full[,1]), ]
        dt.AirDecay.WP.Full <- dt.AirDecay.WP.Full[complete.cases(dt.AirDecay.WP.Full),]
        saveRDS(dt.AirDecay.WP.Full, "DataOutput/dt.AirDecay.WP.Full.RDS")
        
        #subset air decay test data of Main Case
        dt.AirDecay.MC.Full <- dt[dt$chamber_id=="1"]
        dt.AirDecay.MC.Full <- dt.AirDecay.MC.Full[,c("part_id","LeakTestDateTime","chamber_id","air_decay_mc","ad_mc_pass_fail","j32u","drying_time_bypass")]
        dt.AirDecay.MC.Full <- dt.AirDecay.MC.Full[order(dt.AirDecay.MC.Full[,2],dt.AirDecay.MC.Full[,1]), ]
        dt.AirDecay.MC.Full <- dt.AirDecay.MC.Full[complete.cases(dt.AirDecay.MC.Full),]
        saveRDS(dt.AirDecay.MC.Full, "DataOutput/dt.AirDecay.MC.Full.RDS")
        
        #subset Helium test data of Water Passsage
        dt.AirDecay.He.Full <- dt[dt$chamber_id=="2"]
        dt.AirDecay.He.Full <- dt.AirDecay.He.Full[,c("part_id","LeakTestDateTime","chamber_id","helium_test","pass_fail","j32u","drying_time_bypass")]
        dt.AirDecay.He.Full <- dt.AirDecay.He.Full[order(dt.AirDecay.He.Full[,2],dt.AirDecay.He.Full[,1]), ]
        dt.AirDecay.He.Full <- dt.AirDecay.He.Full[complete.cases(dt.AirDecay.He.Full),]
        saveRDS(dt.AirDecay.He.Full, "DataOutput/dt.AirDecay.He.Full.RDS")        


}

Extract.InspectionData = function(InputFile) {
  
  ## Extract raw data from tsv file. 
  ## Duplicates was not removed

  # InputFile <- "DataSource/GateInspection2017-18.tsv"
  
  #Read TSV file
  dt <- read.table(InputFile, sep = '\t', header = TRUE, stringsAsFactors = FALSE, fill=TRUE)
  
  #Convert to data table
  dt <- data.table(dt)
  
  #Keep only XBA parts
  dt <- dt[grepl("XBA",dt$part_id, ignore.case=TRUE), ]
  
  #clean spacer at barcode
  dt[ , part_id := as.character(gsub("    ", "", part_id))]
  
  # Convert dat time into correct format
  dt$datetime <- as.POSIXct(dt$datetime,format="%d/%m/%Y %H:%M")
 
  dt <- dt[order(dt$datetime), ]
 
  saveRDS(dt, "DataOutput/dt.Inspection.Full.RDS")        
  
  
}



Extract.TempHumidity = function(InputFile) {
  
  ## Extract raw data from tsv file. 
  ## All processed data include duplicates and was sorted in order of time/part_id. Row contained NA was removed.  
  
  # #test variable
  # InputFile <- "DataSource/TempRecord.tsv"
  
  #Read TSV file
  dt <- read.table(InputFile, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
  
  #Convert to data table
  dt <- data.table(dt)

  # convert date & time into POSIXct Format
  dt$Date <- as.Date(dt$Date, format = "%d/%m/%Y")
  dt$Date.Time <- as.POSIXct(dt$Date.Time,format="%d/%m/%Y %H:%M")
  
  #sort in oredr of test date
  dt <- dt[order(dt$Date.Time, decreasing = FALSE),]

  saveRDS(dt, "DataOutput/dt.TempHumidity.RDS")        
  
}

Extract.PinningStation = function(InputFile) {
  
  ## Extract raw data from tsv file. 
  ## All processed data include duplicates and was sorted in order of time/part_id. Row contained NA was removed.  
  ## Duplicates was included here
  
  # #test variable
  # InputFile <- c("DataSource/QUK2SH_WJ_Pinning.tsv")
  
  #Read TSV file
  dt <- read.table(InputFile, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
  
  #Convert to data table
  dt <- data.table(dt)
  
  #Keep only XBA parts
  dt <- dt[grepl("XBA",dt$part_id, ignore.case=TRUE), ]
  
  #Assemble Date/Time
  dt$PinningDateTime <- ymd_hms(paste0(dt$time_year,"-",dt$time_month,"-", dt$time_day," ", dt$time_hour,":", dt$time_minute,":",dt$time_second))
  
  #sort in oredr of part id then test date
  dt <- dt[order(dt$PinningDateTime, decreasing = FALSE),]
  
  saveRDS(dt, "DataOutput/dt.Pinning.RDS")        
  
}

KeepOldestRecord.PinningStation = function(dt) {
  
  ## This function is to removed duplicates by keeping only the oldest record of each part id.
  
  # Sort as per order of part id, then time. 
  # Keep only the earliest record of the same part ID
  dt <- dt[order(dt$part_id, dt$PinningDateTime,  decreasing = FALSE),]
  dt <- dt[!duplicated(dt$part_id),]
  
  #sort in oredr of part id then test date
  dt <- dt[order(dt$PinningDateTime, decreasing = FALSE),]
  
  return(dt)
  
}


Extract.FIPGStation = function(InputFile) {
  
  ## Extract raw data from tsv file. 
  ## All processed data include duplicates and was sorted in order of time/part_id. Row contained NA was removed.  
  ## Duplicates was included here
  
  # #test variable
  # InputFile <- c("DataSource/QUK2SH_WJ_FIPG_Bolting.tsv")
  
  #Read TSV file
  dt <- read.table(InputFile, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
  
  #Convert to data table
  dt <- data.table(dt)
  
  #Keep only XBA parts
  dt <- dt[grepl("XBA",dt$part_id, ignore.case=TRUE), ]
  
  #Assemble Date/Time
  dt$FIPGDateTime <- ymd_hms(paste0(dt$time_year,"-",dt$time_month,"-", dt$time_day," ", dt$time_hour,":", dt$time_minute,":",dt$time_second))
  
  #sort in oredr of part id then test date
  dt <- dt[order(dt$FIPGDateTime, decreasing = FALSE),]
  
  saveRDS(dt, "DataOutput/dt.FIPG.RDS")        
  
}


KeepOldestRecord.FIPGStation = function(dt) {
  
  ## This function is to removed duplicates by keeping only the oldest record of each part id.
  
  # # Test Variables
  # dt <- dt.FIPG.Full
  
  # Sort as per order of part id, then time. 
  # Keep only the earliest record of the same part ID
  dt <- dt[order(dt$part_id, dt$FIPGDateTime,  decreasing = FALSE),]
  dt <- dt[!duplicated(dt$part_id),]
  
  #sort in oredr of part id then test date
  dt <- dt[order(dt$FIPGDateTime, decreasing = FALSE),]
  
  return(dt)
  
}


Process.LeakTest.Result.WP = function(dt) {
      ## Add additional column of leak test result baed on spec
  
      lsl = -3
      usl = 2.1
  
      #calculate test result
      dt[dt$air_decay_wp > usl | dt$air_decay_wp < lsl, Result := as.factor("FAIL")]
      dt[dt$air_decay_wp <= usl & dt$air_decay_wp >= lsl, Result := as.factor("PASS")]
  
      #sort in oredr of test date
      dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]

      return(dt) 
}

Process.LeakTest.Result.MC = function(dt) {
  ## Add additional column of leak test result baed on spec
  
  lsl = -6
  usl = 6
  
  #calculate test result
  dt[dt$air_decay_mc > usl | dt$air_decay_mc < lsl, Result := as.factor("FAIL")]
  dt[dt$air_decay_mc <= usl & dt$air_decay_mc >= lsl, Result := as.factor("PASS")]
  
  #sort in oredr of test date
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  return(dt) 
}

Process.LeakTest.Result.He = function(dt) {
  ## Add additional column of leak test result baed on spec
  
  lsl = 0
  usl = 3.0E-6
  
  #calculate test result
  dt[dt$helium_test > usl | dt$helium_test < lsl, Result := as.factor("FAIL")]
  dt[dt$helium_test <= usl & dt$helium_test >= lsl, Result := as.factor("PASS")]
  
  #sort in oredr of test date
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  return(dt) 
}

Process.LeakTest.Data = function(dt) {
        ## Process leak test data. 
        ## Assemble casting date / time based on barcode. Records with incorrect barcode will be dropped.
        ## Convert some columns into factor
  
        # #test variable
        # dt <- dt.AirDecay.WP.Full
        
        #Transfer all barcode into upper case
        dt$part_id <- toupper(dt$part_id)
        
        #Remove data of leak test masters
        MasterList <- readRDS("MasterList.RDS")
        dt <- dt[ !(part_id %in% MasterList), ]
       
  
        #convert some columns to factor
        dt[,1] <- lapply(dt[,1], factor)
        dt[,3] <- lapply(dt[,3], factor)
        dt[,5] <- lapply(dt[,5], factor)
        dt[,6] <- lapply(dt[,6], factor)
        dt[,7] <- lapply(dt[,7], factor)

        #Assemble Cast MC, Die & HourCode
        dt$CastMC <- as.factor(paste0("CastMC#",substr(dt$part_id,10,11)))
        dt$CastDie <- as.factor(paste0("Die#",substr(dt$part_id,12,13)))
        dt$CastMC_Die <- as.factor(paste0(dt$CastMC,dt$CastDie))
        dt$HourCode <- as.factor(substr(dt$part_id,14,14))
        
        #Assemble Cast Date/Time
        HourCode <- readRDS("HourCode.RDS")
        dt <- merge(dt, HourCode, by = "HourCode", all.x = T)
        dt$CastMin <- substr(dt$part_id,15,16)
        dt$CastYear <- paste0("20", substr(dt$part_id,4,5))
        dt$CastMonth <- substr(dt$part_id,6,7)
        dt$CastDay <- substr(dt$part_id,8,9)
        dt$CastDate <- ymd(paste0(dt$CastYear,"-",dt$CastMonth,"-", dt$CastDay))
        dt$CastDateTime <- ymd_hm(paste0(dt$CastYear,"-",dt$CastMonth,"-", dt$CastDay," ", dt$CastHour,":", dt$CastMin))
        
        #Remove unwanted columns
        ColName.Drop <- c("HourCode","CastMin","CastYear","CastMonth","CastDay","CastHour")
        dt <- dt[,-ColName.Drop, with=FALSE]
  
  return(dt) 
}

Process.LeakTest.Master.Data = function(dt) {
          ## Process leak test data. 
          ## Assemble casting date / time based on barcode. Records with incorrect barcode will be dropped.
          ## Convert some columns into factor
          
          # #test variable
          # dt <- dt.AirDecay.WP.Full
          
          #Remove data of leak test masters
          MasterList <- readRDS("MasterList.RDS")
          dt <- dt[ part_id %in% MasterList, ]
          
          
          #convert some columns to factor
          dt[,1] <- lapply(dt[,1], factor)
          dt[,3] <- lapply(dt[,3], factor)
          dt[,5] <- lapply(dt[,5], factor)
          dt[,6] <- lapply(dt[,6], factor)
          dt[,7] <- lapply(dt[,7], factor)
          
          #Assemble Cast MC, Die & HourCode
          dt$CastMC <- as.factor(paste0("CastMC#",substr(dt$part_id,10,11)))
          dt$CastDie <- as.factor(paste0("Die#",substr(dt$part_id,12,13)))
          dt$CastMC_Die <- as.factor(paste0(dt$CastMC,dt$CastDie))
          dt$HourCode <- as.factor(substr(dt$part_id,14,14))
          
          #Assemble Cast Date/Time
          HourCode <- readRDS("HourCode.RDS")
          dt <- merge(dt, HourCode, by = "HourCode", all.x = T)
          dt$CastMin <- substr(dt$part_id,15,16)
          dt$CastYear <- paste0("20", substr(dt$part_id,4,5))
          dt$CastMonth <- substr(dt$part_id,6,7)
          dt$CastDay <- substr(dt$part_id,8,9)
          dt$CastDate <- ymd(paste0(dt$CastYear,"-",dt$CastMonth,"-", dt$CastDay))
          dt$CastDateTime <- ymd_hm(paste0(dt$CastYear,"-",dt$CastMonth,"-", dt$CastDay," ", dt$CastHour,":", dt$CastMin))
          
          #Remove unwanted columns
          ColName.Drop <- c("HourCode","CastMin","CastYear","CastMonth","CastDay","CastHour")
          dt <- dt[,-ColName.Drop, with=FALSE]
  
  return(dt) 
}


Daily.Statics.AirDecay.WP = function(dt.source, lsl, usl) {

        ## Summarize statics data by gourp. All duplicates will be removed before summary.
  
        # #test variable
        # dt.source <- dt.AirDecay.WP.Full
        # lsl = -3
        # usl = 2.1
        
        #Remove duplicates
        dt.source <- dt.source[ , .SD[.N] ,  by = c("part_id") ]
        
        #sort in oredr of test date
        dt.source <- dt.source[order(dt.source$LeakTestDateTime, decreasing = FALSE),]
        
        #calculate test result
        dt.source[dt.source$air_decay_wp > usl | dt.source$air_decay_wp < lsl, Result := as.factor("FAIL")]
        dt.source[dt.source$air_decay_wp <= usl & dt.source$air_decay_wp >= lsl, Result := as.factor("PASS")]
        
 
        dt <- dt.source %>%
              group_by(Date = date(LeakTestDateTime)) %>%
              summarize(Qty = n(),
                        RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
                        Avg.LeakRate = mean(air_decay_wp), 
                        Stdev.LeakRate = sd(air_decay_wp),
                        Max.LeakRate = max(air_decay_wp), 
                        Min.LeakRate = min(air_decay_wp),
                        Range.LeakRate = (Max.LeakRate - Min.LeakRate)
                        )
       
        #Remove NA of Stdev due to low qty (count=1)
        dt <- dt[complete.cases(dt),]
        
        
        return(dt) 
}


Daily.Statics.AirDecay.MC = function(dt.source, lsl, usl) {
  
  ## Summarize statics data by gourp. All duplicates will be removed before summary.
  
  # #test variable
  # dt.source <- dt.AirDecay.MC.Full
  # lsl = -6
  # usl = 6
  
  #Remove duplicates
  dt.source <- dt.source[ , .SD[.N] ,  by = c("part_id") ]
  
  #sort in oredr of test date
  dt.source <- dt.source[order(dt.source$LeakTestDateTime, decreasing = FALSE),]
  
  #calculate test result
  dt.source[dt.source$air_decay_mc > usl | dt.source$air_decay_mc < lsl, Result := as.factor("FAIL")]
  dt.source[dt.source$air_decay_mc <= usl & dt.source$air_decay_mc >= lsl, Result := as.factor("PASS")]
  
  
  dt <- dt.source %>%
    group_by(Date = date(LeakTestDateTime)) %>%
    summarize(Qty = n(),
              RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
              Avg.LeakRate = mean(air_decay_mc), 
              Stdev.LeakRate = sd(air_decay_mc),
              Max.LeakRate = max(air_decay_mc), 
              Min.LeakRate = min(air_decay_mc),
              Range.LeakRate = (Max.LeakRate - Min.LeakRate)
    )
  
  #Remove NA of Stdev due to low qty (count=1)
  dt <- dt[complete.cases(dt),]
  
  
  return(dt) 
}


Daily.Statics.AirDecay.He = function(dt.source, lsl, usl) {
  
  ## Summarize statics data by gourp. All duplicates will be removed before summary.
  
  # #test variable
  # dt.source <- dt.AirDecay.He.Full
  # lsl = 0
  # usl = 3.0E-6
  
  #Remove duplicates
  dt.source <- dt.source[ , .SD[.N] ,  by = c("part_id") ]
  
  #sort in oredr of test date
  dt.source <- dt.source[order(dt.source$LeakTestDateTime, decreasing = FALSE),]
  
  #calculate test result
  dt.source[dt.source$helium_test > usl | dt.source$helium_test < lsl, Result := as.factor("FAIL")]
  dt.source[dt.source$helium_test <= usl & dt.source$helium_test >= lsl, Result := as.factor("PASS")]
  
  
  dt <- dt.source %>%
    group_by(Date = date(LeakTestDateTime)) %>%
    summarize(Qty = n(),
              RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
              Avg.LeakRate = mean(helium_test), 
              Stdev.LeakRate = sd(helium_test),
              Max.LeakRate = max(helium_test), 
              Min.LeakRate = min(helium_test),
              Range.LeakRate = (Max.LeakRate - Min.LeakRate)
    )
  
  #Remove NA of Stdev due to low qty (count=1)
  dt <- dt[complete.cases(dt),]
  
  
  return(dt) 
}

KeepLatestRecord.AirDecay = function(dt) {
          #Remove duplicates
          dt <- dt[ , .SD[.N] ,  by = c("part_id") ]
          
          #sort in oredr of test date
          dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
          return(dt)
}

KeepOldestRecord.AirDecay = function(dt) {
  # # Test Variables
  # dt <- dt.AirDecay.WP.NoMaster
  
  # Sort as per order of part id, then time. 
  # Keep only the earliest record of the same part ID
  dt <- dt[order(dt$part_id, dt$LeakTestDateTime,  decreasing = FALSE),]
  dt <- dt[!duplicated(dt$part_id),]
  
  #sort in oredr of test date
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  return(dt)
}


KeepOldestRecord.Inspction = function(dt) {
      # # Test Variables
      # dt <- dt.Inspection.Gate1
      
      # Sort as per order of part id, then time. 
      # Keep only the earliest record of the same part ID
      dt <- dt[order(dt$part_id, dt$datetime,  decreasing = FALSE),]
      dt <- dt[!duplicated(dt$part_id),]
      
      #sort in oredr of test date
      dt <- dt[order(dt$datetime, decreasing = FALSE),]
  
  return(dt)
}


Sum.Stat.AirDecay.WP = function(dt, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")) {
  
  ## Test Variables
  # dt <- dt.AirDecay.WP.NoMaster
  # Date.Start <- as.Date("2017-01-15")
  # Date.End <- as.Date("2017-01-20")
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start & date(dt$LeakTestDateTime) <= Date.End ,]
  
  ## Summarize statics data by gourp. All duplicates will be removed before summary.

  #Remove duplicates and only keep the latest result of the same part
  dt <- dt[ , .SD[.N] ,  by = c("part_id") ]
  
  #sort in oredr of test date
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  dt <- dt %>%
    group_by(CastMC_Die) %>%
    summarize(Qty = n(),
              RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
              Avg.LeakRate = mean(air_decay_wp), 
              Stdev.LeakRate = sd(air_decay_wp)
    )
  
  
  return(dt) 
}


Hourly.Statics.AirDecay.WP = function(dt.source, lsl, usl) {
  
  ## Summarize statics data by gourp. All duplicates will be removed before summary.
  
  # #test variable
  # dt.source <- dt.good
  # lsl = -3
  # usl = 2.1
  
  #Remove duplicates
  dt.source <- dt.source[ , .SD[.N] ,  by = c("part_id") ]
  
  #sort in oredr of test date
  dt.source <- dt.source[order(dt.source$LeakTestDateTime, decreasing = FALSE),]
  
  #calculate test result
  dt.source[dt.source$air_decay_wp > usl | dt.source$air_decay_wp < lsl, Result := as.factor("FAIL")]
  dt.source[dt.source$air_decay_wp <= usl & dt.source$air_decay_wp >= lsl, Result := as.factor("PASS")]
  
  
  dt <- dt.source %>%
    group_by(HourDate=floor_date(LeakTestDateTime, "1 hour")) %>%
    summarize(Qty = n(),
              RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
              Avg.LeakRate = mean(air_decay_wp), 
              Stdev.LeakRate = sd(air_decay_wp),
              Max.LeakRate = max(air_decay_wp), 
              Min.LeakRate = min(air_decay_wp),
              Range.LeakRate = (Max.LeakRate - Min.LeakRate)
    )
  
  #Remove NA of Stdev due to low qty (count=1)
  dt <- dt[complete.cases(dt),]
  
  
  return(dt) 
}

HalfHourly.Statics.AirDecay.WP = function(dt.source, lsl, usl) {
  
  ## Summarize statics data by gourp. All duplicates will be removed before summary.
  
  # #test variable
  # dt.source <- dt.good
  # lsl = -3
  # usl = 2.1
  
  #Remove duplicates
  dt.source <- dt.source[ , .SD[.N] ,  by = c("part_id") ]
  
  #sort in oredr of test date
  dt.source <- dt.source[order(dt.source$LeakTestDateTime, decreasing = FALSE),]
  
  #calculate test result
  dt.source[dt.source$air_decay_wp > usl | dt.source$air_decay_wp < lsl, Result := as.factor("FAIL")]
  dt.source[dt.source$air_decay_wp <= usl & dt.source$air_decay_wp >= lsl, Result := as.factor("PASS")]
  
  
  dt <- dt.source %>%
    group_by(HourDate=floor_date(LeakTestDateTime, "30 minute")) %>%
    summarize(Qty = n(),
              RejectPrecent = (sum(Result=="FAIL") / Qty)*100,
              Avg.LeakRate = mean(air_decay_wp), 
              Stdev.LeakRate = sd(air_decay_wp),
              Max.LeakRate = max(air_decay_wp), 
              Min.LeakRate = min(air_decay_wp),
              Range.LeakRate = (Max.LeakRate - Min.LeakRate)
    )
  
  #Remove NA of Stdev due to low qty (count=1)
  dt <- dt[complete.cases(dt),]
  
  
  return(dt) 
}


Plot.LineChart.Year = function(dt, VARx, VARy, nominal, lsl, usl, Mean, Sigma, Title) {
  # #Var for testing
  # dt <- dt.Daily.Statics.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "WP Daily Statics"
  # VARx = "Date"
  # VARy =  "Avg.LeakRate"
  # Mean = 1.87
  # Sigma = 7.04

  #prepare contorl lines for ave. chart
  sd1 <- Mean + 1*Sigma
  sd2 <- Mean + 2*Sigma
  sd3 <- Mean + 3*Sigma
  sd1.Neg <- Mean - 1*Sigma
  sd2.Neg <- Mean - 2*Sigma
  sd3.Neg <- Mean - 3*Sigma
  
  SpecLine <- data.frame(ControlValue = c(lsl, nominal,  usl),
                             ControlType = c("LSL", "Nominal",  "USL" ))
  
  LineMean <- data.frame(ControlValue = c(Mean),
                             ControlType = c("Mean"))
  
  ContorlLine1 <- data.frame(ControlValue = c(sd1.Neg, sd1),
                                ControlType = c("-1 Sigma", "1 Sigma"))

  ContorlLine2 <- data.frame(ControlValue = c(sd2.Neg, sd2),
                             ControlType = c("-2 Sigma","2 Sigma" ))
  
  ContorlLine3 <- data.frame(ControlValue = c(sd3.Neg, sd3),
                             ControlType = c("-3 Sigma","3 Sigma" ))

  
  g <- ggplot(dt, aes_string(x = VARx, y = VARy )) +
                geom_line()+
                geom_point()+
                # Remove spec line when process was in stable condition
                # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                geom_hline(data=LineMean, aes(yintercept=ControlValue, colour = ControlType ),   size=1) +
                geom_hline(data=ContorlLine1, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                geom_hline(data=ContorlLine2, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
                geom_hline(data=ContorlLine3, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
                xlab(VARx) +
                ylab(VARy) +
                ggtitle(paste(Title) ) +
                scale_x_date(date_breaks = "1 month", labels = date_format("%b-%y")) + 
                theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
  # print(g)
  
  return(g)
}

Plot.LineChart.Date = function(dt, VARx, VARy, nominal, lsl, usl, Mean, Sigma, Title) {
  # #Var for testing
  # dt <- dt.Daily.Statics.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "WP Daily Statics"
  # VARx = "Date"
  # VARy =  "Avg.LeakRate"
  # Mean = 1.87
  # Sigma = 7.04
  
  #prepare contorl lines for ave. chart
  sd1 <- Mean + 1*Sigma
  sd2 <- Mean + 2*Sigma
  sd3 <- Mean + 3*Sigma
  sd1.Neg <- Mean - 1*Sigma
  sd2.Neg <- Mean - 2*Sigma
  sd3.Neg <- Mean - 3*Sigma
  
  SpecLine <- data.frame(ControlValue = c(lsl, nominal,  usl),
                         ControlType = c("LSL", "Nominal",  "USL" ))
  
  LineMean <- data.frame(ControlValue = c(Mean),
                         ControlType = c("Mean"))
  
  ContorlLine1 <- data.frame(ControlValue = c(sd1.Neg, sd1),
                             ControlType = c("-1 Sigma", "1 Sigma"))
  
  ContorlLine2 <- data.frame(ControlValue = c(sd2.Neg, sd2),
                             ControlType = c("-2 Sigma","2 Sigma" ))
  
  ContorlLine3 <- data.frame(ControlValue = c(sd3.Neg, sd3),
                             ControlType = c("-3 Sigma","3 Sigma" ))
  
  
  g <- ggplot(dt, aes_string(x = VARx, y = VARy )) +
            geom_line()+
            geom_point()+
            # Remove spec line when process was in stable condition
            # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
            geom_hline(data=LineMean, aes(yintercept=ControlValue, colour = ControlType ),   size=1) +
            geom_hline(data=ContorlLine1, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
            geom_hline(data=ContorlLine2, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
            geom_hline(data=ContorlLine3, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
            xlab(VARx) +
            ylab(VARy) +
            ggtitle(paste(Title) ) +
            scale_x_date(date_breaks = "1 day", labels = date_format("%d/%b")) + 
            theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
  # print(g)
  
  return(g)
}


Plot.LineChart.Hour = function(dt, VARx, VARy, nominal, lsl, usl, Mean, Sigma, Title) {
  # #Var for testing
  # dt <- dt.Daily.Statics.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "WP Daily Statics"
  # VARx = "Date"
  # VARy =  "Avg.LeakRate"
  # Mean = 1.87
  # Sigma = 7.04
  
  #prepare contorl lines for ave. chart
  sd1 <- Mean + 1*Sigma
  sd2 <- Mean + 2*Sigma
  sd3 <- Mean + 3*Sigma
  sd1.Neg <- Mean - 1*Sigma
  sd2.Neg <- Mean - 2*Sigma
  sd3.Neg <- Mean - 3*Sigma
  
  SpecLine <- data.frame(ControlValue = c(lsl, nominal,  usl),
                         ControlType = c("LSL", "Nominal",  "USL" ))
  
  LineMean <- data.frame(ControlValue = c(Mean),
                         ControlType = c("Mean"))
  
  ContorlLine1 <- data.frame(ControlValue = c(sd1.Neg, sd1),
                             ControlType = c("-1 Sigma", "1 Sigma"))
  
  ContorlLine2 <- data.frame(ControlValue = c(sd2.Neg, sd2),
                             ControlType = c("-2 Sigma","2 Sigma" ))
  
  ContorlLine3 <- data.frame(ControlValue = c(sd3.Neg, sd3),
                             ControlType = c("-3 Sigma","3 Sigma" ))
  
  
  g <- ggplot(dt, aes_string(x = VARx, y = VARy )) +
            geom_line()+
            geom_point()+
            # Remove spec line when process was in stable condition
            # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
            geom_hline(data=LineMean, aes(yintercept=ControlValue, colour = ControlType ),   size=1) +
            geom_hline(data=ContorlLine1, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
            geom_hline(data=ContorlLine2, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
            geom_hline(data=ContorlLine3, aes(yintercept=ControlValue, colour = ControlType), linetype="dashed",  size=1) +
            xlab(VARx) +
            ylab(VARy) +
            ggtitle(paste(Title) ) +
            scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
            theme(text = element_text(size=10), axis.text.x = element_text(angle = 90))
          # print(g)
  
  return(g)
}

Plot.Daily.Mean.SD.Control.MovingLimit = function(dt, nominal, lsl, usl, Title) {
        # #Var for testing
        # dt <- dt.Daily.Statics.AirDecay.WP
        # nominal = 0
        # lsl = -3
        # usl = 2.1
        # Title = "WP Daily Statics"
        

        #prepare contorl lines for ave. chart
        Mean.ave <- mean(dt$Avg.LeakRate)
        SD.ave <- sd(dt$Avg.LeakRate)
        sd1.ave <- Mean.ave + 1*SD.ave
        sd2.ave <- Mean.ave + 2*SD.ave
        sd3.ave <- Mean.ave + 3*SD.ave
        sd1.Neg.ave <- Mean.ave - 1*SD.ave
        sd2.Neg.ave <- Mean.ave - 2*SD.ave
        sd3.Neg.ave <- Mean.ave - 3*SD.ave
        
        SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
                                   ControlType = c("LSL", "Nominal",  "USL" ))
        
        ContorlLine.ave <- data.frame(ControlValue = c(sd3.Neg.ave, sd2.Neg.ave, sd1.Neg.ave, Mean.ave ,sd1.ave, sd2.ave, sd3.ave),
                                      ControlType = c("-3 Sigma","-2 Sigma","-1 Sigma", "Mean", "1 Sigma","2 Sigma","3 Sigma" ))
        
        g.ave <- ggplot(dt, aes(x = dt$Date, y=dt$Avg.LeakRate)) +
                        geom_line()+
                        geom_point()+
                        # Remove spec line when process was in stable condition
                        # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                        geom_hline(data=ContorlLine.ave, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                        xlab("Date") +
                        ylab("Ave. Leak Rate") +
                        ggtitle(paste("QUK2 SH WJ Leak Rate Ave - ", Title )) +
                        theme(text = element_text(size=10))
        # print(g.ave)
        
        #prepare contorl line for SD chart
        Mean.sd <- mean(dt$Stdev.LeakRate)
        sd1 <- sd(dt$Stdev.LeakRate)
        sd2 <- 2*sd1
        sd3 <- 3*sd1
        
        ContorlLine.sd <- data.frame(ControlValue = c(Mean.sd, sd1, sd2, sd3),
                                     ControlType = c("Mean","1 Sigma", "2 Sigma", "3 Sigma" ))
        
        g.sd <- ggplot(dt, aes(x = dt$Date, y=dt$Stdev.LeakRate)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.sd, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date") +
                      ylab("Std. Dev.") +
                      ggtitle(paste("QUK2 SH WJ Leak Rate Std Dev - ", Title )) +
                      theme(text = element_text(size=10))
        # print(g.sd)

        
        #prepare contorl line for reject rate chart
        Mean.ng <- mean(dt$RejectPrecent)
        sd1.ng <- sd(dt$RejectPrecent)
        sd2.ng <- 2*sd1.ng
        sd3.ng <- 3*sd1.ng
        
        ContorlLine.ng <- data.frame(ControlValue = c(Mean.ng, sd1.ng, sd2.ng, sd3.ng),
                                     ControlType = c("Mean", "1 Sigma", "2 Sigma", "3 Sigma" ))
        
        g.ng <- ggplot(dt, aes(x = dt$Date, y=dt$RejectPrecent)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.ng, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date") +
                      ylab("Reject Precentage") +
                      ggtitle(paste("QUK2 SH WJ Leak Test Reject Rate - ", Title )) +
                      theme(text = element_text(size=10))
        # print(g.ng)        

        #prepare contorl line for qty. chart
        Mean.qty <- mean(dt$Qty)
        ContorlLine.qty <- data.frame(ControlValue = c(Mean.qty), ControlType = c("Mean"))
        
        g.qty <- ggplot(dt, aes(x = dt$Date, y=dt$Qty)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.qty, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date") +
                      ylab("Total Test Qty.") +
                      ggtitle(paste("QUK2 SH WJ Leak Test Qty. - ", Title )) +
                      theme(text = element_text(size=10))
        # print(g.qty)  
        
        multiplot(g.ave, g.sd, g.ng,g.qty, cols=1)
}


Plot.Hourly.WP.Mean.SD.Control.MovingLimit = function(dt, nominal, lsl, usl, Title) {
          # #Var for testing
          # dt <- dt.Hourly.Statics.AirDecay.WP
          # nominal = 0
          # lsl = -3
          # usl = 2.1
          # Title = "WP Hourly Statics"
          
          
          #prepare contorl lines for ave. chart
          Mean.ave <- mean(dt$Avg.LeakRate)
          SD.ave <- sd(dt$Avg.LeakRate)
          sd1.ave <- Mean.ave + 1*SD.ave
          sd2.ave <- Mean.ave + 2*SD.ave
          sd3.ave <- Mean.ave + 3*SD.ave
          sd1.Neg.ave <- Mean.ave - 1*SD.ave
          sd2.Neg.ave <- Mean.ave - 2*SD.ave
          sd3.Neg.ave <- Mean.ave - 3*SD.ave
          
          SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
                                     ControlType = c("LSL", "Nominal",  "USL" ))
          
          ContorlLine.ave <- data.frame(ControlValue = c(sd3.Neg.ave, sd2.Neg.ave, sd1.Neg.ave, Mean.ave ,sd1.ave, sd2.ave, sd3.ave),
                                        ControlType = c("-3 Sigma","-2 Sigma","-1 Sigma", "Mean", "1 Sigma","2 Sigma","3 Sigma" ))
          
          g.ave <- ggplot(dt, aes(x = dt$HourDate, y=dt$Avg.LeakRate)) +
                  geom_line()+
                  geom_point()+
                  # Remove spec line when process was in stable condition
                  # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                  geom_hline(data=ContorlLine.ave, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                  xlab("Date/Time") +
                  ylab("Ave. Leak Rate") +
                  ggtitle(paste("QUK2 SH WJ Leak Rate Ave - ", Title )) +
                  scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
          # print(g.ave)
          
          #prepare contorl line for SD chart
          Mean.sd <- mean(dt$Stdev.LeakRate)
          sd1 <- sd(dt$Stdev.LeakRate)
          sd2 <- 2*sd1
          sd3 <- 3*sd1
          
          ContorlLine.sd <- data.frame(ControlValue = c(Mean.sd, sd1, sd2, sd3),
                                       ControlType = c("Mean","1 Sigma", "2 Sigma", "3 Sigma" ))
          
          g.sd <- ggplot(dt, aes(x = dt$HourDate, y=dt$Stdev.LeakRate)) +
                  geom_line()+
                  geom_point()+
                  geom_hline(data=ContorlLine.sd, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                  xlab("Date/Time") +
                  ylab("Std. Dev.") +
                  ggtitle(paste("QUK2 SH WJ Leak Rate Std Dev - ", Title )) +
                  scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
          # print(g.sd)
          
          
          #prepare contorl line for reject rate chart
          Mean.ng <- mean(dt$RejectPrecent)
          sd1.ng <- sd(dt$RejectPrecent)
          sd2.ng <- 2*sd1.ng
          sd3.ng <- 3*sd1.ng
          
          ContorlLine.ng <- data.frame(ControlValue = c(Mean.ng, sd1.ng, sd2.ng, sd3.ng),
                                       ControlType = c("Mean", "1 Sigma", "2 Sigma", "3 Sigma" ))
          
          g.ng <- ggplot(dt, aes(x = dt$HourDate, y=dt$RejectPrecent)) +
                    geom_line()+
                    geom_point()+
                    geom_hline(data=ContorlLine.ng, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                    xlab("Date/Time") +
                    ylab("Reject Precentage") +
                    ggtitle(paste("QUK2 SH WJ Leak Test Reject Rate - ", Title )) +
                    scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
          # print(g.ng)        

          #prepare contorl line for qty. chart
          Mean.qty <- mean(dt$Qty)
          ContorlLine.qty <- data.frame(ControlValue = c(Mean.qty), ControlType = c("Mean"))
          
          g.qty <- ggplot(dt, aes(x = dt$HourDate, y=dt$Qty)) +
                    geom_line()+
                    geom_point()+
                    geom_hline(data=ContorlLine.qty, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                    xlab("Date/Time") +
                    ylab("Total Test Qty.") +
                    ggtitle(paste("QUK2 SH WJ Leak Test Qty. - ", Title )) +
                    scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
          # print(g.qty)  
          
          multiplot(g.ave, g.sd, g.ng, g.qty, cols=1)
}


Plot.Hourly.WP.Mean.SD.Control.FixedLimit = function(dt, nominal, lsl, usl, Title, Mean.ave, SD.ave, Mean.sd, SD.sd) {
        # #Var for testing
        # dt <- dt.Hourly.Statics.AirDecay.WP
        # nominal = 0
        # lsl = -3
        # usl = 2.1
        # Title = "WP Hourly Statics"
        # Mean.ave = 0.09696946
        # SD.ave = 0.1201799
        # Mean.sd = 0.1635473
        # SD.sd = 0.209185
  
  
        #prepare contorl lines for ave. chart
        sd1.ave <- Mean.ave + 1*SD.ave
        sd2.ave <- Mean.ave + 2*SD.ave
        sd3.ave <- Mean.ave + 3*SD.ave
        sd1.Neg.ave <- Mean.ave - 1*SD.ave
        sd2.Neg.ave <- Mean.ave - 2*SD.ave
        sd3.Neg.ave <- Mean.ave - 3*SD.ave
        
        SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
                                   ControlType = c("LSL", "Nominal",  "USL" ))
        
        ContorlLine.ave <- data.frame(ControlValue = c(sd3.Neg.ave, sd2.Neg.ave, sd1.Neg.ave, Mean.ave ,sd1.ave, sd2.ave, sd3.ave),
                                      ControlType = c("-3 Sigma","-2 Sigma","-1 Sigma", "Mean", "1 Sigma","2 Sigma","3 Sigma" ))
        
        g.ave <- ggplot(dt, aes(x = dt$HourDate, y=dt$Avg.LeakRate)) +
                      geom_line()+
                      geom_point()+
                      # Remove spec line when process was in stable condition
                      # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                      geom_hline(data=ContorlLine.ave, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date/Time") +
                      ylab("Ave. Leak Rate") +
                      ggtitle(paste("QUK2 SH WJ Leak Rate Ave - ", Title )) +
                      scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                      theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
        # print(g.ave)
        
        #prepare contorl line for SD chart
        sd1 <- SD.sd
        sd2 <- 2*sd1
        sd3 <- 3*sd1
        
        ContorlLine.sd <- data.frame(ControlValue = c(Mean.sd, sd1, sd2, sd3),
                                     ControlType = c("Mean","1 Sigma", "2 Sigma", "3 Sigma" ))
        
        g.sd <- ggplot(dt, aes(x = dt$HourDate, y=dt$Stdev.LeakRate)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.sd, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date/Time") +
                      ylab("Std. Dev.") +
                      ggtitle(paste("QUK2 SH WJ Leak Rate Std Dev - ", Title )) +
                      scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                      theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
        # print(g.sd)
        
        
        #prepare contorl line for reject rate chart
        Mean.ng <- mean(dt$RejectPrecent)
        sd1.ng <- sd(dt$RejectPrecent)
        sd2.ng <- 2*sd1.ng
        sd3.ng <- 3*sd1.ng
        
        ContorlLine.ng <- data.frame(ControlValue = c(Mean.ng, sd1.ng, sd2.ng, sd3.ng),
                                     ControlType = c("Mean", "1 Sigma", "2 Sigma", "3 Sigma" ))
        
        g.ng <- ggplot(dt, aes(x = dt$HourDate, y=dt$RejectPrecent)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.ng, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date/Time") +
                      ylab("Reject Precentage") +
                      ggtitle(paste("QUK2 SH WJ Leak Test Reject Rate - ", Title )) +
                      scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                      theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
        # print(g.ng)
        
        #prepare contorl line for qty. chart
        Mean.qty <- mean(dt$Qty)
        
        ContorlLine.qty <- data.frame(ControlValue = c(Mean.qty), ControlType = c("Mean"))
        
        g.qty <- ggplot(dt, aes(x = dt$HourDate, y=dt$Qty)) +
                      geom_line()+
                      geom_point()+
                      geom_hline(data=ContorlLine.qty, aes(yintercept=ControlValue, colour = ControlType ), linetype="dashed",  size=1) +
                      xlab("Date/Time") +
                      ylab("Total Test Qty.") +
                      ggtitle(paste("QUK2 SH WJ Leak Test Qty. - ", Title )) +
                      scale_x_datetime(date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                      theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
        # print(g.qty)  
        
        multiplot(g.ave, g.sd, g.ng, g.qty, cols=1)
}

Plot.SinglePoint.WP.ControlChart = function(dt, nominal, lsl, usl, Title){
  # #Var for testing
  # dt <- dt.ng.Rule2
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  
  
  #prepare contorl lines for ave. chart
  # SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
  #                            ControlType = c("LSL", "Nominal",  "USL" ))
  
  
  g.LT <- ggplot(dt, aes(x = dt$LeakTestDateTime, y=dt$air_decay_wp)) +
                  geom_line()+
                  geom_point()+
                  # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                  geom_hline(aes(yintercept=lsl, colour = 'LSL' ), linetype='dashed', show.legend = TRUE, size=1) +
                  geom_hline(aes(yintercept=nominal, colour = 'Nominal' ),  linetype='solid', show.legend = TRUE, size=1) +
                  geom_hline(aes(yintercept=usl, colour = 'USL' ), linetype='dashed', show.legend = TRUE, size=1) +             
                  xlab("Date/Time") +
                  ylab("Leak Rate") +
                  ggtitle(paste("QUK2 SH WJ Leak Rate - ", Title )) +
                  scale_x_datetime(expand = c(0, 0), date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  return(g.LT)
}

Plot.SinglePoint.WP.ControlChart.CombinedMaster = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2018-01-15")
  # Date.End = as.Date("2018-01-20")

  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start 
                         & date(dt$LeakTestDateTime) <= Date.End, ]
  
  g.LT <- ggplot(dt, aes(x = dt$LeakTestDateTime, y=dt$air_decay_wp, shape= dt$CastMC_Die,colour = dt$CastMC_Die)) +
                  scale_color_brewer(palette="Dark2") +
                  # geom_line()+
                  geom_point()+
                  # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
                  geom_hline(aes(yintercept=lsl, colour = 'LSL' ), linetype='dashed', show.legend = TRUE, size=1) +
                  geom_hline(aes(yintercept=nominal, colour = 'Nominal' ),  linetype='solid', show.legend = TRUE, size=1) +
                  geom_hline(aes(yintercept=usl, colour = 'USL' ), linetype='dashed', show.legend = TRUE, size=1) +             
                  xlab("Date/Time") +
                  ylab("Leak Rate") +
                  ggtitle(paste("QUK2 SH WJ Leak Rate - ", Title )) +
                  scale_x_datetime(expand = c(0, 0), date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
                  theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  
  # subset master part data
  dt.Master <- dt.Master[date(dt.Master$LeakTestDateTime) >= Date.Start 
                                                    & date(dt.Master$LeakTestDateTime) <= Date.End 
                                                    & dt.Master$part_id == ID.Master, ]
  dt.Master <- dt.Master[order(dt.Master$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot Master Leak Rate into previous leak rate chart
  g.LT <- g.LT + geom_point(data = dt.Master, aes(LeakTestDateTime, air_decay_wp, shape=part_id), color = 'red', size = 5)
  
  
  return(g.LT)
}


Plot.SinglePoint.WP.ControlChart.SmallScale.CombinedMaster = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2018-01-15")
  # Date.End = as.Date("2018-01-20")
  
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start 
           & date(dt$LeakTestDateTime) <= Date.End, ]
  
  g.LT <- ggplot(dt, aes(x = dt$LeakTestDateTime, y=dt$air_decay_wp, shape= dt$CastMC_Die,colour = dt$CastMC_Die)) +
    scale_color_brewer(palette="Dark2") +
    geom_line()+
    geom_point()+
    # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, colour = ControlType ),  size=1) +
    geom_hline(aes(yintercept=lsl, colour = 'LSL' ), linetype='dashed', show.legend = TRUE, size=1) +
    geom_hline(aes(yintercept=nominal, colour = 'Nominal' ),  linetype='solid', show.legend = TRUE, size=1) +
    geom_hline(aes(yintercept=usl, colour = 'USL' ), linetype='dashed', show.legend = TRUE, size=1) +             
    xlab("Date/Time") +
    ylab("Leak Rate") +
    scale_y_continuous(limits = c(-4, 3)) + 
    ggtitle(paste("QUK2 SH WJ Leak Rate - ", Title )) +
    scale_x_datetime(expand = c(0, 0), date_breaks = "6 hour", labels = date_format("%d/%b %H:00")) +
    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  
  # subset master part data
  dt.Master <- dt.Master[date(dt.Master$LeakTestDateTime) >= Date.Start 
                         & date(dt.Master$LeakTestDateTime) <= Date.End 
                         & dt.Master$part_id == ID.Master, ]
  dt.Master <- dt.Master[order(dt.Master$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot Master Leak Rate into previous leak rate chart
  g.LT <- g.LT + geom_point(data = dt.Master, aes(LeakTestDateTime, air_decay_wp, shape=part_id), color = 'red', size = 5)
  
  
  return(g.LT)
}


Plot.SinglePoint.WP.Type1 = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to 
  ## 1) subset data as per given period of time, 
  ## 2) then plot the leak rate of individual parts and master part
  ## 3) Plot leak rate trend as per casting date
  
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2016-01-01")
  # Date.End = as.Date("2030-01-01")
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start & date(dt$LeakTestDateTime) <= Date.End ,]
  
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot leka rate chart combined with Master Part Data
  g.LeakRate.WP <- Plot.SinglePoint.WP.ControlChart.CombinedMaster(dt, nominal, lsl, usl, 
                     Title, dt.Master, ID.Master, 
                     Date.Start, Date.End )

  # Plot Leak Rate Chart by cast date / time
  g.LeakRate.CastDate.WP <- Plot.SinglePoint.WP.ControlChart.CastTime(dt, nominal, lsl, usl, Title)
  
  
  multiplot(g.LeakRate.WP, g.LeakRate.CastDate.WP, cols=1)

}

Plot.SinglePoint.WP.Type2 = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to 
  ## 1) subset data as per given period of time, 
  ## 2) then plot the leak rate of individual parts and master part
  ## 3) Plot leak rate trend as per casting date
  ## 4) Plot Temperatur and Humidity trend at the same period of time
  
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2016-01-01")
  # Date.End = as.Date("2030-01-01")
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start & date(dt$LeakTestDateTime) <= Date.End ,]
  
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot leka rate chart combined with Master Part Data
  g.LeakRate.WP <- Plot.SinglePoint.WP.ControlChart.CombinedMaster(dt, nominal, lsl, usl, 
                                                                   Title, dt.Master, ID.Master, 
                                                                   Date.Start, Date.End )
  
  # Plot Leak Rate Chart by cast date / time
  g.LeakRate.CastDate.WP <- Plot.SinglePoint.WP.ControlChart.CastTime(dt, nominal, lsl, usl, Title)
  
  # Plot Temp and Humidity trend
  g.Temp <- Plot.SinglePoint.Temp(dt.TempHumidity, "Assembly Cell Temp Trend",Date.Start, Date.End)
  g.Humidity <- Plot.SinglePoint.Humidity(dt.TempHumidity, "Assembly Cell Humidity Trend",Date.Start, Date.End)
  
  
  multiplot(g.LeakRate.WP, g.Temp, g.Humidity, g.LeakRate.CastDate.WP, cols=1)
  
}


Plot.SinglePoint.WP.SmallScale.Type1 = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to 
  ## 1) subset data as per given period of time within small scale, 
  ## 2) then plot the leak rate of individual parts and master part
  ## 3) Plot leak rate trend as per casting date
  ## 4) Plot Temperatur and Humidity trend at the same period of time
  
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2016-01-01")
  # Date.End = as.Date("2030-01-01")
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start & date(dt$LeakTestDateTime) <= Date.End ,]
  
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot leka rate chart combined with Master Part Data
  g.LeakRate.WP <- Plot.SinglePoint.WP.ControlChart.SmallScale.CombinedMaster(dt, nominal, lsl, usl, 
                                                                   Title, dt.Master, ID.Master, 
                                                                   Date.Start, Date.End )
  
  # Plot Leak Rate Chart by cast date / time
  g.LeakRate.CastDate.WP <- Plot.SinglePoint.WP.ControlChart.CastTime(dt, nominal, lsl, usl, Title)

  multiplot(g.LeakRate.WP, g.LeakRate.CastDate.WP, cols=1)
  
}

Plot.SinglePoint.WP.Type3 = function(dt, nominal, lsl, usl, Title, dt.Master, ID.Master, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to 
  ## 1) subset data as per given period of time, 
  ## 2) then plot the leak rate of individual parts and master part. 
  ##      The leak rate will be ploted at small scale for better observation.
  ## 3) Plot leak rate trend as per casting date
  ## 4) Plot Temperatur and Humidity trend at the same period of time
  
  # #Var for testing
  # dt <- dt.AirDecay.WP.NoMaster
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  # dt.Master <- dt.AirDecay.WP.Master
  # ID.Master ='XBA1601290101A23'
  # Date.Start = as.Date("2016-01-01")
  # Date.End = as.Date("2030-01-01")
  
  dt <- dt[date(dt$LeakTestDateTime) >= Date.Start & date(dt$LeakTestDateTime) <= Date.End ,]
  
  dt <- dt[order(dt$LeakTestDateTime, decreasing = FALSE),]
  
  # Plot leka rate chart combined with Master Part Data
  g.LeakRate.WP <- Plot.SinglePoint.WP.ControlChart.SmallScale.CombinedMaster(dt, nominal, lsl, usl, 
                                                                   Title, dt.Master, ID.Master, 
                                                                   Date.Start, Date.End )
  
  # Plot Leak Rate Chart by cast date / time
  g.LeakRate.CastDate.WP <- Plot.SinglePoint.WP.ControlChart.CastTime(dt, nominal, lsl, usl, Title)
  
  # Plot Temp and Humidity trend
  g.Temp <- Plot.SinglePoint.Temp(dt.TempHumidity, "Assembly Cell Temp Trend",Date.Start, Date.End)
  g.Humidity <- Plot.SinglePoint.Humidity(dt.TempHumidity, "Assembly Cell Humidity Trend",Date.Start, Date.End)
  
  
  multiplot(g.LeakRate.WP, g.Temp, g.Humidity, g.LeakRate.CastDate.WP, cols=1)
  
}

Plot.SinglePoint.Temp = function(dt, Title, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to plot the trend of temp and humidity of the assembly Cell
  
  # # Var for testing
  # dt <- dt.TempHumidity
  # Title = "Assembly Cell Temp Trend"
  # Date.Start = as.Date("2018-01-15")
  # Date.End = as.Date("2018-01-20")
  
  dt <- dt[date(dt$Date.Time) >= Date.Start & date(dt$Date.Time) <= Date.End ,]
  
  dt <- dt[order(dt$Date.Time, decreasing = FALSE),]
  
  g <- ggplot(dt, aes(x = dt$Date.Time, y=dt$Temperature, shape="Temperature"), colour='red') +
    geom_line()+
    geom_point()+
    # stat_smooth(aes(x=dt$Date.Time, y=dt$Temperature), formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
    xlab("Date/Time") +
    ylab("Temperature") +
    ggtitle(paste(Title, " ", Date.Start,"-", Date.End )) +
    # scale_x_datetime(date_breaks = "1 day", labels = date_format("%d/%b")) +
    scale_x_datetime(date_breaks = "12 hour", labels = date_format("%d/%b %H:%M"), expand = c(0, 0),
                     limits = c(floor_date(min(dt$Date.Time), "day"), ceiling_date(max(dt$Date.Time), "day"))) +
    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(g)
  
}

Plot.SinglePoint.Humidity = function(dt, Title, Date.Start = as.Date("2016-01-01"), Date.End = as.Date("2030-01-01")){
  
  ## This function is to plot the trend of temp and humidity of the assembly Cell
  
  #Var for testing
  # dt <- dt.TempHumidity
  # Title = "Assembly Temp Trend"
  # Date.Start = as.Date("2018-02-01")
  # Date.End = as.Date("2018-02-05")
  
  dt <- dt[date(dt$Date.Time) >= Date.Start & date(dt$Date.Time) <= Date.End ,]
  
  dt <- dt[order(dt$Date.Time, decreasing = FALSE),]
  
  g <- ggplot(dt, aes(x = dt$Date.Time, y=dt$RelativeHumidity, shape="Humidity"), colour='blue') +
    geom_line()+
    geom_point()+
    # stat_smooth(aes(x=dt$Date.Time, y=dt$RelativeHumidity), formula = y ~ s(x, k = 24), method = "gam", se = FALSE) +
    xlab("Date/Time") +
    ylab("Humidity") +
    ggtitle(paste(Title, " ", Date.Start,"-", Date.End )) +
    scale_x_datetime(date_breaks = "12 hour", labels = date_format("%d/%b %H:%M"), expand = c(0, 0),
                     limits = c(floor_date(min(dt$Date.Time), "day"), ceiling_date(max(dt$Date.Time), "day"))) +
    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(g)
  
}



Plot.SinglePoint.WP.ControlChart.CastTime = function(dt, nominal, lsl, usl, Title){
  # #Var for testing
  # dt <- dt.ng.Rule2
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  
  
  # #prepare contorl lines for ave. chart
  # SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
  #                            ControlType = c("LSL", "Nominal",  "USL" ))
  
  
  g.LT <- ggplot(dt, aes(x = dt$CastDateTime, y=dt$air_decay_wp, shape= dt$CastMC_Die,colour = dt$CastMC_Die)) +
    scale_color_brewer(palette="Dark2") +
    # geom_line()+
    geom_point()+
    # geom_hline(data=SpecLine.ave, aes(yintercept=ControlValue, linetype = ControlType ), size=1) +
    geom_hline(aes(yintercept=lsl, colour = 'LSL' ), linetype='dashed', show.legend = TRUE, size=1) +
    geom_hline(aes(yintercept=nominal, colour = 'Nominal' ),  linetype='solid', show.legend = TRUE, size=1) +
    geom_hline(aes(yintercept=usl, colour = 'USL' ), linetype='dashed', show.legend = TRUE, size=1) +
    xlab("Cast Date/Time") +
    ylab("Leak Rate") +
    ggtitle(paste("QUK2 SH WJ Leak Rate by Cast Date/Time - ", Title )) +
    scale_x_datetime(expand = c(0, 0), date_breaks = "1 day", labels = date_format("%d/%b")) +
    theme(text = element_text(size=10),axis.text.x = element_text(angle = 90, hjust = 1))
  return(g.LT)
}


Plot.SinglePoint.WP.LeakRate.Dynamic = function(dt, nominal, lsl, usl, Title){
  # #Var for testing
  # dt <- dt.ng.Rule6.cleaned
  # ActualMean <- mean(dt)
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"

  p <- plot_ly(x = ~dt$LeakTestDateTime, y = ~dt$air_decay_wp, name="Leak Rate", type = 'scatter', mode = 'lines+markers') %>%
              add_trace(y= ~usl, name="USL", type = "scatter", mode = 'lines') %>%
              add_trace(y= ~lsl, name="LSL", type = "scatter", mode = 'lines') %>%
              layout(
                title = paste("QUK2 SH WJ Leak Rate - ", Title ),
                # legend = list(orientation = "h", xanchor = "center",  x = 0.5) ,
                xaxis = list(
                        rangeselector = list(
                              buttons = list(
                                    list(count = 1, label = "1 Day", step = "day", stepmode = "backward"),
                                    list(count = 6, label = "6 Hour", step = "hour", stepmode = "backward"),
                                    list(step = "all"))),
                rangeslider = list(type = "day")),
                yaxis = list(title = "Leak Rate"))
  p
}


Plot.SinglePoint.WP.Histogram = function(dt, nominal, lsl, usl, Title){
  # #Var for testing
  # dt <- dt.ng.Rule2
  # nominal = 0
  # lsl = -3
  # usl = 2.1
  # Title = "Air Decay WP"
  
  
  #prepare Histogram of leak rate
  SpecLine.ave <- data.frame(ControlValue = c(lsl, nominal,  usl),
                             ControlType = c("LSL", "Nominal",  "USL" ))
  
  
  g.LT <- ggplot(dt, aes(x = dt$air_decay_wp)) +
          geom_histogram(binwidth=0.05, alpha = 0.9, position = "dodge") +
          geom_density(alpha=.2, fill="#FF6666")+
          geom_vline(data=SpecLine.ave, aes(xintercept=ControlValue, colour = ControlType ),  size=1) +
          xlab("Leak Rate") +
          ylab("Counts") +
          ggtitle(paste("QUK2 SH WJ Leak Rate Histgram - ", Title )) +
          theme(text = element_text(size=10))
  # print(g.LT)
  
  
  g.CastingTime <- ggplot(dt, aes(x = floor_date(dt$CastDateTime, "1 hour"))) +
          geom_histogram(binwidth=1, alpha = 0.9, position = "dodge") +
          geom_density(alpha=.2, fill="#FF6666")+
          xlab("Casting Date") +
          ylab("Counts") +
          ggtitle(paste("QUK2 SH WJ Cast Date/Time Histgram - ", Title )) +
          theme(text = element_text(size=10))
  # print(g.CastingTime)
  
  multiplot(g.LT,  cols=1)
}
