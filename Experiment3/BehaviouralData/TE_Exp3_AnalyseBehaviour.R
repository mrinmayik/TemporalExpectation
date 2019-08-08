####################################################################################################################################
# This script analyses the responses from the 2nd experiment in the Temporal Expectation project
# This script was written by Mrinmayi Kulkarni (mrinmayi@uwm.edu)
####################################################################################################################################

library(reshape)
library(readr)

########################## Set Admin variables ##########################

source("~/GitDir/GeneralScripts/InitialiseR/IntialiseGGPLOT.R")

BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/Experiment/"
DataPath <- paste(BasePath, "Experiment3/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment3/Counterbalancing/", sep = "")

NumBlocks <- 2

toexclude <- c()

########################## Functions ##########################

#Add a column that records the ISI that preceded a particular item. This will be ddplied with Partcipant and block
AddPreviousISICol <- function(df){
  df <- df[order(df$block, df$trial), ]
  df$PrevISI <- c(0, df[1:(nrow(df)-1), "isi"]) #Add a zero for the first trial because this trial was not preceded by any ISI
  Check <- 0
  for(i in 2:nrow(df)){ #Just a sanity check
    Check <- Check + !(df[i, "PrevISI"] == df[i-1, "isi"])
  }
  if(Check > 0){
    print(sprintf("********************* ISI and PrevISI for %s does not match up somewhere *********************", unique(df$Participant)))
  }
  
  #Add a column that's taking the difference of the end trial time of next trial minus the current trial,
  #to make sure that the ISIs are working fine
  df <- df[order(df$trial),]
  df$TrialDur <- c(0, diff(df$EndTrialTime))
  
  
  return(df)
}

AddTrialDur <- function(df){
  df <- df[order(df$Trial), ]
  
  #Add a column that's taking the difference of the end trial time of next trial minus the current trial,
  #to make sure that the ISIs are working fine
  df$TrialDur <- c(diff(df$ObjectTime), 0)
  return(df)
}

#

#========================== Work with Log Data (check timings) ==========================

FileNames <- list.files(path=DataPath, pattern="*.log", full.names=TRUE)

LogData=c()
#Read in data
for(Files in FileNames){
  #[, ColOrd] Orders the columns to put the rando columns at the end of the df
  PartData <- read.table(Files, skip=6, sep="\t", blank.lines.skip=TRUE, header=FALSE, fill=TRUE)[, 1:5]
  PartData <- PartData %>% dplyr::rename(Participant=V1, Trial=V2, Event=V3, Picture=V4, Time=V5)
  
  EncodeLog <- PartData[-(sort(c(which(PartData$Picture=="TestFix"), (which(PartData$Picture=="TestFix")+1)))), ]
  EncodeLog <- EncodeLog[!(EncodeLog$Event %in% c("Response", "")), ]
  if(!(print(nrow(EncodeLog==(192*2*2))))){
    stop
  }
  
  EncodeLog[-(grep(";", EncodeLog$Picture)), "IdealTime"] <- 1000
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_", str_extract(Files, "[0-9][a-z]"), ".csv", sep=""))
  EncodeLog[grep(";", EncodeLog$Picture), "IdealTime"] <- ThisCB$ISI
  
  EncodeLog$Dur <- c((diff(EncodeLog$Time))/10, 0)
  EncodeLog$Discrepancy <- EncodeLog$Dur - EncodeLog$IdealTime
  
  if(!(all(grep("192", EncodeLog[abs(EncodeLog$Discrepancy)>18, "Picture"]) == c(1, 2)))){
    stop(sprintf("Time discrepancy in CB%s", str_extract(Files, "[0-9][a-z]")))
  }
  sprintf("CB%s done!!", str_extract(Files, "[0-9][a-z]"))
}

#========================== Work with Log Data ends



#========================== Work with Encode Data ==========================



