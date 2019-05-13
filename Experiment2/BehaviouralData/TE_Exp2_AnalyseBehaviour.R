####################################################################################################################################
# This script analyses the responses from the 2nd experiment in the Temporal Expectation project
# This script was written by Mrinmayi Kulkarni (mrinmayi@uwm.edu)
####################################################################################################################################

library(openxlsx)
library(reshape)
library(plyr)
library(readr)
library(ggplot2)


########################## Set Admin variables ##########################

BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalAttention/Experiment/"
DataPath <- paste(BasePath, "Experiment2/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment1/Counterbalancing/", sep = "")

NumBlocks <- 2

#Set plotting variables
xaxistheme <- theme(axis.title.x = element_text(face="bold", size=20), axis.text.x = element_text(colour="#000000", size=18)) #, family="Times"
yaxistheme <- theme(axis.title.y = element_text(face="bold", size=20), axis.text.y = element_text(colour="#000000", size=14))
plottitletheme <- theme(plot.title = element_text(face="bold", size=20, hjust=0.5), legend.key.size=unit(1.3, "cm"))
legendtheme <- theme(legend.text=element_text(face="bold", size=10), legend.title=element_text(face="bold", size=16))
bgtheme <- theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#D6D6D6"), 
                 panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#D6D6D6"))

########################## Functions ##########################

#Get mean, median etc. Good for plotting. This function can be used in conjunction with ddply
#That way you can get the mean, median etc per group/condition. 
SummaryData <- function(df, UseVar){
  M=mean(df[,UseVar], na.rm=TRUE)
  SD <- sd(df[,UseVar], na.rm=TRUE)
  SE <- SD / sqrt(nrow(df))
  LCI <- M - 1.96*SE
  HCI <- M + 1.96*SE
  NumOfRows <- nrow(df)
  data.frame(Mean=M, SD=SD, SE=SE, LCI=LCI, HCI=HCI, Rows=NumOfRows)
}

#Make a simple function that makes sure that there are no NAs in a data frame. It'll be helpful to check if anything got
#screwed up with all the merging. If there are any NAs in any column, check merge will throw out a 
#message indicating which df contains NAs, along with a df with the row and column numbers where the NA belongs
CheckMerge <- function(df) {
  if(any(sapply(df, function(x) sum(is.na(x))))){
    print(sprintf("********************* %s has an na somewhere*********************", deparse(substitute(df))))
    which(is.na(df), arr.ind=TRUE)
  }
  else if(!(any(sapply(df, function(x) sum(is.na(x)))))){
    print(sprintf("%s looks good", deparse(substitute(df))))
  }
}

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

#Make a function that will explicitly halt execution is some of the trials are messed up. Pass the checked
#variables here in a list
CheckTrialNumbers <- function(Vars){
  if(!(all(Vars))){
    stop("Something is wrong in the Trial Numbers. INVESTIGATE!!!!")
  }
}

#

#========================== Work with Log Data (check timings) ==========================

#FileNames <- list.files(path=DataPath, pattern="*.log", full.names=TRUE)

#LogData=c()
#Read in data
#for(Files in FileNames){
#  #[, ColOrd] Orders the columns to put the rando columns at the end of the df
#  EncodeData <- rbind(EncodeData, read_tsv(Files)[, ColOrd])
#}





#========================== Work with Encode Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Encode.txt",
                        full.names=TRUE)

EncodeData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_tsv(Files)
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_",
                             paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  #Add a trial column to the CB
  ThisCB$Trial <- 1:384
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by="Trial", all.x=TRUE, all.y=TRUE)
  
  
  EncodeData <- rbind(EncodeData, PartData)
}

(EncodePerParticipant <- ddply(EncodeData, c("Participant", "Block"), summarise,
                               Trials = length(Participant), 
                               IdealTrials = 192,
                               SC = Trials==IdealTrials))

#Add a column for trial duration
EncodeData <- ddply(EncodeData, c("Participant", "Block"), AddTrialDur)
EncodeData$IdealTrialDur <- EncodeData$ISI+1000
#Check if that matches up with what it should be
EncodeData$TimeDiscrepancy <- EncodeData$IdealTrialDur - EncodeData$TrialDur
EncodeData$TimeProblem <- abs(EncodeData$TimeDiscrepancy)>60

View(EncodeData[EncodeData$TimeProblem==TRUE,])
#Any wrong times that aren't the last trial for a block?
(CheckTime <- all(EncodeData[EncodeData$TimeProblem==TRUE, "Trial"] %in% c(192, 384)))
CheckTrialNumbers(CheckTime)

#========================== Work with Encode Data Ends 



#========================== Work with Test Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Test.txt",
                        full.names=TRUE)

TestData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_tsv(Files)
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Test_",
                           paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  #Add a trial column to the CB
  ThisCB$TrialPres <- 1:288
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by.x="Trial", by.y="TrialPres", all.x=TRUE, all.y=TRUE)
  
  
  TestData <- rbind(TestData, PartData)
}





