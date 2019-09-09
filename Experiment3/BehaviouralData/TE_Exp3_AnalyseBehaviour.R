####################################################################################################################################
# This script analyses the responses from the 2nd experiment in the Temporal Expectation project
# This script was written by Mrinmayi Kulkarni (mrinmayi@uwm.edu)
####################################################################################################################################

library(reshape)
library(readr)
library(stringr)

########################## Set Admin variables ##########################

#Initialise basic stuff
source("~/GitDir/GeneralScripts/InitialiseR/IntialiseAdminVar.R")

BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/Experiment/"
DataPath <- paste(BasePath, "Experiment3/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment3/Counterbalancing/", sep = "")

NumBlocks <- 2

FactorLabels <- list("Block" = list("levels"=c("TR", "TI"), 
                                    "labels"=c("Regular", "Irregular")),
                     "Condition" = list("levels"=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                        "labels"=c("Old", "Similar: HI", "Similar: LI", "New")))
#Excluded for incorrect timing, misunderstanding instructions
toexclude <- c(CB1a_1, CB3b_2)

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
  #Read in file and only keep first 5 columns
  PartData <- read.table(Files, skip=6, sep="\t", blank.lines.skip=TRUE, header=FALSE, fill=TRUE)[, 1:5]
  PartData <- PartData %>% dplyr::rename(Participant=V1, Trial=V2, Event=V3, Picture=V4, Time=V5)
  
  #Get rid of the trials where a response was made...
  EncodeLog <- PartData[!(PartData$Event %in% c("Response", "")), ]
  #...And any test trials to check times. The code was TestFix. and then get rid of one trial after that (the picture)
  EncodeLog <- EncodeLog[-(sort(c(which(EncodeLog$Picture=="TestFix"), (which(EncodeLog$Picture=="TestFix")+1)))), ]
  
  #This should leave us with only the encode trials 192*2*2= 192 trials* 2 blocks * 2 rows per trial (Fix+object)
  #If this is not correct halt the execution
  if(!(nrow(EncodeLog)==192*2*2)){
    stop(sprintf("Row numbers don't add up for %s!!", str_extract(Files, "[0-9][a-z]_[0-9]")))
  }
  
  #In the object rows add 1000 because the object was always up for 1s
  #The fixation row has the code with all the information from the datasource. So anything WITHOUT ; is the object row
  EncodeLog[-(grep(";", EncodeLog$Picture)), "IdealTime"] <- 1000
  #Read in the CB sheet to get time of actual ISIs
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_", str_extract(Files, "[0-9][a-z]"), ".csv", sep=""))
  ##Add time from CB to the rows with fixation
  EncodeLog[grep(";", EncodeLog$Picture), "IdealTime"] <- ThisCB$ISI
  
  #Calculate the time difference. /10 because log files give times in microseconds
  EncodeLog$Dur <- c((diff(EncodeLog$Time))/10, 0)
  EncodeLog$Discrepancy <- EncodeLog$Dur - EncodeLog$IdealTime
  
  #Print warning if there is a discrepancy in any trial that isn't the last trial of the block.
  #The last trial should have 192 as trial number in the event code
  if(!(all(grep("192", EncodeLog[abs(EncodeLog$Discrepancy)>18, "Picture"]) == c(1, 2)))){
    print(sprintf("CAREFUL!!!! Time discrepancy in CB%s", str_extract(Files, "[0-9][a-z]_[0-9]")))
  }
  print(sprintf("CB%s done!!", str_extract(Files, "[0-9][a-z]_[0-9]")))
}

#========================== Work with Log Data ends



#========================== Work with Encode Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Encode.txt",
                        full.names=TRUE)

ColOrd <- c("Participant", "ListAssignment", "ListType", "Thirds", "Set", "NumPres", "Trial", "Block", "Condition", "Category", 
            "Items", "ISIType", "ISI", "Picture", "ObjectTime")


EncodeData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_delim(Files, delim=";", skip=1, col_names=FALSE)
  names(PartData) <- c("Trial", "Category", "Items", "ListAssignment", "ListType", "Condition", 
                       "ISIType", "Set", "Thirds", "NumPres", "Block", "ISI", "Picture", "ObjectTime")
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_", paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  #Merge with data
  ThisCB <- merge(ThisCB, PartData, by=c("Trial", "Block"), all.x=TRUE, all.y=TRUE, suffixes=c("_CB", "_Data"))
  
  #Make sure all the information between what is presented and what was supposed to be presented matches up
  #This is just to make sure the changing the name of CB as TE_Encode.txt and TE_Test.txt didn't screw anything up
  if(!(all(all(ThisCB$Picture_CB==ThisCB$Picture_Data) & 
           all(ThisCB$Condition_CB==ThisCB$Condition_Data) & 
           all(ThisCB$ISI_CB==ThisCB$ISI_Data)))){
    stop(sprintf("Data does not match up with CB in %s. INVESTIGATE!!!", PartID))
  }
  EncodeData <- rbind(EncodeData, PartData[, ColOrd])
}

#Make sure the right number of trials are present for everyone
(EncodePerParticipant <- ddply(EncodeData, c("Participant", "Block"), summarise,
                               Trials = length(Participant), 
                               IdealTrials = 192,
                               SC = Trials==IdealTrials))

length(unique(EncodeData$Participant))
(CheckTrials <- all(EncodePerParticipant$SC))
CheckTrialNumbers(CheckTrials)

#Add a column for trial duration
EncodeData <- ddply(EncodeData, c("Participant", "Block"), AddTrialDur)
EncodeData$IdealTrialDur <- EncodeData$ISI+1000
#Check if that matches up with what it should be
EncodeData$TimeDiscrepancy <- EncodeData$IdealTrialDur - EncodeData$TrialDur
EncodeData$TimeProblem <- abs(EncodeData$TimeDiscrepancy)>(17*2)

View(EncodeData[EncodeData$TimeProblem==TRUE,])
#Any wrong times that aren't the last trial for a block?
View(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial==192)), ])
#Get rid of participants that are problematic in terms of timing
toexclude <- c(toexclude, unique(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial==192)), "Participant"]))

EncodeData <- EncodeData[!(EncodeData$Participant %in% toexclude), ]

#========================== Work with Encode Data Ends 



#========================== Work with Test Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Test.txt",
                        full.names=TRUE)

ColdOrd <- c("Participant", "Block", "ListAssignment", "ListType", "Category", "Condition", "Trial", "Picture", 
             "Items", "ObjectTime", "Resp", "RespTime")

TestData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_delim(Files, delim=";", skip=0, col_names=TRUE)
  
#  #Coding this "absolutely" to make sure it's not removing any random trials
#  if(all(PartData[143:144, "Items"]==PartData[145:146, "Items"])){
#    #Are the answers to the repeated the same?
#    SameResp <- cbind(SameResp, (PartData[143:144, "Resp"]==PartData[145:146, "Resp"]))
#    #Is the response the second time around always new (because the object hadn't occurred in the corresponding block)?
#    SecRespNew <- cbind(SecRespNew, PartData[145:146, "Resp"]==3)
#    PartData <- PartData[-(145:146),]
#  }
  
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Test_", paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  ThisCB <- merge(ThisCB, PartData, by=c("Trial", "Block"), all.x=TRUE, all.y=TRUE, suffixes=c("_CB", "_Data"))
  
  #Make sure all the information between what is presented and what was supposed to be presented matches up
  #This is just to make sure the changing the name of CB as TE_Encode.txt and TE_Test.txt didn't screw anything up
  if(!(all(all(ThisCB$Picture_CB==ThisCB$Picture_Data) & 
           all(ThisCB$Condition_CB==ThisCB$Condition_Data) & 
           all(ThisCB$ISI_CB==ThisCB$ISI_Data)))){
    stop(sprintf("Data does not match up with CB in %s. INVESTIGATE!!!", PartID))
  }

  TestData <- rbind(TestData, PartData[, ColdOrd])
}

(TestPerParticipant <- ddply(TestData, c("Participant", "Block"), summarise,
                             Trials = length(Participant), 
                             IdealTrials = 144,
                             SC = Trials==IdealTrials))

(CheckTrials <- all(TestPerParticipant$SC))
CheckTrialNumbers(CheckTrials)

#Exclude the problematic participants from test
TestData <- TestData[!(TestData$Participant %in% toexclude), ]
(CheckParts <- all(unique(TestData$Participant) %in% unique(EncodeData$Participant)))
CheckTrialNumbers(CheckParts)

#Just order the rows
TestData <- TestData[order(TestData$Participant, TestData$Block, TestData$Trial), ]
#Get the codes of what the correct answer should be
TestData$CorrCode <- factor(TestData$ListType, levels=c("Old", "Similar", "New"), labels=c(1, 2, 3))

##### Look at accuracy
TestData$CorrCode <- factor(TestData$ListType, levels=c("Old", "Similar", "New"), labels=c(1, 2, 3))
#Are responses correct?
TestData$Acc <- TestData$Resp==TestData$CorrCode

(PartAcc <- ddply(TestData, c("Participant"), summarise, 
                  BehAcc=sum(Acc), 
                  BehNAcc=sum(!Acc),
                  TotalBehTrials=sum(BehAcc, BehNAcc),
                  IdealTrials=length(Participant),
                  PercAcc=(BehAcc/TotalBehTrials)*100,
                  SC=TotalBehTrials==IdealTrials))
length(unique(PartAcc$Participant))

TotalAcc <- SummaryData(PartAcc, "PercAcc")

PartAcc$Exclude <- (PartAcc$PercAcc<=(TotalAcc$Mean-(2*TotalAcc$SD))) | (PartAcc$PercAcc>=(TotalAcc$Mean+(2*TotalAcc$SD)))
toexclude <- c(toexclude, PartAcc[PartAcc$Exclude==TRUE, "Participant"])

#Remove participants whose accuracy is too low or too high
TestGoodData <- TestData[!(TestData$Participant %in% toexclude), ]
unique(TestGoodData$Participant)
length(unique(TestGoodData$Participant))

#Collapse across trials
TestAcc <- ddply(TestGoodData, c("Participant", "Block", "Condition"), summarise, 
                 BehAcc=sum(Acc), 
                 BehNAcc=sum(!Acc),
                 TotalBehTrials=sum(BehAcc, BehNAcc),
                 IdealTrials=length(Participant),
                 PercAcc=(BehAcc/TotalBehTrials)*100,
                 SC=TotalBehTrials==IdealTrials)
(CheckSumTrials <- all(TestAcc$SC))
(CheckOldNewTrials <- all(TestAcc[TestAcc$Condition %in% c("Old", "New"), "TotalBehTrials"]==48))
(CheckSimTrials <- all(TestAcc[TestAcc$Condition %in% c("Similar_HI", "Similar_LI"), "TotalBehTrials"]==24))
CheckTrialNumbers(c(CheckSumTrials, CheckOldNewTrials, CheckSimTrials))

#Collapse across Participants
SummaryTestAcc <- ddply(TestAcc, c("Block", "Condition"), SummaryData, "PercAcc")
SummaryTestAcc$Block <- factor(SummaryTestAcc$Block, levels=FactorLabels$Block$levels, labels=FactorLabels$Block$labels)
SummaryTestAcc$Condition <- factor(SummaryTestAcc$Condition, 
                                   levels=FactorLabels$Condition$levels, 
                                   labels=FactorLabels$Condition$labels)

TestAccBar <- ggplot(data=SummaryTestAcc, aes(x=Block, y=Mean, fill=Condition)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#00185C", "#D0902B", "#F1D4A6", "#CA2F2F"),
                    breaks=FactorLabels$Condition$labels, 
                    labels=FactorLabels$Condition$labels) + 
  labs(x="Condition", y="Accuracy", fill="Object Type") + 
  geom_hline(yintercept = 100/4, linetype="dashed", size=1) + 
  xaxistheme + yaxistheme + bgtheme + plottitletheme + legendtheme

##### Look at RT now

unique(TestData$Participant)
length(unique(TestData$Participant))

#Calculate RT
TestData$RT <- TestData$RespTime-TestData$ObjectTime
#Collapse RT across trials
TestRT <- ddply(TestData, c("Participant", "Block", "Condition"), SummaryData, "RT")
#Collapse across participants
SummaryTestRT <- ddply(TestRT, c("Block", "Condition"), SummaryData, "Mean")
SummaryTestRT$Block <- factor(SummaryTestRT$Block, levels=FactorLabels$Block$levels, labels=FactorLabels$Block$labels)
SummaryTestRT$Condition <- factor(SummaryTestRT$Condition, 
                                   levels=FactorLabels$Condition$levels, 
                                   labels=FactorLabels$Condition$labels)

TestRTBar <- ggplot(data=SummaryTestRT, aes(x=Block, y=Mean, fill=Condition)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#00185C", "#D0902B", "#F1D4A6", "#CA2F2F"),
                    breaks=FactorLabels$Condition$labels, 
                    labels=FactorLabels$Condition$labels) + 
  labs(x="Condition", y="RT", fill="Object Type") +
  xaxistheme + yaxistheme + bgtheme + plottitletheme + legendtheme





