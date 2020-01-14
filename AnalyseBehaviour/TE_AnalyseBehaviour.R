####################################################################################################################################
# This script analyses the responses from the 2nd experiment in the Temporal Expectation project
# This script was written by Mrinmayi Kulkarni (mrinmayi@uwm.edu)
####################################################################################################################################

library(reshape)
library(readr)
library(stringr)
library(ez)

########################## Set Admin variables ##########################

#Initialise basic stuff
source("~/GitDir/GeneralScripts/InitialiseR/InitialiseAdminVar.R")

Exp <- 5
ExpName <- paste("Exp", Exp, sep="")
BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/"
DataPath <- paste(BasePath, "Experiment/Experiment", Exp, "/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment/Experiment", Exp, "/Counterbalancing/", sep = "")
OutPath <- paste(BasePath, "Analysis/", sep="")

NumBlocks <- 2
Save=0

FactorLabels <- list("Exp3" = list("Block" = list("levels"=c("TR", "TI"), 
                                                  "labels"=c("Regular", "Irregular")),
                                   "Condition" = list("levels"=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                                      "labels"=c("Old", "Similar: HI", "Similar: LI", "New")),
                                   "Resp" = list("levels"=c("Old", "Similar", "New"),
                                                 "labels"=c(1, 2, 3))),
                     "Exp4" = list("Block" = list("levels"=c("TR", "TI"), 
                                                  "labels"=c("Regular", "Irregular")),
                                   "Condition" = list("levels"=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                                      "labels"=c("Old", "Similar: HI", "Similar: LI", "New")),
                                   "Resp" = list("levels"=c("Old", "Similar", "New"),
                                                 "labels"=c(1, 2, 3))),
                     "Exp5" = list("Block" = list("levels"=c("TR", "TI"), 
                                                  "labels"=c("Regular", "Irregular")),
                                   "Condition" = list("levels"=c("Old", "New"), 
                                                      "labels"=c("Old", "New")),
                                   "Resp" = list("levels"=c("Old", "New"),
                                                 "labels"=c(1, 2))))

#Excluded for incorrect timing, misunderstanding instructions
if(Exp==3){
  toexclude <- c("CB1a_1", "CB3b_2", "CB3b_3", "CB9a_3")
  ObjDur <- 1000
  TotalEncodeTrials <- 192
  TotalTestTrials <- 144
  EncodeColNames <- c("Trial", "Category", "Items", "ListAssignment", "ListType", "Condition", 
                       "ISIType", "Set", "Thirds", "NumPres", "Block", "ISI", "Picture", "ObjectTime")
  ColOrd_Encode <- c("Participant", "ListAssignment", "ListType", "Thirds", "Set", "NumPres", "Trial", "Block", "Condition", 
                     "Category", "Items", "ISIType", "ISI", "Picture", "ObjectTime")
  TestColNames <- c("Trial", "Category", "Items", "ListAssignment", "ListType", "Condition", "Block", "Picture", "ObjectTime", "Resp", "RespTime")
  ColdOrd_Test <- c("Participant", "Block", "ListAssignment", "ListType", "Category", "Condition", "Trial", "Picture", 
                    "Items", "ObjectTime", "Resp", "RespTime")
  NumCond <- 3
}else if(Exp==4){
  toexclude <- c("CB11b_4", "CB11b_5")
  ObjDur <- 700
  TotalEncodeTrials <- 192
  TotalTestTrials <- 144
  EncodeColNames <- c("Trial", "Category", "Items", "ListAssignment", "ListType", "Condition", 
                       "ISIType", "Set", "Thirds", "NumPres", "Block", "ISI", "Picture", "ObjectTime")
  ColOrd_Encode <- c("Participant", "ListAssignment", "ListType", "Thirds", "Set", "NumPres", "Trial", "Block", "Condition", 
                     "Category", "Items", "ISIType", "ISI", "Picture", "ObjectTime")
  TestColNames <- c("Trial", "Category", "Items", "ListAssignment", "ListType", "Condition", "Block", "Picture", "ObjectTime", "Resp", "RespTime")
  ColdOrd_Test <- c("Participant", "Block", "ListAssignment", "ListType", "Category", "Condition", "Trial", "Picture", 
                    "Items", "ObjectTime", "Resp", "RespTime")
  NumCond <- 3
}else if(Exp==5){
  toexclude <- c()
  ObjDur <- 700
  TotalEncodeTrials <- 96
  TotalTestTrials <- 96
  EncodeColNames <- c("Category", "Items", "ListAssignment", "ListType", "Condition", "ISIType", "Set", "Thirds", 
                      "NumPres", "Block", "ISI", "Picture", "Trial", "ObjectTime")
  TestColNames <- c("Category", "Items", "ListAssignment", "ListType", "Condition", "Picture", "Block", "Trial", "ObjectTime",
                    "Resp", "RespTime")
  ColOrd_Encode <- c("Participant", "ListAssignment", "ListType", "Thirds", "Set", "NumPres", "Trial", "Block", "Condition", 
                     "Category", "Items", "ISIType", "ISI", "Picture", "ObjectTime")
  ColdOrd_Test <- c("Participant", "Block", "ListAssignment", "ListType", "Category", "Condition", "Trial", "Picture", 
                    "Items", "ObjectTime", "Resp", "RespTime")
  NumCond <- 2
}
#
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
  if(!(nrow(EncodeLog)==TotalEncodeTrials*2*2)){
    stop(sprintf("Row numbers don't add up for %s!!", str_extract(Files, "[0-9]?[0-9][a-z]_[0-9]")))
  }
  
  #In the object rows add 1000 because the object was always up for 1s
  #The fixation row has the code with all the information from the datasource. So anything WITHOUT ; is the object row
  EncodeLog[-(grep(";", EncodeLog$Picture)), "IdealTime"] <- ObjDur
  #Read in the CB sheet to get time of actual ISIs
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_", str_extract(Files, "[0-9]?[0-9][a-z]"), ".csv", sep=""))
  ##Add time from CB to the rows with fixation
  EncodeLog[grep(";", EncodeLog$Picture), "IdealTime"] <- ThisCB$ISI
  
  #Calculate the time difference. /10 because log files give times in microseconds
  EncodeLog$Dur <- c((diff(EncodeLog$Time))/10, 0)
  EncodeLog$Discrepancy <- EncodeLog$Dur - EncodeLog$IdealTime
  
  #Print warning if there is a discrepancy in any trial that isn't the last trial of the block.
  #The last trial should have 192 as trial number in the event code
  if(!(all(grep(TotalEncodeTrials, EncodeLog[abs(EncodeLog$Discrepancy)>18, "Picture"]) == c(1, 2)))){
    print(sprintf("CAREFUL!!!! Time discrepancy in CB%s", str_extract(Files, "[0-9]?[0-9][a-z]_[0-9]")))
  }
  print(sprintf("CB%s done!!", str_extract(Files, "[0-9]?[0-9][a-z]_[0-9]")))
  LogData <- rbind(LogData, EncodeLog)
}

#========================== Work with Log Data ends



#========================== Work with Encode Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Encode.txt",
                        full.names=TRUE)

EncodeData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_delim(Files, delim=";", skip=1, col_names=FALSE)
  names(PartData) <- EncodeColNames
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  CBName <- str_extract(PartID, "[0-9]?[0-9][a-z]")
  
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_", paste(CBName, collapse=""), ".csv", sep=""))
  #Merge with data
  ThisCB <- merge(ThisCB, PartData, by=c("Trial", "Block"), all.x=TRUE, all.y=TRUE, suffixes=c("_CB", "_Data"))
  
  #Make sure all the information between what is presented and what was supposed to be presented matches up
  #This is just to make sure the changing the name of CB as TE_Encode.txt and TE_Test.txt didn't screw anything up
  if(!(all(all(ThisCB$Picture_CB==ThisCB$Picture_Data) & 
           all(ThisCB$Condition_CB==ThisCB$Condition_Data) & 
           all(ThisCB$ISI_CB==ThisCB$ISI_Data)))){
    stop(sprintf("Data does not match up with CB in %s. INVESTIGATE!!!", PartID))
  }
  EncodeData <- rbind(EncodeData, PartData[, ColOrd_Encode])
}

#Make sure the right number of trials are present for everyone
(EncodePerParticipant <- ddply(EncodeData, c("Participant", "Block"), summarise,
                               Trials = length(Participant), 
                               IdealTrials = TotalEncodeTrials,
                               SC = Trials==IdealTrials))

length(unique(EncodeData$Participant))
(CheckTrials <- all(EncodePerParticipant$SC))
CheckTrialNumbers(CheckTrials)

#Add a column for trial duration
EncodeData <- ddply(EncodeData, c("Participant", "Block"), AddTrialDur)
EncodeData$IdealTrialDur <- EncodeData$ISI+ObjDur
#Check if that matches up with what it should be
EncodeData$TimeDiscrepancy <- EncodeData$IdealTrialDur - EncodeData$TrialDur
EncodeData$TimeProblem <- abs(EncodeData$TimeDiscrepancy)>(17*2)

View(EncodeData[EncodeData$TimeProblem==TRUE,])
#Any wrong times that aren't the last trial for a block?
View(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial==TotalEncodeTrials)), ])
#Get rid of participants that are problematic in terms of timing
toexclude <- c(toexclude, unique(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial==TotalEncodeTrials)), "Participant"]))

EncodeData <- EncodeData[!(EncodeData$Participant %in% toexclude), ]

#========================== Work with Encode Data Ends 



#========================== Work with Test Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Test.txt",
                        full.names=TRUE)

TestData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_delim(Files, delim=";", skip=1, col_names=FALSE)
  names(PartData) <- TestColNames
  
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
  CBName <- str_extract(PartID, "[0-9]?[0-9][a-z]")
  
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Test_", CBName, ".csv", sep=""))
  ThisCB <- merge(ThisCB, PartData, by=c("Trial", "Block"), all.x=TRUE, all.y=TRUE, suffixes=c("_CB", "_Data"))
  
  #Make sure all the information between what is presented and what was supposed to be presented matches up
  #This is just to make sure the changing the name of CB as TE_Encode.txt and TE_Test.txt didn't screw anything up
  if(!(all(all(ThisCB$Picture_CB==ThisCB$Picture_Data) & 
           all(ThisCB$Condition_CB==ThisCB$Condition_Data) & 
           all(ThisCB$ISI_CB==ThisCB$ISI_Data)))){
    stop(sprintf("Data does not match up with CB in %s. INVESTIGATE!!!", PartID))
  }

  TestData <- rbind(TestData, PartData[, ColdOrd_Test])
}

(TestPerParticipant <- ddply(TestData, c("Participant", "Block"), summarise,
                             Trials = length(Participant), 
                             IdealTrials = TotalTestTrials,
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
TestData$CorrCode <- factor(TestData$ListType, levels=FactorLabels[[ExpName]]$Resp$levels, labels=FactorLabels[[ExpName]]$Resp$labels)

####Figure out bad subjects
#Based on accuracy
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
LowCutoffAcc  <- quantile(PartAcc$PercAcc, 0.25)-(2*IQR(PartAcc$PercAcc))
HighCutoffAcc  <- quantile(PartAcc$PercAcc, 0.75)+(2*IQR(PartAcc$PercAcc))

PartAcc$Exclude <- (PartAcc$PercAcc<=LowCutoffAcc) #| (PartAcc$PercAcc>=(TotalAcc$Mean+(2*TotalAcc$SD)))
toexclude <- c(toexclude, PartAcc[PartAcc$Exclude==TRUE, "Participant"])

#Based on RT
#Calculate RT
TestData$RT <- TestData$RespTime-TestData$ObjectTime
TestData$ExcludeTrials <- FALSE
TestData[(TestData$RT<100 | TestData$RT>3000), "ExcludeTrials"] <- TRUE
CheckMerge(TestData)

(PartRT <- ddply(TestData, c("Participant"), summarise, 
                 ExTrials=sum(ExcludeTrials), 
                 IncTrials=sum(!ExcludeTrials),
                 TotalBehTrials=sum(ExTrials, IncTrials),
                 IdealTrials=length(Participant),
                 PercEx=(ExTrials/TotalBehTrials)*100,
                 SC=TotalBehTrials==IdealTrials))

TotalExRT <- SummaryData(PartRT, "PercEx")
LowCutoffRT  <- quantile(PartRT$PercEx, 0.25)-(2*IQR(PartRT$PercEx))
HighCutoffRT  <- quantile(PartRT$PercEx, 0.75)+(2*IQR(PartRT$PercEx))

PartRT$Exclude <- (PartRT$PercEx>=HighCutoffRT) #| (PartAcc$PercAcc>=(TotalAcc$Mean+(2*TotalAcc$SD)))
toexclude <- c(toexclude, PartRT[PartRT$Exclude==TRUE, "Participant"])


##### Look at accuracy

#Remove participants whose accuracy is too low or too high
TestGoodData <- TestData[!(TestData$Participant %in% toexclude), ]

#Save out how many trials are excluded to make sure the total number of trials is correct
NumExcludedTrials <- ddply(TestGoodData, c("Participant", "Block", "Condition"), summarise, 
                           NumExcludedTrials=sum(ExcludeTrials))
TestGoodData <- TestGoodData[TestGoodData$ExcludeTrials==FALSE, ]
unique(TestGoodData$Participant)
length(unique(TestGoodData$Participant))

#Collapse across trials
TestAcc <- ddply(TestGoodData, c("Participant", "Block", "Condition"), summarise, 
                 BehAcc=sum(Acc), 
                 BehNAcc=sum(!Acc),
                 TotalGoodTrials=sum(BehAcc, BehNAcc),
                 IdealTrials=length(Participant),
                 PercAcc=(BehAcc/TotalGoodTrials)*100,
                 SC=TotalGoodTrials==IdealTrials)
#Merge with number of trials of excluded
TestAcc <- merge(TestAcc, NumExcludedTrials, by=c("Participant", "Block", "Condition"), all.x=TRUE, all.y=TRUE)
TestAcc$TotalBehTrials <- TestAcc$BehAcc+TestAcc$BehNAcc+TestAcc$NumExcludedTrials

(CheckSumTrials <- all(TestAcc$SC))
(CheckOldNewTrials <- all(TestAcc[TestAcc$Condition %in% c("Old", "New"), "TotalBehTrials"]==48))
(CheckSimTrials <- all(TestAcc[TestAcc$Condition %in% c("Similar_HI", "Similar_LI"), "TotalBehTrials"]==24))
CheckTrialNumbers(c(CheckSumTrials, CheckOldNewTrials, CheckSimTrials))

#Collapse across Participants
SummaryTestAcc <- ddply(TestAcc, c("Block", "Condition"), SummaryData, "PercAcc")
SummaryTestAcc$Block <- factor(SummaryTestAcc$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)
SummaryTestAcc$Condition <- factor(SummaryTestAcc$Condition, 
                                   levels=FactorLabels[[ExpName]]$Condition$levels, 
                                   labels=FactorLabels[[ExpName]]$Condition$labels)

TestAccBar <- ggplot(data=SummaryTestAcc, aes(x=Condition, y=Mean, fill=Block)) +
  stdbar + coord_cartesian(ylim=c(0, 95)) +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  labs(x="Object Type", y="Percent Correct", fill="Condition") + 
  geom_hline(yintercept = 100/NumCond, linetype="dashed", size=1) + 
  xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme

#Do stats on it
Acc_ANOVA <- ezANOVA(data=TestAcc, dv=PercAcc, wid=Participant, within=c(Block, Condition), 
                     detailed=TRUE, type=2)
Acc_ANOVA$ANOVA

if(Save==1){
  jpeg(filename=sprintf("%s/TestAccBar_Exp3.jpeg", OutPath), 
       width=2500, height=2000, res=300)
  plot(TestAccBar)
  dev.off()
}

##### Look at RT now

unique(TestGoodData$Participant)
length(unique(TestGoodData$Participant))

#Collapse RT across trials
TestRT <- ddply(TestGoodData, c("Participant", "Block", "Condition"), SummaryData, "RT")
#Collapse across participants
SummaryTestRT <- ddply(TestRT, c("Block", "Condition"), SummaryData, "Mean")
SummaryTestRT$Block <- factor(SummaryTestRT$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)
SummaryTestRT$Condition <- factor(SummaryTestRT$Condition, 
                                   levels=FactorLabels[[ExpName]]$Condition$levels, 
                                   labels=FactorLabels[[ExpName]]$Condition$labels)

TestRTBar <- ggplot(data=SummaryTestRT, aes(x=Condition, y=Mean, fill=Block)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  labs(x="Object Type", y="Reaction Time", fill="Condition") + 
  xaxistheme + yaxistheme + plottitletheme + legendtheme+ canvastheme + blankbgtheme

#Do stats on it
RT_ANOVA <- ezANOVA(data=TestRT, dv=Mean, wid=Participant, within=c(Block, Condition), 
                     detailed=TRUE, type=2)
RT_ANOVA$ANOVA

if(Save==1){
  jpeg(filename=sprintf("%s/Presentations/Psychonomics2019/Poster/TestRTBar_Exp3.jpeg", BasePath), 
       width=2500, height=2000, res=300)
  plot(TestRTBar)
  dev.off()
}

##### Calculate hits, FAs #####

TestGoodData[TestGoodData$ListType=="Old" & TestGoodData$Resp==1, "RespType"] <- "Hit"
if(Exp %in% c(3, 4)){
  TestGoodData[TestGoodData$ListType=="Similar" & TestGoodData$Resp==2, "RespType"] <- "CR"
  TestGoodData[TestGoodData$ListType=="New" & TestGoodData$Resp==3, "RespType"] <- "CR"
  
  TestGoodData[TestGoodData$ListType=="Old" & (TestGoodData$Resp %in% c(2, 3)), "RespType"] <- "Miss"
  TestGoodData[(TestGoodData$ListType %in% c("Similar", "New")) & TestGoodData$Resp==1, "RespType"] <- "FA"
  
  TestGoodData[(TestGoodData$ListType %in% c("Similar")) & TestGoodData$Resp==3, "RespType"] <- "Incorr"
  TestGoodData[(TestGoodData$ListType %in% c("New")) & TestGoodData$Resp==2, "RespType"] <- "Incorr"
}else if(Exp==5){
  TestGoodData[TestGoodData$ListType=="New" & TestGoodData$Resp==2, "RespType"] <- "CR"
  
  TestGoodData[TestGoodData$ListType=="Old" & TestGoodData$Resp==2, "RespType"] <- "Miss"
  TestGoodData[TestGoodData$ListType=="New" & TestGoodData$Resp==1, "RespType"] <- "FA"
}

CheckMerge(TestGoodData)

#drop=FALSE makes sure that every level of the independent variables will be accounted for
#It's not ideal because it will make a row for Old and Correct Rejection and so on. These trials need to be removed manually
PropResp <- ddply(TestGoodData, c("Participant", "Condition", "RespType", "Block"), .drop=FALSE, summarise, SumResp=length(RespType))
PropResp <- PropResp[which((PropResp$Condition=="Old" & PropResp$RespType %in% c("Hit", "Miss")) |
                           (PropResp$Condition=="New" & PropResp$RespType %in% c("CR", "FA", "Incorr")) | 
                           (PropResp$Condition=="Similar_LI" & PropResp$RespType %in% c("CR", "FA", "Incorr")) |
                           (PropResp$Condition=="Similar_HI" & PropResp$RespType %in% c("CR", "FA", "Incorr"))), ]



TotalTrials <- ddply (TestGoodData, c("Participant", "Condition", "Block"), summarise, TotalTrials=length(ListType))
PropResp <- merge(PropResp, TotalTrials, by=c("Participant", "Condition", "Block"), all.x=TRUE, all.y=TRUE)
CheckMerge(PropResp)

PropResp$PropResp <- PropResp$SumResp/PropResp$TotalTrials
SumProp <- ddply(PropResp, c("Participant", "Condition", "Block"), summarise, SumProp=sum(PropResp))
#Make sure that proportions add up to 1
(CheckTotalProp <- all(round(SumProp$SumProp, 1)==1))
CheckTrialNumbers(CheckTotalProp)

SummaryPropResp <- ddply(PropResp, c("Condition", "RespType", "Block"), SummaryData, "PropResp")
SummaryPropResp$CondType <- paste(SummaryPropResp$Condition, SummaryPropResp$RespType, sep="")

SummaryPropResp_Plot <- SummaryPropResp[SummaryPropResp$RespType %in% c("FA", "Hit"), ]
SummaryPropResp_Plot$CondType <- factor(SummaryPropResp_Plot$CondType, levels=c("OldHit", "Similar_HIFA", "Similar_LIFA", "NewFA"),
                                        labels=c("Hits", "False Alarm: \nSimilar HI", "False Alarm: \nSimilar LI", "False Alarm: \n New"))
SummaryPropResp_Plot$Block <- factor(SummaryPropResp_Plot$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)


PropRespBar <- ggplot(data=SummaryPropResp_Plot, aes(x=CondType, y=Mean, fill=Block)) +
  stdbar + 
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  labs(x="Response Type", y="Mean", fill="Condition") +
  xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme


PropResp_AOV <- PropResp[PropResp$RespType %in% c("FA"),]

#Do stats on it
if(Exp==5){
  PropResp_ANOVA <- ezANOVA(data=PropResp_AOV, dv=PropResp, wid=Participant, within=c(Block), 
                            detailed=TRUE, type=2)
}else{
  PropResp_ANOVA <- ezANOVA(data=PropResp_AOV, dv=PropResp, wid=Participant, within=c(Block, Condition), 
                    detailed=TRUE, type=2)
}
PropResp_ANOVA$ANOVA

if(Save==1){
  jpeg(filename=sprintf("%s/Presentations/Psychonomics2019/Poster/PropRespBar_Exp3.jpeg", BasePath), 
       width=2500, height=2000, res=300)
  plot(PropRespBar)
  dev.off()
}

#Only look at new and old trials to replicate analysis from Ward & Jones (2019)
#PropResp_NoSim <- PropResp[PropResp$Condition %in% c("Old", "New"), ]
if(Exp==5){
  CorrReg <- ddply(PropResp, c("Participant", "Block"), summarise, 
                   CorrReg_New=PropResp[Condition=="Old" & RespType=="Hit"]-PropResp[Condition=="New" & RespType=="FA"])
}else{
  CorrReg <- ddply(PropResp, c("Participant", "Block"), summarise, 
                       CorrReg_New=PropResp[Condition=="Old" & RespType=="Hit"]-PropResp[Condition=="New" & RespType=="FA"],
                       CorrReg_SimHI=PropResp[Condition=="Old" & RespType=="Hit"]-PropResp[Condition=="Similar_HI" & RespType=="FA"],
                       CorrReg_SimLI=PropResp[Condition=="Old" & RespType=="Hit"]-PropResp[Condition=="Similar_LI" & RespType=="FA"])
}

SummaryCorrReg_NoSim <- ddply(CorrReg, c("Block"), SummaryData, "CorrReg_New")
SummaryCorrReg_NoSim$Block <- factor(SummaryCorrReg_NoSim$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)

CorrReg_NoSimBar <- ggplot(data=SummaryCorrReg_NoSim, aes(x=Block, y=Mean, fill=Block)) +
  stdbar + coord_cartesian(ylim=c(0, 0.85)) + 
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  labs(x="Condition", y="Hits – False Alarms") +
  xaxistheme + yaxistheme + bgtheme + plottitletheme + legendtheme + theme(legend.position = "None") + canvastheme

if(Save==1){
  jpeg(filename=sprintf("%s/Presentations/Psychonomics2019/Poster/CorrReg_NoSimBar_Exp3.jpeg", BasePath), 
       width=1500, height=2000, res=300)
  plot(CorrReg_NoSimBar)
  dev.off()
}


#Look at corrected recognition in similar
CorrReg_Long <- melt(CorrReg)

SummaryCorrReg <- ddply(CorrReg_Long, c("Block", "variable"), SummaryData, "value")
SummaryCorrReg$Block <- factor(SummaryCorrReg$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)
if(Exp != 5){
  SummaryCorrReg$variable <- factor(SummaryCorrReg$variable, levels=c("CorrReg_SimHI", "CorrReg_SimLI", "CorrReg_New"), 
                                    labels=FactorLabels[[ExpName]]$Condition$labels[2:4])
}

CorrRegBar <- ggplot(data=SummaryCorrReg, aes(x=variable, y=Mean, fill=Block)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  labs(x="Condition", y="Hits - False Alarms") +
  xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme


#Do stats on it
if(Exp==5){
  CorrReg_ANOVA <- ezANOVA(data=CorrReg_Long, dv=value, wid=Participant, within=c(Block), 
                           detailed=TRUE, type=2)
}else{
  CorrReg_ANOVA <- ezANOVA(data=CorrReg_Long, dv=value, wid=Participant, within=c(Block, variable), 
                           detailed=TRUE, type=2)
}
CorrReg_ANOVA$ANOVA



##### Look at accuracy by quarts #####
#Merge with encoding so that information about when the item was presented during encoding is included
ThirdsData <- merge(TestData, EncodeData[, c("Participant", "Block", "Condition", "Items", "Thirds")], 
                      by=c("Participant", "Block", "Condition", "Items"), all.x=TRUE, all.y=TRUE)
all(ThirdsData[as.data.frame(CheckMerge(ThirdsData))$row, "Condition"]=="New")
#Get rid of bad trials and participants
#Easier to do it this was than merge with TestGoodData and deal with all the NAs
ThirdsData <- ThirdsData[!(ThirdsData$Participant %in% toexclude), ]
ThirdsData <- ThirdsData[ThirdsData$ExcludeTrials==FALSE, ]
#Fill the thirds with New with new because these items were not presented during encoding
ThirdsData[ThirdsData$Condition=="New", "Thirds"] <- "New"
CheckMerge(ThirdsData)

ThirdsAcc <- ddply(ThirdsData, c("Participant", "Block", "Condition", "Thirds"), summarise, 
                   BehAcc=sum(Acc), 
                   BehNAcc=sum(!Acc),
                   TotalGoodTrials=sum(BehAcc, BehNAcc),
                   IdealTrials=length(Participant),
                   PercAcc=(BehAcc/TotalGoodTrials)*100,
                   SC=TotalGoodTrials==IdealTrials)
all(ThirdsAcc$SC)
#Collapse across participants
SummaryThirdsAcc <- ddply(ThirdsAcc, c("Block", "Condition", "Thirds"), SummaryData, "PercAcc")
SummaryThirdsAcc$Block <- factor(SummaryThirdsAcc$Block, FactorLabels[[ExpName]]$Block$levels,
                                 FactorLabels[[ExpName]]$Block$labels)
SummaryThirdsAcc$Condition <- factor(SummaryThirdsAcc$Condition, FactorLabels[[ExpName]]$Condition$levels,
                                     FactorLabels[[ExpName]]$Condition$labels)

for(i in 1:(length(FactorLabels[[ExpName]]$Condition$labels))-1){
  Cond <- FactorLabels[[ExpName]]$Condition$labels[i]
  SummaryThirdsAcc_Cond <- SummaryThirdsAcc[SummaryThirdsAcc$Condition %in% c("New", Cond),]
  
  ThirdsCondLine <- ggplot(data=SummaryThirdsAcc_Cond, aes(x=Thirds, y=Mean, group=Block)) +
    geom_point() + geom_line(aes(colour=Block), size=1.2) + 
    #geom_segment(aes(x = 5, y = 40, xend = 5, yend = 85)) + 
    geom_hline(aes(yintercept=33), linetype="dashed", size=1) +
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9) +
    coord_cartesian(ylim=c(25, 95)) +
    ggtitle(Cond) + 
    scale_linetype_manual(values=c("twodash", "dotted")) +
    scale_color_manual(values=c("#FFC2A3", "#123C69")) +
    labs(x="Thirds", y="Accuracy", colour="Block") + 
    xaxistheme + yaxistheme + plottitletheme + legendtheme + blankbgtheme
  
  assign(paste("ThirdsLine_", FactorLabels[[ExpName]]$Condition$levels[i], sep=""),
         ThirdsCondLine)
}








