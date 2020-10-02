####################################################################################################################################
# This script analyses the responses from the 2nd experiment in the Temporal Expectation project
# This script was written by Mrinmayi Kulkarni (mrinmayi@uwm.edu)
####################################################################################################################################

library(reshape)
library(readr)
library(stringr)
library(ez)
library(BayesFactor)
library(effsize)

########################## Set Admin variables ##########################

#Initialise basic stuff
source("~/GitDir/GeneralScripts/InitialiseR/InitialiseAdminVar.R")
source("~/GitDir/GeneralScripts/InitialiseR/InitialiseStatsFunc.R")

Exp <- 5
ExpName <- paste("Exp", Exp, sep="")
BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/"
DataPath <- paste(BasePath, "Experiment/Experiment", Exp, "/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment/Experiment", Exp, "/Counterbalancing/", sep = "")
OutPath <- paste(BasePath, "Analysis/", sep="")

NumBlocks <- 2
Save=0
ExcludeTrials <- FALSE

FactorLabels <- list("Exp3" = list("Block" = list("levels"=c("TR", "TI"), 
                                                  "labels"=c("Regular", "Irregular")),
                                   "Condition" = list("levels"=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                                      "labels"=c("Old", "Similar: HS", "Similar: LS", "New")),
                                   "Resp" = list("levels"=c("Old", "Similar", "New"),
                                                 "labels"=c(1, 2, 3))),
                     "Exp4" = list("Block" = list("levels"=c("TR", "TI"), 
                                                  "labels"=c("Regular", "Irregular")),
                                   "Condition" = list("levels"=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                                      "labels"=c("Old", "Similar: HS", "Similar: LS", "New")),
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
  MatchString <- sprintf("^%s", TotalEncodeTrials)
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
  MatchString <- sprintf("^%s", TotalEncodeTrials)
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
  MatchString <- sprintf("%s$", TotalEncodeTrials)
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
  #The $ in grep matches 96 only when it is at the end of the line
  if(!(all(grep(MatchString, EncodeLog[abs(EncodeLog$Discrepancy)>18, "Picture"]) == c(1, 2)))){
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
if(Exp==4){
  #This person id being taken out accidentally, so put them back
  toexclude <- toexclude[toexclude != "CB9b_4"]
}

EncodeData <- EncodeData[!(EncodeData$Participant %in% toexclude), ]
unique(EncodeData$Participant)
length(unique(EncodeData$Participant))


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
any(PartAcc$Exclude)
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
any(PartRT$Exclude)
#This is just to look at RT distribution. We are not excluding people for it
#toexclude <- c(toexclude, PartRT[PartRT$Exclude==TRUE, "Participant"])


##### Look at accuracy

#Remove participants whose accuracy is too low or too high
TestGoodData <- TestData[!(TestData$Participant %in% toexclude), ]

#Save out how many trials are excluded to make sure the total number of trials is correct
NumExcludedTrials <- ddply(TestGoodData, c("Participant", "Block", "Condition"), summarise, 
                           NumExcludedTrials=sum(ExcludeTrials))
if(ExcludeTrials==TRUE){
  TestGoodData <- TestGoodData[TestGoodData$ExcludeTrials==FALSE, ]
}else if(ExcludeTrials==FALSE){
  TestGoodData <- TestGoodData
}
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
if(ExcludeTrials==TRUE){
  (CheckOldNewTrials <- all(TestAcc[TestAcc$Condition %in% c("Old", "New"), "TotalBehTrials"]==48))
  (CheckSimTrials <- all(TestAcc[TestAcc$Condition %in% c("Similar_HI", "Similar_LI"), "TotalBehTrials"]==24))
}else if(ExcludeTrials==FALSE){
  (CheckOldNewTrials <- all(TestAcc[TestAcc$Condition %in% c("Old", "New"), "TotalGoodTrials"]==48))
  (CheckSimTrials <- all(TestAcc[TestAcc$Condition %in% c("Similar_HI", "Similar_LI"), "TotalGoodTrials"]==24))
}
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

##### Look at RT now #####

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
                     detailed=TRUE, type=3)
RT_ANOVA$ANOVA
#Another way:
#summary(aov(formula=Mean~(Block*Condition)+Error(Participant/(Block*Condition)), data=TestRT))

#Do post-hoc tests
TestRT_byCond <- ddply(TestGoodData, c("Participant", "Condition"), SummaryData, "RT")
RT_PostHoc <- c()
for(cond in FactorLabels[[ExpName]]$Condition$levels){
  #What should this condition be compared to?
  compareto <- FactorLabels[[ExpName]]$Condition$levels[(FactorLabels[[ExpName]]$Condition$levels != cond)]
  for(comp in compareto){
    phtest <- twosample_ttest(grp1=TestRT_byCond[TestRT_byCond$Condition==cond, "Mean"],
                              grp2=TestRT_byCond[TestRT_byCond$Condition==comp, "Mean"],
                              paired=TRUE)
    
    #For manual cohen's d calculation
    RTdiff <- ddply(TestRT_byCond, c("Participant"), summarise, RTDiff=Mean[Condition==cond]-Mean[Condition==comp])
    #For appropriate manual calculation for paired sample sizes, we need the correlation between the two groups:
    #http://www.real-statistics.com/students-t-distribution/paired-sample-t-test/cohens-d-paired-samples/
    r <- cor(TestRT_byCond[TestRT_byCond$Condition==cond, "Mean"], TestRT_byCond[TestRT_byCond$Condition==comp, "Mean"])
    
    CohensD <- cohen.d(formula=Mean~Condition | Subject(Participant), 
                       data=TestRT_byCond[TestRT_byCond$Condition %in% c(comp,cond), ], paired=TRUE, pooled=TRUE,
                       within=TRUE)
    RT_PostHoc <- rbind(RT_PostHoc, data.frame(X=cond, Y=comp, 
                                               W1=phtest$shapiro1$statistic,
                                               W.p1=phtest$shapiro1$p.value,
                                               W2=phtest$shapiro2$statistic,
                                               W.p2=phtest$shapiro2$p.value,
                                               t=phtest$ttest$statistic,
                                               t.df=phtest$ttest$parameter,
                                               t.p=phtest$ttest$p.value,
                                               d_effsize=CohensD$estimate,
                                               d_srm=mean(RTdiff$RTDiff)/(sd(RTdiff$RTDiff)/sqrt(2*(1-r))),
                                               d_diff=mean(RTdiff$RTDiff)/sd(RTdiff$RTDiff),
                                               d_t=phtest$ttest$statistic/sqrt(24),
                                               sig=phtest$ttest$p.value<=0.05))
    
  }
}
if(Exp!=5){
  RT_PostHoc$sig_BFCorrected <- RT_PostHoc$t.p<(0.05/6)
}
SummaryTestRT_byCond <- ddply(TestRT_byCond, "Condition", SummaryData, "Mean")


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
                                        labels=c("Hits", "False Alarm: \nSimilar HS", "False Alarm: \nSimilar LS", "False Alarm: \n New"))
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


########## Compute stats on dprime ##########
#First, figure out which blocks for which the FA rate was 0 or the hit rate was 1, because running 
#qnorm on these values gives -Inf and Inf, respectively.
#Make a separate columns for adjusted values so that the ones with zero can be counted
PropResp$AdjProp <- NA
MaxHitRows <- (PropResp$RespType=="Hit" & PropResp$PropResp==1)
MinFARows <- (PropResp$RespType=="FA" & PropResp$PropResp==0)
PropResp[!(MaxHitRows | MinFARows), "AdjProp"]  <- PropResp[!(MaxHitRows | MinFARows), "PropResp"] 
#This basically changes the value of 0 FAs to 1/2 a FA as suggested in 
#http://www.kangleelab.com/sdt-d-prime-calculation---other-tips.html
PropResp[(MinFARows), "AdjProp"] <- 1/(2*PropResp[(MinFARows), "TotalTrials"])
PropResp[(MaxHitRows), "AdjProp"] <- 1-(1/(2*PropResp[(MaxHitRows), "TotalTrials"]))

#Count how many such values were replaced
PropResp$MaxHitRows <- MaxHitRows
PropResp$MinFARows <- MinFARows
(NumAdjResp_CondBlock <- ddply(PropResp, c("Condition", "Block"), summarise,
                               MaxHit=sum(MaxHitRows),
                               MinFA=sum(MinFARows)))
(NumAdjResp_Cond <- ddply(PropResp, c("Condition"), summarise,
                               MaxHit=sum(MaxHitRows),
                               MinFA=sum(MinFARows)))
(NumAdjResp_Block <- ddply(PropResp, c("Participant", "Block"), summarise,
                               MaxHit=sum(MaxHitRows),
                               MinFA=sum(MinFARows)))
twosample_ttest(NumAdjResp_Block[NumAdjResp_Block$Block=="TR", "MinFA"], 
                NumAdjResp_Block[NumAdjResp_Block$Block=="TI", "MinFA"], paired=TRUE)$ttest


#Now calculate qnorm on adjusted values
PropResp$QNormResp <- qnorm(PropResp$AdjProp)
#Finally, calculate dprime
if(Exp==5){
  DprimeData <- ddply(PropResp, c("Participant", "Block"), summarise,
                      DPrime=QNormResp[Condition=="Old" & RespType=="Hit"] -
                        QNormResp[Condition=="New" & RespType=="FA"])
  
  DPrimeAboveChance <- list("TPData"=c())
  for(block in FactorLabels[[ExpName]]$Block$levels){
    DPrimeAboveChance[[block]][["New"]] <- onesample_ttest(DprimeData[DprimeData$Block==block, "DPrime"], chance=0)
    DPrimeAboveChance[["TPData"]] <- rbind(DPrimeAboveChance$TPData, 
                                           data.frame(Condition="New", Block=block, 
                                                      t=DPrimeAboveChance[[block]][["New"]]$ttest$statistic,
                                                      p=DPrimeAboveChance[[block]][["New"]]$ttest$p.value))
  }
  
  
  SummaryDprime <- ddply(DprimeData, c("Block"), SummaryData, "DPrime")
  SummaryDprime$Block <- factor(SummaryDprime$Block, levels=FactorLabels[[ExpName]]$Block$levels, 
                                labels=FactorLabels[[ExpName]]$Block$labels)
  
  DPrimeBar <- ggplot(data=SummaryDprime, aes(x=Block, y=Mean, fill=Block)) +
    stdbar +
    scale_fill_manual(values=c("#ff9a76", "#679b9b"),
                      breaks=FactorLabels[[ExpName]]$Block$labels, 
                      labels=FactorLabels[[ExpName]]$Block$labels) + 
    coord_cartesian(ylim=c(0, 3.2)) +
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
    labs(x="Block", y="d'") +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
  DPrimeDot <- ggplot(data=DprimeData, aes(x=Block, y=DPrime, fill=Block)) + 
    geom_dotplot(binaxis = "y", stackdir = "center", position=position_dodge(0.8), dotsize=0.5) +
    scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                      breaks=FactorLabels[[ExpName]]$Block$levels, 
                      labels=FactorLabels[[ExpName]]$Block$levels)  +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
  #Save out dprime plot
  if(Save==1){
    jpeg(filename=sprintf("%s/WritingsAndPresentations/ExpectationMeeting2020/PosterGraphics/PropRespBar_%s.jpeg", 
                          BasePath, ExpName), 
         width=2500, height=2000, res=300)
    plot((DPrimeBar + posterlegendtheme + posterxaxistheme + posteryaxistheme))
    dev.off()
  }
  
  
  Dprime_ANOVA <- twosample_ttest(grp1=DprimeData[DprimeData$Block=="TR", "DPrime"],
                                  grp2=DprimeData[DprimeData$Block=="TI", "DPrime"],
                                  paired=TRUE)
  Dprime_ANOVA
  
  Dprime_BF <- ttestBF(x=DprimeData[DprimeData$Block=="TR", "DPrime"], 
                       y=DprimeData[DprimeData$Block=="TI", "DPrime"], 
                       paired=TRUE)
}else{
  DprimeData <- ddply(PropResp, c("Participant", "Block"), summarise,
                      New=QNormResp[Condition=="Old" & RespType=="Hit"] -
                        QNormResp[Condition=="New" & RespType=="FA"],
                      Similar_HI=QNormResp[Condition=="Old" & RespType=="Hit"] -
                        QNormResp[Condition=="Similar_HI" & RespType=="FA"],
                      Similar_LI=QNormResp[Condition=="Old" & RespType=="Hit"] -
                        QNormResp[Condition=="Similar_LI" & RespType=="FA"])
  
  #Make sure d prime is higher than zero
  DPrimeAboveChance <- list("TPData"=c())
  for(cond in FactorLabels[[ExpName]]$Condition$levels[2:4]){
    for(block in FactorLabels[[ExpName]]$Block$levels){
      DPrimeAboveChance[[block]][[cond]] <- onesample_ttest(DprimeData[DprimeData$Block==block, cond], chance=0)
      CohensD <- cohen.d(d=DprimeData[DprimeData$Block==block, cond], f=NA, mu=0)
      DPrimeAboveChance[["TPData"]] <- rbind(DPrimeAboveChance$TPData, 
                                                       data.frame(Condition=cond, Block=block, 
                                                                  t=DPrimeAboveChance[[block]][[cond]]$ttest$statistic,
                                                                  p=DPrimeAboveChance[[block]][[cond]]$ttest$p.value,
                                                                  d=CohensD$estimate))
    }
  }
  
  
  
  
  DprimeData_Long <- melt(DprimeData, id.vars=c("Participant", "Block"))
  DprimeData_Long <- DprimeData_Long %>% dplyr::rename(Condition=variable, DPrime=value)
  
  SummaryDprime <- ddply(DprimeData_Long, c("Condition", "Block"), SummaryData, "DPrime")
  
  SummaryDprime$Condition <- factor(SummaryDprime$Condition, levels=FactorLabels[[ExpName]]$Condition$levels, 
                                    labels=FactorLabels[[ExpName]]$Condition$labels)
  SummaryDprime$Block <- factor(SummaryDprime$Block, levels=FactorLabels[[ExpName]]$Block$levels, 
                                labels=FactorLabels[[ExpName]]$Block$labels)
  
  DPrimeBar <- ggplot(data=SummaryDprime, aes(x=Condition, y=Mean, fill=Block)) +
    stdbar + 
    coord_cartesian(ylim=c(0, 3.2)) +
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
    scale_fill_manual(values=c("#ff9a76", "#679b9b"),
                      breaks=FactorLabels[[ExpName]]$Block$labels, 
                      labels=FactorLabels[[ExpName]]$Block$labels) + 
    labs(x="Condition", y="d'", fill="Block") +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
  DPrimeDot <- ggplot(data=DprimeData_Long, aes(x=Condition, y=DPrime, fill=Block)) +
    stat_summary(fun.y=mean, geom="bar", position="dodge", color="#000000", size=1.5) +
    stat_summary(fun.data=mean_cl_normal, geom="errorbar",
                 width=0.3, size=0.9, position=position_dodge(.9)) + 
    scale_fill_manual(values=c("#ff9a76", "#679b9b"),
                      breaks=FactorLabels[[ExpName]]$Block$levels, 
                      labels=FactorLabels[[ExpName]]$Block$levels)  +
    geom_dotplot(binaxis = "y", stackdir = "center", position="dodge", dotsize=0.7) +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
  #Save out dprime plot
  if(Save==1){
    jpeg(filename=sprintf("%s/WritingsAndPresentations/ExpectationMeeting2020/PosterGraphics/PropRespBar_%s.jpeg", 
                          BasePath, ExpName), 
         width=3000, height=2000, res=300)
    plot((DPrimeBar + posterlegendtheme + posterxaxistheme + posteryaxistheme))
    dev.off()
  }
  
  
  
  #ANOVA on dPrime
  DprimeData_Long$Block <- factor(DprimeData_Long$Block)
  DprimeData_Long$Condition <- factor(DprimeData_Long$Condition)
  
  Dprime_ANOVA <- ezANOVA(data=DprimeData_Long, dv=DPrime, wid=Participant, within=c(Block, Condition), 
                          detailed=TRUE, type=2)
  Dprime_ANOVA$ANOVA
  
  DPrime_BF <- anovaBF(formula=DPrime~Block*Condition, data=DprimeData_Long)
  DPrime_BF/max(DPrime_BF)
}



##### Unfortunately, collapsing dPrime across blocks isn't as trivial as averaging numbers differently like
#we did for RT. The proportions of hits, FAs etc need to be recalculated, this time, without taking into account 
#the block
PropResp_byCond <- PropResp[, c("Participant", "Condition", "Block", "RespType", "SumResp")]
PropResp_byCond <- ddply(PropResp_byCond, c("Participant", "Condition", "RespType"), summarise, SumResp=sum(SumResp))

TotalTrials_byCond <- ddply (TestGoodData, c("Participant", "Condition"), summarise, TotalTrials=length(ListType))

PropResp_byCond <- merge(PropResp_byCond, TotalTrials_byCond, by=c("Participant", "Condition"), all.x=TRUE, all.y=TRUE)
CheckMerge(PropResp_byCond)
#Calculate proportion by block
PropResp_byCond$PropResp <- PropResp_byCond$SumResp/PropResp_byCond$TotalTrials

PropResp_byCond$AdjProp <- NA
#Adjust for 0 FAs
MaxHitRows <- (PropResp_byCond$RespType=="Hit" & PropResp_byCond$PropResp==1)
MinFARows <- (PropResp_byCond$RespType=="FA" & PropResp_byCond$PropResp==0)
PropResp_byCond[!(MaxHitRows | MinFARows), "AdjProp"]  <- PropResp_byCond[!(MaxHitRows | MinFARows), "PropResp"] 
PropResp_byCond[(MinFARows), "AdjProp"] <- 1/(2*PropResp_byCond[(MinFARows), "TotalTrials"])
PropResp_byCond[(MaxHitRows), "AdjProp"] <- 1-(1/(2*PropResp_byCond[(MaxHitRows), "TotalTrials"]))

#Count how many such values were replaced
PropResp_byCond$MaxHitRows <- MaxHitRows
PropResp_byCond$MinFARows <- MinFARows
(NumAdjResp_Cond <- ddply(PropResp_byCond, c("Condition"), summarise,
                          MaxHit=sum(MaxHitRows),
                          MinFA=sum(MinFARows)))
PropResp_byCond$QNormResp <- qnorm(PropResp_byCond$AdjProp)


if(Exp==5){
}else{
  DprimeData_byCond <- ddply(PropResp_byCond, c("Participant"), summarise,
                             New=QNormResp[Condition=="Old" & RespType=="Hit"] -
                               QNormResp[Condition=="New" & RespType=="FA"],
                             Similar_HI=QNormResp[Condition=="Old" & RespType=="Hit"] -
                               QNormResp[Condition=="Similar_HI" & RespType=="FA"],
                             Similar_LI=QNormResp[Condition=="Old" & RespType=="Hit"] -
                               QNormResp[Condition=="Similar_LI" & RespType=="FA"])
  
  DprimeData_byCond_Long <- melt(DprimeData_byCond, id.vars=c("Participant"))
  DprimeData_byCond_Long <- DprimeData_byCond_Long %>% dplyr::rename(Condition=variable, DPrime=value)
  
  #Posthoc tests for DPrime main effect
  DPrime_PostHoc <- c()
  for(cond in FactorLabels[[ExpName]]$Condition$levels[2:4]){
    #What should this condition be compared to?
    compareto <- FactorLabels[[ExpName]]$Condition$levels[2:4][(FactorLabels[[ExpName]]$Condition$levels[2:4] != cond)]
    for(comp in compareto){
      phtest <- twosample_ttest(grp1=DprimeData_byCond_Long[DprimeData_byCond_Long$Condition==cond, "DPrime"],
                                grp2=DprimeData_byCond_Long[DprimeData_byCond_Long$Condition==comp, "DPrime"],
                                paired=TRUE)
      
      #For manual calculation
      Dprimediff <- ddply(DprimeData_byCond_Long, c("Participant"), summarise, 
                          Dprimediff=DPrime[Condition==cond]-DPrime[Condition==comp])
      r <- cor(DprimeData_byCond_Long[DprimeData_byCond_Long$Condition==cond, "DPrime"], 
               DprimeData_byCond_Long[DprimeData_byCond_Long$Condition==comp, "DPrime"])
      
      
      
      #CohensD <- cohen.d(formula=DPrime~Condition | Subject(Participant), 
      #                   data=DprimeData_byCond_Long[DprimeData_byCond_Long$Condition %in% c(comp,cond), ],
      #                           paired=TRUE, pooled=TRUE, within=TRUE)
      
      DPrime_PostHoc <- rbind(DPrime_PostHoc, data.frame(X=cond, Y=comp, 
                                                         W1=phtest$shapiro1$statistic,
                                                         W.p1=phtest$shapiro1$p.value,
                                                         W2=phtest$shapiro2$statistic,
                                                         W.p2=phtest$shapiro2$p.value,
                                                         t=phtest$ttest$statistic,
                                                         t.df=phtest$ttest$parameter,
                                                         t.p=phtest$ttest$p.value,
                                                         d_effsize=CohensD$estimate,
                                                         d_srm=mean(Dprimediff$Dprimediff)/(sd(RTdiff$Dprimediff)/sqrt(2*(1-r))),
                                                         d_diff=mean(Dprimediff$Dprimediff)/sd(RTdiff$Dprimediff),
                                                         d_t=phtest$ttest$statistic/sqrt(24),
                                                         sig=phtest$ttest$p.value<=0.05))
      #CohensD=CohensD$estimate,
      #CohensD=CohensD_formula$estimate,
      #sig=phtest$ttest$p.value<=0.05))
      
    }
  }
  DPrime_PostHoc$p_BFCorrected <- DPrime_PostHoc$t.p*3
  DPrime_PostHoc$sig_BFCorrected <- DPrime_PostHoc$t.p<(0.05/3)
  
  SummaryDprime_byCond <- ddply(DprimeData_byCond_Long, "Condition", SummaryData, "DPrime")
  
  
}
  
########## Calculate scores from Stark et al. (2013) ##########

#### Traditional Recognition Score

PropNewData <- PropResp[PropResp$Condition %in% c("New", "Old"), ]
#CR=Similar that was correctly rejected
#Incorr=Similar that was called new
TradRecogScore <- ddply(PropNewData, c("Participant", "Block"), summarise, 
                        TradRecogScore=PropResp[RespType=="Hit"]-PropResp[RespType=="FA"])

SummaryTradRecog <- ddply(TradRecogScore, c("Block"), SummaryData, "TradRecogScore")
SummaryTradRecog$Block <- factor(SummaryTradRecog$Block, 
                                 levels=FactorLabels[[ExpName]]$Block$levels,
                                 labels=FactorLabels[[ExpName]]$Block$labels)

TradRecogBar <- ggplot(data=SummaryTradRecog, aes(x=Block, y=Mean, fill=Block)) +
  stdbar +
  scale_fill_manual(values=c("#ff9a76", "#679b9b"),
                    breaks=FactorLabels[[ExpName]]$Block$labels, 
                    labels=FactorLabels[[ExpName]]$Block$labels) + 
  coord_cartesian(ylim=c(0, 0.85)) +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  labs(x="Condition", y="Hits minus False Alarms") +
  xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme

TradRecog_ttest <- twosample_ttest(grp1=TradRecogScore[TradRecogScore$Block=="TR", "TradRecogScore"],
                                grp2=TradRecogScore[TradRecogScore$Block=="TI", "TradRecogScore"],
                                paired=TRUE)
TradRecog_ttest

TradRecog_BF <- ttestBF(x=TradRecogScore[TradRecogScore$Block=="TR", "TradRecogScore"], 
                        y=TradRecogScore[TradRecogScore$Block=="TI", "TradRecogScore"], 
                        paired=TRUE)

#### BPS Score

if(Exp %in% c(3, 4)){
  PropSimData <- PropResp[PropResp$Condition %in% c("Similar_HI", "Similar_LI"), ]
  #CR=Similar that was correctly rejected
  #Incorr=Similar that was called new
  BPSScore <- ddply(PropSimData, c("Participant", "Condition", "Block"), summarise, 
                    BPSScore=PropResp[RespType=="CR"]-PropResp[RespType=="Incorr"])
  
  SummaryBPSScore <- ddply(BPSScore, c("Condition", "Block"), SummaryData, "BPSScore")
  SummaryBPSScore$Condition <- factor(SummaryBPSScore$Condition, 
                                      levels=FactorLabels[[ExpName]]$Condition$levels,
                                      labels=FactorLabels[[ExpName]]$Condition$labels)
  SummaryBPSScore$Block <- factor(SummaryBPSScore$Block, 
                                  levels=FactorLabels[[ExpName]]$Block$levels,
                                  labels=FactorLabels[[ExpName]]$Block$labels)
  
  BPSBar <- ggplot(data=SummaryBPSScore, aes(x=Condition, y=Mean, fill=Block)) +
    stdbar +
    scale_fill_manual(values=c("#ff9a76", "#679b9b"),
                      breaks=FactorLabels[[ExpName]]$Block$labels, 
                      labels=FactorLabels[[ExpName]]$Block$labels) + 
    coord_cartesian(ylim=c(0, 0.48)) +
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
    labs(x="Condition", y="BPS Score") +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
  
  #ANOVA on BPS
  #BPSScore_Long <- melt(BPSScore, id.vars=c("Participant", "Block"))
  BPSScore$Block <- factor(BPSScore$Block)
  BPSScore$Condition <- factor(BPSScore$Condition)
  
  BPSScore_ANOVA <- ezANOVA(data=BPSScore, dv=BPSScore, wid=Participant, within=c(Block, Condition), 
                          detailed=TRUE, type=2)
  BPSScore_ANOVA$ANOVA
  
  BPSScore_BF <- anovaBF(formula=BPSScore~Block*Condition, data=BPSScore)
  BPSScore_BF/max(BPSScore_BF)
  
  for(cond in c("Similar_HI", "Similar_LI")){
    BPSScore_Cond <- BPSScore[BPSScore$Condition==cond,]
    
    print(sprintf("T-test on %s", cond))
    BPSScore_ttest <- twosample_ttest(grp1=BPSScore_Cond[BPSScore_Cond$Block=="TR", "BPSScore"],
                                       grp2=BPSScore_Cond[BPSScore_Cond$Block=="TI", "BPSScore"],
                                       paired=TRUE)
    
    print(BPSScore_ttest$ttest)
  }
  
}




########################### Get response Pattern ###########################

TestGoodData$RespName <- factor(TestGoodData$Resp, 
                                levels=FactorLabels[[ExpName]]$Resp$labels, 
                                labels=FactorLabels[[ExpName]]$Resp$levels)
#Count responses
if(Exp==5){
  RespPatterns <- ddply(TestGoodData, c("Participant", "Condition", "Block"), summarise,
                        OldResp=sum(RespName=="Old"),
                        NewResp=sum(RespName=="New"))
  
}else{
  RespPatterns <- ddply(TestGoodData, c("Participant", "Condition", "Block"), summarise,
                        OldResp=sum(RespName=="Old"),
                        SimResp=sum(RespName=="Similar"),
                        NewResp=sum(RespName=="New"))
}

#Make long
RespPatterns_Long <- melt(RespPatterns, id.vars=c("Participant", "Condition", "Block"))
RespPatterns_Long <- RespPatterns_Long %>% dplyr::rename(Response=variable,
                                                         NumResp=value)

#Collapse across participants
SummaryRespPatterns <- ddply(RespPatterns_Long, c("Condition", "Block", "Response"), SummaryData, "NumResp")

SummaryRespPatterns$Condition <- factor(SummaryRespPatterns$Condition, 
                                        levels=FactorLabels[[ExpName]]$Condition$levels,
                                        labels=FactorLabels[[ExpName]]$Condition$labels)
SummaryRespPatterns$Block <- factor(SummaryRespPatterns$Block, 
                                    levels=FactorLabels[[ExpName]]$Block$levels,
                                    labels=FactorLabels[[ExpName]]$Block$labels)

if(Exp==5){
  RespPatternBar <- ggplot(data=SummaryRespPatterns, aes(x=interaction(Response, Condition), y=Mean, fill=Block)) +
    stdbar + 
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) +
    annotate("text", x=1:4, y=-3.5, label=rep(FactorLabels[[ExpName]]$Resp$levels, 2),
             size=6) +
    annotate("text", x=c(1.5, 3.5), y=-6, label=FactorLabels[[ExpName]]$Resp$levels,
             size=7, fontface="bold") +
    coord_cartesian(ylim=c(0, 45)) + 
    theme(plot.margin = unit(c(1, 1, 5, 1), "lines"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    annotate("segment", x=2.5, xend=2.5, y=-2.2, yend =-6.5,
             colour="black", size=1.5) +
    plottitletheme + legendtheme + blankbgtheme + yaxistheme 
  
  RespPatternBarLayout <- ggplot_gtable(ggplot_build(RespPatternBar))
  RespPatternBarLayout$layout$clip[RespPatternBarLayout$layout$name == "panel"] <- "off"
  grid.draw(RespPatternBarLayout)
}else{
  RespPatternBar <- ggplot(data=SummaryRespPatterns, aes(x=interaction(Response, Condition), y=Mean, fill=Block)) +
    stdbar + 
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) +
    annotate("text", x=1:12, y=-3.5, label=rep(FactorLabels[[ExpName]]$Resp$levels, 4),
             size=6) +
    annotate("text", x=c(2, 5, 8, 11), y=-6, label=FactorLabels[[ExpName]]$Condition$labels,
             size=7, fontface="bold") +
    coord_cartesian(ylim=c(0, 45)) + 
    theme(plot.margin = unit(c(1, 1, 5, 1), "lines"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    annotate("segment", x=3.5, xend=3.5, y=-2.2, yend =-6.5,
             colour="black", size=1.5) +
    annotate("segment", x=6.5, xend=6.5, y=-2.2, yend =-6.5,
             colour="black", size=1.5) +
    annotate("segment", x=9.5, xend=9.5, y=-2.2, yend =-6.5,
             colour="black", size=1.5) +
    plottitletheme + legendtheme + blankbgtheme + yaxistheme 
  
  RespPatternBarLayout <- ggplot_gtable(ggplot_build(RespPatternBar))
  RespPatternBarLayout$layout$clip[RespPatternBarLayout$layout$name == "panel"] <- "off"
  grid.draw(RespPatternBarLayout)
}
  


########################### Get participant info ###########################

PartInfo <- read.xlsx(paste(BasePath, "Experiment/DemographicsInformation.xlsx", sep=""), sheet=ExpName)

#Only keep the participants that have good data
Demographics <- PartInfo[PartInfo$Participant %in% unique(TestGoodData$Participant), ]
length(Demographics$Participant)

CheckTrialNumbers(all(sort(unique(TestGoodData$Participant)) == sort(Demographics$Participant)))

SummaryData(Demographics, "Age")
min(Demographics$Age)
max(Demographics$Age)
sum(Demographics$Sex=="F")
ddply(Demographics, c("Awareness"), summarise, ln=length(Awareness))
#

########################### Extra analyses ###########################
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

CorrReg_Long <- melt(CorrReg)
if(Exp != 5){
  #Look at corrected recognition in similar for experiments 3 and 4
  
  
  SummaryCorrReg <- ddply(CorrReg_Long, c("Block", "variable"), SummaryData, "value")
  SummaryCorrReg$Block <- factor(SummaryCorrReg$Block, levels=FactorLabels[[ExpName]]$Block$levels, labels=FactorLabels[[ExpName]]$Block$labels)
  SummaryCorrReg$variable <- factor(SummaryCorrReg$variable, levels=c("CorrReg_SimHI", "CorrReg_SimLI", "CorrReg_New"), 
                                    labels=FactorLabels[[ExpName]]$Condition$labels[2:4])
  CorrRegBar <- ggplot(data=SummaryCorrReg, aes(x=variable, y=Mean, fill=Block)) +
    stdbar +
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
    scale_fill_manual(values=c("#FFC2A3", "#123C69"),
                      breaks=FactorLabels[[ExpName]]$Block$labels, 
                      labels=FactorLabels[[ExpName]]$Block$labels) + 
    labs(x="Condition", y="Hits - False Alarms") +
    xaxistheme + yaxistheme + plottitletheme + legendtheme + canvastheme + blankbgtheme
  
}


#Do stats on it
if(Exp==5){
  CorrReg_ANOVA <- ezANOVA(data=CorrReg_Long, dv=value, wid=Participant, within=c(Block), 
                           detailed=TRUE, type=2)
}else{
  CorrReg_ANOVA <- ezANOVA(data=CorrReg_Long, dv=value, wid=Participant, within=c(Block, variable), 
                           detailed=TRUE, type=2)
}
CorrReg_ANOVA$ANOVA



##### Look at accuracy by quarts 
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

for(i in 1:((length(FactorLabels[[ExpName]]$Condition$labels))-1)){
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








