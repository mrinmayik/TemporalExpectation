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
DataPath <- paste(BasePath, "Experiment2/Data/", sep = "")
CBPath <- paste(BasePath, "Experiment1/Counterbalancing/", sep = "")

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

ColOrd <- c("Participant", "ListAssignment", "ListType", "Thirds", "Set", "NumPres", "Trial", "Block", "Condition", "Category", 
            "Items", "ISIType", "ISI", "Picture", "ObjectTime")


EncodeData=c()
#Read in data
for(Files in FileNames){
  PartData <- read_tsv(Files)
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  
  PartData <- PartData[, !(names(PartData) %in% c("Items"))]
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Encode_",
                             paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  #Add a trial column to the CB
  ThisCB$Trial <- 1:384
  
  ThisCB$FileName <- as.data.frame(str_split_fixed(ThisCB$Picture, ".png", 2))[,1]
  
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by="Trial", all.x=TRUE, all.y=TRUE)
  
  
  EncodeData <- rbind(EncodeData, PartData[, ColOrd])
}

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
EncodeData$TimeProblem <- abs(EncodeData$TimeDiscrepancy)>70

View(EncodeData[EncodeData$TimeProblem==TRUE,])
#Any wrong times that aren't the last trial for a block?
View(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial %in% c(192, 384))), ])

toexclude <- c(toexclude, unique(EncodeData[which(EncodeData$TimeProblem==TRUE & !(EncodeData$Trial %in% c(192, 384))), "Participant"]))

EncodeData <- EncodeData[!(EncodeData$Participant %in% toexclude), ]

#========================== Work with Encode Data Ends 



#========================== Work with Test Data ==========================

#ListFiles
FileNames <- list.files(path=DataPath, pattern="*Test.txt",
                        full.names=TRUE)

ColdOrd <- c("Participant", "Block", "ListAssignment", "ListType", "Category", "Condition", "Trial", "Picture", 
             "Items", "ObjectTime", "Resp", "RespTime")

TestData=c()
SameResp <- c()
SecRespNew <- c()
#Read in data
for(Files in FileNames){
  PartData <- read_tsv(Files)
  
  #Coding this "absolutely" to make sure it's not removing any random trials
  if(all(PartData[143:144, "Items"]==PartData[145:146, "Items"])){
    #Are the answers to the repeated the same?
    SameResp <- cbind(SameResp, (PartData[143:144, "Resp"]==PartData[145:146, "Resp"]))
    #Is the response the second time around always new (because the object hadn't occurred in the corresponding block)?
    SecRespNew <- cbind(SecRespNew, PartData[145:146, "Resp"]==3)
    PartData <- PartData[-(145:146),]
  }
  
  #Add trial number to help with merging
  PartData$Trial <- rep_len(1:144, 288)
  
  PartID <- paste(strsplit(strsplit(Files, "//")[[1]][2], "_")[[1]][1:2], collapse="_")
  PartData$Participant <- PartID
  
  #Read in CB
  ThisCB <- read.csv(paste(CBPath, "CB_Test_", paste(strsplit(PartID, "")[[1]][3:4], collapse=""), ".csv", sep=""))
  #Make an items column for merging purposes
  ThisCB$FileName <- as.data.frame(str_split_fixed(ThisCB$Picture, ".png", 2))[,1]
  
  #Add a trial column to the CB
  #ThisCB$Trial <- 1:288
  
  PartData <- PartData %>% dplyr::rename(DispPic=Items)
  
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by.x=c("DispPic", "Trial"), by.y=c("FileName", "Trial"), all.x=TRUE, all.y=TRUE)
  
  
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
SummaryTestAcc$Block <- factor(SummaryTestAcc$Block, levels=c("TR", "TI"), labels=c("Regular", "Irregular"))
SummaryTestAcc$Condition <- factor(SummaryTestAcc$Condition, 
                                   levels=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                   labels=c("Old", "Similar: HI", "Similar: LI", "New"))

TestAccBar <- ggplot(data=SummaryTestAcc, aes(x=Block, y=Mean, fill=Condition)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#00185C", "#D0902B", "#F1D4A6", "#CA2F2F"),
                    breaks=c("Old", "Similar: HI", "Similar: LI", "New"), 
                    labels=c("Old", "Similar: HI", "Similar: LI", "New")) + 
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
SummaryTestRT$Block <- factor(SummaryTestRT$Block, levels=c("TR", "TI"), labels=c("Regular", "Irregular"))
SummaryTestRT$Condition <- factor(SummaryTestRT$Condition, 
                                   levels=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                   labels=c("Old", "Similar: HI", "Similar: LI", "New"))

TestRTBar <- ggplot(data=SummaryTestRT, aes(x=Block, y=Mean, fill=Condition)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#00185C", "#D0902B", "#F1D4A6", "#CA2F2F"),
                    breaks=c("Old", "Similar: HI", "Similar: LI", "New"), 
                    labels=c("Old", "Similar: HI", "Similar: LI", "New")) + 
  labs(x="Condition", y="RT", fill="Object Type") +
  xaxistheme + yaxistheme + bgtheme + plottitletheme + legendtheme

##### Look at data by thirds

NumParticipants <- length(FileNames)-length(unique(toexclude))

EncodeData <- EncodeData[!(EncodeData$Participant %in% toexclude), ]
#Add the response to encoding data
EncodeWithResp <- merge(EncodeData, TestData, by=c("Participant", "Block", "Items", "ListType", "Category", "ListAssignment", "Condition"), 
                        all.x=TRUE, all.y=TRUE, suffixes=c("_Encode", "_Test"))

#Check trial numbers
all(EncodeWithResp[as.data.frame(CheckMerge(EncodeWithResp))$row, "Condition"]=="New")
NewWithResp <- EncodeWithResp[EncodeWithResp$Condition !="New", ]

EncodeWithResp <- EncodeWithResp[EncodeWithResp$Condition !="New", ]
CheckMerge(EncodeWithResp)



#Only keep first presentation
EncodeWithResp <- EncodeWithResp[EncodeWithResp$NumPres==1, ]
#Participants*192/2 trials(only one presentation)*2 block
(TrialNumbers <- NumParticipants*(192/2)*2 == nrow(EncodeWithResp))
CheckTrialNumbers(TrialNumbers)

#Collapse across trials
ThirdsAcc <- ddply(EncodeWithResp, c("Participant", "Block", "Thirds"), summarise, 
                   BehAcc=sum(Acc), 
                   BehNAcc=sum(!Acc),
                   TotalBehTrials=sum(BehAcc, BehNAcc),
                   PercAcc=(BehAcc/TotalBehTrials)*100,
                   IdealTrials=32,
                   SC=TotalBehTrials==IdealTrials)


#Collapse across participants
SummaryThirdsAcc <- ddply(ThirdsAcc, c("Block", "Thirds"), SummaryData, "PercAcc")
SummaryThirdsAcc$Block <- factor(SummaryThirdsAcc$Block, levels=c("TR", "TI"), labels=c("Regular", "Irregular"))
SummaryThirdsAcc$Thirds <- factor(SummaryThirdsAcc$Thirds)

ThirdsLine <- ggplot(data=SummaryThirdsAcc, aes(x=Thirds, y=Mean, group=Block)) +
  geom_point() + 
  geom_line(aes(colour=Block), size=1.2) + 
  #geom_segment(mapping=aes(x = 3.5, y = 30, xend = 3.5, yend = 85), size=0.5) + 
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9) +
  geom_hline(aes(yintercept=33), linetype="dashed", size=1) +
  #scale_linetype_manual(values=c("twodash", "dotted"))+
  coord_cartesian(ylim=c(25, 90)) +  
  scale_color_manual(values=c('#feb24c','#f03b20'))+
  labs(x="Thirds", y="Accuracy", colour="Block") + 
  xaxistheme + yaxistheme + plottitletheme + legendtheme + bgtheme

##Look at this by condition

#Collapse across trials
ThirdsAcc_Cond <- ddply(EncodeWithResp, c("Participant", "Block", "Thirds", "Condition"), summarise, 
                        BehAcc=sum(Acc), 
                        BehNAcc=sum(!Acc),
                        TotalBehTrials=sum(BehAcc, BehNAcc),
                        PercAcc=(BehAcc/TotalBehTrials)*100)

ThirdsAcc_Cond <- ddply(ThirdsAcc_Cond, c("Participant", "Block", "Thirds"), mutate, 
                        IdealTrials=sum(TotalBehTrials), SC=IdealTrials==32)

#Collapse across participants
SummaryThirdsAcc_Cond <- ddply(ThirdsAcc_Cond, c("Block", "Thirds", "Condition"), SummaryData, "PercAcc")
SummaryThirdsAcc_Cond$Block <- factor(SummaryThirdsAcc_Cond$Block, levels=c("TR", "TI"), labels=c("Regular", "Irregular"))
SummaryThirdsAcc_Cond$Condition <- factor(SummaryThirdsAcc_Cond$Condition, 
                                          levels=c("Old", "Similar_HI", "Similar_LI"), 
                                          labels=c("Old", "Similar: HI", "Similar: LI"))
SummaryThirdsAcc_Cond$Thirds <- factor(SummaryThirdsAcc_Cond$Thirds)

for(cond in unique(SummaryThirdsAcc_Cond$Condition)){
  ThirdsLine_Cond <- ggplot(data=SummaryThirdsAcc_Cond[SummaryThirdsAcc_Cond$Condition==cond,], 
                       aes(x=Thirds, y=Mean, group=Block)) +
    geom_point() + 
    geom_line(aes(colour=Block), size=1.2) + 
    #geom_segment(mapping=aes(x = 3.5, y = 30, xend = 3.5, yend = 85), size=0.5) + 
    geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9) +
    geom_hline(aes(yintercept=33), linetype="dashed", size=1) +
    #scale_linetype_manual(values=c("twodash", "dotted"))+
    coord_cartesian(ylim=c(40, 93)) +  
    ggtitle(cond) + 
    scale_color_manual(values=c('#feb24c','#f03b20'))+
    labs(x="Thirds", y="Accuracy", colour="Block") + 
    xaxistheme + yaxistheme + plottitletheme + legendtheme + bgtheme
    
    plot(ThirdsLine_Cond)
}

##### Look at first block

EncodeWithResp <- EncodeWithResp[order(EncodeWithResp$Participant, EncodeWithResp$ObjectTime_Test, EncodeWithResp$Trial_Test), ]

TRFirst <- c("CB1a_1","CB1b_1", "CB3b_1", "CB5a_1", "CB5b_1", "CB8a_1", "CB8b_1")
TIFirst <- c("CB2a_2", "CB2b_1", "CB2b_2", "CB2b_3", "CB4a_1", "CB4b_1", "CB6a_1", "CB6a_2", "CB6b_1", "CB7a_2", "CB7b_1")

EncodeWithResp[EncodeWithResp$Participant %in% TRFirst, "BlockNum"] <- "TRFirst"
EncodeWithResp[EncodeWithResp$Participant %in% TIFirst, "BlockNum"] <- "TIFirst"
CheckMerge(EncodeWithResp)

#Collapse across trials
BlockNumAcc <- ddply(EncodeWithResp, c("Participant", "Block", "Condition", "BlockNum"), summarise, 
                     BehAcc=sum(Acc), 
                     BehNAcc=sum(!Acc),
                     TotalBehTrials=sum(BehAcc, BehNAcc),
                     PercAcc=(BehAcc/TotalBehTrials)*100,
                     IdealTrials=96,
                     SC=TotalBehTrials==IdealTrials)


SummaryBlockNumAcc <- ddply(BlockNumAcc, c("Block", "Condition", "BlockNum"), SummaryData, "PercAcc")
SummaryBlockNumAcc$BlockNum_Cond <- paste(SummaryBlockNumAcc$Block, SummaryBlockNumAcc$BlockNum, sep="_")
SummaryBlockNumAcc$Condition <- factor(SummaryBlockNumAcc$Condition, 
                                          levels=c("Old", "Similar_HI", "Similar_LI"), 
                                          labels=c("Old", "Similar: HI", "Similar: LI"))
SummaryBlockNumAcc$BlockNum_Cond <- factor(SummaryBlockNumAcc$BlockNum_Cond, 
                                           levels=c("TR_TRFirst", "TI_TRFirst", "TI_TIFirst","TR_TIFirst"),
                                           labels=c("Regular First: Regular",
                                                    "Regular First: Irregular",
                                                    "Irregular First: Irregular",
                                                    "Irregular First: Regular"))

BlockNumBar <- ggplot(data=SummaryBlockNumAcc, aes(x=BlockNum_Cond, y=Mean, fill=Condition)) +
  stdbar +
  geom_errorbar(mapping=aes(ymin=Mean-SE, ymax=Mean+SE), width=0.2, size=0.9, position=position_dodge(.9)) + 
  scale_fill_manual(values=c("#00185C", "#D0902B", "#F1D4A6", "#CA2F2F"),
                    breaks=c("Old", "Similar: HI", "Similar: LI", "New"), 
                    labels=c("Old", "Similar: HI", "Similar: LI", "New")) + 
  labs(x="Block Order", y="Accuracy", fill="Object Type") +
  xaxistheme + yaxistheme + bgtheme + plottitletheme + legendtheme












