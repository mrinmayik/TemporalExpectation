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

toexclude <- c()

#Set plotting variables
xaxistheme <- theme(axis.title.x = element_text(face="bold", size=20), axis.text.x = element_text(colour="#000000", size=18)) #, family="Times"
yaxistheme <- theme(axis.title.y = element_text(face="bold", size=20), axis.text.y = element_text(colour="#000000", size=14))
plottitletheme <- theme(plot.title = element_text(face="bold", size=20, hjust=0.5), legend.key.size=unit(1.3, "cm"))
legendtheme <- theme(legend.text=element_text(face="bold", size=10), legend.title=element_text(face="bold", size=16))
bgtheme <- theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
                 panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#D6D6D6"), 
                 panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "#D6D6D6"))
stdbar <- geom_bar(stat="identity", position="dodge", color="#000000", size=1.5)


########################## Functions ##########################

#Get mean, median etc. Good for plotting. This function can be used in conjunction with ddply
#That way you can get the mean, median etc per group/condition. 
SummaryData <- function(df, UseVar, RMNA=FALSE){
  M=mean(df[,UseVar], na.rm=RMNA)
  SD <- sd(df[,UseVar], na.rm=RMNA)
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
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by="Trial", all.x=TRUE, all.y=TRUE)
  
  
  EncodeData <- rbind(EncodeData, PartData[, ColOrd])
}

(EncodePerParticipant <- ddply(EncodeData, c("Participant", "Block"), summarise,
                               Trials = length(Participant), 
                               IdealTrials = 192,
                               SC = Trials==IdealTrials))

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
  ThisCB$Items <- as.data.frame(str_split_fixed(ThisCB$Picture, ".png", 2))[,1]
  
  #Add a trial column to the CB
  #ThisCB$Trial <- 1:288
  #Combine this with the data
  PartData <- merge(PartData, ThisCB, by=c("Items", "Trial"), all.x=TRUE, all.y=TRUE)
  
  
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

#### Look at RT now

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

View(TestData)
?geom_errorbar










