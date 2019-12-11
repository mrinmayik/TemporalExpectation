library(openxlsx)
library(reshape)
library(plyr)
#library(dplyr)

########################## Set Admin variables ##########################

#What all to do right now
Make=1
Check=0
Save=1
FinalCB <- data.frame()

BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/Experiment/"


#List of counterbalances with lists assigned to conditions in the order given in CondNames
ListRot <- list("CondNames"=list("TR"=list("TR_Old", "TR_New"),
                                 "TI"=list("TI_Old", "TI_New")),
                "CB1"=list("TR"=c(1, 2),
                           "TI"=c(3, 4)),
                "CB2"=list("TR"=c(2, 3),
                           "TI"=c(4, 1)),
                "CB3"=list("TR"=c(3, 4),
                           "TI"=c(1, 2)),
                "CB4"=list("TR"=c(4, 1),
                           "TI"=c(2, 3)))



################################ Sanity check functions ################################
#Make sure that conditions and categories are evenly spread out across sets. To make sure that not all
#4 objects in a set are eventually in the Similar condition, or that not all of the objects are Tools 
#etc.
#CountRows <- function(df, Col){
  count(df[, Col])
}
#CheckCB <- function(df=NULL, Build=1) {
  if(Build==1){ #This is initially building the CB
    Check <- list()
    Checked <- 0
    #Condition per ISI is also controlled in the excel sheet
    Check$CatPerISI <- ddply(df, c("ISIType"), CountRows, "Category")
    Check$CatPerSet <- ddply(df, c("Set"), CountRows, "Category")
    Check$CondPerSet <- ddply(df, c("Set"), CountRows, "Condition")
    Check$OldPerThirds <- ddply(df[df$Condition=="Old",], c("Thirds"), CountRows, "Condition")
    Check$SimPerThirds <- ddply(df[df$ListType=="Similar",], c("Thirds"), CountRows, "Condition")
    Checked <- ifelse(any(any(Check$CatPerISI$freq>5),
                          any(Check$CatPerSet$freq>=4),
                          any(Check$CondPerSet$freq>=4),
                          any(Check$CondPerThirds$freq>=8),
                          any(Check$OldPerThirds$freq>=18),
                          any(Check$SimPerThirds$freq>=10)), 1, 0)
    return(list(Checked, Check))
  }else if(Build==2){ #Only need to make sure here that the ISI are equally split across conditions in the first ISIs
    #Everything else is taken care of above
    CheckTIISI <- list()
    CheckedTIISI <- 0
    df <- df[df$NumPres==1,]
    CheckTIISI$CondPerISI <- ddply(df, c("ISI"), count, "Condition")
    CheckedTIISI <- ifelse(any(any(CheckTIISI$CondPerISI$freq>=13)), 1, 0)
    return(list(CheckedTIISI, CheckTIISI))
  }
  
}
#CheckCB() #initialise

#Make sure that after the copying and sorting the trials, the same 4 objects are presented one after another
CheckRepetitions <- function(df){
  if(all(df[1:4, "Items"] == df[5:8, "Items"])){
    Good <- 1
  }
  else if(!(all(df[1:4, "Items"] == df[5:8, "Items"]))){
    stop("Order of items in consecutive presentations is not the same. Investigate!!!!!")
  }
  return(Good)
}

#Randomise ISIs in the TI block either similar to Thavabalasingham et al., 2017 or like Debbie's suggestion
RandomiseTI <- function(df, TIType){
  #Some jugaad required here. We want to make sure the subsequent presentations of the TI objects are paired with
  #opposite ISIs. Since we're now changing around the order of the ISIs (See ISICombo variable), we no longer know
  #which are the shortest and longest ISIs
  ShortISI <- order(ISICombo)[1:2]
  LongISI <- order(ISICombo)[3:4]
  
  #First randominse the set of  trials
  df[1:4, "ISIType"] <- sample(4)
  #df[5:8, "ISIType"] <- sample(4)
  
  ##### Use this if you want to randomise the TI block exactly like in Thavabalasingham et al., 2017 like 
  if(TIType=="Rand"){
    MeanISI <- ISICombo #jittered around whatever is the ISIcombo for this participant
    SDISI <- c(TIJitters[[as.character(MeanISI[1])]],
               TIJitters[[as.character(MeanISI[2])]],
               TIJitters[[as.character(MeanISI[3])]],
               TIJitters[[as.character(MeanISI[4])]])
    
    RandISI <- c(0, 0, 0, 0)
    #Set ISIs. Just make sure a negative ISI isn't returned
    while(any(RandISI<=17)){
      for(i in 1:4){
        RandISI[i] <- round(rnorm(1, mean=MeanISI[df[i, "ISIType"]], sd=SDISI[df[i, "ISIType"]]), digits=0)
      }
    }
    df[1:4, "ISI"] <- RandISI
    df[5:8, "ISI"] <- RandISI[sample(4)]
  }
  ##### For the time being, using the Debbie version where the trials in a set that were short ISIs in
  ##### the first presentation, should become the long ISIs in the second presentation 
  else if(TIType=="Shuffle"){
    df[which(row.names(df)==5:8 & df[1:4, "ISIType"] %in% ShortISI), "ISIType"] <- LongISI[sample(1:2)]
    df[which(row.names(df)==5:8 & df[1:4, "ISIType"] %in% LongISI), "ISIType"] <- ShortISI[sample(1:2)]
    
    df[df$ISIType==1, "ISI"] <- ISICombo[1]
    df[df$ISIType==2, "ISI"] <- ISICombo[2]
    df[df$ISIType==3, "ISI"] <- ISICombo[3]
    df[df$ISIType==4, "ISI"] <- ISICombo[4]
  }
  return(df)
}


################################ Read in files ################################

#Read in the master sheet that has all of the original details for how each participant should be organised
MasterList <- read.xlsx(paste(BasePath, "Experiment5/Counterbalancing/Counterbalancing_MasterSheet_Exp5.xlsx", sep=""), sheet="ListAssignment", 
                        cols=1:3, colNames=TRUE)

ISIComboDict <- read.xlsx(paste(BasePath, "Experiment5/Counterbalancing/Counterbalancing_MasterSheet_Exp5.xlsx", sep=""), sheet="ISIRotation", 
                          cols=1:5, rows=2:26, colNames=TRUE)

#Change this to 1, 2, 3 and so on and so forth for different participants
Part=1

#Setup information about the irregular condition
TIMethod = "Rand"
TIJitters <- list("100" = 40,
                  "500" = 80,
                  "1000" = 80,
                  "2000" = 80)
#ISI Combination: Got from CounterbalancingMasterSheet (Sheet: ISIRotation). This is to make sure that not all participants
#in the regular condition have the same ISI combination
ISICombo <- unlist(list(ISIComboDict[ISIComboDict$Participant==Part, c("1stDelay", "2ndDelay", "3rdDelay", "4thDelay")]))


#Setup which CB has to be run. Because it was so simple in this study, it was not added to the excel sheet
#Also setup which condition will be run first
#Only 4 counterbalances are used. So the CBs will be repeated for >4 participants
if(Part%%4 %in% 1:3){
  CB=Part%%4 
}else if(Part%%4 == 0){
  CB=4 
}
if(!(Part %in% 1:4)){
  stop("CB number isn't correct!! Investigate!!!!")
}

#Setup which block is run first. For odd numbered participants TR is first, for even numbered participants 
#TI is first
if(Part%%2 == 0){
  UseCondOrd <- c("TI", "TR")
}else if(Part%%2 == 1){
  UseCondOrd <- c("TR", "TI")
}







