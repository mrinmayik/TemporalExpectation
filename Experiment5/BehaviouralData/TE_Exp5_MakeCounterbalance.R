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
CountRows <- function(df, Col){
  count(df[, Col])
}
CheckCB <- function(df=NULL, Build=1) {
  if(Build==1){ #This is initially building the CB
    Check <- list()
    Checked <- 0
    #Condition per ISI is also controlled in the excel sheet
    Check$TypePerISI <- ddply(df, c("ISIType"), CountRows, "SceneType")
    Check$TypePerSet <- ddply(df, c("Set"), CountRows, "SceneType")
    Checked <- ifelse(any(any(Check$TypePerISI$freq>6),
                          any(Check$TypePerSet$freq>3)), 1, 0)
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
CheckCB() #initialise

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

#Initialise some vectors
FinalList <- data.frame(matrix(NA, nrow=96, ncol=5))
names(FinalList) <- c(names(MasterList), "ListType", "Condition")


################################ Build the actual counterbalancing ################################

#Make blank workbook
#OutSheet <- createWorkbook()
Count<-1

FinalCB_Encode <- c()
FinalCB_Test <- c()
#Make encoding blocks first
for(Cond in UseCondOrd){
  #Subset MasterList to only include the lists that you need for this condition, for encoding 
  #(list numbers got from the ListRotation list)
  UseList <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][1], ]
  
  #Sanity check
  if(nrow(UseList) > 48){
    stop("UseList has more than 96 trials. Investigate!!!!")
  }
  
  #Assign conditions based on the ListRotation list. From ListRot, get me the current CB number that I'm working with
  #For the current condition. List rotation is setup such that the first number is always for the Old condition followed
  #by similar and then new
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "ListType"] <- "Old"
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "Condition"] <- "Old"
  
  DoAgain <- 1
  Count<-1
  #Keep doing the sampling until you all conditions are satisfied
  while(DoAgain>0){
    Count <- Count+1
    
    #Choose the indices of the old (or similar) trials from the vector made from the appropriate sets. 
    #To those rows, assign a shuffled list of objects from UseList that are supposed to be old (or similar)
    FinalList <- UseList[sample(1:48, 48), ]
    
    #Add the necessary columns
    FinalList$ISIType <- rep_len(1:4, 48)
    FinalList$Set <- sort(rep_len(1:(48/4), 48))
    
    #This column will be helpful to look at if performance improves through the thirds of the encoding block,
    #as participants learn the rhythm of presentation.
    #It's easier to add this column through the counterbalance, rather than add it to the data, and then make errors 
    #because of how the data is ordered
    FinalList$Thirds <- sort(rep_len(1:3, nrow(FinalList)))
    
    CheckList <- CheckCB(FinalList, 1)[[2]]
    DoAgain <- CheckCB(FinalList, 1)[[1]]
    
  }
  
  #Repeat the same thing 2 times so that each set is presented twice
  FinalListRpt <- rbind(FinalList, FinalList)
  FinalEncode <- FinalListRpt[order(FinalListRpt$Set), ]
  FinalEncode$NumPres <- rep_len(c(1, 1, 1, 1, 2, 2, 2, 2), nrow(FinalEncode))
  if(!(all(ddply(FinalEncode, c("Set"), CheckRepetitions)$V1==1))){
    stop("Somthing wrong with how items are repeated!!!!")
  }
  
  FinalEncode$Block <- Cond
  
  #Set regular ISIs depending on whether you're in encoding for TR or TI
  if(Cond=="TR"){
    FinalEncode[FinalEncode$ISIType==1, "ISI"] <- ISICombo[1]
    FinalEncode[FinalEncode$ISIType==2, "ISI"] <- ISICombo[2]
    FinalEncode[FinalEncode$ISIType==3, "ISI"] <- ISICombo[3]
    FinalEncode[FinalEncode$ISIType==4, "ISI"] <- ISICombo[4]
  }else if (Cond=="TI"){
    DoAgainTI <- 1
    while(DoAgainTI>0){
      FinalEncode <- ddply(FinalEncode, c("Set"), RandomiseTI, TIMethod)
      CheckListTI <- CheckCB(FinalEncode, 2)[[2]]
      DoAgainTI <- CheckCB(FinalEncode, 2)[[1]]
    }
  }
  #This is not necessary from the presentation POV, but may be needed so that a new analysis script doesn't need to be written
  FinalEncode[, "Picture"] <- FinalEncode[, "Scenes"]
  
  #Because the Test should be setup such that the items encoded in the first quarter of the experiment are 
  #tested first, get a list of objects split up by quarters
  #split divides the data in the vector x into the groups defined by f
  #unique is the function rapply is applying because each object is repreated twice in Final encode
  QuartItems  <- rapply(split(FinalEncode[, "Picture"], ceiling(seq_along(1:96)/24)), unique, how="list")
  
  ################################ Done with Encoding ################################
  ################################     Now do test    ################################
  #Just get old and new from the list for now so that they can be randomised based on quarters from
  #encoding
  UseList <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][1], ]

  #Build Uselist from the quarters made above
  UseList <- rbind(UseList[sample(which(UseList$Scenes %in% QuartItems$`1`)),],
                   UseList[sample(which(UseList$Scenes %in% QuartItems$`2`)),],
                   UseList[sample(which(UseList$Scenes %in% QuartItems$`3`)),],
                   UseList[sample(which(UseList$Scenes %in% QuartItems$`4`)),])
  
  #Now add the new to this mess
  NewTest <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][2], ]
  #Get positions of new objects
  NewPos <- sample(1:nrow(UseList), nrow(NewTest))
  NewTest_RowName <- as.integer(rownames(NewTest))
  UseList_RowName <- as.integer(rownames(UseList))
  for(Pos in 1:length(NewPos)){
    UseList_RowName <- append(UseList_RowName, NewTest_RowName[Pos], after=NewPos[Pos])
  }
  
  FinalTest <- rbind(UseList, NewTest)
  FinalTest <- FinalTest[as.character(UseList_RowName),]
  
  
  


    
    
    
    
    


