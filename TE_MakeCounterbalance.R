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


#List of counterbalances with lists assigned to conditions in the order given in CondNames
ListRot <- list("CondNames"=list("TR"=list("TR_Old", "TR_Similar", "TR_New"),
                                 "TI"=list("TI_Old", "TI_Similar", "TI_New")),
                "CB1"=list("TR"=c(1, 2, 3),
                           "TI"=c(4, 5, 6)),
                "CB2"=list("TR"=c(2, 3, 4),
                           "TI"=c(5, 6, 1)),
                "CB3"=list("TR"=c(3, 4, 5),
                           "TI"=c(6, 1, 2)),
                "CB4"=list("TR"=c(4, 5, 6),
                           "TI"=c(1, 2, 3)),
                "CB5"=list("TR"=c(5, 6, 1),
                           "TI"=c(2, 3, 4)),
                "CB6"=list("TR"=c(6, 1, 2),
                           "TI"=c(3, 4, 5)))

BasePath <- "/Users/mrinmayi/GoogleDrive/Mrinmayi/Research/TemporalExpectation/Experiment/"



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
MasterList <- read.xlsx(paste(BasePath, "Experiment1/Counterbalancing/Counterbalancing_MasterSheet.xlsx", sep=""), sheet="ListAssignment", 
                        cols=1:3, colNames=TRUE)

ColOrd <- c("Orig.Order", "Order_Assign.Conds", "Order_by.Run", "Condition", "List.ID", "SceneType", "Scene.ID", "RAND.Scene",
            "Object", "ENC.Run", "AssociatePosition", "TestRun")

ISIRotation <- read.xlsx(paste(BasePath, "Experiment1/Counterbalancing/Counterbalancing_MasterSheet.xlsx", sep=""), sheet="RotateISIAcrossParts", 
                         cols=1:8, rows=1:289, colNames=TRUE)

ISIComboDict <- read.xlsx(paste(BasePath, "Experiment1/Counterbalancing/Counterbalancing_MasterSheet.xlsx", sep=""), sheet="ISIRotation", 
                          cols=1:5, rows=28:52, colNames=TRUE)

#Change this to 1, 2, 3 and so on and so forth for different participants
Part=8
#This will alternate between a and b to yoke participants. So there will be 
#a 1a, 1b, 2a, 2b and so on
Ver="a"

#Figure out how ITIs will be randomised based on which experiment we're on
Experiment=4
if(Experiment==4){
  TIMethod = "Rand"
  TIJitters <- list("100" = 40,
                    "500" = 80,
                    "1000" = 80,
                    "2000" = 80)
  #ISI Combination: Got from CounterbalancingMasterSheet (Sheet: ISIRotation). This is to make sure that not all participants
  #in the regular condition have the same ISI combination
  print("***********DID YOU CHANGE THE ISI COMBO?!?!?!?!***********")
  #ISICombo <- c(100, 1000, 2000, 500)
  ISICombo <- unlist(list(ISIComboDict[ISIComboDict$Participant==Part, c("1stDelay", "2ndDelay", "3rdDelay", "4thDelay")]))
} else if(Experiment %in% 1:3) {
  TIMethod = "Shuffle"
  #ISI Combination: Got from CounterbalancingMasterSheet (Sheet: ISIRotation). This is to make sure that not all participants
  #in the regular condition have the same ISI combination
  print("***********DID YOU CHANGE THE ISI COMBO?!?!?!?!***********")
  ISICombo <- c(500, 2500, 50, 1000)
}


#Just get the conditions for each set for that particular participant
CB=unique(ISIRotation[which(ISIRotation$Participant==Part), "CB"])
if(length(CB)>1){
  stop("Something is wrong in the ISIRotation CB number. INVESTIGATE!!!!")
}

#The participants will be yoked such that the objects that were tested on the High Interefernce condition for 1a
#will be tested on the Low interference condition for 1b
SimType <- list("a" = c("Similar_HI", "Similar_LI"),
                "b" = c("Similar_LI", "Similar_HI"))

#From ISIRotation, get the order of conditions for each set
UseRot <- ISIRotation[which(ISIRotation$CB==CB & ISIRotation$Participant==Part), names(ISIRotation) %in% c("ISI_1", "ISI_2", "ISI_3", "ISI_4")]
UseRot <- UseRot[sample(nrow(UseRot)),]
#The conditions will be assigned in this order
VecRot <- as.vector(t(UseRot))

#Decide whether the TI condition will come first or TR
UseCondOrd <- ISIRotation[which(ISIRotation$CB==CB & ISIRotation$Participant==Part), "CondOrd"][1:2]
  
  
#Sanity Check
if(length(VecRot) > 96){
  stop("VecRot has more than 96 trials. Investigate!!!!")
}
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
  UseList <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][1:2], ]
  
  #Sanity check
  if(nrow(UseList) > 96){
    stop("UseList has more than 96 trials. Investigate!!!!")
  }
  
  #Assign conditions based on the ListRotation list. From ListRot, get me the current CB number that I'm working with
  #For the current condition. List rotation is setup such that the first number is always for the Old condition followed
  #by similar and then new
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "ListType"] <- "Old"
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][2], "ListType"] <- "Similar"
  
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "Condition"] <- "Old"
  #Make high and low interference assignments based on whether we're doing version a or b of a counterbalance
  UseList[UseList$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][2], "Condition"] <- rep_len(SimType[[Ver]], 48)
  
  DoAgain <- 1
  Count<-1
  #Keep doing the sampling until you all conditions are satisfied
  while(DoAgain>0){
    Count <- Count+1
    
    #From ISIRotation, get the order of conditions for each set
    UseRot <- ISIRotation[which(ISIRotation$CB==CB & ISIRotation$Participant==Part), names(ISIRotation) %in% c("ISI_1", "ISI_2", "ISI_3", "ISI_4")]
    UseRot <- UseRot[sample(nrow(UseRot)),]
    #The conditions will be assigned in this order
    VecRot <- as.vector(t(UseRot))
    
    #Choose the indices of the old (or similar) trials from the vector made from the appropriate sets. 
    #To those rows, assign a shuffled list of objects from UseList that are supposed to be old (or similar)
    FinalList[VecRot=="Old", ] <- UseList[sample(1:48, 48), ]
    FinalList[VecRot=="Similar", ] <- UseList[sample(49:96, 48), ]
    
    #Add the necessary columns
    FinalList$ISIType <- rep_len(1:4, 96)
    FinalList$Set <- sort(rep_len(1:(96/4), 96))
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
  ddply(FinalEncode, c("Set"), CheckRepetitions)
  
  #rm(FinalList)
  #FinalList <- FinalListRptSort
  #FinalEncode$EncodeOrTest <- 1 #1 = encoding, 2 = Test-- Don't need this anymore because of the way the experiment is setup
  #Yay for eliminating IF statements!!
  FinalEncode$Block <- Cond
  #FinalEncode$FirstLastTrial <- 0-- Don't need this anymore because of the way the experiment is setup
  #FinalEncode[1, "FirstLastTrial"] <- 1
  #FinalEncode[nrow(FinalEncode), "FirstLastTrial"] <- 2
  
  #Set regular ISIs depending on whether you're in encoding for TR or TI
  if(Cond=="TR"){
    FinalEncode[FinalEncode$ISIType==1, "ISI"] <- ISICombo[1]
    FinalEncode[FinalEncode$ISIType==2, "ISI"] <- ISICombo[2]
    FinalEncode[FinalEncode$ISIType==3, "ISI"] <- ISICombo[3]
    FinalEncode[FinalEncode$ISIType==4, "ISI"] <- ISICombo[4]
  }
  else if (Cond=="TI"){
    DoAgainTI <- 1
    while(DoAgainTI>0){
      FinalEncode <- ddply(FinalEncode, c("Set"), RandomiseTI, TIMethod)
      CheckListTI <- CheckCB(FinalEncode, 2)[[2]]
      DoAgainTI <- CheckCB(FinalEncode, 2)[[1]]
    }
  }
  
  FinalEncode[, "Picture"] <- paste(FinalEncode[, "Items"], "_1", sep="")
  #Prepare some stuff for the corresponding testing session
  Similar_HI <- FinalEncode[FinalEncode$Condition=="Similar_HI", "Items"]
  Similar_LI <- FinalEncode[FinalEncode$Condition=="Similar_LI", "Items"]
  
  #Because the Test should be setup such that the items encoded in the first quarter of the experiment are 
  #tested first, get a list of objects split up by quarters
  #split divides the data in the vector x into the groups defined by f
  #unique is the function rapply is applying because each object is repreated twice in Final encode
  QuartItems  <- rapply(split(FinalEncode[, "Items"], ceiling(seq_along(1:96)/24)), unique, how="list")
  
  ################################ Done with Encoding ################################
  ################################     Now do test    ################################
  #Just get old and new from the list for now so that they can be randomised based on quarters from
  #encoding
  UseList <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][1:2], ]
  
  #Build Uselist from the quarters made above
  UseList <- rbind(UseList[sample(which(UseList$Items %in% QuartItems$`1`)),],
                   UseList[sample(which(UseList$Items %in% QuartItems$`2`)),],
                   UseList[sample(which(UseList$Items %in% QuartItems$`3`)),],
                   UseList[sample(which(UseList$Items %in% QuartItems$`4`)),])
  
  
  #Now add the new to this mess
  NewTest <- MasterList[MasterList$ListAssignment %in%  ListRot[[paste("CB", CB, sep="")]][[Cond]][3], ]
  #Get positions of new objects
  NewPos <- sample(1:nrow(UseList), nrow(NewTest))
  NewTest_RowName <- as.integer(rownames(NewTest))
  UseList_RowName <- as.integer(rownames(UseList))
  for(Pos in 1:length(NewPos)){
    UseList_RowName <- append(UseList_RowName, NewTest_RowName[Pos], after=NewPos[Pos])
  }
  
  FinalTest <- rbind(UseList, NewTest)
  FinalTest <- FinalTest[as.character(UseList_RowName),]
  
  #Assign conditions based on the ListRotation list
  FinalTest[FinalTest$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "ListType"] <- "Old"
  FinalTest[FinalTest$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][2], "ListType"] <- "Similar"
  FinalTest[FinalTest$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][3], "ListType"] <- "New" 
  
  FinalTest[FinalTest$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][1], "Condition"] <- "Old"
  #Assign HI and LI based on items that were assigned HI and LI in study
  FinalTest[FinalTest$Items %in% Similar_HI, "Condition"] <- "Similar_HI"
  FinalTest[FinalTest$Items %in% Similar_LI, "Condition"] <- "Similar_LI"
  #Rename New
  FinalTest[FinalTest$ListAssignment == ListRot[[paste("CB", CB, sep="")]][[Cond]][3], "Condition"] <- "New"
  
  #FinalTest$EncodeOrTest <- 2
  FinalTest$Block <- Cond
  #FinalTest$FirstLastTrial <- 0
  #FinalTest[1, "FirstLastTrial"] <- 1 
  #FinalTest[nrow(FinalTest), "FirstLastTrial"] <- 2
  
  #FinalTest <- merge(FinalTest, FinalEncode[,c("Category", "Items", "ISIType", "Set", "ISI")], 
  #                 by=c("Category", "Items"), all.x=TRUE)
  #FinalTest[FinalTest$Condition=="New", c("ISIType", "Set", "ISI")] <- 0
  #FinalTest[, c("ISIType", "Set", "ISI")] <- 0
  
  FinalTest[FinalTest$Condition=="Old", "Picture"] <- paste(FinalTest[FinalTest$Condition=="Old", "Items"], "_1", sep="")
  FinalTest[FinalTest$Condition=="New", "Picture"] <- paste(FinalTest[FinalTest$Condition=="New", "Items"], "_1", sep="")
  FinalTest[FinalTest$Condition=="Similar_HI", "Picture"] <- paste(FinalTest[FinalTest$Condition=="Similar_HI", "Items"], "_2", sep="")
  FinalTest[FinalTest$Condition=="Similar_LI", "Picture"] <- paste(FinalTest[FinalTest$Condition=="Similar_LI", "Items"], "_3", sep="")
  FinalTest$Trial <- 1:nrow(FinalTest)
  
  
  FinalCB_Encode <- rbind(FinalCB_Encode, FinalEncode)
  FinalCB_Test <- rbind(FinalCB_Test, FinalTest)
  #assign(paste(Cond, "_Encode", sep=""), FinalEncode)
}

#Need to do this explicitly because for some reason openxlsx isn't doing it
#Add some sheets to the workbook
#addWorksheet(OutSheet, paste(Cond, "_Encode", sep=""))
#Write the data to the sheets
#writeData(OutSheet, sheet = paste(Cond, "_Encode", sep=""), x=FinalList)
# Export the file
#saveWorkbook(OutSheet, paste(BasePath, "CB", CB, "_", Ver, ".xlsx", sep=""))

FinalCB_Encode$Trial <- c(1:192, 1:192)

if(Save==1){
  write.csv(FinalCB_Encode, file = paste(BasePath, "CB_Encode_", Part, "a.csv", sep=""), row.names=FALSE)
  write.csv(FinalCB_Test, file = paste(BasePath, "CB_Test_", Part, "a.csv", sep=""), row.names=FALSE)
}

#Make the CB for the yoked participant such that the object in the Similar_HI condition is now in the Similar_LI condition, but everything else
#(e.g. order of stim presentation is kept same)
FinalCB_Encode_Yoked <- FinalCB_Encode
FinalCB_Encode_Yoked$Condition <- factor(FinalCB_Encode_Yoked$Condition,
                                         levels=c("Old", "Similar_HI", "Similar_LI"), 
                                         labels=c("Old", "Similar_LI", "Similar_HI"))
FinalCB_Test_Yoked <- FinalCB_Test
FinalCB_Test_Yoked$Condition <- factor(FinalCB_Test_Yoked$Condition, 
                                       levels=c("Old", "Similar_HI", "Similar_LI", "New"), 
                                       labels=c("Old", "Similar_LI", "Similar_HI", "New"))

FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Old", "Picture"] <- paste(FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Old", "Items"], "_1", sep="")
FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="New", "Picture"] <- paste(FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="New", "Items"], "_1", sep="")
FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Similar_HI", "Picture"] <- paste(FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Similar_HI", "Items"], "_2", sep="")
FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Similar_LI", "Picture"] <- paste(FinalCB_Test_Yoked[FinalCB_Test_Yoked$Condition=="Similar_LI", "Items"], "_3", sep="")

if(Save==1){
  write.csv(FinalCB_Encode_Yoked, file = paste(BasePath, "CB_Encode_", Part, "b.csv", sep=""), row.names=FALSE)
  write.csv(FinalCB_Test_Yoked, file = paste(BasePath, "CB_Test_", Part, "b.csv", sep=""), row.names=FALSE)
}

#ptm <- proc.time()
#Blah <-sort(sample(540, 90))#, prob=rep_len(1, 540)))
#while(any(diff(Blah)==1)){
#  Blah <-sort(sample(540, 90))#, prob=rep_len(1, 540)))
#  #print(Blah)
#}
#proc.time() - ptm



