
library(dplyr)

library(data.table)

Eurosc_II_DateMonth_randomised <- fread(file="../modelInput/expandEurosc_II_DateMonth_randomised.csv", data.table=F)


# cardioDataCalEurosc_II <- nacsa.predict %>%
#   select(HospCode, Gender,Payer,CCS,NYHA,PrevMI,IntervalMI,PCI,PrevCABG,PrevValve,PrevAoAscArch,PrevAoDesc,Hypertension,PulHTN,CrCl,Aortic.AAS,Other.Mech.Support,PrevThoracic,Diabetes,Smoking,PulmonaryDisease,Stroke,NeuroDys,PVD,PreopAF,PreopVFT,PreopCHB.pacing,LMS,PAsys,LVF2,CardiogenicShock,VentilatedPreop,Urgency,PrevOp,BMI,Mobility,FirstOperatorGrade,CABG,NumberValves,AVProcedure,PreopDialysis,Creatinine,Age,PumpCase,CPS,Endocarditis,RecentMI,PostInfarctVSD,MVProcedure,TVProcedure,PVProcedure,DaysBetweenLHCOp,CADExtent,Nitrates,Inotropes,Weight,NumberGrafts,Ao.Root.Procedure,Ao.Asc.Procedure,Ao.Arch.Procedure,Ao.Desc.Procedure,Ao.Abdo.Procedure,Impeller,VAD,IABP,Median.Sternotomy,Partial.Sternotomy,Thoracotomy,Mini.Thoracotomy,imdDecile,Mortality,OpDate,Year) 
#   with single quotes around variables currently omitted 

cardioDataCalEurosc_II <- Eurosc_II_DateMonth_randomised

cardioDataCalEurosc_II$V1 <- NULL
colnames(cardioDataCalEurosc_II)

sum(is.na(cardioDataCalEurosc_II$mtly)) #0

sum(as.character(cardioDataCalEurosc_II$mtly) == "") #0

nrow(cardioDataCalEurosc_II)  #224318


table(cardioDataCalEurosc_II$NumberValves)


#Baseline characteristics ######################


#procedures missing outcome is calculated in a by selecting for year range 2012-2019 first 

#remove all rows where mtly is NA
cardioDataCalEurosc_II <- cardioDataCalEurosc_II[!is.na(cardioDataCalEurosc_II$mtly), ]

sum(is.na(cardioDataCalEurosc_II$mtly)) #0

nrow(cardioDataCalEurosc_II)  #224318

length(unique(cardioDataCalEurosc_II$HospCode)) #42 / 45 in original dataset

sum(cardioDataCalEurosc_II$mtly)  #6100


sum(cardioDataCalEurosc_II$mtly) / nrow(cardioDataCalEurosc_II) * 100 #2.719%


colnames(cardioDataCalEurosc_II)

################################################
#add link id 

unique(cardioDataCalEurosc_II$ES_II_link_ID)



#################################################################
#Gender

unique(cardioDataCalEurosc_II$Gender)


#################################################################
#Payer

unique(cardioDataCalEurosc_II$Payer)

table(cardioDataCalEurosc_II$Payer)

sum(is.na(cardioDataCalEurosc_II$Payer)) 

cardioDataCalEurosc_II$Payer[is.na(cardioDataCalEurosc_II$Payer)] = 1

#re-order levels
cardioDataCalEurosc_II$Payer  <- gsub('2', '0', cardioDataCalEurosc_II$Payer)  

cardioDataCalEurosc_II$Payer <- as.numeric(as.character(cardioDataCalEurosc_II$Payer))

unique(cardioDataCalEurosc_II$Payer)


##################################################################
#CCS class 

cardioDataCalEurosc_II$CCS <- as.numeric(as.character(cardioDataCalEurosc_II$CCS))

unique(cardioDataCalEurosc_II$CCS)


#####################################################################
#NYHA

cardioDataCalEurosc_II$NYHA  <- gsub('1', '0', cardioDataCalEurosc_II$NYHA)

cardioDataCalEurosc_II$NYHA  <- gsub('2', '1', cardioDataCalEurosc_II$NYHA)

cardioDataCalEurosc_II$NYHA  <- gsub('3', '2', cardioDataCalEurosc_II$NYHA)

cardioDataCalEurosc_II$NYHA  <- gsub('4', '3', cardioDataCalEurosc_II$NYHA)

cardioDataCalEurosc_II$NYHA <- as.numeric(as.character(cardioDataCalEurosc_II$NYHA))

length(cardioDataCalEurosc_II$NYHA[is.na(cardioDataCalEurosc_II$NYHA)] )

unique(cardioDataCalEurosc_II$NYHA )


#################################################################
#PrevMI

unique(cardioDataCalEurosc_II$PrevMI)


#################################################################
#IntervalMI
#dataset v1_2: added 5.>90 days

unique(cardioDataCalEurosc_II$IntervalMI)

#re-order levels
cardioDataCalEurosc_II$IntervalMI  <- gsub('1', '7', cardioDataCalEurosc_II$IntervalMI)  #7 is temp 

cardioDataCalEurosc_II$IntervalMI  <- gsub('5', '1', cardioDataCalEurosc_II$IntervalMI)  

cardioDataCalEurosc_II$IntervalMI  <- gsub('7', '5', cardioDataCalEurosc_II$IntervalMI)

cardioDataCalEurosc_II$IntervalMI  <- gsub('2', '10', cardioDataCalEurosc_II$IntervalMI)

cardioDataCalEurosc_II$IntervalMI  <- gsub('4', '2', cardioDataCalEurosc_II$IntervalMI)

cardioDataCalEurosc_II$IntervalMI  <- gsub('10', '4', cardioDataCalEurosc_II$IntervalMI)

cardioDataCalEurosc_II$IntervalMI <- as.numeric(as.character(cardioDataCalEurosc_II$IntervalMI))


unique(cardioDataCalEurosc_II$IntervalMI)


#################################################################
#PCI

unique(cardioDataCalEurosc_II$PCI)

#re-order levels
cardioDataCalEurosc_II$PCI  <- gsub('1', '7', cardioDataCalEurosc_II$PCI)  #7 is temp 

cardioDataCalEurosc_II$PCI  <- gsub('3', '1', cardioDataCalEurosc_II$PCI)

cardioDataCalEurosc_II$PCI  <- gsub('7', '3', cardioDataCalEurosc_II$PCI)

cardioDataCalEurosc_II$PCI <- as.numeric(as.character(cardioDataCalEurosc_II$PCI))

unique(cardioDataCalEurosc_II$PCI)


#################################################################
#PrevCABG

unique(cardioDataCalEurosc_II$PrevCABG)


#################################################################
#PrevValve

unique(cardioDataCalEurosc_II$PrevValve)


#################################################################
#PrevAoAscArch

unique(cardioDataCalEurosc_II$PrevAoAscArch)


#################################################################
#Diabetes

unique(cardioDataCalEurosc_II$Diabetes)

cardioDataCalEurosc_II$Diabetes <- as.numeric(as.character(cardioDataCalEurosc_II$Diabetes))

unique(cardioDataCalEurosc_II$Diabetes)	

#################################################################
#Smoking

unique(cardioDataCalEurosc_II$Smoking)


########################################################
#Chronic lung disease

unique(as.character(cardioDataCalEurosc_II$PulmonaryDisease))

cardioDataCalEurosc_II$PulmonaryDisease <- as.numeric(as.character(cardioDataCalEurosc_II$PulmonaryDisease))

unique(cardioDataCalEurosc_II$PulmonaryDisease)


#################################################################
#Stroke

unique(cardioDataCalEurosc_II$Stroke)

cardioDataCalEurosc_II$Stroke[is.na(cardioDataCalEurosc_II$Stroke) ] <- 0

unique(cardioDataCalEurosc_II$Stroke)


#################################################################
#NeuroDys

unique(cardioDataCalEurosc_II$NeuroDys)


#################################################################
#PVD

unique(cardioDataCalEurosc_II$PVD)


#################################################################
#CardiacRhythm

cardioDataCalEurosc_II$CardiacRhythm[cardioDataCalEurosc_II$PreopAF == 1] <- 1

sum(cardioDataCalEurosc_II$CardiacRhythm != cardioDataCalEurosc_II$PreopAF, na.rm = T)  #0

cardioDataCalEurosc_II$CardiacRhythm[cardioDataCalEurosc_II$PreopVFT == 1] <- 2

table(cardioDataCalEurosc_II$PreopVFT)  #440
table(cardioDataCalEurosc_II$CardiacRhythm)  #440

#PreopCHB.pacing
unique(cardioDataCalEurosc_II$PreopCHB.pacing)

cardioDataCalEurosc_II$CardiacRhythm[cardioDataCalEurosc_II$PreopCHB.pacing == 1] <- 3

table(cardioDataCalEurosc_II$PreopCHB.pacing)  #verified 


cardioDataCalEurosc_II$CardiacRhythm[is.na(cardioDataCalEurosc_II$CardiacRhythm) ] <- 0


#remove component variables:
cardioDataCalEurosc_II$PreopAF <- NULL

cardioDataCalEurosc_II$PreopVFT <- NULL

cardioDataCalEurosc_II$PreopCHB.pacing <- NULL

unique(cardioDataCalEurosc_II$CardiacRhythm)


#################################################################
#LMS

unique(cardioDataCalEurosc_II$LMS)


##################################################################
#Pulmonary Artery Systolic Pressure 
# 
# library(Hmisc)
# 
# cardioDataCalEurosc_II$PAsys <- as.numeric(as.character(cardioDataCalEurosc_II$PAsys))
# 
# cardioDataCalEurosc_II$PAsys <- with(cardioDataCalEurosc_II, impute(PAsys, median))
# 
# unique(cardioDataCalEurosc_II$PAsys)

###########################################################
#Left ventricular ejection fraction

cardioDataCalEurosc_II$LVF2 <- as.numeric(as.character(cardioDataCalEurosc_II$LVF2))

cardioDataCalEurosc_II$LVF2  <- gsub('0', '7', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2  <- gsub('3', '0', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2  <- gsub('1', '6', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2  <- gsub('2', '1', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2  <- gsub('6', '2', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2  <- gsub('7', '3', cardioDataCalEurosc_II$LVF2)

cardioDataCalEurosc_II$LVF2 <- as.numeric(cardioDataCalEurosc_II$LVF2)

unique(cardioDataCalEurosc_II$LVF2)

###########################################################
#Left ventricular ejection fraction

# cardioDataCalEurosc_II$LVEF <- as.numeric(as.character(cardioDataCalEurosc_II$LVEF))
# 
# hist(cardioDataCalEurosc_II$LVEF)
# 
# library(Hmisc)
# 
# cardioDataCalEurosc_II$LVEF <- with(cardioDataCalEurosc_II, impute(LVEF, median))
# 
# unique(cardioDataCalEurosc_II$LVEF)


#################################################################
#CardiogenicShock

unique(cardioDataCalEurosc_II$CardiogenicShock)


#################################################################
#VentilatedPreop

unique(cardioDataCalEurosc_II$VentilatedPreop)


#################################################################
#Procedure Urgency

unique(cardioDataCalEurosc_II$Urgency)

cardioDataCalEurosc_II$Urgency <- as.numeric(as.character(cardioDataCalEurosc_II$Urgency))

cardioDataCalEurosc_II$Urgency[is.na(cardioDataCalEurosc_II$Urgency) ] <- 1

cardioDataCalEurosc_II$Urgency <- gsub('1', '0', cardioDataCalEurosc_II$Urgency)

cardioDataCalEurosc_II$Urgency <- gsub('2', '1', cardioDataCalEurosc_II$Urgency)

cardioDataCalEurosc_II$Urgency <- gsub('3', '2', cardioDataCalEurosc_II$Urgency)

cardioDataCalEurosc_II$Urgency <- gsub('4', '3', cardioDataCalEurosc_II$Urgency)

length(cardioDataCalEurosc_II$Urgency[is.na(cardioDataCalEurosc_II$Urgency)] )

cardioDataCalEurosc_II$Urgency <- as.numeric(as.character(cardioDataCalEurosc_II$Urgency))

unique(cardioDataCalEurosc_II$Urgency)


###########################################################
#number of PrevOp

unique(as.character(cardioDataCalEurosc_II$PrevOp))

cardioDataCalEurosc_II$PrevOp <- as.numeric(as.character(cardioDataCalEurosc_II$PrevOp))

cardioDataCalEurosc_II$PrevOp[is.na(cardioDataCalEurosc_II$PrevOp)] = 0

unique(cardioDataCalEurosc_II$PrevOp)


#################################################################
#BMI

unique(cardioDataCalEurosc_II$BMI)	#approx. normal

hist(cardioDataCalEurosc_II$BMI)	

cardioDataCalEurosc_II$BMI <- with(cardioDataCalEurosc_II, impute(BMI, median))

length(cardioDataCalEurosc_II$BMI[is.na(cardioDataCalEurosc_II$BMI)] )

unique(cardioDataCalEurosc_II$BMI)

###########################################################
#Poor Mobility 

cardioDataCalEurosc_II$Mobility <- as.numeric(as.character(cardioDataCalEurosc_II$Mobility))

unique(cardioDataCalEurosc_II$Mobility)



#################################################################
#FirstOperatorGrade

unique(cardioDataCalEurosc_II$FirstOperatorGrade)

sum(is.na(cardioDataCalEurosc_II$FirstOperatorGrade)) 

table(cardioDataCalEurosc_II$FirstOperatorGrade)

#re-order levels
cardioDataCalEurosc_II$FirstOperatorGrade  <- gsub('1', '7', cardioDataCalEurosc_II$FirstOperatorGrade)  #7 is temp

cardioDataCalEurosc_II$FirstOperatorGrade  <- gsub('2', '1', cardioDataCalEurosc_II$FirstOperatorGrade)  #7 is temp

cardioDataCalEurosc_II$FirstOperatorGrade  <- gsub('3', '2', cardioDataCalEurosc_II$FirstOperatorGrade)  #7 is temp

cardioDataCalEurosc_II$FirstOperatorGrade  <- gsub('4', '3', cardioDataCalEurosc_II$FirstOperatorGrade)  #7 is temp

cardioDataCalEurosc_II$FirstOperatorGrade  <- gsub('7', '0', cardioDataCalEurosc_II$FirstOperatorGrade)  #7 is temp

cardioDataCalEurosc_II$FirstOperatorGrade[is.na(cardioDataCalEurosc_II$FirstOperatorGrade)] <- 0

cardioDataCalEurosc_II$FirstOperatorGrade <- as.numeric(as.character(cardioDataCalEurosc_II$FirstOperatorGrade))

unique(cardioDataCalEurosc_II$FirstOperatorGrade)


#################################################################
#CABG

unique(cardioDataCalEurosc_II$CABG)


#################################################################
#NumberValves

unique(cardioDataCalEurosc_II$NumberValves)

cardioDataCalEurosc_II$NumberValves[is.na(cardioDataCalEurosc_II$NumberValves) ] <- 0

unique(cardioDataCalEurosc_II$NumberValves)


#################################################################
#AVProcedure

unique(cardioDataCalEurosc_II$AVProcedure)

table(cardioDataCalEurosc_II$AVProcedure)

#re-order levels
cardioDataCalEurosc_II$AVProcedure  <- gsub('7', '0', cardioDataCalEurosc_II$AVProcedure)  

cardioDataCalEurosc_II$AVProcedure  <- gsub('1', '10', cardioDataCalEurosc_II$AVProcedure)  #10 is temp

#2-6 becomes 1: repair
cardioDataCalEurosc_II$AVProcedure  <- gsub('2', '1', cardioDataCalEurosc_II$AVProcedure)
cardioDataCalEurosc_II$AVProcedure  <- gsub('3', '1', cardioDataCalEurosc_II$AVProcedure)
cardioDataCalEurosc_II$AVProcedure  <- gsub('4', '1', cardioDataCalEurosc_II$AVProcedure)
cardioDataCalEurosc_II$AVProcedure  <- gsub('5', '1', cardioDataCalEurosc_II$AVProcedure)
cardioDataCalEurosc_II$AVProcedure  <- gsub('6', '1', cardioDataCalEurosc_II$AVProcedure)

#set replacement as 2
cardioDataCalEurosc_II$AVProcedure  <- gsub('10', '2', cardioDataCalEurosc_II$AVProcedure)  #10 is temp

#set 2 to 3 
cardioDataCalEurosc_II$AVProcedure  <- gsub('2', '3', cardioDataCalEurosc_II$AVProcedure)  #10 is temp

#set 9 repair or replacement to 2
cardioDataCalEurosc_II$AVProcedure  <- gsub('9', '2', cardioDataCalEurosc_II$AVProcedure)  #10 is temp

cardioDataCalEurosc_II$AVProcedure <- as.numeric(as.character(cardioDataCalEurosc_II$AVProcedure))

unique(cardioDataCalEurosc_II$AVProcedure)


#################################################################
#PreopDialysis

unique(cardioDataCalEurosc_II$PreopDialysis)

cardioDataCalEurosc_II$PreopDialysis[is.na(cardioDataCalEurosc_II$PreopDialysis) ] <- 0

#re-order levels
cardioDataCalEurosc_II$PreopDialysis  <- gsub('1', '7', cardioDataCalEurosc_II$PreopDialysis)  #7 is temp 

cardioDataCalEurosc_II$PreopDialysis  <- gsub('3', '1', cardioDataCalEurosc_II$PreopDialysis)  #7 is temp 

cardioDataCalEurosc_II$PreopDialysis  <- gsub('2', '3', cardioDataCalEurosc_II$PreopDialysis)  #7 is temp 

cardioDataCalEurosc_II$PreopDialysis  <- gsub('7', '2', cardioDataCalEurosc_II$PreopDialysis)  #7 is temp 

cardioDataCalEurosc_II$PreopDialysis <- as.numeric(as.character(cardioDataCalEurosc_II$PreopDialysis))

unique(cardioDataCalEurosc_II$PreopDialysis)


#################################################################
#Creatinine

unique(cardioDataCalEurosc_II$Creatinine)

hist(cardioDataCalEurosc_II$Creatinine)

cardioDataCalEurosc_II$Creatinine <- with(cardioDataCalEurosc_II, impute(Creatinine, median))

length(cardioDataCalEurosc_II$Creatinine[is.na(cardioDataCalEurosc_II$Creatinine)] )

unique(cardioDataCalEurosc_II$Creatinine)


#################################################################
#Age

unique(cardioDataCalEurosc_II$Age)	

hist(cardioDataCalEurosc_II$Age)

cardioDataCalEurosc_II$Age <- round(cardioDataCalEurosc_II$Age)

length(cardioDataCalEurosc_II$Age[is.na(cardioDataCalEurosc_II$Age)] )

cardioDataCalEurosc_II$Age

sort(unique(cardioDataCalEurosc_II$Age ))


#################################################################
#PumpCase

unique(cardioDataCalEurosc_II$PumpCase)

cardioDataCalEurosc_II$PumpCase[is.na(cardioDataCalEurosc_II$PumpCase)] <- 0

unique(cardioDataCalEurosc_II$PumpCase)



#################################################################
#CPS

unique(cardioDataCalEurosc_II$CPS)


#################################################################
#Endocarditis

unique(cardioDataCalEurosc_II$Endocarditis)


#################################################################
#RecentMI

unique(cardioDataCalEurosc_II$RecentMI)


#################################################################
#PostInfarctVSD

unique(cardioDataCalEurosc_II$PostInfarctVSD)


###############################################################
#MVProcedure

unique(cardioDataCalEurosc_II$MVProcedure)


#re-order levels
cardioDataCalEurosc_II$MVProcedure  <- gsub('7', '0', cardioDataCalEurosc_II$MVProcedure)  

cardioDataCalEurosc_II$MVProcedure  <- gsub('1', '10', cardioDataCalEurosc_II$MVProcedure)  #10 is temp

#2-6 becomes 1: repair
cardioDataCalEurosc_II$MVProcedure  <- gsub('2', '1', cardioDataCalEurosc_II$MVProcedure)
cardioDataCalEurosc_II$MVProcedure  <- gsub('3', '1', cardioDataCalEurosc_II$MVProcedure)
cardioDataCalEurosc_II$MVProcedure  <- gsub('4', '1', cardioDataCalEurosc_II$MVProcedure)
cardioDataCalEurosc_II$MVProcedure  <- gsub('5', '1', cardioDataCalEurosc_II$MVProcedure)
cardioDataCalEurosc_II$MVProcedure  <- gsub('6', '1', cardioDataCalEurosc_II$MVProcedure)

#set replacement as 2
cardioDataCalEurosc_II$MVProcedure  <- gsub('10', '2', cardioDataCalEurosc_II$MVProcedure)  #10 is temp

#set 2 to 3 
cardioDataCalEurosc_II$MVProcedure  <- gsub('2', '3', cardioDataCalEurosc_II$MVProcedure)  #10 is temp

#set 9 repair or replacement to 2
cardioDataCalEurosc_II$MVProcedure  <- gsub('9', '2', cardioDataCalEurosc_II$MVProcedure)  #10 is temp

cardioDataCalEurosc_II$MVProcedure <- as.numeric(as.character(cardioDataCalEurosc_II$MVProcedure))


#cardioDataCalEurosc_II$MVProcedure[cardioDataCalEurosc_II$MVProcedure > 0] <- 1

unique(cardioDataCalEurosc_II$MVProcedure)


#################################################################
#TVProcedure

unique(cardioDataCalEurosc_II$TVProcedure)

#re-order levels
cardioDataCalEurosc_II$TVProcedure  <- gsub('7', '0', cardioDataCalEurosc_II$TVProcedure)  

cardioDataCalEurosc_II$TVProcedure  <- gsub('1', '10', cardioDataCalEurosc_II$TVProcedure)  #10 is temp

#2-6 becomes 1: repair
cardioDataCalEurosc_II$TVProcedure  <- gsub('2', '1', cardioDataCalEurosc_II$TVProcedure)
cardioDataCalEurosc_II$TVProcedure  <- gsub('3', '1', cardioDataCalEurosc_II$TVProcedure)
cardioDataCalEurosc_II$TVProcedure  <- gsub('4', '1', cardioDataCalEurosc_II$TVProcedure)
cardioDataCalEurosc_II$TVProcedure  <- gsub('5', '1', cardioDataCalEurosc_II$TVProcedure)
cardioDataCalEurosc_II$TVProcedure  <- gsub('6', '1', cardioDataCalEurosc_II$TVProcedure)

#set replacement as 2
cardioDataCalEurosc_II$TVProcedure  <- gsub('10', '2', cardioDataCalEurosc_II$TVProcedure)  #10 is temp

#set 2 to 3 
cardioDataCalEurosc_II$TVProcedure  <- gsub('2', '3', cardioDataCalEurosc_II$TVProcedure)  #10 is temp

#set 9 repair or replacement to 2
cardioDataCalEurosc_II$TVProcedure  <- gsub('9', '2', cardioDataCalEurosc_II$TVProcedure)  #10 is temp

cardioDataCalEurosc_II$TVProcedure <- as.numeric(as.character(cardioDataCalEurosc_II$TVProcedure))

unique(cardioDataCalEurosc_II$TVProcedure)


#################################################################
#PVProcedure

unique(cardioDataCalEurosc_II$PVProcedure)


#re-order levels
cardioDataCalEurosc_II$PVProcedure  <- gsub('7', '0', cardioDataCalEurosc_II$PVProcedure)  

cardioDataCalEurosc_II$PVProcedure  <- gsub('1', '10', cardioDataCalEurosc_II$PVProcedure)  #10 is temp

#2-6 becomes 1: repair
cardioDataCalEurosc_II$PVProcedure  <- gsub('2', '1', cardioDataCalEurosc_II$PVProcedure)
cardioDataCalEurosc_II$PVProcedure  <- gsub('3', '1', cardioDataCalEurosc_II$PVProcedure)
cardioDataCalEurosc_II$PVProcedure  <- gsub('4', '1', cardioDataCalEurosc_II$PVProcedure)
cardioDataCalEurosc_II$PVProcedure  <- gsub('5', '1', cardioDataCalEurosc_II$PVProcedure)
cardioDataCalEurosc_II$PVProcedure  <- gsub('6', '1', cardioDataCalEurosc_II$PVProcedure)

#set replacement as 2
cardioDataCalEurosc_II$PVProcedure  <- gsub('10', '2', cardioDataCalEurosc_II$PVProcedure)  #10 is temp

#set 2 to 3 
cardioDataCalEurosc_II$PVProcedure  <- gsub('2', '3', cardioDataCalEurosc_II$PVProcedure)  #10 is temp

#set 9 repair or replacement to 2
cardioDataCalEurosc_II$PVProcedure  <- gsub('9', '2', cardioDataCalEurosc_II$PVProcedure)  #10 is temp

cardioDataCalEurosc_II$PVProcedure <- as.numeric(as.character(cardioDataCalEurosc_II$PVProcedure))


unique(cardioDataCalEurosc_II$PVProcedure)


#################################################################
#DaysBetweenLHCOp

unique(cardioDataCalEurosc_II$DaysBetweenLHCOp)

hist(cardioDataCalEurosc_II$DaysBetweenLHCOp)

cardioDataCalEurosc_II$DaysBetweenLHCOp <- with(cardioDataCalEurosc_II, impute(DaysBetweenLHCOp, median))

length(cardioDataCalEurosc_II$DaysBetweenLHCOp[is.na(cardioDataCalEurosc_II$DaysBetweenLHCOp)] )

unique(cardioDataCalEurosc_II$DaysBetweenLHCOp)


#################################################################
#CADExtent

unique(cardioDataCalEurosc_II$CADExtent)

cardioDataCalEurosc_II$CADExtent  <- gsub('4', '0', cardioDataCalEurosc_II$CADExtent)  

cardioDataCalEurosc_II$CADExtent[is.na(cardioDataCalEurosc_II$CADExtent)] <- 0

cardioDataCalEurosc_II$CADExtent <- as.numeric(as.character(cardioDataCalEurosc_II$CADExtent))


unique(cardioDataCalEurosc_II$CADExtent)


#################################################################
#Nitrates

unique(cardioDataCalEurosc_II$Nitrates)

#################################################################
#Inotropes 

unique(cardioDataCalEurosc_II$Inotropes)

#################################################################
#Weight

unique(cardioDataCalEurosc_II$Weight)

hist(cardioDataCalEurosc_II$Weight)

cardioDataCalEurosc_II$Weight <- with(cardioDataCalEurosc_II, impute(Weight, median))

length(cardioDataCalEurosc_II$Weight[is.na(cardioDataCalEurosc_II$Weight)] )

unique(cardioDataCalEurosc_II$Weight)


#################################################################
#NumberGrafts

unique(cardioDataCalEurosc_II$NumberGrafts)

cardioDataCalEurosc_II$NumberGrafts[is.na(cardioDataCalEurosc_II$NumberGrafts)] <- 0

unique(cardioDataCalEurosc_II$NumberGrafts)


#################################################################
#Ao.Root.Procedure

unique(cardioDataCalEurosc_II$Ao.Root.Procedure)

cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('2', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('3', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('4', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('5', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('6', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('7', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('8', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('9', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  
cardioDataCalEurosc_II$Ao.Root.Procedure  <- gsub('10', '1', cardioDataCalEurosc_II$Ao.Root.Procedure)  

cardioDataCalEurosc_II$Ao.Root.Procedure <- as.numeric(as.character(cardioDataCalEurosc_II$Ao.Root.Procedure))

unique(cardioDataCalEurosc_II$Ao.Root.Procedure)


#################################################################
#Ao.Asc.Procedure

unique(cardioDataCalEurosc_II$Ao.Asc.Procedure)


cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('2', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('3', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('4', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('5', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('6', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('7', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('8', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('9', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  
cardioDataCalEurosc_II$Ao.Asc.Procedure  <- gsub('10', '1', cardioDataCalEurosc_II$Ao.Asc.Procedure)  

cardioDataCalEurosc_II$Ao.Asc.Procedure <- as.numeric(as.character(cardioDataCalEurosc_II$Ao.Asc.Procedure))

unique(cardioDataCalEurosc_II$Ao.Asc.Procedure)


#################################################################
#Ao.Arch.Procedure

unique(cardioDataCalEurosc_II$Ao.Arch.Procedure)


cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('2', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('3', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('4', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('5', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('6', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('7', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('8', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('9', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('10', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('11', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  
cardioDataCalEurosc_II$Ao.Arch.Procedure  <- gsub('12', '1', cardioDataCalEurosc_II$Ao.Arch.Procedure)  

cardioDataCalEurosc_II$Ao.Arch.Procedure <- as.numeric(as.character(cardioDataCalEurosc_II$Ao.Arch.Procedure))

unique(cardioDataCalEurosc_II$Ao.Arch.Procedure)


#################################################################
#Ao.Desc.Procedure

unique(cardioDataCalEurosc_II$Ao.Desc.Procedure)


cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('2', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('3', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('4', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('5', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('6', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('7', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('8', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('9', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('10', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('11', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  
cardioDataCalEurosc_II$Ao.Desc.Procedure  <- gsub('12', '1', cardioDataCalEurosc_II$Ao.Desc.Procedure)  

cardioDataCalEurosc_II$Ao.Desc.Procedure <- as.numeric(as.character(cardioDataCalEurosc_II$Ao.Desc.Procedure))

unique(cardioDataCalEurosc_II$Ao.Desc.Procedure)



#################################################################
#mechanicalSupport 


unique(cardioDataCalEurosc_II$Impeller)

unique(cardioDataCalEurosc_II$Other.Mech.Support)

unique(cardioDataCalEurosc_II$VAD)

unique(cardioDataCalEurosc_II$IABP)

cardioDataCalEurosc_II$mechanicalSupport[cardioDataCalEurosc_II$Impeller == 1] <- 1 

cardioDataCalEurosc_II$mechanicalSupport[cardioDataCalEurosc_II$Other.Mech.Support == 1] <- 1 

cardioDataCalEurosc_II$mechanicalSupport[cardioDataCalEurosc_II$VAD == 1] <- 1 

cardioDataCalEurosc_II$mechanicalSupport[cardioDataCalEurosc_II$IABP == 1] <- 1 

table(cardioDataCalEurosc_II$IABP)
table(cardioDataCalEurosc_II$VAD)
table(cardioDataCalEurosc_II$Impeller)
table(cardioDataCalEurosc_II$Other.Mech.Support)
table(cardioDataCalEurosc_II$mechanicalSupport)

cardioDataCalEurosc_II$mechanicalSupport[is.na(cardioDataCalEurosc_II$mechanicalSupport)] <- 0

#Remove component variables 

cardioDataCalEurosc_II$IABP <- NULL
cardioDataCalEurosc_II$VAD <- NULL
cardioDataCalEurosc_II$Impeller <- NULL
cardioDataCalEurosc_II$Other.Mech.Support <- NULL

unique(cardioDataCalEurosc_II$mechanicalSupport)


#################################################################
#Median.Sternotomy

unique(cardioDataCalEurosc_II$Median.Sternotomy)

#################################################################
#Partial.Sternotomy

unique(cardioDataCalEurosc_II$Partial.Sternotomy)

#################################################################
#Thoracotomy

#unique(cardioDataCalEurosc_II$Thoracotomy)

#################################################################
#Mini.Thoracotomy 

unique(cardioDataCalEurosc_II$Mini.Thoracotomy)

#################################################################
#imdDecile

unique(cardioDataCalEurosc_II$imdDecile)

cardioDataCalEurosc_II$imdDecile[is.na(cardioDataCalEurosc_II$imdDecile)] <- 1

unique(cardioDataCalEurosc_II$imdDecile)


#################################################################
#Hypertension

unique(cardioDataCalEurosc_II$Hypertension)

#################################################################
#PulHTN

unique(cardioDataCalEurosc_II$PulHTN)

#################################################################
#Aortic.AAS

unique(cardioDataCalEurosc_II$Aortic.AAS)

#################################################################
#Operation

# Operation: 1-cabg only, 2-avr only, 3-cabg+avr, 4-cabg+mvr, 5-mvr only, 6, aortic +/-other, 99-no procedure recorded, 7-NA->other, TD: manually added 8-double valve procedure


unique(cardioDataCalEurosc_II$Operation)

cardioDataCalEurosc_II$Operation  <- gsub('99', '9', cardioDataCalEurosc_II$Operation)  

cardioDataCalEurosc_II$Operation[(cardioDataCalEurosc_II$NumberValves==2)]=8

unique(cardioDataCalEurosc_II$Operation)

#################################################################
#CrCl

unique(cardioDataCalEurosc_II$CrCl)

#################################################################
#yearMonth

unique(cardioDataCalEurosc_II$yearMonth)
unique(cardioDataCalEurosc_II$Year)

#################################################################
#OpDate

unique(cardioDataCalEurosc_II$OpDate)

#################################################################
#Year

unique(cardioDataCalEurosc_II$Year)

ncol(cardioDataCalEurosc_II)

#################################################################
#HospCode

unique(cardioDataCalEurosc_II$HospCode)

#store backup version for later one hot-encoding conversion 
HospCode <- cardioDataCalEurosc_II$HospCode

#Three dataset versions: 1.convert to ordinal; 2.hot-encode; 3. No HospCode 
#1. convert to ordinal 

cardioDataCalEurosc_II$HospCode <- unclass(factor(cardioDataCalEurosc_II$HospCode))

sort(unique(cardioDataCalEurosc_II$HospCode))

colnames(cardioDataCalEurosc_II)

#Save ordinal version of pre-processed dataset 

#save(cardioDataCalEurosc_II, file = "C:/Users/qiani/Documents/ADB role university/Euroscore_expansion/expandESII_DtMon_randPreproc_OrdinalCentre.csv")

write.csv(cardioDataCalEurosc_II, file = "../modelInput/expandESII_DtMon_randPreproc_OrdinalCentre.csv")

#Split into training and test 
cardioDataCalEurosc_II_Train_newData <- cardioDataCalEurosc_II[cardioDataCalEurosc_II$Year %in% c(2012:2016), ] 
cardioDataCalEurosc_II_Test_newData <- cardioDataCalEurosc_II[cardioDataCalEurosc_II$Year %in% c(2017:2019), ]

nrow(cardioDataCalEurosc_II_Train_newData)  #157196
nrow(cardioDataCalEurosc_II_Test_newData)   #69891 

head(cardioDataCalEurosc_II_Test_newData$ES_II_link_ID)
tail(cardioDataCalEurosc_II_Test_newData$ES_II_link_ID)


# library(data.table)
# train <- fread(file="D:/Installation WorkPCToLaptop/Dashboard/cardiacMLCalDriftAnalysis/CardiacML_CalibrationDrift_latestVersionsDocumentationsFromOneDrive/DriftAdaptation/Euroscore_II/data_for_ml/2012_2016/ES_II_var_preProcessed_training_2012_2016.csv")
# colnames(train)

write.csv(cardioDataCalEurosc_II_Train_newData, file = "../modelInput/2012_2016/varSelectES_II_Month_preProcessed_training_2012_2016.csv")

write.csv(cardioDataCalEurosc_II_Test_newData, file = "../modelInput/2017_2019/varSelectES_II_Month_preProcessed_test_2017_2019.csv")


#Data checking ############################################################
#only need to check ordinal version - not needed as checking should be done before full imputation 

