
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#ES II 
#$e2<-exp(-5.324537+0.0285181+((nacsa$Age_Imputed_ES2-60)*0.0285181)+(as.numeric(nacsa$Gender)*0.2196434)+(nacsa$CrCl.Category.Moderate*0.303553)+(nacsa$CrCl.Category.Severe*0.8592256)+(nacsa$CrCl.Category.Dialysis*0.6421508)+(nacsa$PVD*0.5360268)+(nacsa$PoorMobility*0.2407181)+(nacsa$PrevSurgery*1.118599)+(nacsa$PulmonaryDisease*0.1886564)+(nacsa$Endocarditis*0.6194522)+(nacsa$CPS*1.086517)+(nacsa$Diabetes.Insulin*0.3542749)+(nacsa$NYHA2*0.1070545)+(nacsa$NYHA3*0.2958358)+(nacsa$NYHA4*0.5597929)+(nacsa$CCS4*0.2226147)+(nacsa$LVF2.Moderate*0.3150652)+(nacsa$LVF2.Poor*0.8084096)+(nacsa$LVF2.VeryPoor*0.9346919 )+(nacsa$RecentMI*0.1528943)+(as.numeric(nacsa$PAP.Moderate)*0.1788899)+(as.numeric(nacsa$PAP.Severe)*0.3491475)+(nacsa$Urgency.Urgent*0.3174673)+(nacsa$Urgency.Emergency*0.7039121)+(nacsa$Urgency.Salvage*1.362947)+(nacsa$ThoracicAortic*0.6527205)+(nacsa$SingleNonCABG*0.0062118)+(nacsa$TwoProcedures*0.5521478)+(nacsa$ThreeProcedures*0.9724533))


nacsa.predict$imdDecile <- nacsa.predict$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.

unique(nacsa.predict$imdDecile)

expandES_II_var_raw <- subset(nacsa.predict, select=c(HospCode, Operation, Gender,Payer,CCS,NYHA,PrevMI,IntervalMI,PCI,PrevCABG,PrevValve,PrevAoAscArch,PrevAoDesc,Hypertension,PulHTN,CrCl,Aortic.AAS,Other.Mech.Support,PrevThoracic,Diabetes,Smoking,PulmonaryDisease,Stroke,NeuroDys,PVD,PreopAF,PreopVFT,PreopCHB.pacing,LMS,PAsys,LVF2,CardiogenicShock,VentilatedPreop,Urgency,PrevOp,BMI,Mobility,FirstOperatorGrade,CABG,NumberValves,AVProcedure,PreopDialysis,Creatinine,Age,PumpCase,CPS,Endocarditis,RecentMI,PostInfarctVSD,MVProcedure,TVProcedure,PVProcedure,DaysBetweenLHCOp,CADExtent,Nitrates,Inotropes,Weight,NumberGrafts,Ao.Root.Procedure,Ao.Asc.Procedure,Ao.Arch.Procedure,Ao.Desc.Procedure,Ao.Abdo.Procedure,Impeller,VAD,IABP,Median.Sternotomy,Partial.Sternotomy,Thoracotomy,Mini.Thoracotomy,imdDecile,Mortality,OpDate,Year))


ncol(expandES_II_var_raw) #74 variables including outcome 

expandEurosc_II_DateMonth <- expandES_II_var_raw

ncol(expandEurosc_II_DateMonth)

#ES II variables require further pre-processing 

write.csv(x=expandEurosc_II_DateMonth, file="../modelInput/expandedEurosc_II_DateMonth.csv")

library(data.table)

expandEurosc_II_DateMonth <- fread(file="../modelInput/expandedEurosc_II_DateMonth.csv", data.table = F)


set.seed(7)
#perform row randomisation 
expandEurosc_II_DateMonth <- expandEurosc_II_DateMonth[sample(nrow(expandEurosc_II_DateMonth),replace=FALSE),]

expandEurosc_II_DateMonth$V1
#verified randomisation and seed replication

colnames(expandEurosc_II_DateMonth)

#Eurosc_II_DateMonth$OpDate <- NULL

expandEurosc_II_DateMonth_randomised <- expandEurosc_II_DateMonth
tail(expandEurosc_II_DateMonth_randomised)
#checked consistency

#rename id column 
names(expandEurosc_II_DateMonth_randomised)[names(expandEurosc_II_DateMonth_randomised) == 'V1'] <- 'ES_II_link_ID'
nrow(expandEurosc_II_DateMonth_randomised)  #642125 records checked 


#remove missing outcome for ES II 
expandEurosc_II_DateMonth_randomised$mtly <- expandEurosc_II_DateMonth_randomised$Mortality
expandEurosc_II_DateMonth_randomised$Mortality <- NULL

sum(is.na(expandEurosc_II_DateMonth_randomised$mtly))  #4188
4188/nrow(expandEurosc_II_DateMonth_randomised) * 100  #0.6522%

#remove all rows where mtly is NA
expandEurosc_II_DateMonth_randomised <- expandEurosc_II_DateMonth_randomised[!is.na(expandEurosc_II_DateMonth_randomised$mtly), ]

sum(is.na(expandEurosc_II_DateMonth_randomised$mtly)) #0

nrow(expandEurosc_II_DateMonth_randomised)  #637937

length(unique(expandEurosc_II_DateMonth_randomised$HospCode)) #45

sum(expandEurosc_II_DateMonth_randomised$mtly)  #20812


sum(expandEurosc_II_DateMonth_randomised$mtly) / nrow(expandEurosc_II_DateMonth_randomised) * 100 #3.26%


colnames(expandEurosc_II_DateMonth_randomised)
expandEurosc_II_DateMonth_randomised$ES_II_link_ID



expandEurosc_II_DateMonth_randomised <- expandEurosc_II_DateMonth_randomised[expandEurosc_II_DateMonth_randomised$Year %in% c(2012:2019), ] 


#Get Year-Month data 
expandEurosc_II_DateMonth_randomised$yearMonth <- as.POSIXct(as.character(expandEurosc_II_DateMonth_randomised$OpDate), format="%Y-%m-%d")

expandEurosc_II_DateMonth_randomised$yearMonth <- format(as.Date(expandEurosc_II_DateMonth_randomised$yearMonth), "%Y-%m")

unique(expandEurosc_II_DateMonth_randomised$yearMonth)


nrow(expandEurosc_II_DateMonth_randomised) #224318 for inclusion after exclusion of patients before year 2012

expandEurosc_II_DateMonth_randomised$ES_II_link_ID

write.csv(x=expandEurosc_II_DateMonth_randomised, file="../modelInput/expandEurosc_II_DateMonth_randomised.csv", row.names = F)

#read in randomised version
expandEurosc_II_DateMonth_randomised <- fread(file="../modelInput/expandEurosc_II_DateMonth_randomised.csv")

expandEurosc_II_DateMonth_randomised$ES_II_link_ID
#verified 


unique(expandEurosc_II_DateMonth_randomised)

#There are missing column values
colNm <- colnames(expandEurosc_II_DateMonth_randomised)[colSums(is.na(expandEurosc_II_DateMonth_randomised)) > 0] 
colNm

colnames(expandEurosc_II_DateMonth_randomised)  #74 + 2 (ES_II_link_ID & yearMonth) = 76 including outcome (73 Actual risk factors)

library(DataExplorer)
create_report(expandEurosc_II_DateMonth_randomised)

#Exclude risk factors based on missingness and correlation report above

#Exclude following as contain either only one case or contains a subgroup including only 1 case - see correlation matrix
table(expandEurosc_II_DateMonth_randomised$Thoracotomy) #1 case
table(expandEurosc_II_DateMonth_randomised$PrevAoDesc)  #1 case
table(expandEurosc_II_DateMonth_randomised$Ao.Abdo.Procedure) #contain category 11 (anatomic) with only 1 case 
table(expandEurosc_II_DateMonth_randomised$PrevThoracic) #1 case

expandEurosc_II_DateMonth_randomised$Thoracotomy <- NULL
expandEurosc_II_DateMonth_randomised$PrevAoDesc <- NULL
expandEurosc_II_DateMonth_randomised$Ao.Abdo.Procedure <- NULL
expandEurosc_II_DateMonth_randomised$PrevThoracic <- NULL

#Exclude PAsys (Pulmonary Artery Systolic Pressure) is excluded as missingness rate following backfilling exceeded threshold of 30% (missingness = 68.53%)
expandEurosc_II_DateMonth_randomised$PAsys <- NULL

expandEurosc_II_DateMonth_randomised$V1
expandEurosc_II_DateMonth_randomised$ES_II_link_ID
ncol(expandEurosc_II_DateMonth_randomised)  #76 - 5 = 71 variables - 1 outcome - 2 (ES_II_link_ID & yearMonth) = 68 variables
colnames(expandEurosc_II_DateMonth_randomised)


write.csv(x=expandEurosc_II_DateMonth_randomised, file="../modelInput/expandEurosc_II_DateMonth_randomised.csv")

###################
#TD:

#pre-processing ES II variables will be done in separate script 
#go to C:/Users/qiani/Documents/ADB role university/Euroscore_expansion
#go to Preprocessing_ES_II.R

