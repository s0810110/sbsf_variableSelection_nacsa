
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

devtools::install_github("itsrainingdata/ccdrAlgorithm"). #use instead of "itsrainingdata/ccdrAlgorithm/dev"
#devtools::install_github(c("itsrainingdata/sparsebn", "itsrainingdata/sparsebnUtils/dev", "itsrainingdata/ccdrAlgorithm/dev", "gujyjean/discretecdAlgorithm"))
devtools::install_github("itsrainingdata/sparsebn")

library("sparsebn")
library(data.table)


cardioDataCal_AKI_Train <- fread('../modelInput/2017_2019/varSelectES_II_Month_preProcessed_test_2017_2019.csv', data.table=FALSE)


N <- 1
# Drop first N columns of dataframe
cardioDataCal_AKI_Train <- cardioDataCal_AKI_Train[, (N + 1):ncol(cardioDataCal_AKI_Train)]
cardioDataCal_AKI_Train

colnames(cardioDataCal_AKI_Train)

# Move metadata to the right-hand side of the dataset: mtly, Year, yearMonth, OpDate, ES_II_link_ID
# Partition these during cv and save separately as a single dataframe with correspondence to prediction

# Reordering columns for outcome and year
new_cols <- c(setdiff(colnames(cardioDataCal_AKI_Train), "mtly"), "mtly")
df <- cardioDataCal_AKI_Train[, new_cols]

new_cols <- c(setdiff(colnames(df), "Year"), "Year")
df <- df[, new_cols]

new_cols <- c(setdiff(colnames(df), "yearMonth"), "yearMonth")
df <- df[, new_cols]

new_cols <- c(setdiff(colnames(df), "OpDate"), "OpDate")
df <- df[, new_cols]

new_cols <- c(setdiff(colnames(df), "ES_II_link_ID"), "ES_II_link_ID")
df <- df[, new_cols]

df

colnames(df)

new_cols <- c(colnames(df)[colnames(df) != "Year"], "Year")
df <- df[, new_cols]
df  # Update re-ordered dataframe
cardioDataCal_Eurosc_II_Train <- df

cardioDataCal_Eurosc_II_TrainInput <- cardioDataCal_Eurosc_II_Train[, 1:61]

colnames(cardioDataCal_Eurosc_II_TrainInput)


#non sparse version #########################



#hotEncTrainTop18_graph <- sparsebnData(hotEncTrainTop18, type = "discrete")
cardioDataCal_Eurosc_II_TrainInput_graph <- sparsebnData(cardioDataCal_Eurosc_II_TrainInput, type = "continuous")


#Structure learning
cardioDataCal_Eurosc_II_TrainInput_graph.learn <- estimate.dag(cardioDataCal_Eurosc_II_TrainInput_graph)

#Model selection

selected.lambda <- select.parameter(cardioDataCal_Eurosc_II_TrainInput_graph.learn, cardioDataCal_Eurosc_II_TrainInput_graph) #5

selected.lambda = 10


#plot for first 50 (not necessarily best ones) from all urine metabolites 
tiff("xgboostHotEnc_varAll.tiff", units="in", width=5.8, height=5, res=300)


plot(cardioDataCal_Eurosc_II_TrainInput_graph.learn[[selected.lambda]], vertex.label = get.nodes(cardioDataCal_Eurosc_II_TrainInput_graph.learn[[selected.lambda]])
     , vertex.size = 10
     , vertex.label.cex = 0.5
     , vertex.label.color = gray(0)
     , vertex.color = gray(0.9)
     , edge.color = gray(0)
     , edge.arrow.size = 0.2)

dev.off() 

