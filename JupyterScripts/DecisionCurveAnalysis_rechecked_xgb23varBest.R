

library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


library(data.table)



set.seed(7);


#Xgboost ----------------------------------

df_Xgboost_test <- fread('modelOutput/df_Eurosc_II_xgboost_test_var23Best.csv', 
                         select=c('xgboost_test', 'mtly'), data.table=FALSE)

df_Xgboost_test <- df_Xgboost_test[complete.cases(df_Xgboost_test), ]


df_logistic_test <- fread('modelOutput/df_Eurosc_II_logisticReg_test.csv', 
                         select=c('logisticREg_test', 'mtly'), data.table=FALSE)

df_logistic_test <- df_logistic_test[complete.cases(df_logistic_test), ]




#n_models=list(`Logistic Regression`=df_lr_test, `Neuronetwork`=df_NN_test, `Random Forest`=df_rf_test, `EuroSCORE II`=df_test_Eurosc, `Weighted SVM`=df_svc_test, `Xgboost`=df_Xgboost_test )
n_models=list(`Xgboost`=df_Xgboost_test, `Logistic Regression`=df_logistic_test )

names(n_models)


library(data.table)
library(dplyr) #for %>%
#for interactive plot 
library(plotly)

ntbft <- function(data,outcome,frm=NULL, exterdt=NULL,pred=NULL,xstart=0.01,
                  xstop=0.99,step=0.01,type="treated"){
  
  pt<-seq(from=xstart,to=xstop,by=step)
  lpt<-length(pt)
  if(type=="treated") coef<-cbind(rep(1,lpt),rep(0,lpt))
  if(type=="untreated") coef<-cbind(rep(0,lpt),rep(1,lpt))
  if(type=="overall") coef<-cbind(rep(1,lpt),rep(1,lpt))
  if(type=="adapt") coef<-cbind(1-pt,pt)
  response<-as.vector(t(data[outcome]))
  if(is.data.frame(exterdt)) response<-as.vector(t(exterdt[outcome]))
  event.rate<-mean(response)
  nball<- event.rate-(1-event.rate)*pt/(1-pt)
  nbnone<- 1-event.rate-event.rate*(1-pt)/pt
  if(is.null(pred)){
    model<-glm(frm,data=data,family=binomial("logit"))
    pred<-model$fitted.values
    if(is.data.frame(exterdt))
      pred<-predict(model,newdata=exterdt,type="response")
  }
  
  # pred and response should be of the same length
  N<-length(pred)
  nbt<-rep(NA,lpt)
  nbu<-rep(NA,lpt)
  for(t in 1:lpt){
    tp<-sum(pred>=pt[t] & response==1)
    fp<-sum(pred>=pt[t] & response==0)
    fn<-sum(pred<pt[t] & response==1)
    tn<-sum(pred<pt[t] & response==0)
    nbt[t]<-tp/N-fp/N*(pt[t]/(1-pt[t]))
    nbu[t]<-tn/N-fn/N*((1-pt[t])/pt[t])
  }
  
  nb<-data.frame(pt)
  names(nb)<-"threshold"
  nb["all"]<-coef[,1]*nball
  nb["none"]<-coef[,2]*nbnone
  nb["pred"]<-coef[,1]*nbt+coef[,2]*nbu
  return(nb)
}

plot.ntbft<-function(nb,nolines=2:dim(nb)[2],
                     nobands=NULL,ymin=-0.1,
                     ymax=max(nb[,c(nolines,nobands)],na.rm=T),
                     legpos=c(0.8,0.8), ylimMin=-0.01, ylimMax=0.01){
  ylow<-nb[,1]
  yup<-nb[,1]
  if(!is.null(nobands)){
    ylow<-nb[,min(nobands)]
    yup<-nb[,max(nobands)]
  }
  nb.melt<-melt(nb[,c(1,nolines)],
                id.vars="threshold",
                value.name="Netbenefit",variable.name="Models")
  print(ggplot(data = nb.melt, aes(x=threshold,y=Netbenefit,
                                   colour=Models,group=Models)) + coord_cartesian(ylim = c(ylimMin, ylimMax)) +
          theme(legend.position=legpos)+
          geom_line()+
          scale_color_manual(values=c('black', 'orange', 'green', 'blue')) +  #remove if using automatic random colors
          #geom_ribbon(data=nb, aes(x=threshold, ymin=ylow,ymax=yup), linetype=2,alpha=0.2) +
          scale_y_continuous(limits=c(min(nb.melt$Netbenefit),max(nb.melt$Netbenefit))) +
          scale_x_continuous(limits=c(min(ylow),max(ylow)))  +
          xlab("Threshold probability")+ylab("Net benefit"))
}



xstart <- 0.01; xstop <- 0.99; step <- 0.01

#predVect <-as.vector(t(n_modelsComplete["LR_prob_singleVec"]))

#type <- "overall"
#type <- "treated"

#n_modelsComplete <- n_models[[i]][complete.cases(n_models[[i]]), ] #remove missing or empty values

#nb.NN<-ntbft(data=n_models[[2]], outcome="mtly",
#                 pred = n_models[[2]][,1], xstart=xstart, xstop=xstop,
#                 step=step, type="overall")

library(ggplot2)
library(reshape2)

#a <- toString(names(n_models[i+1]))

#nb <- cbind(nb.simple, nb.NN$pred)
#colnames(nb)[i + 4] <- names(n_models[i + 2])

#ames(nb)[names(nb)=="pred"] <- names(n_models[i])


nb_total <- data.frame()
summaryStat <- data.frame()


for (i in seq_along(n_models)) {
  print(i)

  n_modelsComplete <- n_models[[i]][complete.cases(n_models[[i]]), ] #remove missing or empty values
  
  nb <- ntbft(data=n_modelsComplete, outcome="mtly",
               pred = n_modelsComplete[,1], xstart=xstart, xstop=xstop,
               step=step, type="treated")
  if(i == 1) {
    nb_total <- nb
    colnames(nb_total)[i + 3] <- names(n_models[i])
  } else {
    nb_total <- cbind(nb_total, nb$pred)
    colnames(nb_total)[i + 3] <- names(n_models[i]) #else we start from iteration i=2
  }
  
}

tiff("results/combined/clinicInform_23VarModel/XgbLR_2017_2019_DecisionCurveAnalysisTreated.tiff", units="in", width=5, height=5, res=300)
#tiff("../modelOutput/Figures/Calibration/Yearly_Euroscore_II_2017_2019_DecisionCurveAnalysisOverall.tiff", units="in", width=5, height=5, res=300)
#tiff("C:/Installation WorkPCToLaptop/Dashboard/cardiacMLCalDriftAnalysis/code_Euroscore_II_TestOnlyRequired30_newDataset/Training2012_2016/Figures/Calibration/Yearly_Euroscore_II_2012_2016_DecisionCurveAnalysisOverall.tiff", units="in", width=5, height=5, res=300)

#for treated 
q <- plot.ntbft(nb_total)

#for overall
#q <- plot.ntbft(nb_total, ylimMin=-0.08, ylimMax=1)


fig <- ggplotly(q, autosize = F, width = 650, height = 500)

fig <- plotly_build(fig)

#fig$x$data[[1]]$name <- 'Group A'

# m <- list(
# l = 50,
# r = 50,
# b = 100,
# t = 100,
# pad = 4
# )

#fig$x$layout$margin <- list(b=10,r=7, t=10.5, l=4.5)

for (i in seq_along(fig$x$data) ) {
  
  
  
  fig$x$data[[i]]$text <- sub("as.factor.*", "", fig$x$data[[i]]$text)
  
}


fig <- fig %>%
  add_annotations( text="Key", xref="paper", yref="paper",
                   x=1.02, xanchor="left",
                   y=0.65, yanchor="bottom",    # Same y as legend below
                   legendtitle=TRUE, showarrow=FALSE )  %>%
  layout( legend=list(y=0.65, yanchor="top" ), title = list(text = paste0("Euroscore II feature models: Decision Curve Analysis (Years 2017 - 2019)"), x = 0), xaxis=list(tickangle=270, title = "\n threshold", tickfont = list(size = '15')))


# fig$height  = "100%"
# fig$width = "100%"



fig %>% config(displayModeBar = F)

q         

dev.off()   

#treated
#write.csv(x=nb_total, file="C:/Installation WorkPCToLaptop/Dashboard/cardiacMLCalDriftAnalysis/code_Euroscore_II_TestOnlyRequired30_newDataset/Training2012_2016/2012_2016_Euroscore_II_DCA_NetBenefit_Treated.csv")
#write.csv(x=nb_total, file="modelOutput/2017_2019_xgboost_DCA_NetBenefit_Treated.csv")
write.csv(x=nb_total, file="modelOutput/2017_2019_combinedXgbLR_DCA_NetBenefit_Treated_var23Best.csv")

head(nb_total)

#overall
#write.csv(x=nb_total, file="../modelOutput/2017_2019_Euroscore_II_DCA_NetBenefit_Overall.csv")



plot(nb_total[,1], nb_total[,4], ylim=c(0, 0.01))

#Plot by procedures ===================================


df_Xgboost_test <- fread('modelOutput/df_Eurosc_II_xgboost_test.csv', data.table=FALSE)

df_Xgboost_test <- df_Xgboost_test[complete.cases(df_Xgboost_test), ]


#n_models=list(`Logistic Regression`=df_lr_test, `Neuronetwork`=df_NN_test, `Random Forest`=df_rf_test, `EuroSCORE II`=df_test_Eurosc, `Weighted SVM`=df_svc_test, `Xgboost`=df_Xgboost_test )
n_models=list(`Xgboost`=df_Xgboost_test )

head(n_models)

alpha = unique(df_Xgboost_test[3])
n_models = list()
procedureName = c("cabg only", "avr only", "cabg+avr", "cabg+mvr", "mvr only", "aortic +/-other", "other", "double valve", "not recorded") #vector

for (i in seq(1:nrow(alpha))) {
  print(i)
  #add names to list items: https://stackoverflow.com/questions/27153263/adding-elements-to-a-list-in-for-loop-in-r
  n_models[[paste0(procedureName[i])]] = df_Xgboost_test[df_Xgboost_test[[3]]==i,]
  
}





nb_total <- data.frame()
summaryStat <- data.frame()


for (i in seq_along(n_models)) {
  print(i)
  
  n_modelsComplete <- n_models[[i]][complete.cases(n_models[[i]]), ] #remove missing or empty values
  
  nb <- ntbft(data=n_modelsComplete, outcome="mtly",
              pred = n_modelsComplete[,1], xstart=xstart, xstop=xstop,
              step=step, type="treated")
  if(i == 1) {
    nb_total <- nb
    colnames(nb_total)[i + 3] <- names(n_models[i])
  } else {
    nb_total <- cbind(nb_total, nb$pred)
    colnames(nb_total)[i + 3] <- names(n_models[i]) #else we start from iteration i=2
  }
  
}

head(nb_total)

#write to csv and read in juypter for multi subplot 
write.csv(nb_total, file="modelOutput/nb_total_dcaProc.csv", row.names = F)

nb_total <- fread(file="modelOutput/nb_total_dcaProc.csv", header = T, data.table = F)


#for treated 
q <- plot.ntbft(nb_total)
q
