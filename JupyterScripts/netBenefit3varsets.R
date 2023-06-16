library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


library(data.table)
set.seed(7)


#Xgboost ----------------------------------

var20_XgboostTrain <- fread('results/var20_sfbs_Xgboost.csv', 
                            select=c('var20_xgb_sfbs', 'mtly'), data.table=FALSE)


var23_XgboostTrain <- fread('results/var23_sfbs_Xgboost.csv', 
                            select=c('var23_xgb_sfbs', 'mtly'), data.table=FALSE)

var25_XgboostTrain <- fread('results/var25_sfbs_Xgboost.csv', 
                            select=c('var25_xgb_sfbs', 'mtly'), data.table=FALSE)

n_models=list(`var20_XgboostTrain`=var20_XgboostTrain, `var23_XgboostTrain`=var23_XgboostTrain, `var25_XgboostTrain`=var25_XgboostTrain )

names(n_models)


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


#CI for bet benefit (don't think bootstrap is needed as CI can be calculated from nb across all thresholds) -----------------------------------
#take arithmetic mean of net benefit 


#https://www.cyclismo.org/tutorial/R/confidence.html
#verified as same as ci from library(gmodels)
confidence_interval <- function(vector, interval) {
  # Standard deviation of sample
  vec_sd <- sd(vector)
  # Sample size
  n <- length(vector)
  # Mean of sample
  vec_mean <- mean(vector)
  # Error according to t distribution
  error <- qt(interval, df = n - 1) * vec_sd / sqrt(n)
  # Confidence interval as a vector
  result <- list(meanVal = vec_mean, lower = vec_mean - error, upper = vec_mean + error)
  return(result)
}

xstart <- 0.01; xstop <- 0.99; step <- 0.01

nb_total <- data.frame()
summaryStat <- data.frame()


for (i in seq_along(n_models)) {
  print(i)
  
  n_modelsComplete <- n_models[[i]][complete.cases(n_models[[i]]), ] #remove missing or empty values
  
  nb <- ntbft(data=n_modelsComplete, outcome="mtly",
              pred = n_modelsComplete[,1], xstart=xstart, xstop=xstop,
              step=step, type="treated")
  
  summaryStat[i, 'Model Name'] <- names(n_models[i])
  
  #obtain 95% CI
  summaryStat[i, 'net benefit mean'] = confidence_interval(nb$pred, 0.975)$meanVal
  summaryStat[i, 'net benefit 95% CI lower'] = confidence_interval(nb$pred, 0.975)$lower
  summaryStat[i, 'net benefit 95% CI upper'] = confidence_interval(nb$pred, 0.975)$upper
  
  # if(i == 1) {
  #   nb_total <- nb
  #   colnames(nb_total)[i + 3] <- names(n_models[i])
  # } else {
  #   nb_total <- cbind(nb_total, nb$pred)
  #   colnames(nb_total)[i + 3] <- names(n_models[i]) #else we start from iteration i=2
  # }
  
}

head(summaryStat)

write.csv(x=summaryStat, file="results/netBenefit3varsetsXgb.csv")
