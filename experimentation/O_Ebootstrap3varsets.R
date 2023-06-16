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




#bootstrap CI for residual o-e -----------------------------------

# Bootstrap 95% CI for residual o-e
library(boot)
# function to obtain residual o-e from the data
residualO_E_boot_Func <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  
  d$group <- 1
  
  meanExpectedVSObservedByDecile <- aggregate(.~group, d, mean, na.rm = TRUE)
  
  #observed - predicted
  o_eResidual <- meanExpectedVSObservedByDecile$mtly - meanExpectedVSObservedByDecile[,2]
  
  return(o_eResidual)
}


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


summaryStat <- data.frame()

rowCount <- 0

for (i in seq(n_models)) {
  
  n_modelsComplete <- n_models[[i]][complete.cases(n_models[[i]]), ] #remove missing or empty values
  
  
  
  n_modelsComplete$group <- 1
  
  meanExpectedVSObservedByDecile <- aggregate(.~group, n_modelsComplete, mean, na.rm = TRUE)
  
  #observed - predicted 
  o_eResidual <- meanExpectedVSObservedByDecile$mtly - meanExpectedVSObservedByDecile[,2]
  
  meanExpectedVSObservedByDecile$`O - E individual` <- o_eResidual
  
  
  
  for (j in seq_along(1)) { #group 1: only iterate once unlike deciles which has 10 iterations
    
    weight <- (i - 1) * 1  #iteration 2: 1 * 10 
    rowCount <- weight + j  #iteration 2: 1 * 10 + 1 -> 11 : 10+10 -> 20
    #iteration 3: 2 * 10 + 1 -> 21 : 20+10 -> 30
    print(i)
    print(j)
    summaryStat[rowCount, 'Model Name'] <- names(n_models[i])
    
    
    n_modelsCompleteDecile_j <- meanExpectedVSObservedByDecile[meanExpectedVSObservedByDecile$group == (0 + j), ]
    
    n_modelsComplete_j_FullList <- n_modelsComplete[n_modelsComplete$group == (0 + j),  ]
    
    summaryStat[rowCount, 'Group'] <- 0 + j  #0 + 1 (1) : 0 + 1 (1)
    
    #summaryStat[rowCount, 'O - E'] <- n_modelsCompleteDecile_j$`O - E`
    
    # bootstrapping with 100 replications (t-distribution assumed)
    results <- boot(data=n_modelsComplete_j_FullList, statistic=residualO_E_boot_Func,
                    R=100)
    #obtain 95% CI
    summaryStat[rowCount, 'O - E mean'] = confidence_interval(results$t, 0.975)$meanVal
    summaryStat[rowCount, 'O - E 95% CI lower'] = confidence_interval(results$t, 0.975)$lower
    summaryStat[rowCount, 'O - E 95% CI upper'] = confidence_interval(results$t, 0.975)$upper
    
  }
}

write.csv(x=summaryStat, file="results/O_E_BootStrap3varsetsXgb.csv")

