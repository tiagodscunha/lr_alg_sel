library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")

performance_at_k_max <- function(values, k){
  max(values[1:k],na.rm = T)
}

performance_at_k_min <- function(values, k){
  min(values[1:k],na.rm = T)
}

bestNNLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  k_vals <- 1:20
  
  preds <- data.frame(
    k = k_vals,
    preds = NA,
    kendall = NA
  )
  
  for(i in k_vals){
    pred <- nn_rank(train.x = training_LR, test.x = testing_LR, n = 1, y = target_train_LR, k=i)
    preds[i,]$preds <- list(pred)
    preds[i,]$kendall <- Kendall(pred,target_test_LR)$tau
  }
  
  #print(preds)
  
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}


bestRTLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  k_vals <- c(0.8,0.9,0.95)
  
  preds <- data.frame(
    k = k_vals,
    preds = NA,
    kendall = NA
  )
  
  for(pos in 1:length(k_vals)){
    
    i <- k_vals[pos]
    
    model <- rankTrees(training_LR,target_train_LR, gama=i)
    pred <- as.numeric(PredRTrees(model,testing_LR))
    preds[pos,]$preds <- list(pred)
    preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
  }
  
  #print(preds)
  
  print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}


bestRFRLR <- function(training_LR,testing_LR,target_train_LR,target_test_LR){
  
  gama <- c(0.8,0.9,0.95)
  sizeF <- c(100)
  
  preds <- expand.grid(gama,sizeF)
  colnames(preds) <- c("gama","size")
  
  preds$preds = NA
  preds$kendall = NA
  
  
  
  for(pos in 1:dim(preds)[1]){
    
    pred <- RFR2(dx=training_LR,target_train_LR,testing_LR,target_test_LR, sizeForest=preds[pos,]$size, gama=preds[pos,]$gama)  
    preds[pos,]$preds <- list(pred)
    preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
  }
  
  #print(preds)
  print(preds[which(preds$kendall == max(preds$kendall)),])
  final <- unlist(preds[which(preds$kendall == max(preds$kendall)),][1,]$preds)
  
  #print(final)
  
  final
  
}


LOOCV <- function(data,targets, n=1, method, baseline,performance, goal){
  
  result <- data.frame(matrix(nrow = nrow(data), ncol = dim(targets)[2]))
  rownames(result) <- data$dataset
  
  for(i in 1:nrow(data)){  #LOOCV
    training <- as.matrix(data[-i,])
    testing <- as.matrix(data[i,])
    target_train <- targets[-i,]
    target_test <- targets[i,]
    
    if(method == "kNN"){
      prediction <- bestNNLR(training,testing,target_train,target_test)
      #prediction <- nn_rank(train.x = training, test.x = testing, n, y = target_train)
    }
    else if(method == "RT"){
      # model <- rankTrees(training,target_train)
      # prediction <- as.numeric(PredRTrees(model,testing))
      prediction <- bestRTLR(training,testing,target_train,target_test)
    }
    else if(method == "RFR"){
      #prediction <-  RFR2(training,target_train,testing,target_test)  
      prediction <- bestRFRLR(training,testing,target_train,target_test)
    }
    else if(method == "bound"){
      prediction <- target_test
    }
    else{ #baseline
      prediction <- average_rankings(target_train,baseline)
    }
    
    if(length(prediction) == length(target_test)){
      new_performance_vector <-performance[i,order(prediction)]
      
      if(goal=="max"){
        result[i,] <- as.numeric(lapply(1:dim(targets)[2], performance_at_k_max, values=new_performance_vector))
      }
      else {
        result[i,] <- as.numeric(lapply(1:dim(targets)[2], performance_at_k_min, values=new_performance_vector))
      }
    }
    else {
      print("ignored this instance")
      if(goal=="max"){
        result[i,] <- 0
      }
      else {
        result[i,] <- 1
      }
    }
    
    
  }
  
  tmp <- apply(result,2,mean,na.rm=T)
  #print(tmp)
  tmp
}

rank_evaluation <- function(dataset, ranking, perf_values, goal){
  
  data <- merge(dataset,ranking, by.x="dataset",by.y="dataset")  
  #print(data)
  
  #organize rankings - assume fixed order (alphabetical) and assign the corresponding rank value for each pair dataset-algorithm
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  targets <- lapply(orders, function(rank1,rank2){
    unlist(lapply(rank2, function(x){
      positions <- unlist(which(rank1 == x))
      if(length(positions)>0){
        return (unlist(positions))
      }
      else {
        return (length(algorithms)/2) #(length(algorithms)) #last position!  # (length(algorithms)/2) assigning mean ranking improves results for IR but decreases for RP
      }
    }))
  }, rank2=algorithms)
  targets_matrix <- as.matrix(do.call(rbind, targets))
  
  performance <- lapply(1:dim(targets_matrix)[1], function(index,perf,targets){
    new_order <- targets[index,]
    performance_vector <- as.numeric(unlist(strsplit(as.character(perf[index]),",")))
    
    result <- unlist(lapply(1:length(performance_vector), function(x, vector, ordering){
      vector[ordering[x]]  #warning vem daqui!! - funciona bem mas devia resolver problema
    }, vector = performance_vector, ordering = new_order))
    result
  }, targets= targets_matrix, perf=perf_values$performance)
  performance_matrix <- as.matrix(do.call(rbind, performance))
  
  performance_knn <- lapply(1:10, function(x){
    LOOCV(data=dataset[,2:dim(dataset)[2]],
          targets=targets_matrix,
          method="kNN",
          baseline=NA,
          performance=performance_matrix,
          goal=goal,
          n=x)
  })
  
  performance_baseline <- rep(lapply(1:1,
                                     LOOCV,
                                     data=dataset[,2:dim(dataset)[2]],
                                     targets=targets_matrix,
                                     method="baseline",
                                     baseline=algorithms,
                                     performance=performance_matrix,
                                     goal=goal),10)
  
  performance_bound <- rep(lapply(1:1,
                                  LOOCV,
                                  data=dataset[,2:dim(dataset)[2]],
                                  targets=targets_matrix,
                                  method="bound",
                                  baseline=algorithms,
                                  performance=performance_matrix,
                                  goal=goal),10)
  
  
  performance_rankingTrees <- rep(lapply(1:1,
                                         LOOCV,
                                         data=dataset[,2:dim(dataset)[2]],
                                         targets=targets_matrix,
                                         method="RT",
                                         baseline=algorithms,
                                         performance=performance_matrix,
                                         goal=goal),10)
  
  performance_RFR <- rep(lapply(1:1,
                                LOOCV,
                                data=dataset[,2:dim(dataset)[2]],
                                targets=targets_matrix,
                                method="RFR",
                                baseline=algorithms,
                                performance=performance_matrix,
                                goal=goal),10)
  
  list(knn=performance_knn, baseline=performance_baseline, rk=performance_rankingTrees, rfr = performance_RFR, bound=performance_bound)
}

average_rankings <- function(targets_matrix,algorithms){
  result <- apply(targets_matrix, 2, mean)
  names(result) <- algorithms
  result
}

valuesToString <- function(x){
  paste0(paste0(x[1], " +- "), x[2])
}


removeCFS <- function(df,threshold){
  library(caret)
  
  corr_m <- cor(df)
  corr_m[is.na(corr_m)] <- 0
  
  
  toRemove <- findCorrelation(corr_m, cutoff = threshold)
  print(length(toRemove))
  df1 <- as.data.frame(df[,-(which(colnames(df) %in% colnames(df)[toRemove]))])
  
  print("metafeatures removed due to CFS")
  print(setdiff(colnames(df),colnames(df1)))
  
  df1
  
}


replaceNA <- function(df){
  df <- apply(df,2,function(x){
    x[which(is.na(x))] <- mean(x, na.rm = T)
    x
  })
}

normalizeMatrix <- function(df){
  df1 <- as.data.frame(normalize(df))
  colnames(df1) <- colnames(df)
  
  tmp <- data.frame(dataset=rownames(df))
  df1 <- cbind(tmp,df1)
  df1
}

mergeUnifiedDataset <- function(A,B,C){
  
  library(data.table) ## 1.9.3
  library(splitstackshape)
  library(plyr)
  library(BBmisc)
  
  D1 <- merge(A,B,by.x="dataset",by.y="dataset")
  df <- merge(D1,C,by.x="dataset",by.y="dataset")
  
  rownames(df) <- df$dataset
  df$dataset <- NULL
  df <- removeCFS(df,0.9)
  df <- normalizeMatrix(df)
  
  df
}

run_experiment <- function(type,metric){
  
  if(type == "IR"){
    B <- read.csv("metafeatures_1000users/mf_B.csv", sep=";")
    C <- read.csv("metafeatures_1000users/mf_C.csv", sep=";")
    D <- read.csv("metafeatures_1000users/mf_D.csv", sep=";")
    E <- read.csv("metafeatures_1000users/mf_E.csv", sep=";")
    goal <- "max"
    
    targets <- read.csv(paste0("performance/performance_",metric,".csv"), sep=";")
    perf_values <- read.csv(paste0("performance/performance_",metric,"_values.csv"), sep=";")
  }
  else{
    B <- read.csv("metafeatures_1000users/mf_B.csv", sep=";")
    C <- read.csv("metafeatures_1000users/mf_C.csv", sep=";")
    D <- read.csv("metafeatures_1000users/mf_D.csv", sep=";")
    E <- read.csv("metafeatures_1000users/mf_E.csv", sep=";")
    goal<-"min"
    
    targets <- read.csv(paste0("performance/performance_",metric,".csv"), sep=";")
    perf_values <- read.csv(paste0("performance/performance_",metric,"_values.csv"), sep=";")
  }
  B[is.na(B)] <- 0
  D[is.na(D)] <- 0
  
  
  colnames(perf_values) <- c("dataset","performance")
  
  
  #IR meta-problem
  A_result <- rank_evaluation(B,targets,perf_values,goal)
  B_result <- rank_evaluation(C,targets,perf_values,goal)
  C_result <- rank_evaluation(D,targets,perf_values,goal)
  D_result <- rank_evaluation(E,targets,perf_values,goal)
  
  list(A_result,B_result,C_result,D_result)  
}


NDCG <- run_experiment("IR","ndcg")
AUC <- run_experiment("IR","auc")
NMAE <- run_experiment("RP","nmae")
RMSE <- run_experiment("RP","rmse")

save(NDCG, AUC, NMAE, RMSE, file="results/regular_metatarget_graphics_base_impact_new_metafeatures.Rda")

