library(cvTools)
library(labelrank)
library(Kendall)
library(reshape2)
library(ggplot2)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")

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
  sizeF <- c(10,20,30,50,100,200)
  
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

LOOCV <- function(data,targets, n=1, method, baseline){
  results <- list()
  
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
      #model <- rankTrees(training,target_train)
      #prediction <- as.numeric(PredRTrees(model,testing))
      prediction <- bestRTLR(training,testing,target_train,target_test)
    }
    else if(method == "RFR"){
      #prediction <-  RFR2(training,target_train,testing,target_test)  
      prediction <- bestRFRLR(training,testing,target_train,target_test)
      #prediction <- average_rankings(target_train,baseline)
    }
    else{ #baseline
      prediction <- average_rankings(target_train,baseline)
    }
    
    if(length(prediction) == length(target_test)){
      results <- append(results, Kendall(prediction,target_test)$tau)
    }
    else {
      print("ignored this instance")
      results <- append(results,-1)
    }
  }
  
  list(
    mean(unlist(results)), 
    sd(unlist(results))
  )   
}

rank_evaluation <- function(dataset, ranking){
  
  data <- merge(dataset,ranking, by.x="dataset",by.y="dataset")  
  #print(data)
  
  #organize rankings - assume fixed order (alphabetical) and assign the corresponding rank value for each pair dataset-algorithm
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  levels_ranking <- lapply(data$levels, function(x){unlist(strsplit(as.character(x), split = ','))})
  #print(algorithms)

  targets <- lapply(1:length(orders), function(index1,rank2,levels_ranking){
    rank1 <- orders[[index1]]
    unlist(lapply(rank2, function(x, levels_ranking){
      positions <- unlist(which(rank1 == x))
      if(length(positions)>0){
        return (as.numeric(levels_ranking[[index1]][positions]))
      }
      else {
        return (length(algorithms)/2) #(length(algorithms)) #last position!  # (length(algorithms)/2) assigning mean ranking improves results for IR but decreases for RP
      }
    }, levels_ranking=levels_ranking))
  }, rank2=algorithms, levels_ranking=levels_ranking)
  
  targets_matrix <- as.matrix(do.call(rbind, targets))
  #print(targets_matrix)
  
  performance_knn <- lapply(1:1,
                            LOOCV,
                            data=dataset[,2:dim(dataset)[2]],
                            targets=targets_matrix,
                            method="kNN",
                            baseline=NA)
  
  performance_baseline <- rep(lapply(1:1,
                                     LOOCV,
                                     data=dataset[,2:dim(dataset)[2]],
                                     targets=targets_matrix,
                                     method="baseline",
                                     baseline=algorithms),10)
  
  performance_rankingTrees <- rep(lapply(1:1,
                                         LOOCV,
                                         data=dataset[,2:dim(dataset)[2]],
                                         targets=targets_matrix,
                                         method="RT",
                                         baseline=algorithms),10)
  
  performance_RFR <- rep(lapply(1:1,
                                LOOCV,
                                data=dataset[,2:dim(dataset)[2]],
                                targets=targets_matrix,
                                method="RFR",
                                baseline=algorithms),10)
  
  list(
    knn=performance_knn, 
    baseline=performance_baseline,
    rk=performance_rankingTrees,
    rfr = performance_RFR
  )
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

run_experiment <- function(type){
  
  
  original <- read.csv(paste0("metafeatures_landmarkers_all/original_",type, ".csv"),sep=";")
  ranking <- read.csv(paste0("metafeatures_landmarkers_all/ranking_",type, ".csv"),sep=";")
  pairwise <- read.csv(paste0("metafeatures_landmarkers_all/pairwise_",type, ".csv"),sep=";")
  ratio <- read.csv(paste0("metafeatures_landmarkers_all/ratio_",type, ".csv"),sep=";")
  
  if(type == "IR"){
    targets <- read.csv("targets/IR.csv", sep=";")
  }
  else{
    targets <- read.csv("targets/RP.csv", sep=";")
  }
  
  #IR meta-problem
  A_result <- rank_evaluation(original,targets)
  B_result <- rank_evaluation(ranking,targets)
  C_result <- rank_evaluation(pairwise,targets)
  D_result <- rank_evaluation(ratio,targets)
  
  alternative_table <- data.frame(datasets=c("absolute","ranking","pairwise","ratio"),
                                  avg = c(
                                    valuesToString(A_result$baseline[[1]]), 
                                    valuesToString(B_result$baseline[[1]]), 
                                    valuesToString(C_result$baseline[[1]]),
                                    valuesToString(D_result$baseline[[1]])
                                  ),
                                  knn = c(
                                    valuesToString(A_result$knn[[1]]),
                                    valuesToString(B_result$knn[[1]]),
                                    valuesToString(C_result$knn[[1]]),
                                    valuesToString(D_result$knn[[1]])
                                  ),
                                  rt = c(
                                    valuesToString(A_result$rk[[1]]),
                                    valuesToString(B_result$rk[[1]]),
                                    valuesToString(C_result$rk[[1]]),
                                    valuesToString(D_result$rk[[1]])
                                  ),
                                  rf = c(
                                    valuesToString(A_result$rfr[[1]]),
                                    valuesToString(B_result$rfr[[1]]),
                                    valuesToString(C_result$rfr[[1]]),
                                    valuesToString(D_result$rfr[[1]])
                                  )
  )
  
  alternative_table
}


IR <- run_experiment("IR")
RP <- run_experiment("RP")

save(IR, RP, file="results/landmarkers_metafeatures_graphics.Rda")
