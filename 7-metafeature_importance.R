library(ggplot2)
library(caret)
library(reshape2)
library(e1071)
library(clValid)
library(clusterSim)
library(mlbench)

source("labelrankingforests-master/RankTrees/RankTrees.R")
source("labelrankingforests-master/RankTrees/PredRTrees.R")
source("labelrankingforests-master/RFR.R")


bestRFRLRmodel <- function(data,targets){

  results <- NULL

  for(i in 1:nrow(data)){  #LOOCV
    training_LR <- as.matrix(data[-i,])
    testing_LR <- as.matrix(data[i,])
    target_train_LR <- targets[-i,]
    target_test_LR <- targets[i,]

    gama <- c(0.8,0.95)
    sizeF <- c(100)#,20,30,50,100,200)

    preds <- expand.grid(gama,sizeF)
    colnames(preds) <- c("gama","size")

    preds$preds = NA
    preds$kendall = NA

    for(pos in 1:dim(preds)[1]){

      pred <- RFR2(dx=training_LR,target_train_LR,testing_LR,target_test_LR, sizeForest=preds[pos,]$size, gama=preds[pos,]$gama)
      preds[pos,]$preds <- list(pred)
      preds[pos,]$kendall <- Kendall(pred,target_test_LR)$tau
    }

    final <- preds[which(preds$kendall == max(preds$kendall)),][1,]
    results <- rbind(results,final)

  }

  print(results)


  bestSettings <- results[which(results$kendall == max(results$kendall)),][1,]


  model <- RFR2_justModels(data,targets, gama=bestSettings$gama, sizeForest=bestSettings$size)


  model

}


calculate_feature_importance <- function(dataset, performance){

  data <- merge(dataset, performance, by.x="dataset", by.y="dataset")[,-1]
  rownames(data) <- dataset[,1]

  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  levels_ranking <- lapply(data$levels, function(x){unlist(strsplit(as.character(x), split = ','))})

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

  removecolumns <- c("ranking","performance","levels")
  data <- data[,which(!colnames(data) %in% removecolumns)]

  model1 <- bestRFRLRmodel(data,targets_matrix)
  #model2 <- rankTrees(data,targets_matrix)

  list(model1,model1)
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

  if(type == "IR"){
    A <- read.csv("metafeatures_statistical/mf.csv", sep=";")
    B <- read.csv("metafeatures_landmarkers/B_IR.csv", sep=";")  #landmarkers datasets have NAs
    C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")

    targets <- read.csv("targets/IR.csv", sep=";")
  }
  else{
    A <- read.csv("metafeatures_statistical/mf.csv", sep=";")
    B <- read.csv("metafeatures_landmarkers/B_RP.csv", sep=";")
    C <- read.csv("metafeatures_graph/graph_metafeatures_final.csv", sep=",")

    targets <- read.csv("targets/RP.csv", sep=";")
  }

  D <- mergeUnifiedDataset(A,B,C)

  lr_A <- calculate_feature_importance(A,targets)
  lr_B <- calculate_feature_importance(B,targets)
  lr_C <- calculate_feature_importance(C,targets)
  lr_D <- calculate_feature_importance(D,targets)

  list(lr_A,lr_B,lr_C,lr_D)  #TODO trocar por C
}

IR <- run_experiment("IR")
RP <- run_experiment("RP")

averageResults <- function(data){

  metafeatures <- unique(data$metafeatures)
  avg_rank <- unlist(lapply(metafeatures, function(mf){
    ranks <- data[data$metafeatures == mf,]$rank
    mean(ranks)
  }))

  tmp <- data.frame(
    metafeatures = metafeatures,
    rank = avg_rank
  )

  tmp <- tmp[order(tmp$rank),]
  tmp
}


A <- averageResults(rbind(IR[[1]][[1]],RP[[1]][[1]]))
B <- averageResults(rbind(IR[[2]][[1]],RP[[2]][[1]]))
C <- averageResults(rbind(IR[[3]][[1]],RP[[3]][[1]]))
D <- averageResults(rbind(IR[[4]][[1]],RP[[4]][[1]]))

saveRDS(list(A,B,C,D), "results/feature_importance.rds")

tmp <- readRDS("results/feature_importance.rds")
A <- tmp[[1]]
B <- tmp[[2]]
C <- tmp[[3]]
D <- tmp[[4]]

A <- A[which(A$rank < 58),]

A$metafeatures <- as.character(A$metafeatures)
A[which(A$metafeatures == "colCounts_entropy"),]$metafeatures <- "I.count.entropy"
A[which(A$metafeatures == "colCounts_mean"),]$metafeatures <- "I.count.mean"
A[which(A$metafeatures == "colMeans_skewness"),]$metafeatures <- "I.mean.skewness"
A[which(A$metafeatures == "colCounts_kurtosis"),]$metafeatures <- "I.count.kurtosis"
A[which(A$metafeatures == "ratings_skewness"),]$metafeatures <- "R.ratings.skewness"


g1 <- ggplot(A,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("RM") + coord_flip()
g1

B <- B[which(B$rank < 6),]

B$metafeatures <- as.character(B$metafeatures)
B[which(B$metafeatures == "BiasedMatrixFactorization.NMAE"),]$metafeatures <- "BMF.NMAE"
B[which(B$metafeatures == "LatentFeatureLogLinearModel.NMAE"),]$metafeatures <- "LFLLM.NMAE"
B[which(B$metafeatures == "LatentFeatureLogLinearModel.RMSE"),]$metafeatures <- "LFLLM.RMSE"


g2 <- ggplot(B,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("SL") + coord_flip()
g2

C <- C[which(C$rank < 50),]

C$metafeatures <- as.character(C$metafeatures)
C[which(C$metafeatures == "all_similarity_variance_skewness"),]$metafeatures <- "G.sim.var.skew"
C[which(C$metafeatures == "communities_diversity_variance_skewness"),]$metafeatures <- "com.diver.var.skew"
C[which(C$metafeatures == "communities_alpha_variance_variance"),]$metafeatures <- "com.alpha.var.var"

g3 <- ggplot(C,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("GR") + coord_flip()
g3

D <- D[which(D$rank < 49),]

D$metafeatures <- as.character(D$metafeatures)
D[which(D$metafeatures == "colCounts_gini"),]$metafeatures <- "I.count.gini"
D[which(D$metafeatures == "ratings_skewness"),]$metafeatures <- "R.ratings.skewness"
D[which(D$metafeatures == "colMeans_mode"),]$metafeatures <- "I.mean.mode"
D[which(D$metafeatures == "colMeans_entropy"),]$metafeatures <- "I.mean.entropy"


g4 <- ggplot(D,aes(x=metafeatures,y=rank)) + geom_bar(stat = "identity",  position = "identity", width=0.5) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5)) +
  ggtitle("CM") + coord_flip()
g4

library(grid)
library(gridExtra)
grid.arrange(arrangeGrob(g1,g2,g3,g4,nrow = 2, ncol=2,left=textGrob("Metafeature importance (average rank)", rot = 90, vjust = 1)) )



