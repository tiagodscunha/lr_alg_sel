

extractMetatargetsFromLevels <- function(data){
  
  algorithms <- sort(unique(unlist(strsplit(levels(data$ranking), split = ","))))
  orders <- lapply(data$ranking, function(x){unlist(strsplit(as.character(x), split = ','))})
  levels_ranking <- lapply(data$levels, function(x){unlist(strsplit(as.character(x), split = ','))})

  final_values <-lapply(1:length(orders), function(index1,rank2,levels_ranking){
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
  
  targets_matrix <- as.matrix(do.call(rbind, final_values))
  targets_matrix
}

extractMetatargetsFromRankings <- function(data){
  
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
  targets_matrix
}

correctDatasetNames <- function(datasets){
  
  unlist(lapply(datasets, function(d){
    
    clean <- gsub(".csv", "", d)
    clean <- gsub("_", "-", clean)
    
    clean <- gsub("amazon", "AMZ", clean)
    clean <- gsub("movielens", "ML", clean)
    clean <- gsub("movietweetings", "MT", clean)
    clean <- gsub("bookcrossing", "BC", clean)
    clean <- gsub("flixter", "FX", clean)
    clean <- gsub("jester", "JT", clean)
    clean <- gsub("tripadvisor", "TA", clean)
    clean <- gsub("yahoo", "YH", clean)
    clean <- gsub("yelp", "YE", clean)
    
    clean <- gsub("automotive", "auto", clean)
    clean <- gsub("digital-music", "music", clean)
    clean <- gsub("instant-video", "video", clean)
    clean <- gsub("instruments", "inst", clean)
    clean <- gsub("pet-supplies", "pet", clean)
    clean <- gsub("latest", "lat", clean)
    clean <- gsub("recsys2014", "RS", clean)
    
    clean
  }))
  
  
}


compareMetatargets <- function(a,b,final){
  
  colnames(a) <- c("dataset","RankingA")
  colnames(b) <- c("dataset","RankingB")
  
  tmp <- merge(a,b,by.x="dataset",by.y="dataset")
  tmp <- merge(tmp,final,by.x="dataset",by.y="dataset")
  
  #print(str(tmp))
  
  keepcolumns <- c("dataset","ranking","levels")
  tmp1 <- tmp[,which(colnames(tmp) %in% keepcolumns)]
  
  keepcolumns1 <- c("dataset","RankingA")
  tmp2 <- tmp[,which(colnames(tmp) %in% keepcolumns1)]
  colnames(tmp2) <- c("dataset","ranking")
  
  keepcolumns2 <- c("dataset","RankingB")
  tmp3 <- tmp[,which(colnames(tmp) %in% keepcolumns2)]
  colnames(tmp3) <- c("dataset","ranking")
    
  final_values <- extractMetatargetsFromLevels(tmp1)
  a_values <- extractMetatargetsFromRankings(tmp2)
  b_values <- extractMetatargetsFromRankings(tmp3)
  
  # print(final_values)
  # print(a_values)
  # print(b_values)
  
  res <- data.frame(
    dataset <- correctDatasetNames(tmp$dataset),
    A = apply(a_values,1, function(x){toString(x,collapse="'")}),
    B = apply(b_values,1, function(x){toString(x,collapse="'")}),
    outcome = apply(final_values,1, function(x){toString(x,collapse="'")})
    )
  
  res$correlation1 <- unlist(lapply(1:dim(final_values)[1],function(x,a_values,final_values){
    cor(a_values[x,],final_values[x,])
    }, a_values=a_values, final_values=final_values))
  res$correlation2 <- unlist(lapply(1:dim(final_values)[1],function(x,b_values,final_values){
    cor(b_values[x,],final_values[x,])
  }, b_values=b_values, final_values=final_values))
  
  # res$correlation1 <- lapply(res$correlation1, paste, collapse = ", ")
  # res$correlation2 <- lapply(res$correlation2, paste, collapse = ", ")
  res
  
}


run_experiment <- function(type){
  
  result <- NULL
  
  if(type == "IR"){
    targets <- read.csv("targets/IR.csv", sep=";")
    
    keepcolumn <- c("dataset","ranking")
    keepcolumn1 <- c("dataset","levels")
    
    keepcolumn <- c("dataset","ranking","levels")
    final <- targets[,which(colnames(targets) %in% keepcolumn)]
    
    ndcg_orig <- read.csv("performance/performance_ndcg.csv", sep=";")
    auc_orig <- read.csv("performance/performance_auc.csv", sep=";")
    result <- compareMetatargets(ndcg_orig,auc_orig,final)
  }
  else{
    targets <- read.csv("targets/RP.csv", sep=";")
    
    keepcolumn <- c("dataset","ranking","levels")
    final <- targets[,which(colnames(targets) %in% keepcolumn)]

    
    rmse_orig <- read.csv("performance/performance_rmse.csv", sep=";")
    nmae_orig <- read.csv("performance/performance_nmae.csv", sep=";")
    result <- compareMetatargets(rmse_orig,nmae_orig,final)
  }
  
  result
}


IR <- run_experiment("IR")
colnames(IR) <- c("dataset","NDCG","AUC","out","cor(NDCG,out)", "cor(AUC,out)")
RP <- run_experiment("RP")
colnames(RP) <- c("dataset","RMSE","NMAE","out","cor(RMSE,out)", "cor(NMAE,out)")

write.table(format(IR, digits=3), "metatargets_analysis/IR_metatarget.csv", row.names = F, col.names = T, sep = ";")
write.table(format(RP, digits=3), "metatargets_analysis/RP_metatarget.csv", row.names = F, col.names = T, sep = ";")

final <- data.frame(
  `NDCG.out` = IR$`cor(NDCG,out)`,
  `AUC.out` = IR$`cor(AUC,out)`,
  `RMSE.out` = RP$`cor(RMSE,out)`,
  `NMAE.out` = RP$`cor(NMAE,out)`
)


library(ggplot2)
library(reshape2)
library(plyr)

final2 <- melt(final)
colnames(final2) <- c("correlations","value")
final2$correlations <- revalue(final2$correlations, c("NDCG.out"="NDCG", 
                               "AUC.out"="AUC", 
                               "RMSE.out"="RMSE", 
                               "NMAE.out"="NMAE"))

cbPalette <- c("#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")


ggplot(final2, aes(value, fill = correlations)) + 
  scale_fill_manual(values=cbPalette) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(limits = c(0.8, 1)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text= element_text(size = 16)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.y=element_blank())

ggplot(final2, aes(value, fill = correlations)) + 
  scale_fill_manual(values=cbPalette) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(limits = c(0, 0.8)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text= element_text(size = 16)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

##################

IR_tmp <- IR[which(IR$`cor(NDCG,out)` <0.8 | IR$`cor(AUC,out)` < 0.8),]
RP_tmp <- RP[which(RP$`cor(RMSE,out)`<0.8 | RP$`cor(NMAE,out)` < 0.8),]


write.table(format(IR_tmp, digits=3), "metatargets_analysis/IR_metatarget_problems.csv", row.names = F, col.names = T, sep = ";")
write.table(format(RP_tmp, digits=3), "metatargets_analysis/RP_metatarget_problems.csv", row.names = F, col.names = T, sep = ";")


