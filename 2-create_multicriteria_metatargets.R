#\cite{Ribeiro2013}
#esta estratégia já faz precisamente o que queremos
#paralelismo entre user, algorithm e item por dataset, métrica e algoritmo
#pareto frontiers calculadas com skyline operator algoritmos (rPref)
#https://cran.r-project.org/web/packages/rPref/rPref.pdf

library(ggplot2)
library(plyr)
library(rPref)
library(scales)

paretoRanking <- function(A,A_p,B,B_p,reversed){

  A <- merge(A,A_p,by.x="dataset",by.y="dataset")
  colnames(A) <- c("dataset","ranking","performance")
  
  B <- merge(B,B_p,by.x="dataset",by.y="dataset")
  colnames(B) <- c("dataset","ranking","performance")

  #algorithm | A | B| (per dataset)
  rankings <- lapply(unique(A$dataset), function(dataset, A, B){
    
    tmp <- A[which(A$dataset == dataset),]
    A_ranking <- unlist(strsplit(as.character(tmp$ranking), split = ','))
    A_perf <- as.numeric(unlist(strsplit(as.character(tmp$performance), split = ',')))
    
    if(reversed){
      A_perf <- -A_perf
    }
    x <- cbind(A_ranking,A_perf)

    tmp <- B[which(B$dataset == dataset),]
    B_ranking <- unlist(strsplit(as.character(tmp$ranking), split = ','))
    B_perf <- as.numeric(unlist(strsplit(as.character(tmp$performance), split = ',')))
    
    if(reversed){
      B_perf <- -B_perf
    }
    y <- cbind(B_ranking,B_perf)

    final <- merge(x,y,by.x="A_ranking",by.y="B_ranking")
    final$dataset <- dataset
    colnames(final) <- c("algorithm","A","B","dataset")
    final$A <- as.numeric(as.character(final$A))
    final$B <- as.numeric(as.character(final$B))
    
    final
  }, A=A, B=B)
  
  final_rankings <-  ldply(rankings, data.frame)

  g <- ggplot(data = final_rankings, aes(x=A,y=B, colour=dataset)) + 
    geom_point(mapping=aes(x=A, y=B, shape=algorithm))
  
  nalgs <- length(unique(final_rankings$algorithm))
  
  orders <- lapply(rankings, function(x,nalgs){
    psel(x, high(A)*high(B), at_least = nalgs)
  }, nalgs=nalgs)
  
  new_ranking <- lapply(orders, function(x){
    d <- x$dataset[1]
    perf_avg <- unlist(abs(apply(rbind(x$A,x$B),2,mean)))
    
    if(max(perf_avg) > 1){
      perf_avg <- rescale(perf_avg)
    }

    tmp <- cbind(as.character(d),
                 paste(x$algorithm, sep=",", collapse=","),
                 paste(x$.level, sep=",", collapse=","),
                 paste(perf_avg, sep=",", collapse=",")
                 )
    tmp
  })
  new_ranking
  
  pareto_ranking <- ldply(new_ranking, data.frame)
  colnames(pareto_ranking) <- c("dataset","ranking","levels","performance")
  list(g,orders,pareto_ranking,final_rankings)
}

ndcg_orig <- read.csv("performance/performance_ndcg.csv", sep=";")
ndcg_values <- read.csv("performance/performance_ndcg_values.csv", sep=";")
auc_orig <- read.csv("performance/performance_auc.csv", sep=";")
auc_values <- read.csv("performance/performance_auc_values.csv", sep=";")

IR_ranking <- paretoRanking(ndcg_orig,ndcg_values,auc_orig,auc_values,FALSE)
rm(ndcg_orig,ndcg_values,auc_orig,auc_values)
write.table(IR_ranking[[3]], "targets/IR.csv", sep=";",row.names = F,  quote = F)

rmse_orig <- read.csv("performance/performance_rmse.csv", sep=";")
rmse_values <- read.csv("performance/performance_rmse_values.csv", sep=";")
nmae_orig <- read.csv("performance/performance_nmae.csv", sep=";")
nmae_values <- read.csv("performance/performance_nmae_values.csv", sep=";")

RP_ranking <- paretoRanking(rmse_orig,rmse_values,nmae_orig,nmae_values,TRUE)
rm(nmae_orig,nmae_values,rmse_orig,rmse_values)
write.table(RP_ranking[[3]], "targets/RP.csv", sep=";",row.names = F, quote = F)


