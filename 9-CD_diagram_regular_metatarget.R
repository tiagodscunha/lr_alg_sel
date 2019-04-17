library(scmamp)

load("results/detailed_meta_results_regular_metatarget.Rda")

#all algorithms, regardless of strategy
tmp <- rbind(NDCG[,1:4],AUC[,1:4],NMAE[,1:4],RMSE[,1:4])
colnames(tmp) <- c("KNN","AVG","RT","RF")
plotCD (tmp, alpha=0.05, cex=1.25)

#per strategy
par(mfrow=c(2,2))

tmp <- rbind(NDCG[which(NDCG$strategy == "RM"),1:4],AUC[which(AUC$strategy == "RM"),1:4],NMAE[which(NMAE$strategy == "RM"),1:4],RMSE[which(RMSE$strategy == "RM"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(NDCG[which(NDCG$strategy == "SL"),1:4],AUC[which(AUC$strategy == "SL"),1:4],NMAE[which(NMAE$strategy == "SL"),1:4],RMSE[which(RMSE$strategy == "SL"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(NDCG[which(NDCG$strategy == "GR"),1:4],AUC[which(AUC$strategy == "GR"),1:4],NMAE[which(NMAE$strategy == "GR"),1:4],RMSE[which(RMSE$strategy == "GR"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(NDCG[which(NDCG$strategy == "UN"),1:4],AUC[which(AUC$strategy == "UN"),1:4],NMAE[which(NMAE$strategy == "UN"),1:4],RMSE[which(RMSE$strategy == "CM"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)
#aggregated by strategy (best model in previous task)

par(mfrow=c(1,1))

getPerformance <- function(NDCG,AUC,NMAE,RMSE,strategy,algorithm){
  tmp <- c(
    AUC[which(AUC$strategy == strategy),which(colnames(AUC) == algorithm)],
    NDCG[which(NDCG$strategy == strategy),which(colnames(NDCG) == algorithm)],
    NMAE[which(NMAE$strategy == strategy),which(colnames(NMAE) == algorithm)],
    RMSE[which(RMSE$strategy == strategy),which(colnames(RMSE) == algorithm)]
  )
  tmp
}

tmp <- data.frame(
  RM = getPerformance(NDCG,AUC,NMAE,RMSE,"RM","knn"),
  SL = getPerformance(NDCG,AUC,NMAE,RMSE,"SL","knn"),
  GR = getPerformance(NDCG,AUC,NMAE,RMSE,"GR","knn"),
  CM = getPerformance(NDCG,AUC,NMAE,RMSE,"UN","knn"),
  AVG = getPerformance(NDCG,AUC,NMAE,RMSE,"UN","baseline")
)
plotCD (tmp, alpha=0.05, cex=1.25)

#não está a ser usado no paper. esperamos por pedido dos reviewers?