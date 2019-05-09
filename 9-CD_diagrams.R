library(scmamp)

load("results/detailed_meta_results.Rda")

IR_old <- IR
RP_old <- RP

load("results/detailed_meta_results_alternative_metafeatures.Rda")

IR <- rbind(IR_old,IR)
RP <- rbind(RP_old,RP)


#per strategy
par(mfrow=c(2,2))

tmp <- rbind(IR[which(IR$strategy == "RM"),1:4],RP[which(RP$strategy == "RM"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(IR[which(IR$strategy == "SL"),1:4],RP[which(RP$strategy == "SL"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(IR[which(IR$strategy == "GR"),1:4],RP[which(RP$strategy == "GR"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

tmp <- rbind(IR[which(IR$strategy == "UN"),1:4],RP[which(RP$strategy == "UN"),1:4])
plotCD (tmp, alpha=0.05, cex=1.25)

#aggregated by strategy (best model in previous task)

par(mfrow=c(1,1))

getPerformance <- function(IR,RP,strategy,algorithm){
  tmp <- c(
    IR[which(IR$strategy == strategy),which(colnames(IR) == algorithm)],
    RP[which(RP$strategy == strategy),which(colnames(RP) == algorithm)]
    )
  tmp
}

tmp <- data.frame(
  RM = getPerformance(IR,RP,"RM","knn"),
  SL = getPerformance(IR,RP,"SL","knn"),
  GR = getPerformance(IR,RP,"GR","rfr"),
  CM = getPerformance(IR,RP,"UN","knn"),
  B = getPerformance(IR,RP,"B","knn"),
  C = getPerformance(IR,RP,"C","knn"),
  D = getPerformance(IR,RP,"D","knn"),
  E = getPerformance(IR,RP,"E","knn"),
  AVG = getPerformance(IR,RP,"UN","baseline")
)
plotCD (tmp, alpha=0.05, cex=1.25)


load("detailed_meta_results_landmarkers.Rda")

tmp <- data.frame(
  AB = getPerformance(IR,RP,"AB","knn"),
  RK = getPerformance(IR,RP,"RK","knn"),
  PW = getPerformance(IR,RP,"PW","knn"),
  RT = getPerformance(IR,RP,"RT","knn")
)

plotCD (tmp, alpha=0.05, cex=1.25)


######################

load("results/detailed_meta_results_regular_metatarget.Rda")

AUC_old <- AUC
NDCG_old <- NDCG
NMAE_old <- NMAE
RMSE_old <- RMSE

load("results/detailed_meta_results_regular_metatarget_new_metafeatures.Rda")

IR <- rbind(AUC_old,AUC,NDCG_old,NDCG)
RP <- rbind(NMAE_old,NMAE,RMSE_old,RMSE)


tmp <- data.frame(
  RM = getPerformance(IR,RP,"RM","knn"),
  SL = getPerformance(IR,RP,"SL","knn"),
  GR = getPerformance(IR,RP,"GR","rfr"),
  CM = getPerformance(IR,RP,"UN","knn"),
  B = getPerformance(IR,RP,"B","knn"),
  C = getPerformance(IR,RP,"C","knn"),
  D = getPerformance(IR,RP,"D","knn"),
  E = getPerformance(IR,RP,"E","knn"),
  AVG = getPerformance(IR,RP,"UN","baseline")
)

plotCD (tmp, alpha=0.05, cex=1.25)

