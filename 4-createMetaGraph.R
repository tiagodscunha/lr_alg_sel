library(ggplot2)
library(reshape2)

getMainValue <- function(x){
  as.numeric(unlist(strsplit(as.character(x)," "))[1])
}

getSDValue <- function(x){
  as.numeric(unlist(strsplit(as.character(x)," "))[3])
}

processDataset <- function(dt,problem){
  tmp <- apply(dt[,2:dim(dt)[2]],2,function(x){
    unlist(lapply(x,getMainValue))
  })
  rownames(tmp) <- dt$datasets
  tmp <- melt(tmp)
  
  colnames(tmp) <- c("strategy","algorithm","performance")
  
  levels(tmp$strategy) <- c(levels(tmp$strategy), "CM","RM")
  tmp[which(tmp$strategy == "SM"),]$strategy <- "RM"  
  tmp[which(tmp$strategy == "UN"),]$strategy <- "CM"
  
  levels(tmp$algorithm) <- c(levels(tmp$algorithm),"AVG","KNN","RT","RFR")
  tmp[which(tmp$algorithm == "avg"),]$algorithm <- "AVG"
  tmp[which(tmp$algorithm == "knn"),]$algorithm <- "KNN"
  tmp[which(tmp$algorithm == "rt"),]$algorithm <- "RT"
  tmp[which(tmp$algorithm == "rf"),]$algorithm <- "RFR"
  
  print(levels(tmp$strategy))
  tmp$strategy <- factor(tmp$strategy,levels(tmp$strategy)[c(6,2,3,5,1,4)])
  tmp$problem <- problem
  tmp
}

processDataset2 <- function(dt,problem){
  tmp <- apply(dt[,2:dim(dt)[2]],2,function(x){
    unlist(lapply(x,getMainValue))
  })
  rownames(tmp) <- dt$datasets
  tmp <- melt(tmp)
  
  colnames(tmp) <- c("strategy","algorithm","performance")
  tmp$problem <- problem
  
  levels(tmp$algorithm) <- c(levels(tmp$algorithm),"AVG","KNN","RT","RFR")
  
  tmp[which(tmp$algorithm == "avg"),]$algorithm <- "AVG"
  tmp[which(tmp$algorithm == "knn"),]$algorithm <- "KNN"
  tmp[which(tmp$algorithm == "rt"),]$algorithm <- "RT"
  tmp[which(tmp$algorithm == "rf"),]$algorithm <- "RFR"
  
  tmp
}

createSimpleGraphic <- function(tmp){
  
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")
  
  g <- ggplot(tmp, aes(x=algorithm,y=performance,group=algorithm,fill=algorithm)) + 
    scale_fill_manual(values=cbPalette) +
    geom_bar(stat = "identity", width = 0.7, position = "dodge") +
    guides(fill = guide_legend(title = "Meta-algorithms"))+
    facet_grid(problem ~ strategy) +
    ylab("Kendall's tau") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + 
    coord_cartesian(ylim=c(0.5,1))
  g
  
}

load("results/graphics.Rda")

tmp <- processDataset(IR,"IR")
tmp1 <- processDataset(RP,"RP")
tmp <- rbind(tmp,tmp1)
print(tmp)

g1 <- createSimpleGraphic(tmp)
g1

load("results/regular_metatarget_graphics.Rda")

NDCG_dt <- processDataset(NDCG,"NDCG")
AUC_dt <- processDataset(AUC,"AUC")
NMAE_dt <- processDataset(NMAE,"NMAE")
RMSE_dt <- processDataset(RMSE,"RMSE")

tmp <- rbind(NDCG_dt,AUC_dt,NMAE_dt,RMSE_dt)
g1 <- createSimpleGraphic(tmp)
g1

load("results/new_metafeatures_graphics.Rda")

tmp <- processDataset2(IR,"IR")
tmp1 <- processDataset2(RP,"RP")
tmp <- rbind(tmp,tmp1)
print(tmp)
g1 <- createSimpleGraphic(tmp)
g1

load("results/landmarkers_metafeatures_graphics.Rda")

tmp <- processDataset2(IR,"IR")
tmp1 <- processDataset2(RP,"RP")
tmp <- rbind(tmp,tmp1)
print(tmp)

tmp$strategy <- as.character(tmp$strategy)
tmp[which(tmp$strategy == "absolute"),]$strategy <- "AB"
tmp[which(tmp$strategy == "ranking"),]$strategy <- "RK"
tmp[which(tmp$strategy == "pairwise"),]$strategy <- "PW"
tmp[which(tmp$strategy == "ratio"),]$strategy <- "RT"

g1 <- createSimpleGraphic(tmp)
g1

