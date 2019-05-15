library(plyr)
library(reshape2)
library(ggplot2)

make_graph <- function(data, metric){
  
  cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#9999CC", "#66CC99")
  
  data<- data[which(data$algorithms != "AVG"),] 
  
  ggplot(data=data, aes(x=variable, y=value, group=algorithms, color=algorithms)) +
    geom_line() +
    geom_point() +
    xlab("Algorithms") + ylab("Lift (%)") +
    facet_grid(problem ~ strategy, scales = "free") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(text= element_text(size = 16)) +
    theme(legend.title=element_blank()) +
    scale_color_manual(values = cbPalette) +
    theme(axis.title.x=element_blank())
}


correctDataset <- function(elem,inverted){
  
  letters_vec <- c("SM","SL","GR","UN")

  all_data <- lapply(1:length(elem), function(index,letters_vec){
    x <- elem[[index]]

    number_labels <- length(unlist(x$baseline[[1]]))
    data_graph <- data.frame(AVG = unlist(x$baseline[[1]]),
                             KNN = unlist(x$knn[[1]]),
                             RF = unlist(x$rfr[[1]]),
                             RT = unlist(x$rk[[1]])
    )
    
    data_graph <- as.data.frame(apply(data_graph,1,function(t){
      unlist(lapply(t,function(y){
        
        if(inverted){
          res <- (- (y - t[which(names(t) == "AVG")]))*100
        }
        else{
          res <- (y - t[which(names(t) == "AVG")])*100
        }
        res
        }))
    }))
    
    rownames(data_graph)<-NULL
    colnames(data_graph) <- c(1:number_labels)
    data_graph$algorithms <- c("AVG","KNN","RF","RT")
    
    data_melted <- melt(data_graph, id=c("algorithms"))
    data <- data_melted
    data$strategy <- letters_vec[index]
    #print(data)
    
    data
  },letters_vec=letters_vec)
  
  
  final <- do.call("rbind",all_data)
  

  
  final$strategy <- as.factor(final$strategy)
  levels(final$strategy) = c("SM","SL","GR","UN")
  
  levels(final$strategy) <- c(levels(final$strategy), "CM","RM")
  
  final[which(final$strategy == "SM"),]$strategy <- "RM"  
  final[which(final$strategy == "UN"),]$strategy <- "CM"
  
  print(levels(final$strategy))
  final$strategy <- factor(final$strategy,levels(final$strategy)[c(6,2,3,5,1,4)])
  print(final)
  
 final
}

load("results/base_graphics.Rda")

tmp <- correctDataset(IR,F)
tmp1<- correctDataset(RP,T)

tmp$problem <- "IR"
tmp1$problem<- "RP"

tmp <-rbind(tmp,tmp1)
make_graph(tmp)


load("results/regular_metatarget_graphics_base_impact.Rda")

NDCG <- correctDataset(NDCG,F)
NDCG$problem <- "NDCG"
AUC <- correctDataset(AUC,F)
AUC$problem <- "AUC"
NMAE <- correctDataset(NMAE,T)
NMAE$problem <- "NMAE"
RMSE <- correctDataset(RMSE,T)
RMSE$problem <- "RMSE"

tmp <-rbind(NDCG,AUC,NMAE,RMSE)
make_graph(tmp)

load("results/base_graphics_related_metafeatures.Rda")


correctDataset2 <- function(elem,inverted,letters_vec = c("B","C","D","E")){
  
  all_data <- lapply(1:length(elem), function(index,letters_vec){
    x <- elem[[index]]
    
    number_labels <- length(unlist(x$baseline[[1]]))
    data_graph <- data.frame(AVG = unlist(x$baseline[[1]]),
                             KNN = unlist(x$knn[[1]]),
                             RF = unlist(x$rfr[[1]]),
                             RT = unlist(x$rk[[1]])
    )
    
    data_graph <- as.data.frame(apply(data_graph,1,function(t){
      unlist(lapply(t,function(y){
        
        if(inverted){
          res <- (- (y - t[which(names(t) == "AVG")]))*100
        }
        else{
          res <- (y - t[which(names(t) == "AVG")])*100
        }
        res
      }))
    }))
    
    rownames(data_graph)<-NULL
    colnames(data_graph) <- c(1:number_labels)
    data_graph$algorithms <- c("AVG","KNN","RF","RT")
    
    data_melted <- melt(data_graph, id=c("algorithms"))
    data <- data_melted
    data$strategy <- letters_vec[index]
    #print(data)
    
    data
  },letters_vec=letters_vec)
  
  
  final <- do.call("rbind",all_data)
  
  
  
  final$strategy <- as.factor(final$strategy)
  print(final)
  
  final
}


tmp <- correctDataset2(IR,F)
tmp1<- correctDataset2(RP,T)

tmp$problem <- "IR"
tmp1$problem<- "RP"

tmp <-rbind(tmp,tmp1)
make_graph(tmp)


load("results/base_graphics_landmarkers.Rda")

tmp <- correctDataset2(IR,F,c("AB","RK","PW","RT"))
tmp1<- correctDataset2(RP,T,c("AB","RK","PW","RT"))

tmp$problem <- "IR"
tmp1$problem<- "RP"

tmp <-rbind(tmp,tmp1)
make_graph(tmp)


load("results/regular_metatarget_graphics_base_impact_new_metafeatures.Rda")

NDCG <- correctDataset2(NDCG,F,c("B","C","D","E"))
NDCG$problem <- "NDCG"
AUC <- correctDataset2(AUC,F,c("B","C","D","E"))
AUC$problem <- "AUC"
NMAE <- correctDataset2(NMAE,T,c("B","C","D","E"))
NMAE$problem <- "NMAE"
RMSE <- correctDataset2(RMSE,T,c("B","C","D","E"))
RMSE$problem <- "RMSE"

tmp <-rbind(NDCG,AUC,NMAE,RMSE)
make_graph(tmp)




