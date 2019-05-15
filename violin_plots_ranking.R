library(reshape2)

load("results/detailed_meta_results.Rda")
IR_1 <- IR
RP_1 <- RP

load("results/detailed_meta_results_alternative_metafeatures.Rda")


ir <- rbind(IR_1,IR)
ir$task <- "IR"

rp <- rbind(RP_1,RP)
rp$task <- "RP"

all <- rbind(ir,rp)

all$dataset <- sort(read.csv("metafeatures_statistical/mf.csv",sep=";")$dataset,decreasing = F)

#corrigir datasets: onde estão?

all$dataset <- unlist(lapply(all$dataset,function(x){
  tmp <- unlist(strsplit(as.character(x),"[.]"))
  tmp <- tmp[1]
  tmp <- gsub("amazon", "AMZ", tmp)
  tmp <- gsub("jester", "JT", tmp)
  tmp <- gsub("movielens", "ML", tmp)
  tmp <- gsub("movietweetings", "MT", tmp)
  tmp <- gsub("yahoo", "YH", tmp)
  tmp <- gsub("tripadvisor", "TA", tmp)
  tmp <- gsub("bookcrossing", "BC", tmp)
  tmp <- gsub("yelp", "YE", tmp)
  tmp <- gsub("flixter", "FL", tmp)
  
  tmp <- gsub("recsys2014", "RS14", tmp)
  tmp <- gsub("digital-music", "music", tmp)
  tmp <- gsub("instant-video", "video", tmp)
  tmp <- gsub("_", "-", tmp)
  tmp
}))

all <- melt(all,id.vars = c("strategy","dataset"))
all$variable <- NULL
all$value <- as.numeric(all$value)

levels(all$strategy) <- c(levels(all$strategy),"CM")
all[which(all$strategy == "UN"),]$strategy <- "CM"

library(ggplot2)

#res <- all[which(all$strategy == "B" | all$strategy == "C" | all$strategy == "D" | all$strategy == "E"),]
res <- all[which(all$strategy == "RM" | all$strategy == "SL" | all$strategy == "GR" | all$strategy == "CM"),]
#res <- all[which(all$strategy == "B"),]


p <- ggplot(res, aes(x=dataset,y=value,fill=strategy)) + 
  geom_violin() +
  coord_flip() + 
  facet_wrap( ~ strategy, scales="free", ncol=2) + 
  theme(text = element_text(size=8)) + 
  guides(fill=guide_legend(title="Metafeatures"))
  
p