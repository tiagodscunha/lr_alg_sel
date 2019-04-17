suppressMessages(library(hash))
suppressMessages(library(igraph))
suppressMessages(library(entropy))
suppressMessages(library(ineq))
suppressMessages(library(e1071))


postFunctionsVector <- function(mf, name, data){
  
  data <- data[is.finite(data)]

  #central tendency
  #mf[paste(name,"median",sep="_")] = median(data)
  mf[paste(name,"mean",sep="_")] = mean(data)
  
  #dispersion
  #mf[paste(name,"max",sep="_")] = max(data)
  #mf[paste(name,"min",sep="_")] = min(data)
  #mf[paste(name,"sd",sep="_")] = sd(data)
  mf[paste(name,"variance",sep="_")] = var(data)
  
  #shape
  mf[paste(name,"skewness",sep="_")] = skewness(data)
  #mf[paste(name,"kurtosis",sep="_")] = kurtosis(data)
  
  # #information-theoretical
  mf[paste(name,"entropy",sep="_")] = entropy(data)
  # mf[paste(name,"gini",sep="_")] = ineq(data,type="Gini")
  
}

vectorMetafeatures <- function(graph, objects, mf_hash, target){
  
  postFunctionsVector(mf_hash,paste0(target,"_alpha"),alpha_centrality(graph, nodes = objects))
  postFunctionsVector(mf_hash,paste0(target,"_closeness"),closeness(graph, vids = objects))
  postFunctionsVector(mf_hash,paste0(target,"_constraint"),constraint(graph, nodes = objects))
  postFunctionsVector(mf_hash,paste0(target,"_degree"),degree(graph, v = objects))
  postFunctionsVector(mf_hash,paste0(target,"_diversity"),diversity(graph, vids = objects))
  postFunctionsVector(mf_hash,paste0(target,"_eccentricity"),eccentricity(graph, vids = objects))
  postFunctionsVector(mf_hash,paste0(target,"_page_rank"),page_rank(graph, vids = objects)$vector)
  postFunctionsVector(mf_hash,paste0(target,"_strength"),strength(graph, vids = objects))
  
  postFunctionsVector(mf_hash,
                      paste0(target,"_neighors"),
                      unlist(lapply(objects, function(obj){
                        length(neighbors(graph,v = obj))
                      }))) 
  
  
  #return(mf_hash)
}

getValuesByKey <- function(results, key){
  results[which(startsWith(names(results), key))]
}

decomposeByMetric <- function(original, mf_hash, function_name, targets = c("g_","user_","item_")){

  users <- getValuesByKey(original,"U")
  items <- getValuesByKey(original,"I")
  
  tmp <- list()
  if("g_" %in% targets && "user_" %in% targets && "item_" %in% targets){
    tmp <- list(original,users,items)
  }
  else {
    tmp <- list(original)
    target <- ""
  }
  
  lapply(1:length(tmp), function(index, mf_hash, targets, function_name){
    results <- tmp[[index]]
    target <- targets[index]
    postFunctionsVector(mf_hash,paste0(target,function_name),results)
  }, mf_hash=mf_hash, targets=targets, function_name)
  
  return(mf_hash)
}

vectorMetafeaturesPost <- function(graph, mf_hash, target, candidates){

  decomposeByMetric(authority_score(graph)$vector, mf_hash, paste0(target,"_authority.score"), candidates)
  decomposeByMetric(coreness(graph), mf_hash, paste0(target,"_coreness"), candidates)
  decomposeByMetric(eigen_centrality(graph)$vector, mf_hash, paste0(target,"_eigen.centrality"), candidates)
  decomposeByMetric(hub_score(graph)$vector, mf_hash, paste0(target,"_hub.score"), candidates)
  decomposeByMetric(knn(simplify(graph))$knn, mf_hash, paste0(target,"_knn"), candidates)
  decomposeByMetric(local_scan(graph), mf_hash, paste0(target,"_local.scan"), candidates)
  #decomposeByMetric(max_cardinality(graph)$alpha, mf_hash, paste0(target,"_max.cardinality"), candidates) #erro grid

  #return (mf_hash)
}

postFunctionsMatrix <- function(mf_hash, target, matrix){
  
  matrix[!is.finite(matrix)] <- 0
  
  postFunctionsVector(mf_hash,paste0(target,"_sum"),apply(matrix, 1, sum))
  postFunctionsVector(mf_hash,paste0(target,"_mean"),apply(matrix, 1, mean))
  postFunctionsVector(mf_hash,paste0(target,"_count"),apply(matrix, 1, function(x){length(which(x>0))}))
  postFunctionsVector(mf_hash,paste0(target,"_variance"),apply(matrix, 1, var))
  
}

matrixMetafeatures <- function(graph,objects, mf_hash, target){
  
  postFunctionsMatrix(mf_hash,paste0(target,"_similarity"),similarity(graph, vids = objects))
  postFunctionsMatrix(mf_hash,paste0(target,"_distances"),distances(graph, v = objects))
}

getVertexBySetID <- function(graph,membership,names){
  lapply(1:max(membership), function(id, g){
    x <- names[which(membership == id)] 
    #y <- V(g)[!is.na(match(V(g)$name, x))]  
    x
  }, g=graph)
}

subgraphMetafeatures <- function(graph,sets_elements,mf_hash,target){
  
  new_mf <- hash()
  
  lapply(1:ifelse(length(sets_elements)<10,length(sets_elements),10) , function(x,graph,mf,target){
    elem <- sets_elements[[x]]

      sg <- induced_subgraph(graph = graph,v = elem)
  
      tryCatch({
        vectorMetafeaturesPost(sg,mf,paste0(target,"-",x),c("g_")) 
        vectorMetafeatures(sg, elem, mf, paste0(target,"-",x)) 
      }, error = function(e) {
        print(e)
        return(NA)
      })
    

    
  }, graph=graph, target=target, mf=new_mf)

  postProcessSubgraphmetafeatures(sets_elements,new_mf,target,mf_hash)
}

postProcessSubgraphmetafeatures <- function(sets_elements,mf_hash,target,hash_mf){

  target_keys <- unique(unlist(lapply(keys(mf_hash),function(x){
    tmp <- gsub(paste0("g_",target),'',x)
    tmp <- gsub(target,'',tmp)
    tmp <- gsub('[[:digit:]]+', '',tmp)
    tmp <- gsub('-_', '',tmp)
    tmp
  })))
  #print(target_keys)

  for(index in 1:length(target_keys)){
    filtered <- unlist(lapply(1:length(sets_elements), function(x){
      new_key <- paste0(target,"-",x,"_",target_keys[index])
      matches <- match(new_key,keys(mf_hash))
      
      result <- unlist(lapply(1:length(matches), function(y){
        keys(mf_hash)[matches[y]]
      }))

      result <<- result[!is.na(result)]

      if(is.na(result)){  
        new_key <- paste0("g_",target,"-",x,"_",target_keys[index])
        matches <- match(new_key,keys(mf_hash))
        
        result <- unlist(lapply(1:length(matches), function(y){
          keys(mf_hash)[matches[y]]
        }))
      }
      result <<- result[!is.na(result)]
      result
    }))
    
    filtered_vals <- unlist(lapply(filtered, function(x){
      mf_hash[[x]]
    }))
    
    # print(paste0(target,target_keys[index]))
    # print(filtered)
    # print(target_keys[index])
    # print(filtered_vals)
    
    postFunctionsVector(hash_mf, paste0(target,target_keys[index]), filtered_vals)
  }

}

setMetafeatures <- function(graph,mf_hash){
  g1 <- as.undirected(graph)
  
  x <- cluster_louvain(g1)
  x_v <- getVertexBySetID(g1,x$membership,x$names)
  subgraphMetafeatures(g1,x_v,mf_hash,"communities_")

  y <- components(g)
  y_v <- getVertexBySetID(g,y$membership, names(y$membership))
  subgraphMetafeatures(g,y_v,mf_hash,"components_")
}


tStartA <- proc.time()
file <- "datasets/movielens100k.csv"

dataset <- read.csv(file,sep=";")
colnames(dataset) <- c("user","item","rating")

mf <- hash()

all_users <- unique(dataset$user)
all_items <- unique(dataset$item)

#create graph
g <- graph.empty() 

g <- add.vertices(g,nv=length(all_users),attr=list(name=paste0('U',all_users),
                                                                type=rep(TRUE,length(all_users)), color = "red"))
g <- add.vertices(g,nv=length(all_items),attr=list(name=paste0('I',all_items),
                                                 type=rep(TRUE,length(all_items)), color = "green"))

dataset$user <- unlist(lapply(dataset$user,function(x){ paste0('U',x)}))
dataset$item <- unlist(lapply(dataset$item,function(x){ paste0('I',x)}))

edgeListVec <- as.vector(t(as.matrix(dataset[,1:2])))
g <- add.edges(g,edgeListVec, label = as.numeric(dataset[,3]), weight = as.numeric(dataset[,3]))

g <- subgraph.edges(g, sample(c(1:gsize(g)),0.10*gsize(g)))

rm(dataset,all_users,all_items,edgeListVec)

print("graph is built")

# print(is_bipartite(g))
#print(g, g=TRUE, v=TRUE, e=TRUE)

### GRAPH MEASURES ### - 5
#g.diameter.null - porque temos grafo bipartido, a maior distancia geodesica será sempre o valor máximo do rating - retirar
#g.edge_connectivity.null - sempre zero porque não há caminhos no grafo - retirar
#g.min_cut.null - como não há caminhos no grafo, não se aplica
#g.reciprocity.null - como nunca há arestas de items para users, não se aplica - retirar
#g.transitivity.null - como não há ligações entre items, não se aplica - retirar

#g.edge_density.null
#g.girth.null
#g.gorder.null (numebr of vertex)
#g.gsize.null (number of edges)
#g.radius.null

mf$edge_density <- edge_density(g)
mf$girth <- girth(g)$girth
mf$gorder <- gorder(g)
mf$gsize <- gsize(g)
mf$radius <- radius(g)

print("graph measures finished")


### VERTEX MEASURES ### - results are vectors - 192
#vectorMetafeatures: 3 * 9 * 4 = 108
#vectorMetafeaturesPost: 3 * 7 * 4 = 84

#{user,item,g}.count_triangles.pf - it is impossible to have triangles - remove
#{user,item,g}.betweeness.pf - no paths, cant be used - remove

#{user,item,g}.page_rank.pf - does not make sense for users - features removed in CFS
#{user,item,g}.hub_score.pf - does not make sense for items - features removed in CFS
#{user,item,g}.local_scan.pf - does not make sense for items - features removed in CFS
#{user,item,g}.authority_score.pf - does not make sense for users - features removed in CFS
 
#{user,item,g}.alpha.pf
#{user,item,g}.closeness.pf
#{user,item,g}.constraint.pf
#{user,item,g}.coreness.pf
#{user,item,g}.degree.pf
#{user,item,g}.diversity.pf
#{user,item,g}.eccentricity.pf
#{user,item,g}.eigen_centrality.pf
#{user,item,g}.edge_betweeness.pf
#{user,item,g}.knn.pf
#{user,item,g}.max_cardinality.pf
#{user,item,g}.neighors.pf 
#{user,item,g}.strength.pf

users <- V(g)[which(startsWith(V(g)$name, "U"))]
items <- V(g)[which(startsWith(V(g)$name, "I"))]
all <- V(g)

x <- vectorMetafeatures(g, users, mf, "user")
x <- vectorMetafeatures(g, items, mf, "item")
x <- vectorMetafeatures(g, all, mf, "g")
x <- vectorMetafeaturesPost(g,mf,"",c("g","user","item"))

print("vertex metafeatures finished")


### PAIRWISE COMPARISON OF VERTEX MEASURES ### - results are matrices vertex x vertex - 3 * 2 * 4 * 4 = 96
#{user/user, item/item, all/all}.similarity.pf_matrix
#{user/user, item/item, all/all}.distances.pf_matrix - no paths - to remove

users <- sample(users, ifelse(length(users) < 100, length(users),100))
items <- sample(items, ifelse(length(items) < 100, length(items),100))
all <- sample(all,ifelse(length(all) < 100, length(all),100))

x <- matrixMetafeatures(g,users,mf,"user")
x <- matrixMetafeatures(g,items,mf,"item")
x <- matrixMetafeatures(g,all,mf,"all")

print("matrix metafeatures finished")

rm(users)
rm(items)
rm(all)


### SETS OF VERTEX MEASURES ###
#{communities,components}.[{subgraph}.{vertex measures}.pf].pf
#2 * (7+9) * 4 * 4 = 512

x <- setMetafeatures(g,mf)

print("communities metafeatures finished")


cat(paste(keys(mf), collapse=";"))
cat("\n")
cat(paste(values(mf), collapse=";"))
cat("\n")


tFinishA <- proc.time()
timeA = as.numeric((tFinishA - tStartA)["elapsed"])

print(timeA)

