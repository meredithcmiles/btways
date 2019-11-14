### ------------------------------------------------------------ ###

### FUNCTION: bootstrapTrait
# takes a LONG (not wide) database of raw comparative data
# returns n bootstrapped samples of size k (variable size not implemented)

bootstrapTrait <- function(traitname, dataframe, n){

  taxa <- tree$tip.label
  samples <- matrix(NA, nrow=length(taxa), ncol=n)
  rownames(samples) <- taxa

if(is.character(traitname)){
  trait <- dataframe[,which(colnames(dataframe) == traitname)]
  names(trait) <- dataframe$species
} else {
  trait <- traitname
}

  trait <- trait[!is.na(trait)]
  dataframe <- dataframe[!is.na(trait),]

  for (i in 1:length(taxa)){

    if (taxa[i] %in% names(trait) == TRUE){

      rec <- which(names(trait)==taxa[i])
      subdata <- trait[rec]

      if(length(subdata)==1){
        subdata <- c(subdata, subdata, subdata)
      } else if (length(subdata)==2){
        subdata <- c(subdata[1:2], subdata[1:2])
      }

      for (j in 1:n){

        samp <- sample(subdata, size=3, replace=TRUE)
        samples[i,j] <- mean(samp)

      }
    }
  }

  return(samples)

}

### ------------------------------------------------------------ ###

### FUNCTION: bootstrapResid
# instead of bootstrapping raw data, bootstraps the residuals
# from regressing one variable on another

bootstrapResid <- function(y, x, tree){
  
  y<-y[complete.cases(y),]
  x<-x[complete.cases(x),]

  x<-x[rownames(x) %in% rownames(y),]
  y<-y[rownames(y) %in% rownames(x),]

  out<-matrix(NA, nrow=length(tree$tip.label), ncol=ncol(y))
  rownames(out)<-tree$tip.label

  for (i in 1:ncol(x)){
    
    x1<-x[,i]
    y1<-y[,i]
    r<-rstandard(lm(y1~x1))
    r[r>2] <- NA
    r[r<-2] <- NA
    out[tree$tip.label %in% rownames(x),i]<-r
    
  }

  return(out)

}

### ------------------------------------------------------------ ###

### FUNCTION: binarize
# currently still in 'on the fly' format; needs to be generalized
# to support clustering across different values of k

binarize <- function(x){
  # input x is a matrix of bootstrapped datasets, one per column
  n <- ncol(x) # we will cluster separately across each
  
  clustmat<-matrix(NA, nrow=n, ncol=4) # initialize output
  clusters<-matrix(NA, nrow=nrow(x), ncol=n) # initialize output
  rownames(clusters)<-rownames(x)

  for (i in 1:n){ # looping across every bootstrap sample...
    dat<-x[,i]
    
    # kmeans doesn't like NA cases; create pruned dataset
    dat<-dat[complete.cases(dat)]
    ind<-which(rownames(x) %in% names(dat)==TRUE)

    clust<-kmeans(dat, 2) # kmeans cluster

    # order fix - keeps assignment to groups consistent across samples
    if (clust$centers[1]>clust$centers[2]){
      which1<-which(clust$cluster==1)
      which2<-which(clust$cluster==2)

      clust$centers<-clust$centers[c(2,1)]
      clust$withinss<-clust$withinss[c(2,1)]

      clust$cluster[which1]<-2
      clust$cluster[which2]<-1
    }

    # bugfix - skip iteration if all species belong to one cluster
    if(length(levels(as.factor(clust$cluster))) == 1){
      i <- i-1
      next
    } else {
      # store the iteration's data
      clustmat[i, 1:2]<-clust$centers
      clustmat[i, 3:4]<-clust$withinss
      clusters[ind,i]<-clust$cluster
    }

  }
  # output list
  return(list("summary"=clustmat, "clustering"=clusters, "trait.avg"=bootstrapMeans(x)))
}

### ------------------------------------------------------------ ###

### FUNCTION: clusterSummary
# summary + diagnostic tool for continuous-to-discrete clustering

clusterSummary <- function(clust.out){
  clust<-clust.out$clustering[complete.cases(clust.out$clustering),]
  taxa<-rownames(clust)

  results<-matrix(NA, nrow=length(taxa), ncol=3)
  rownames(results)<-taxa
  colnames(results)<-c("cluster", "p1", "p2")

  for (i in 1:length(taxa)){

    if (rownames(clust)[i] %in% taxa == TRUE){
      n1<-length(which(clust[i,]==1))
      n2<-length(which(clust[i,]==2))

      results[i,2] <- n1/ncol(clust)
      results[i,3] <- n2/ncol(clust)

      if (n1>n2){
        results[i,1]<-1
      } else if (n2>n1) {
        results[i,1]<-2
      } else {
        results[i,1]<-sample(c(1,2), size=1)
      }
    }
  }

  boxplot(clust.out$trait.avg~results[,1])
  nms <- list(tree$tip.label, c("discrete", "p1", "p2", "avg"))
  out <- matrix(NA, nrow = length(tree$tip.label), ncol = 4, dimnames = nms)

  ind <- match(rownames(clust), rownames(out))

  out[ind,1:3] <- results[,1:3]
  out[ind,4] <- clust.out$trait.avg

  return(as.data.frame(out))
}
