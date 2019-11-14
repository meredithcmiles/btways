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
