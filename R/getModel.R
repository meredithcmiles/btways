getModel<-function(info, traits=NULL, ...){

  nstate<-max(info$rPar)

  ratenames<-paste("q", info[,1], info[,2], sep="")
  valnames<-paste("q", info$rVal, info$cVal, sep="")

  freepar<-ratenames[ratenames %in% valnames]

  parnames<-levels(as.factor(ratenames))

  adj<-matrix(NA, nrow=nstate, ncol=nstate)

  for (i in 1:nrow(info)){
    if(is.na(info$rVal[i])==FALSE){
      rate = paste("q", info$rVal[i], info$cVal[i], sep="")
      x = info$rPar[i]
      y = info$cPar[i]
      adj[x,y] <- rate
    }
  }

  if(is.null(traits) == FALSE){

    len = sapply(traits, length)
    n = prod(len)

    if(n != nrow(adj)){

      stop("names must equal matrix dimensions")

    } else {

      nms = names(namePars(traits, ...))
      dimnames(adj) = list(nms, nms)

    }
  }

  return(adj)

}
