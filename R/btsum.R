btsum<-function(files=NULL, ...){

  if (is.null(files)){

      files<-list.files(pattern = "Log.txt")
  }

  modelnames<-substr(files, start=9, stop=11)
  nmodel<-c(1:length(files))

  info<-getInfo(files[1])

  ratenames<-paste("q", info[,1], info[,2], sep="")
  est<-which(!is.na(info[,3]))
  parnames<-paste("q", info[est,1], info[est,2], sep="")
  parloc<-matrix(c(info[est,1], info[est,2]), ncol=2)
  npar<-length(parnames)

  # read in mcmc summary
  for (i in 1:length(files)){
    readin = readMCMC(files[i], ...)

    nms = names(readin$info)

    #if ("model" %in% names == TRUE){
    #  type = "rj"
    #} else {
    #  type = "norm"
    #}

    print(paste("Finished reading file", i))

    q<-c(0.05, 0.25, 0.5, 0.75, 0.95)
    tmp<-summary(as.mcmc(readin$rates), quantiles=q)
    mcmcsum<-cbind(tmp$statistics[,1:3], tmp$quantiles)
    rates<-parnames
    num<-rep(nmodel[i], times=npar)
    prior<-rep(modelnames[i], times=npar)

    if (i==1){
      concat<- cbind("lh" = readin$info$lh, readin$rates)
      out<-data.frame("n"=num, "prior"=prior, "par"= parnames)
      out<-cbind(out, mcmcsum)
    } else {
      add<-data.frame("n"=num, "prior"=prior, "par" = parnames)
      add<-cbind(add, mcmcsum)
      out<-rbind(out, add)
      concat<-rbind(concat, cbind("lh" = readin$info$lh, readin$rates))
    }
  }
  rownames(out)<-NULL
  return(list("concat"=concat, "summary"=out))
}
