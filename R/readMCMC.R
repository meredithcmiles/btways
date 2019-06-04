readMCMC<-function(file, relative=FALSE){

  options(stringsAsFactors = FALSE)

  info<-getInfo(file)

  ratenames<-paste("q", info[,1], info[,2], sep="")
  est<-which(!is.na(info[,3]))
  parnames<-paste("q", info[est,1], info[est,2], sep="")
  parloc<-matrix(c(info[est,1], info[est,2]), ncol=2)

  skp<-startline(file)-1


  readin<-read.delim(file, sep="\t", skip=skp, header=TRUE, fill=TRUE)

  cols <- colnames(readin)
  out<-readin[,which(cols %in% parnames)]

  rj = "Model.string" %in% cols
  glob = "Global.Rate" %in% cols

  if(relative == TRUE){
    if(glob == FALSE){
      rateswitch = TRUE
    } else {rateswitch = FALSE}
  } else {
    if(glob == FALSE){
      rateswitch = FALSE
    } else {rateswitch = TRUE}
  }

  if(rj == TRUE & glob == TRUE){

    info = list("npar" = readin$No.Off.Parmeters, "n0" = readin$No.Off.Zero,
                "lh" = readin$Lh, "model" = readin$Model.string,
                "globrate" = readin$Global.Rate)
    out = list("info" = info, "rates" = out)

  } else if (rj==TRUE & glob == FALSE){

    info = list("npar" = readin$No.Off.Parmeters, "n0" = readin$No.Off.Zero,
                "lh" = readin$Lh, "model" = readin$Model.string)
    out = list("info" = info, "rates" = out)

  } else if (rj == FALSE & glob == TRUE){

    info = list("lh" = readin$Lh, "globrate" = readin$Global.Rate)
    out = list("info" = info, "rates" = out)

  } else if (rj == FALSE & glob == FALSE){

    out = list("info" = list("lh" = readin$Lh), "rates" = out)

  }

  if(rateswitch == TRUE){
    out = switchRates(out)
  }

  return(out)

}
