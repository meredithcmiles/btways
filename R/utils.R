# small functions for quality-of-life, etc

startline = function(file){
  # raw output for the MCMC starts on line 25
  skp<-25
  # we'll use line length as a clue to find where to read
  tmp<-readLines(file, n=100)
  tmplen<-nchar(tmp)
  out = min(which(tmplen>100))
  return(out)
}

modelcols = function(file){
  skp<-startline(file)-1
  readin<-as.matrix(read.delim(file, sep="\t", skip=skp, header=TRUE, fill=TRUE, nrow = 1))
  return(colnames(readin))
}

meanRates = function(btsum){
  parnames = colnames(btsum$concat)
  out = matrix(NA, nrow = length(parnames), ncol = 8,
               dimnames = list(parnames, colnames(btsum$summary)[4:11]))
  colnames(out)
  btsum = btsum$summary[,-(1:2)]

  for(i in 1:length(parnames)){
    ind = which(btsum$par == parnames[i])
    par = btsum[ind,2:ncol(btsum)]
    out[i,] = colMeans(par)
  }
  return(out)
}

switchRates = function(mcmc){

  relative = round(sum(mcmc$rates[1,])) == 1
  globrate = "globrate" %in% names(mcmc$info)

  if (relative == TRUE){

    if(globrate == FALSE){
      stop("There appears to be a problem here. \n
           All relative rates should come with a global rate.")
    }

    mcmc$rates = as.data.frame(lapply(mcmc$rates, function(vec){vec * mcmc$info$globrate}))

    } else {

      if(globrate == FALSE){
        globrate = rowSums(mcmc$rates)
        mcmc$info = data.frame(mcmc$info, globrate)
      }

      mcmc$rates = as.data.frame(lapply(mcmc$rates, function(vec){vec / mcmc$info$globrate}))
    }

  return(mcmc)
}
