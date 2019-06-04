getInfo<-function(file, states=NULL){

  options(stringsAsFactors = FALSE)

  # raw output for the MCMC starts on line 25
  skp<-25
  # we'll use line length as a clue to find where to read
  tmp<-readLines(file, n=100)
  tmplen<-nchar(tmp)
  # the last line of rate definitions occurs right before
  # the only line with exactly 18 characters in it
  end<-which(tmplen==18)-1
  n = skp + end

  # now we can read only the relevant information
  readin<-readLines(file, n = n)[26:end]

  if(readin[1]=="Restrictions:"){
    readin = readin[-1]
  }

  readin = trimws(readin, which="both")

  x = substr(readin, start=1, stop=3)
  y = trimws(substr(readin, start=4, stop=max(nchar(readin))), which="both")

  readin = data.frame("par" = x, "val" = y)


  for(i in 1:length(readin)){

    if(readin[i,2] %in% c("RJ MCMC", "None", "0.000000")==FALSE){
      par.i = readin[i,1]
      val.i = readin[i,2]

      eq = as.matrix(readin[which(readin[,1]==val.i),])
      par.eq = as.vector(eq[1])
      val.eq = as.vector(eq[2])



      if(val.eq == "None"){
        readin[i,2] = par.eq
        readin[which(readin[,2]==par.i),2] = par.eq
      }

    }

  }

  # replace free parameters with their id
  readin$val[which(readin$val=="None")] <- readin$par[which(readin$val=="None")]
  readin$val[which(readin$val=="RJ MCMC")] <- readin$par[which(readin$val=="RJ MCMC")]

  # encode zero parameters as missing data
  readin$val[which(readin$val=="0.000000")] <- NA

  # we'll store some data for easier manipulation later
  x<-as.numeric(substr(readin$par, 2, 2))   # adj matrix row index / parameter name part 1
  y<-as.numeric(substr(readin$par, 3, 3))   # adj matrix col index / parameter name part 2
  x1<-as.numeric(substr(readin$val, 2, 2)) # parameter definition pt. 1
  x2<-as.numeric(substr(readin$val, 3, 3)) # parameter definition pt. 2

  npar<-length(x)

  nstate<-max(as.numeric(x))
  nrate<-(npar^2)-npar

  out<-data.frame(rPar=x, cPar=y,
                  rVal=x1, cVal=x2)

  return(out)

}
