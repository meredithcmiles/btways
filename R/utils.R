# matches model parameters with rate names
ratemodel = function(file, mean) {
  model = getModel(file)
  N = nrow(model)

  return(matrix(mean[match(model, names(mean))], nrow = N, ncol = N))
}

# determines starting line of a BayesTraits logfile
# (definitely works for MCMC, unsure if ML)
startline = function(file){
  # raw output for the MCMC starts on line 25
  skp = 25
  # we'll use line length as a clue to find where to read
  tmp = readLines(file, n=100)
  tmplen = nchar(tmp)
  out = min(which(tmplen>100))
  return(out)
}

# extract column names from a logfile
# no need to import actual model
modelcols = function(file){
  skp = startline(file)-1
  readin = as.matrix(read.delim(file, sep="\t", skip=skp, header=TRUE, fill=TRUE, nrow = 1))
  return(colnames(readin))
}

# return a matrix summarizing model parameterization
getModel = function(x, traits=NULL, ...){

  if(is.character(x)){
    if(length(x) == 1){
      info = getInfo(x)
    } else {
      info = getInfo(x[1])
    }
  } else if (is.data.frame(x)){
    if ("Iteration" %in% colnames(x)){
      msg = cat("You've supplied a chain read. Please supply one of the following:", "\n",
                "(1) one or more logfile locations [character vector]", "\n",
                "(2) information dataframe from getInfo")
      stop(msg)
    } else if ("rPar" %in% colnames(s)){
      info = x
    } else {
      msg = cat("I'm not sure what you gave me to read. Please supply one of the following:", "\n",
                "(1) one or more logfile locations [character vector]", "\n",
                "(2) information dataframe from getInfo")
      stop(msg)
    }
  }

  nstate = max(info$rPar)

  ratenames = paste("q", info[,1], info[,2], sep="")
  valnames = paste("q", info$rVal, info$cVal, sep="")

  freepar = ratenames[ratenames %in% valnames]

  parnames = levels(as.factor(ratenames))

  adj = matrix(NA, nrow=nstate, ncol=nstate)

  for (i in 1:nrow(info)){
    if(is.na(info$rVal[i])==FALSE){
      rate = paste("q", info$rVal[i], info$cVal[i], sep="")
      x = info$rPar[i]
      y = info$cPar[i]
      adj[x,y] = rate
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

# compute summary stats
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

# convert rates between relative (NormaliseQMatrix) and absolute
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


# assign names to model parameters
namePars = function(traits, abbr = NULL){

  N = length(traits)
  len = sapply(traits, length)
  n = prod(len)

  for (i in 1:N){

    k = i+1

    if (i == 1){

      nrep = prod(len[k:length(len)])
      nmrow = as.vector(sapply(traits[[i]], rep, times = nrep))

    } else if (i < N){

      nrep = prod(len[k:length(len)])
      add = as.vector(sapply(traits[[i]], rep, times = nrep))
      times = n/length(add)
      add = rep(add, times = times)

      nmrow = paste(nmrow, add, sep = "|")

    } else if (i == N){

      nrep = n/length(traits[[i]])
      add = rep(traits[[i]], times = nrep)

      nmrow = paste(nmrow, add, sep = "|")

    }
  }
  return(sapply(nmrow, strsplit, split = "|", fixed = TRUE))
}


# return target parameters for a given trait
# use to automate analysis for one of 2+ traits in a model
getq = function(model, traits, which){

  N = length(traits)
  states = namePars(traits)

  if(is.character(which)){
    ind = which(names(traits) == which)
  } else {
    ind = which
  }

  statediff = vector("list", length = N)

  for (i in 1:N){

    subnms = sapply(states, function(x){x[i]})
    subm = matrix(FALSE, nrow = nrow(model), ncol = ncol(model), dimnames = list(subnms, subnms))

    for (x in 1:nrow(subm)){

      subm[x,names(subm[x,]) != colnames(subm)[x]] = TRUE

    }

    statediff[[i]] = subm

  }

  whichpar = statediff[[ind]]
  statediff[[ind]] = NULL

  for (i in 1:length(statediff)){

    whichpar[which(statediff[[i]] == TRUE)] = FALSE

  }

  out = model[whichpar]

  return(whichpar)

}
