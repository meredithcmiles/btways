getq = function(info, traits, which){

  N = length(traits)
  states = namePars(traits)
  model = getModel(info, traits)

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
  statediff[[ind]] <- NULL

  for (i in 1:length(statediff)){

    whichpar[which(statediff[[i]] == TRUE)] = FALSE

  }

  out = model[whichpar]

  return(model[whichpar])

}
