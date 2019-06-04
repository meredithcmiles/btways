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
