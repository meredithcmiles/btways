plotinit = function(chains){

  N = length(chains)
  rj = "npar" %in% names(chains$info)
  mult = is.null(names(chains))

  if(rj == FALSE){
  
    if(mult == FALSE){ # most basic form, for single non-RJ

      info = chains$info
      rates = chains$rates
      out = cbind(info, rates)
      readin = out
      
    } else if (mult == TRUE){ # >1 non-RJ

      len = unlist(lapply(chains, function(x){nrow(x[[2]])}))
      readin = as.data.frame(matrix(NA, nrow = sum(len), ncol = 2 + ncol(chains[[1]]$rates)))
      colnames(readin) = c("lh", colnames(chains[[1]]$rates))
      starts = c(1, len[2:length(len)]+1)

      for (i in 1:N){

        if(i==1){ # exception for first iteration
          out = data.frame("lh" = chains[[i]]$info$lh,
                           chains[[i]]$rates)
          readin[(1:len[1]),] = out
          ns = len[1]+1
        } else {
          end = ns + len[i] - 1
          out = data.frame("lh" = chains[[i]]$info$lh,
                           chains[[i]]$rates)
          readin[ns:end,] = out
          ns = end + 1
        }
      } # exit loop
    }
  } else if (rj == TRUE){ #RJ, regardless of N
    
    len = unlist(lapply(chains, function(x){nrow(x[[2]])}))
    readin = as.data.frame(matrix(NA, nrow = sum(len), ncol = 2 + ncol(chains[[1]]$rates)))
    colnames(readin) = c("npar", "lh", colnames(chains[[1]]$rates))
    starts = c(1, len[2:length(len)]+1)
    
    for (i in 1:N){
      
      if(i==1){
        out = data.frame("npar" = chains[[i]]$info$npar, "lh" = chains[[i]]$info$lh,
        chains[[i]]$rates)
        readin[(1:len[1]),] = out
        ns = len[1]+1
      } else {
        end = ns + len[i] - 1
        out = data.frame("npar" = chains[[i]]$info$npar, chains[[i]]$info$lh,
        chains[[i]]$rates)
        readin[ns:end,] = out
        ns = end + 1
      }
    }    
  }
  return(out)
}
