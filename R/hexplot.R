hexplot = function(readin, nrates = NULL, pars = NULL, hexbins = 50) {

  if(is.data.frame(readin) == FALSE){

    if(is.list(readin)){
      npar = readin$info$npar

      if (is.null(pars)){
        pars = colnames(readin$rates)
      }

      if (is.null(nrates)){
        df = readin$rates
        lh = rep(readin$info$lh, times = length(pars))

      } else if (length(nrates) == 1){
        df = readin$rates[which(npar==nrates),]
        lh = rep(readin$info$lh[which(npar==nrates)], times = length(pars))

      } else if (length(nrates) > 1) {
        df = readin$rates[which(npar %in% nrates),]
        lh = rep(readin$info$lh[which(npar %in% nrates)], times = length(pars))
      }

    } else if(is.character(readin)){
      readin = plotinit(readin)
    }


      npar = readin$npar
  }

      if (is.null(pars)){
        pars = colnames(readin)[3:ncol(readin)]
      }

      if (is.null(nrates)){
        if("npar" %in% colnames(readin)){
        df = readin
        lh = rep(readin$lh, times = length(pars))
        nrates = 1:max(readin$npar)
        } else {
          df = readin
          lh = rep(readin$lh, times = length(pars))
        }

      } else if (length(nrates) == 1){
        df = readin[which(readin$npar==nrates),]
        lh = rep(readin$lh[which(readin$npar==nrates)], times = length(pars))

      } else if (length(nrates) > 1) {
        df = readin[which(readin$npar %in% nrates),]
        lh = rep(readin$lh[which(readin$npar %in% nrates)], times = length(pars))
      }


  df = df[,which(colnames(df) %in% pars)]
  df = cbind(lh, gather(df, "par", "val"))
  df$val[which(df$val <= 0)] = NA

  p = ggplot(data = df, aes(x=val, y=lh)) + facet_wrap(vars(par)) +
    theme_bw(base_size = 11) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    xlab("Transition rate") +
    ylab("Log-likelihood") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    geom_hex(bins = hexbins, colour = NA) +
    scale_fill_viridis_c(end = 1, trans = "log", guide = FALSE)

  p
}
