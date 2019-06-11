lhplot  = function(readin, whichpar, npar = NULL, hexbins = 50, RJ = TRUE){

 if(is.data.frame(readin) == FALSE) {

    if(is.character(readin)){
      readin = plotinit(readin)
    } else if (is.list(readin)){
      readin = cbind(readin[[1]], readin[[2]])
    }
 }

  df = data.frame("par" = readin[,which(colnames(readin) == whichpar)],
                  "npar" = readin$npar,
                  "lh" = readin$lh)

  p = ggplot(data = df, aes(x = par, y = lh)) + facet_wrap(vars(npar)) +
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
