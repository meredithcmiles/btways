readMCMC<-function(files, relative=FALSE, nrates = NULL){

  N = length(files)
  parnames<- levels(as.factor(getModel(files[1])))

  if(N>1){
    output = vector(mode = "list", length = N)
  }

  for (i in 1:N){

    file = files[i]

   options(stringsAsFactors = FALSE)

   if (i == 1){
     cat("Reading chains: ")
   }

   skp<-startline(file)-1
   readin<-read.delim(file, sep="\t", skip=skp, header=TRUE, fill=TRUE)

   if (i < N){
     cat(paste(i, ", ", sep=""))
   } else {
     cat(paste(i, "...", sep=""),"\n",
         "   READIN COMPLETE")
   }

   cols <- colnames(readin)
   out<-readin[,which(cols %in% parnames)]

   rj = "Model.string" %in% cols
   glob = "Global.Rate" %in% cols

#   if(rj == TRUE){
#     subs = substr(readin$Model.string, 2, nchar(readin$Model.string[1]))
#     models = strsplit(subs,split = " ", fixed = TRUE)
#     models = lapply(models, function(vec){vec[which(vec == "Z")] = NA; return(vec)})
#     readin$Model.string = models
#   }

   if(relative == TRUE){
     if(glob == FALSE){
       rateswitch = TRUE
     } else {rateswitch = FALSE}
   } else {
     if(glob == FALSE){
       rateswitch = FALSE
     } else {rateswitch = TRUE}
   }

   if(rj == TRUE){
     if(glob == TRUE){
       info = list("npar" = readin$No.Off.Parmeters, "nzero" = readin$No.Off.Zero,
                 "lh" = readin$Lh, "model" = readin$Model.string,
                 "globrate" = readin$Global.Rate)
       out = list("info" = info, "rates" = out)
     } else {
       info = list("npar" = readin$No.Off.Parmeters, "nzero" = readin$No.Off.Zero,
                   "lh" = readin$Lh, "model" = readin$Model.string)
       out = list("info" = info, "rates" = out)
       }

   } else {
     if (glob == TRUE){

       info = list("lh" = readin$Lh, "globrate" = readin$Global.Rate)
       out = list("info" = info, "rates" = out)

     } else {
       out = list("info" = list("lh" = readin$Lh), "rates" = out)
     }
   }

   if(rateswitch == TRUE){
     out = switchRates(out)
   }

   if(N == 1){output = out; break}

    output[[i]] = out

  }

  return(output)

}
