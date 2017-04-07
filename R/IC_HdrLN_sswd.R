#' Calculating IC 95 percent of  hdr estimated from log normal distribution, using non parametric bootstrap samples and without calculating weight accordind species
#' @param edr10 A dataframe containing the edr10 for all the var_group  we want to calculate hdr th
#' @param th The desired probability (i.e. 0.05 for HDR5)
#' @param B TNumber of bootstrap samples
#' @examples
#' IC_HdrLN_sswd(edr10_obs, 0.05,199)
#' @return a data frame containing the hdr calculated from log normal distribution and the 95 percent confidence interval
#' @export
#' @details boostrap sample are build using weighted sample as in sswd

#edr10.df <- edr10_obs

IC_HdrLN_sswd <- function(edr10.df, th, B)
{

  library(dplyr)
  library(sampling)
  library(data.table)

  ##### Pre pocess
  #####

  dat <- edr10.df
  # set var_grp
  VarGrpName<-names(dat)[1]


  # arrange edr10 by Var Group alphabetic oredr
  dat<- arrange (dat, dat[,1])

  # put into list , 1 vargroup level by element
  dat.ls <- split(dat,droplevels(dat[,1]) )
  N <- length(dat.ls)

  ##### Calculating hdr of real data (not bootstraped samples)
  #####
  hdrR <- Hdr_LN(dat,th)
  hdrR[,2] <- round( hdrR[,2],2)

  ##### Calculating IC

  ## Bootstrap on edr10_obs



  # on recupere la taille des échantillons par var_grp et doseType
  Nb <- table(dat[,1])
  # on initilise une list de taille B
  SampleList<-vector("list",B)

  # on initialise une liste qui contiendra l'échantillon bootstrap de chaque class
  SampleBootClass<-vector("list",B)

  for (i in 1 : B)
  {



    SampleBootClass <- lapply(dat.ls,function(x){

                        sample_n(x,size=nrow(x),replace=TRUE, weight=x$w_ij)
    })

    SampleBootAll_df <- data.frame(Reduce(rbind, SampleBootClass))

    # ces indices sont utilisées pour créer l'échantillon
    SampleList[[i]]<-SampleBootAll_df



  }


  ## Calculating Hdr th from LN distribution for all the bootstraped samples

  hdr_bt_list <- vector("list", B)
  hdr_bt_list <- lapply ( SampleList, Hdr_LN2,th)


  ## putting in a list (1 element by var_grp level), all the B bootstrap sample hdr5
  hdr_tmp <- vector("list",N)
  for (i in 1 : N) {
    hdr_tmp[[i]] <-sapply(hdr_bt_list,"[", i,2 )
  }
  names(hdr_tmp) <-  hdr_bt_list[[1]][,1]

  # putting in a list (1 element by var_grp level), the quantile 2.5 and 97.5 of the bootstrap samples hdr5
  # qtl <- vector("list",N)
  # for (i in 1 : N) {
  # qtl[[i]] <- quantile( hdr_tmp[[i]],c(0.025,0.975))
  # qtl[[i]] <- round(qtl[[i]],4)
  # }
  # names(qtl) <-  names(hdr_tmp)
  #
  #
  # binf <- unlist(lapply(qtl,"[",1))
  # bsup <- unlist(lapply(qtl,"[",2))
  #
  #
  # qtl_res <- data.frame(binf=binf, bsup=bsup, var_grp=names(binf))

  binf <- vector(length=N)
  bsup <- vector(length=N)

  for (i in 1:N) {
    binf[i] <- 10^(quantile(log10(hdr_tmp[[i]]),0.025) - quantile(log10(hdr_tmp[[i]]),0.5) + log10(hdrR[i,2]))
    bsup[i] <- 10^(quantile(log10(hdr_tmp[[i]]),0.975) - quantile(log10(hdr_tmp[[i]]),0.5) + log10(hdrR[i,2]))
  }

  binf <- as.numeric(sprintf("%4.3e",binf))
  bsup <- as.numeric(sprintf("%4.3e",bsup))



  ## a build a dataframe of results
  res<- cbind(hdrR,binf,bsup)
  names(res) [3:4]<- c("q2.5%", "q97.5%")
  rownames(res) <- NULL
  res <- res [,-5]
  return(res)
}

