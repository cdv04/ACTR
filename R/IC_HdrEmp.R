#' Calculating IC 95 percent of empirical hdr using non parametric bootstrap samples
#' @param edr10 A dataframe containing the edr10 for all the var_group  we want to calculate hdr th
#' @param th The desired probability (i.e. 0.05 for HDR5)
#' @param B TNumber of bootstrap samples
#' @examples
#' IC_HdrEmp(edr10_obs, 0.05,199)
#' @return a data frame containing the hdr calculated from empirical distribution and the 95 percent confidence interval
#' @export


#edr10.df <- edr10_obs

IC_HdrEmp <- function(edr10.df, th, B)
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
    hdr <- Hdr_Emp(edr10.df,th)


  ##### Calculating IC

  ## Bootstrap on edr10_obs

    set.seed(5678)

    # on recupere la taille des échantillons par var_grp et doseType
    Nb <- table(dat[,1])
    # on initilise une list de taille B
    SampleList<-vector("list",B)

    for (i in 1 : B)
    {

      # des indices sont tirées au sort avec replacement en fonction de la varGroup (i.e. Class)
      ind<-data.table(strata(dat, VarGrpName,Nb , "srswr"))$ID_unit

      # ces indices sont utilisées pour créer l'échantillon
      SampleList[[i]]<-data.frame(dat[ind,])
      SampleList[[i]] <- plotting_position(SampleList[[i]])

      rm(ind)


    }


  ## Calculating Hdr th from empirical distribution for all the bootstraped samples

    hdr_bt_list <- vector("list", B)
    hdr_bt_list <- lapply ( SampleList, Hdr_Emp,th)


    ## putting in a list (1 element by var_grp level), all the B bootstrap sample hdr5
    hdr_tmp <- vector("list",N)
    for (i in 1 : N) {
       hdr_tmp[[i]] <-sapply(hdr_bt_list,"[", i,2 )
    }
    names(hdr_tmp) <-  hdr_bt_list[[1]][,1]

    # putting in a list (1 element by var_grp level), the quantile 2.5 and 97.5 of the bootstrap samples hdr5
    qtl <- vector("list",N)
    for (i in 1 : N) {
    qtl[[i]] <- quantile( hdr_tmp[[i]],c(0.025,0.975))
    qtl[[i]] <- as.numeric(sprintf("%4.2e",qtl[[i]]))
    }
    names(qtl) <-  names(hdr_tmp)


    binf <- unlist(lapply(qtl,"[",1))
    bsup <- unlist(lapply(qtl,"[",2))


    qtl_res <- data.frame(binf=binf, bsup=bsup, var_grp=names(binf))

  ## a build a dataframe of results
    res<- cbind(hdr,qtl_res)
    names(res) [3:4]<- c("q2.5%", "q97.5%")
    rownames(res) <- NULL
    res <- res [,-5]
    return(res)
  }

