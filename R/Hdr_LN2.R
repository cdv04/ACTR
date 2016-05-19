#' Calculating HDR5 of observed data from log normal distribution.
#' @param edr10 A dataframe containing the edr10 for all the var_group  we want to calculate hdr th
#' @param th The desired probability (i.e. 0.05 for HDR5)
#' @details This function is necessary for IC_HdrLN2, when IC of hdr th are etsimated without caclculation weight of the bootstrap sample
#' @examples
#' Hdr_LN(edr10_obs,0.05)
#' @return a data frame containing the hdr th calculated from empirical distribution
#' @export


#edr10.df <- edr10_obs
Hdr_LN2 <- function (edr10.df,th)
{
  library(dplyr)

  dat <- edr10.df
  # set var_grp
  VarGrpName<-names(dat)[1]


  # arrange edr10 by Var Group alphabetic oredr
  dat<- arrange (dat, dat[,1])

  # put into list , 1 vargroup level by element
  dat.ls <- split(dat,droplevels(dat[,1]) )


  #####################################################################################
  ## calculating hdr th
  ##

  N <- length(dat.ls)
  hdr <- vector(length=N) # initialize hdr5 vector

  VarGrpName<-vector(length=N)

  for ( i in 1 : N)
  {

    dat_tmp <- dat.ls[[i]]
    Mu <-mean(log10(dat_tmp$EDR10))
    Sigma <- sd(log10(dat_tmp$EDR10))
    hdr[i] <- 10^qnorm(th, Mu, Sigma)
    VarGrpName[i] <- as.character(dat_tmp[1,1])

  }


  hdr <- data.frame(VarGrpName, hdr)
  names( hdr)[1] <- names(dat)[1]
  names(hdr)[2] <- paste0("hdr",th*100 )

  return(hdr)
}
