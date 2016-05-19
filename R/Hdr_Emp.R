#' Calculating HDR5 of observed data from log normal distribution
#' @param edr10 A dataframe containing the edr10 for all the var_group  we want to calculate hdr th
#' @param th The desired probability (i.e. 0.05 for HDR5)
#' @examples
#' Hdr_Emp(edr10_obs,0.05)
#' @return a data frame containing the hdr th calculated from empirical distribution
#' @export

Hdr_Emp <- function (edr10.df,th)
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

    VarGrpName<-vector(length=N) # initialize hdr5 vector

    for ( i in 1 : N)
    {

      tmp <- dat.ls[[i]]
      VarGrpName[i] <-as.character(tmp[1,1])

      if ( th <= tmp$p_i_w[1]) {
        hdr[i] <-tmp$EDR10[1]

      } else if ( th >= max(tmp$p_i_w)) {
        hdr[i] <- max(tmp$EDR10)


      }else {
        ind <- max(which(tmp$p_i_w <= th))
        hdr[i] <- exp (log(tmp$EDR10)[ind+1] - (((tmp$p_i_w[ind+1] -th) *( log(tmp$EDR10)[ind+1] - log(tmp$EDR10)[ind]))/ (tmp$p_i_w[ind+1] - tmp$p_i_w[ind]) ))
        #hdr5_obs[i] <- 10^(tmp$s_ij[ind+1] -((tmp$s_ij[ind+1]-tmp$s_ij[ind]) * (tmp$p_i_w[ind+1] - th) / (tmp$p_i_w[ind+1]-tmp$p_i_w[ind])))
        rm(ind)


        }

    }


    hdr <- data.frame(VarGrpName, hdr)
    names( hdr)[1] <- names(dat)[1]
    names(hdr)[2] <- paste0("hdr",th*100 )

  return(hdr)
}
