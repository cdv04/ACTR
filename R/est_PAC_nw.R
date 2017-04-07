#' Estimation of not weighted (nw) means and standard deviation of EDR10 and ED50 samples
#' (in log scale)
#' @details
#' The function estimates unweighted mean and standard deviation of EDR10 and ED50
#' samples. This function is used by IC_HDrLN_sswd when mean and standard deviation
#' of weighted bootstrap sample of ED50 are calculated.
#'
#' @param dataset A dataframe
#' @param var_grp A group variable telling how counts have to be done (ie.by Class)
#' @param n_min  A integer telling the minimum number of EDR10 and ED50 desired
#' @return a data frame with the unweighted Mu_A, Mu_C, Sigma_A and Sigma_C
#' @examples
#' library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' myPAC <- est_PAC_nw (actr17KP,Class)
#' @export


#dat <- actr17KP
# var_group="Class"
est_PAC_nw<- function (dat, var_grp) {
  library(dplyr)


  grp <- substitute(var_grp)
  ind_grp <- which(colnames(dat)==grp)

  ## pre-processing
  #select Acute data
  dat_acute.df <- filter(dat, DoseType == "Acute")
  # order dat.Acute by var_grp (alphabetic order) and SpeciesComp (alphabetic order)
  dat_acute.df <- arrange(dat_acute.df, dat_acute.df[,ind_grp],SpeciesComp )
    # transforme dataframe into list for further developement
  dat_acute.list  <-  split(dat_acute.df,  droplevels(dat_acute.df[,ind_grp]))

  # same for chronic
  dat_chronic.df <- filter(dat, DoseType == "Chronic")
  dat_chronic.df <- arrange(dat_chronic.df, dat_chronic.df[,ind_grp],SpeciesComp)
  dat_chronic.list  <-  split(dat_chronic.df,  droplevels(dat_chronic.df[,ind_grp]))


  # Calculating MuA and MuC
  MuA <- sapply(dat_acute.list, function(x){
    round(mean(log10(x$ED)),2)
  })

  MuC <- sapply(dat_chronic.list, function(x){
    round(mean(log10(x$ED)),2)
  })

  # Calculating wsigma_A and wsigma_C
  SigmaA<- sapply(dat_acute.list, function(x){
    round(sd(log10(x$ED)),2)
  })

  SigmaC <- sapply(dat_chronic.list, function(x){
    round(sd(log10(x$ED)),2)
  })


  # taking the levels of the var_grp
    grp_level <- (levels(dat_acute.df[,ind_grp]))


  # summary dataframe (on garde le w dans les nom pour que ça fonctionne ds la fonction IC_HdrLN_act_sswd)
  PAC<-data.frame(grp = grp_level,
                       wMuA_lg = MuA,
                       wSigmaA_lg = SigmaA,
                       wMuC_lg = MuC,
                       wSigmaC_lg = SigmaC)
  names(PAC)[1] <- names(dat)[ind_grp]
  rownames(PAC) <- NULL
  return(PAC)

}


