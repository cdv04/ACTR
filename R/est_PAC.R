#' Estimation of weighted means and standard deviation of EDR10 and ED50 samples
#' (in log scale)
#' @details
#' The function estimates weighted mean and standard deviation of EDR10 and ED50
#' samples. Weight ar 1/(n_i * n_s), where n_i is the number of ED mesaured on
#' the same species and n_s is the numbre of species in the modality of var_grp
#' (ie. Class).
#' More details into "Acute to chronic species sensivity distribution
#' extrapolation,  2004 (Duboudin et al, Ennv Tox & Chem , vol 23 n 7, p1774 - 1785)
#'
#' @param dataset A dataframe
#' @param var_grp A group variable telling how counts have to be done (ie.by Class)
#' @param n_min  A integer telling the minimum number of EDR10 and ED50 desired
#' @return a data frame with the weighted Mu_A, Mu_C, Sigma_A and Sigma_C
#' @examples
#' library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' myPAC <- est_PAC (ciprKP,Class)
#' @export


#dat <- ciprKP

est_PAC<- function (dat, var_grp) {
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


  ## Calcul of needed variables
  # creating n_i variable (n_i is the number of data available for species i)
  # k is the var_grp index
  for(k in 1 : length(dat_acute.list))
  {
    ac_list_tmp <- split(dat_acute.list[[k]], droplevels(dat_acute.list[[k]]$SpeciesComp))
    ac_list_tmp <- lapply(ac_list_tmp, function(x) {
      x$n_i <- length(x$ED)
      x
    })
    ac_list_tmp <- unsplit(ac_list_tmp, droplevels(dat_acute.list[[k]]$SpeciesComp))
    dat_acute.list[[k]] <- ac_list_tmp
  }
  # creating other variable
  dat_acute.list <- lapply(dat_acute.list, function(x){
    x$SpeciesComp <- droplevels(x$SpeciesComp)
    x$n_s <- nlevels(x$SpeciesComp)
    x$w_ij <- 1/(x$n_i*x$n_s)
    x$s_ij_w <- log10(x$ED)*x$w_ij
    µ<-sum(x$s_ij_w)
    x$sss_ij <- (log10(x$ED)-µ)^2
    x$sss_ij_w <- x$sss_ij * x$w_ij
    x

  })




  ## Calcul of needed variables
  # creating n_i variable (n_i is the number of data available for species i)
  # k is the var_grp index
  for(k in 1 : length(dat_chronic.list))
  {
    chro_list_tmp <- split(dat_chronic.list[[k]], droplevels(dat_chronic.list[[k]]$SpeciesComp))
    chro_list_tmp <- lapply(chro_list_tmp, function(x) {
      x$n_i <- length(x$ED)
      x
    })
    chro_list_tmp <- unsplit(chro_list_tmp, droplevels(dat_chronic.list[[k]]$SpeciesComp))
    dat_chronic.list[[k]] <- chro_list_tmp
  }
  # creating other variable
  dat_chronic.list <- lapply(dat_chronic.list, function(x){
    x$SpeciesComp <- droplevels(x$SpeciesComp)
    x$n_s <- nlevels(x$SpeciesComp)
    x$w_ij <- 1/(x$n_i*x$n_s)
    x$s_ij_w <- log10(x$ED)*x$w_ij
    µ <- sum(x$s_ij_w)
    x$sss_ij <- (log10(x$ED)-µ)^2
    x$sss_ij_w <- x$sss_ij * x$w_ij
    x

  })

  # Calculating wMuA and wMuC
  wMuA <- sapply(dat_acute.list, function(x){
    round(sum(x$s_ij_w),2)
  })

  wMuC <- sapply(dat_chronic.list, function(x){
    round(sum(x$s_ij_w),2)
  })

  # Calculating wsigma_A and wsigma_C
  wSigmaA<- sapply(dat_acute.list, function(x){
    n <- dim(x)[1]
    round(sqrt(sum(x$sss_ij_w)*n/(n-1)),2)
  })

  wSigmaC <- sapply(dat_chronic.list, function(x){
    n<-dim(x)[1]
    round(sqrt(sum(x$sss_ij_w)*n/(n-1)),2)
  })


  # taking the levels of the var_grp
    grp_level <- (levels(dat_acute.df[,ind_grp]))


  # summary dataframe
  PAC<-data.frame(grp = grp_level,
                       wMuA_lg = wMuA,
                       wSigmaA_lg = wSigmaA,
                       wMuC_lg = wMuC,
                       wSigmaC_lg = wSigmaC)
  names(PAC)[1] <- names(dat)[ind_grp]
  rownames(PAC) <- NULL
  return(PAC)

}


