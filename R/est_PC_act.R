
#' Prediction of wMuC_act and wSigmaC_act, i.e on edr10_act
#'
#'
#'
#'
#' @param edr10_act A dataframe containing the ACT edr10
#' @param var_group A variable of regroupement as "Class" or "Phylum"
#' @return a dataframe
#' @examples
#'library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC_obs <- est_PAC(ciprKP,Class)
#' PC_pred <- predict_PC(PAC_obs,bm_mu, bm_sigma)
#' PAC_gen <- cbind(PA_obs,PC_pred[,-1] )
#' edr10_act <- gen_edr10(PAC_gen,ciprKP.ac)
#' PC_act <- est_PC_act(edr10_act,Class)
#' @export
# wpara_lg <- wpara_obs_lg
# form_mu <- bm_mu
# form_sigma <- bm_sigma

# edr10_act <- edr10_act
# grp <- "Class"

est_PC_act <-function(edr10_act, var_grp)
{

  dat <- edr10_act
  grp <- substitute(var_grp)
  ind_grp <- which(colnames(dat)==grp)

  # reorder data by var_group alphabetic order
  dat <- arrange (dat, dat[,ind_grp], SpeciesComp)
  dat_list <- split(dat, droplevels(dat[, ind_grp]))


  ## Calcul of needed variables
  # creating n_i variable (n_i is the number of data available for species i)
  # k is the var_grp index

  for(k in 1 : length(dat_list))
  {
    dat_list_tmp <- split(dat_list[[k]], droplevels(dat_list[[k]]$SpeciesComp))
    dat_list_tmp <- lapply(dat_list_tmp, function(x) {
      x$n_i <- length(x$EDR10_act)
      x
    })
    dat_list_tmp <- unsplit(dat_list_tmp, droplevels(dat_list[[k]]$SpeciesComp))
    dat_list[[k]] <- dat_list_tmp
  }
  # creating other variable
  dat_list <- lapply(dat_list, function(x){
    x$SpeciesComp <- droplevels(x$SpeciesComp)
    x$n_s <- nlevels(x$SpeciesComp)
    x$w_ij <- 1/(x$n_i*x$n_s)
    x$s_ij_w <- log10(x$EDR10_act)*x$w_ij
    µ <- sum(x$s_ij_w)
    x$sss_ij <- (log10(x$EDR10_act)-µ)^2
    x$sss_ij_w <- x$sss_ij * x$w_ij
    x

  })

  # Calculating MuC_act and SigmaC_act

  wMuC_act <- sapply(dat_list, function(x){
    round(sum(x$s_ij_w),2)
  })

  wSigmaC_act <- sapply(dat_list, function(x){
    n<-dim(x)[1]
    round(sqrt(sum(x$sss_ij_w)*n/(n-1)),2)
  })



  grp_level <- (levels(dat[,ind_grp]))


  # summary dataframe
 paraC_act<-data.frame(grp=grp_level,
                       wMuC_lg_act=  wMuC_act,
                       wSigmaC_lg_act=  wSigmaC_act)
  #names(paraC_act)[1] <- grp
  names(paraC_act)[1] <- names(dat)[1]
  rownames(paraC_act) <- NULL
  return(paraC_act)

}
