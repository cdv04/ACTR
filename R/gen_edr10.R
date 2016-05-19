#' Genertae edr10_act from ED50 (acute data) and regression coefficients
#'
#' ED50 are standardized (substraction mu_A and dividing by sigma_ A)
#' and then scaled into chronic data (ED10) adding muC_act and multiplying by
#' sigma_C_act
#'
#'
#' @param paracACT A dataframe containing mu_C_act and sigma_C_acy
#' @param acute_df A dataframe containing the ED50
#' @return a data frame containing the predicted EDR10 (=EDR10_act)
#' @examples
#'library(ACTR)
#' data(cipr)
#' ciprKP <- subset_data(cipr, Class,6)
#' PAC_obs <- est_PAC(ciprKP,Class)
#' PC_pred <- predict_PC(PAC_obs,bm_mu, bm_sigma)
#' PAC_gen <- cbind(PA_obs,PC_pred[,-1] )
#' edr10_act <- gen_edr10(PAC_gen,ciprKP.ac)

#' @export

#cipr_kp_ac <- filter(cipr_kp, DoseType=="Acute")
# acute_df <-  cipr_kp_ac
# paraACreg<- para_ACReg_lg
# acute_df <- ciprKP.ac
gen_edr10 <- function (PAC_gen,acute_df) {

  library(dplyr)

  ## reorder paraACT by var_group (given by rownames of paraACT) alphabetic order
  PAC_gen <- arrange (PAC_gen, PAC_gen[,1])


  # permits to know the index of the var_grp in the acute_df
  var_grp <- names(PAC_gen)[1]
  ind_grp <- which(colnames(acute_df)==var_grp)

  ## reorder acute_df by var_group (given by rownames of PAC_gen) alphabetic order
  acute_df <- arrange (acute_df, acute_df[,ind_grp])

  # transform acute df into a list
  acute_list <-  split(acute_df,  droplevels(acute_df[,ind_grp]))


  ## generate edr10
  # initilization of a list to contain the edr10_act
  N <- dim(PAC_gen)[1]


  for (i in 1 : N )
  {
    wMuA_lg <- PAC_gen$wMuA_lg[i]
    wSigmaA_lg <- PAC_gen$wSigmaA_lg[i]
    MuC_pred <- PAC_gen$MuC_pred[i]
    SigmaC_pred <- PAC_gen$SigmaC_pred[i]
    grp <- PAC_gen[i,1] # permits to take Class or Phylum or other

    acute_list[[i]]$EDR10_act <- 10^(SigmaC_pred * ((log10(acute_list[[i]]$ED) - wMuA_lg )/  wSigmaA_lg) + MuC_pred)

  }

  toMatch <- c("EDR10_act", "SpeciesComp", "ID", "sub.id", "Umbrella")
  ind_tmp <- unique (grep(paste(toMatch,collapse="|"),names(acute_list[[1]]), value=FALSE))

  ind_kpt <- c(ind_grp,ind_tmp)
  edr10_act_list <- lapply(acute_list, "[",,ind_kpt)



  edr10_act_df <- data.frame (Reduce (rbind, edr10_act_list ))
  return(edr10_act_df)
}


