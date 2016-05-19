#' Split edr10_obs from dataset
#' @param dat A dataframe containing edr10 and ed50
#' @param var_grp A variable group (kept in the return dataset, i.e. Class)
#' @examples
#' edr10_obs <- split_edr10_obs(ciprKP, Class)
#' @return a data frame contianing the edr10_obs, var_grp (i.e. Class or Phylum), SpeciesComp, Id, subid and Umbrella effect
#' @export


# dat <-ciprKP
# ind_grp <- 4

split_edr10_obs<-function (dat, var_grp) {

  library(dplyr)


  grp <- substitute (var_grp)
  ind_grp<-which(colnames(dat)==grp)


  tmp_Chro <- filter(dat, DoseType =="Chronic")

  dat_Chro <- data.frame(tmp_Chro[,ind_grp], tmp_Chro$SpeciesComp, tmp_Chro$ID,
                         tmp_Chro$sub.id, tmp_Chro$Umbrella, tmp_Chro$ED)

  names(dat_Chro) <- c(names(dat)[ind_grp], "SpeciesComp","ID", "sub.id",
                       "Umbrella", "EDR10_obs")

  return(dat_Chro)
}
