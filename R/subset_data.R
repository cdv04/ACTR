
#' Subset dataset according a minimal number of ED desired by modalities of group variable (ie. Class)
#'
#' @param dataset A dataframe
#' @param var_grp A group variable telling how counts have to be done (ie.by Class)
#' @param n_min A integer telling the minimum number of EDR10 and ED50 desired
#' @return a subset of the original dataset
#' @examples
#' data(cipr)
#' new<-subset_data(cipr, Class, 6)
#' new<-subset_data(cipr, Phylum, 6)
#' @export

subset_data <- function(dat, var_grp,n_min) {

  library(dplyr)
  library(data.table)

  grp <- substitute(var_grp)
  ind_grp <- which(colnames(dat)==grp)
  dat$var_grp<-dat[,ind_grp]



  nb_acute <- dat %>%
    filter (DoseType=="Acute") %>%
    group_by (var_grp) %>%
    summarise (
      nb_ED_acute = n(),
      nb_Spe_acute = n_distinct(SpeciesComp))

  nb_chronic <- dat %>%
    filter(DoseType == "Chronic") %>%
    group_by (var_grp) %>%
    summarise (
      nb_ED_chronic = n(),
      nb_Spe_chronic = n_distinct(SpeciesComp))


  # data frame of all the available data
  nb_all <- full_join(nb_acute,nb_chronic, by="var_grp")
  nb_all <- data.table( nb_all)
  nb_all <- setcolorder(nb_all, c("var_grp","nb_ED_acute", "nb_ED_chronic",
                                "nb_Spe_acute", "nb_Spe_chronic"))
  nb_all_all <- replace(nb_all, is.na(nb_all),0)


  # data frame with only Class having at least n_min EDR10 and k ED50
  nb_all_n_min <- nb_all_all[nb_all_all$nb_ED_acute >= n_min & nb_all_all$nb_ED_chronic >= n_min,]
  # droplevels of var_group to have only the selecetd one
  nb_all_n_min$var_grp <- droplevels(nb_all_n_min$var_grp)


  #keep modalities of var_group having at least n_min EDR10 and n_min ED50
  toMatch <- nb_all_n_min$var_grp
  matches <- unique (grep(paste(toMatch,collapse="|"),
                          dat$var_grp, value=FALSE))
  sub_data <- dat[matches,]


  sub_data<- sub_data[-dim(dat)[2]] # need to remove the last column


  # droplevels for each of the factor variable
  ind_fact<-which(sapply(sub_data,is.factor))


  for ( i in 1:length(ind_fact)){
    j<-ind_fact[i]
    sub_data[,j]<-droplevels(sub_data[,j])
  }


  return( sub_data )
}



