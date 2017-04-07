#' Print the number of available EDR10 and ED50, present in
#' the dataset, by group variable level.
#'
#' The function prints the count for :  1) all the modalities of the group
#' variable, 2) the modalities having at least 1 EDR10 or 1 ED50,
#' 3) modalities having at least k (argument on the function) EDR10 and k ED50.
#'
#'#'
#' @param dataset A dataframe
#' @param var_grp A group variable telling how counts have to be done (ie.by Class)
#' @param n_min A integer telling the minimum number of EDR10 and ED50 desired
#' @examples
#' data(cipr)
#' a <- nb_ED(cipr, Class, 6)
#' nb_ED(cipr, Phylum, 6)
#' @return the number of Outliers according the chosen coefficient
#' @export

#dat <- cipr
#grp <- substitute(Class)

nb_ED<-function(dat,var_grp,n_min)
{

  library(dplyr)
  library(data.table)

  grp <- substitute(var_grp)
  ind_grp <- which(colnames(dat)==grp)
  dat$var_grp<-dat[,ind_grp]

  nb_acute <- dat %>%
    filter(DoseType=="Acute") %>%
    group_by(var_grp) %>%
    summarise(
    nb_ED_acute=n(),
    nb_Spe_acute=n_distinct(SpeciesComp))

  nb_chronic<-dat %>%
  filter(DoseType=="Chronic") %>%
  group_by(var_grp) %>%    summarise(
    nb_ED_chronic=n(),
    nb_Spe_chronic=n_distinct(SpeciesComp))


  # data frame of all the available data
  nb_all<-full_join(nb_acute,nb_chronic, by="var_grp")


   nb_all<-data.table( nb_all)
  nb_all<-setcolorder(nb_all, c("var_grp","nb_ED_acute", "nb_ED_chronic",
                                   "nb_Spe_acute", "nb_Spe_chronic"))

  names(nb_all)[1]<-names(dat)[ind_grp] # rename var_grp by the name of the variable given in argument
  nb_all_all<-replace(nb_all, is.na(nb_all),0)



  # data frame with only Class having at least 1 EDR10 or 1 ED50
  nb_all_atleast1<-na.omit(nb_all)



  # data frame with only Class having at least n_min EDR10 and k ED50
  nb_all_n_min<-nb_all_all[nb_all_all$nb_ED_acute>=n_min & nb_all_all$nb_ED_chronic>=n_min,]


  #sink("nb_ED_outputs.txt", append=FALSE)
  res <- list( nb_all_all,  nb_all_atleast1  ,nb_all_n_min)
  names(res) <- c("all data", "group having at least 1 EDR10 or ED50", "group having at least the desired number of EDR10 and ED50")
  return(res)

}
