#'Build bootstrap sample of data according a grouping variable (i.e. Class or Phylum)
#'
#'B bootstrap samples of an original dataset are build with replacement using strata as
#'  variable group (i.e.Class or Phylum ) and DoseType (Acute / Chronic)
#'
#' @param dataset A dataframe
#' @param var_grp A grouping variable
#' @param B The number of bootstrap replicates
#' @return The function returns a list containing a bootstrap sample in each element
#' @examples
#' data(cipr)
#' new <- subset_data(cipr, Class,6)
#' new.boot<-boot_data(ciprKP,Class,200)
#' @export

#dat = ciprKP
#

boot_data <- function(dat, var_grp,B) {

  library(sampling)
  library(data.table)
  grp <- substitute(var_grp)
  ind_grp <- which(colnames(dat)==grp)

  # creation d'une variable d'interaction var_grp * DoseTYpe utilisée pour
  # récupérer la taille des sous groupe de données par var_grp et DoseType
  # necessaire au bootstrap
  dat$VG_DT<-interaction(dat[,ind_grp], dat$DoseType, sep="_")

  # on recupere la taille des échantillons par var_grp et doseType
  Nb<-table(dat$VG_DT)


  # on initilise une list de taille B
  SampleList<-vector("list",B)

  for (i in 1 : B)
  {
    # des indices sont tirées au sort avec replacement en fonction de la Class et DoseTYpe
    ind<-data.table(strata(dat, c("VG_DT"),Nb , "srswr"))$ID_unit
    # ces indices sont utilisées pour créer l'échantillon
    SampleList[[i]]<-data.frame(dat[ind,])
    SampleList[[i]] <- select(SampleList[[i]],-VG_DT)
    rm(ind)

  }

  return(SampleList)
}
