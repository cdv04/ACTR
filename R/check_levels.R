
#' Print all the levels (modalities) of Umbrella Effects
#'
#' Aim : check that there i no mutation effect in the dataset
#'
#' Outputs are printed in file
#'
#' @param dataset A dataframe containing a variable named "Umbrella"
#' @examples
#' check_no_mutation(cipr)
#' @export




check_no_mutation <- function(dataset) {

  print("*********************************************************************",
        quote=FALSE)
  print ("Please check there is no mutation effect", quote=FALSE)
  print("*********************************************************************",
        quote=FALSE)

  print (levels(dataset$Umbrella))

  if (length(grep( "uta",levels(dataset$Umbrella)))>0) {
    message(" /!\ mutation effect seems to be included in the dataset")
  }


}



#' Print all the levels (modalities)  of Dose Type
#'
#' Aim : check there are only CHRONIC and ACUTE dose types in the datset
#'
#' Outputs are printed in file
#'
#' @param dataset A dataframe containing a variable named "DoseType"
#' @examples
#' check_dosetype(cipr)




check_dosetype<- function(dataset) {


  print("*********************************************************************",
        quote=FALSE)
  print ("Please check there are only ACUTE and CHRONIC dose types in the",
         quote=FALSE)
  print ("dataset",
         quote=FALSE)
  print("*********************************************************************",
        quote=FALSE)

  print (levels(dataset$DoseType))

  if (length(levels(dataset$DoseType))>2) {
    message(" /!\ there are more than ACUTE and CHRONIC dose type in the dataset")
  }

}


#' Print all the levels (modalities) of Class
#'
#' Aim : check each Class'name is writtten with the
#' same orthography (needed for the ACT method)
#'
#' Outputs are printed in file
#'
#' @param dataset A dataframe containing a variable named "Class"
#' @examples
#' check_class(cipr)



check_class<- function(dataset) {

  print("*********************************************************************",
        quote=FALSE)
  print ("Please check names of Class are always written with the same",
         quote=FALSE)
  print ("orthography, it's needed for the ACT method" ,quote=FALSE)
  print("*********************************************************************",
        quote=FALSE)

  print (levels(dataset$Class))

  }




#' Print all the levels (modalities) of Species for each Class of for each Dose Type
#'
#' Aim :  check that species names are always writtten with the
#' same orthography (needed for the ACT method)
#'
#' Outputs are printed in file
#'
#' @param dataset A dataframe containing DoseType, Class and SpeciesComp variables
#' @examples
#' check_species_byclass(cipr)


check_species_byclass<-function(dataset)
{
  library(dplyr)
 # split data by dose type oand order by alphabetic order of Class and put into list
  acute_df <- filter(dataset, DoseType == "Acute") #select Acute data
  acute_df <- arrange(acute_df, Class) # order dataset.Acute by Class (alphabetic order)
  acute_list  <-  split(acute_df,  droplevels(acute_df$Class))

  chronic_df <- filter(dataset, DoseType == "Chronic")
  chronic_df <- arrange(chronic_df, Class)
  chronic_list  <-  split(chronic_df,  droplevels(chronic_df$Class))

  print("*********************************************************************",
        quote = FALSE)
  print ("Please check names of Species by Class are always written with the ",
         quote = FALSE)
  print ("same orthography, it's needed for the ACT method" ,quote=FALSE)
  print("*********************************************************************",
        quote = FALSE)
  cat("\n")
  cat("\n")

  print("****************************** ACUTE *****************************",
        quote=FALSE)

  # print the level of Species for Acute Dose Type (LSGA)
  ac_spe_name<-sapply(acute_list, function(x)
  {
    levels(droplevels(x$SpeciesComp))
  })
  print(ac_spe_name)
  cat("\n")
  cat("\n")
  print("******************************** CHRONIC ***************************", quote=FALSE)


  # print the level for Chronic
  chro_spe_name<-sapply(chronic_list, function(x)
  {
    levels(droplevels(x$SpeciesComp))
  })
  print(chro_spe_name)
  cat("\n")

}




#' Print levels of different variables in order to check data are ready for
#' applying ACT method
#'
#' cf help page of check_no_mutation , check_dosetype, check_class
#' and check_species_byclass functions
#'
#'
#'
#' Are printed :
#'
#' Levels of Umbrella effect (no mutation expected)
#'
#' Levels of dose type (are printed)only chronic and acute expected)
#'
#' Levels of class  (strictly same orthography needed)
#'
#' This function calls following functions:
#'
#' check_no_mutation,
#'
#' check_dosetype
#'
#' check_class
#'
#' check_species_byclass
#'
#'
#' @param dataset A dataframe containing a variable named "Class"
#' @examples
#' check_all(cipr)
#' @details  Outputs are printed in the "check_all_output.txt " file
#' @export


check_all<-function(dataset){

  sink("check_all_output.txt", append=FALSE)

  check_no_mutation(dataset)
  cat("\n")
  cat("\n")
  check_dosetype(dataset)
  cat("\n")
  cat("\n")
  check_class(dataset)
  cat("\n")
  cat("\n")
  check_species_byclass(dataset)
  sink()
}
