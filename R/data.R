#'  EDR10 and ED50 selected from FREDERICA modelling for the ACT :Acute to
#'  Chronic SSD extrapolation for project for ionising radiation.
#'
#'
#' A dataset containing ED (Effective Dose) derived from dose response
#' modelling of selected FREDERICA tests
#'
#' @format A data frame with 786 rows and 22 variables:
#' \itemize{
#'   \item Ecosystem : ecosystem of the studied species
#'   \item Kingdom : kinddom of the studied species
#'   \item Phylum: phylum of the studied species
#'   \item Class: class of the studied species
#'   \item Order: order of the studied species
#'   \item Family: family of the studied species
#'   \item Genus: genus of the studied species:
#'   \item SpeciesComp: Combination of Genus and Species variables in a unique
#'                      variable
#'   \item Sp_common : name of species according common designation
#'   \item Sp_latin : name of species according latin designation
#'   \item DoseType : type of exposition
#'   \item ID :Reference ID number of the data (automatically generated in
#'            FREDERICA)
#'   \item subID : Reference subID number of the dose-response modeled data from
#'                 which the ED value is derived
#'   \item RadType : radiation type of the exposition
#'   \item Umbrella : umbrella type of the studied effect
#'   \item Effect description : free description of the studied effect
#'   \item Model : model used for building the dose-response curve
#'   \item ED : effective dose redived from the dose-reponse curve. ED is ED50
#'          when DoseType is "Acute", and ED is EDR10 when DoseType is "Chronic"
#'          EDR10 is expressed in µGy/h and ED50 in Gy
#'   \item SE : standard eror of the ED
#'
#' }
#' @source All_v120316.csv dataset
"cipr"
