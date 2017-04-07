#' Calculating plotting position of edr10 obs and edr_10 act
#' Plooting position are calculated according method described in SSWD help doc page 15
#'
#' @param Edr10 A dataframe containing edr10
#' @return the same dataframe with added columns containing the ploting position (w_i_j) and intermediate variables
#' @examples
#' pp_obs <- plotting_position (edr10_obs)
#' @export


# dat <- edr10_obs
# dat <- edr10_act

plotting_position<- function(Edr10)
{
  library(dplyr)

  dat <- Edr10


  ### rename edr10_obs and edr10_act variables by edr10



  ### arrannge data and edr10_act by var_grp (i.e. Class) and Species Comp
  dat <- arrange(dat,dat[,1], SpeciesComp)


  ### put data and edr10 into Class, 1 element by var_group
  dat_list <- split (dat, dat[,1])

  ### creating n_s variable (n_s = number of different species into var_group level (i.e. Class))
  dat_list <- lapply (dat_list, function (x){
    x$n_s <- nlevels(droplevels(x$SpeciesComp))
    x
  })



  ### Calculating weights
  #creating n_i variable (n_i is the number of data available for species_i, k is the var_grp index (i.e. Class))
  K <- length(dat_list)
  for (k in 1: K)
  {
    tmp_dat <- split (dat_list[[k]], droplevels(dat_list[[k]]$SpeciesComp))
    tmp_dat <- lapply ( tmp_dat, function (x) {
      x$n_i <- length(x$ED)
      x
    })
    tmp_dat <- data.frame(Reduce(rbind,tmp_dat))
    dat_list[[k]] <- tmp_dat
  }


  #creating w_ij = 1(n_i * n_s)  ( j = index of ED assigned to species i )
  dat_list <- lapply ( dat_list, function (x){
    x$w_ij <- 1 / (x$n_i * x$n_s)
    x
  })



  # reordoring s_ij (=log10(ED) by increasing order)
  dat_list <- lapply(dat_list, function(x){
    x$s_ij <- log10(x$ED)
    x <- arrange ( x, s_ij)
    x
  })


  #cacluclating r_i (cf formula on SSWD help doc p 15)
  dat_list <- lapply(dat_list, function(x){
    n <- dim(x)[1]
    x$r_i <- n*cumsum(x$w_ij)
    x
  })




  #calculating p_i_w (weighted cumulative probability , cf formula on SSWD help doc p 15)
  dat_list <- lapply(dat_list, function(x){
    n<-dim(x)[1]
    r1<-x$r_i[1]
    a<-ifelse(r1<0.5,0,0.5) #a= Hazen parameter, takes value 0 if r1<0.5, takes value 0.5 (defaults value) otherwise
    x$p_i_w <-  (x$r_i - a) / (n + 1 - 2*a)
    x
  })



  ### Post Processing

  dat_df <- data.frame (Reduce(rbind,dat_list))

  ind_edr10 <- grep("EDR10",names(dat_df))

  nameType <- strsplit(names(dat)[ind_edr10],"_")[[1]][2]

  names(dat_df)[ind_edr10] <- c("EDR10")
  #names(dat_df)[1] <- names(Ed)

  dat_df$type <- nameType



  return(dat_df)
}


