#' Print estimate of hdrX_obs and hdrX_act, and their ratio (obs /act) and the mean and median of this ratio
#'
#'
#' @param myHdr_obs Data frame containing hdrX_obs
#' @param myHdr_act Data frame containing hdrX_obs
#' @return a list containing hdrX_obs and hdrX_act, and their ration (obs /act) and the mean and median of this ratio
#' @examples
#' myhdr5 <-resume_hdr(hdr5_obs, hdr5_act)
#' @export



# myHdrX_obs <- hdr5_EO
# myHdrX_act <-hdr5_EA

resume_hdr <- function(myHdrX_obs, myHdrX_act){


  ## dataframe with hdr5_obs and hdr5_act and the ration obs / act

  CompHdr <- cbind(myHdrX_obs[,1:2],myHdrX_act[,2])
  tmpName <- names(  CompHdr)[2]
  tmpName_obs <- paste0(tmpName , "_obs")

  tmpName_act <- paste0(tmpName,"_act")
  names(CompHdr)[c(2,3)]<- c(tmpName_obs,tmpName_act)
  CompHdr$R_obs_act <- round(CompHdr[,2] / CompHdr[,3],2)

  #CompHdr
  Moy_r <- mean(CompHdr$R_obs_act )
  Med_r <- median(CompHdr$R_obs_act)
  res<-list(CompHdr, Moy_r, Med_r)
  names(res) <- c(tmpName,"moy (hdr_obs / hdr_act)","med (hdr_obs / hdr_act)")
  return(res)
}
