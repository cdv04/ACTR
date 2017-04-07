#' Plot hdr_th_obs and hdr_th_act with their 95 percent confidence intervals
#' @param myHdr5_obs Data frame containing hdr5_obs
#' @param myHdr5_act Data frame containing hdr5_obs
#' @examples
#' plot_hdr5 <-view_hdr(hdr5_obs, hdr5_act)
#' @return A ggplot object
#' @export

# myHdrX_obs <- hdr5_LNO
# myHdrX_act <- hdr5_LNA


view_hdr<- function(myHdrX_obs, myHdrX_act) {


  ## data process for graph


  myHdrX_act$type <- c("act")
  myHdrX_obs$type <- c("obs")
  myhdrX <- rbind(myHdrX_obs, myHdrX_act)
  names(myhdrX)[3:4] <- c("binf", "bsup")
  myhdrX$type <- as.factor(myhdrX$type)
  myhdrX$type <- factor(myhdrX$type,levels=c("act","obs"))

  library(ggplot2)
  hdrX_plot <- ggplot(myhdrX,aes(x=myhdrX[,2], y=Class, fill=type, colour=type)) +
    geom_errorbarh(aes(xmin=binf, xmax=bsup), size=1.5)+
    geom_point(size=6, aes(colour=type, shape=type))+
    scale_x_log10(limits=c(min(myhdrX[,3])/10 ,max(myhdrX[,4]*10)), breaks=c(1e-04, 1e-03, 1e-02, 1e-01, 1e0, 1e+01, 1e+02, 1e+03, 1e+04, 1e+05) ) +
    theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))+
    xlab(names(myhdrX)[2])+
    theme_bw()+
    scale_colour_manual(values=c("grey60", "grey20"))+
    scale_fill_manual(values=c("grey60", "grey20"))+
    theme(axis.title.x=element_text(face="bold", size=rel(2)))+
    theme(axis.title.y=element_text(face="bold", size=rel(2)))+
    theme(strip.text = element_text(face="bold", size=rel(1.5)))+
    theme(axis.text.y = element_text(size=rel(2)))
  return(hdrX_plot )
}



