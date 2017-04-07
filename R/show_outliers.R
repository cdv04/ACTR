#' Returns outliers with var_group, ID, sub_ID and Ed values
#'
#' @param dataset A dataframe
#' @param var_grp A group variable telling how outliers have to be detected
#' @param coef this determines how far the plot ‘whiskers’ extend out from the
#' box. If coef is positive, the whiskers extend to the most extreme data point
#' which is no more than coef times the length of the box away from the box.
#' A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned).
#' @examples
#'
#' @return the detected Outliers by var_grp level
#' @export

#dat <- cipr
#grp <- substitute(Class)


show_outliers<-function(data,var_grp, coef)
{

  grp <- substitute(var_grp)
  ind_grp <- which(colnames(data)==grp)

  data.List<-split(data, droplevels(data[, ind_grp]))
  Res.List<-vector("list", length(data.List))
  Ind.List<-vector("list", length(data.List))

  names(Res.List)<-names(data.List)
  names(Ind.List)<-names(data.List)


  for (i in 1 :length(data.List))
  {
    EDOut<-10^boxplot.stats(log10(data.List[[i]]$ED),coef=coef)$out
    if(length(EDOut)!=0)
    {
      toMatch<-EDOut
      Ind.List[[i]] <- unique (grep(paste(toMatch,collapse="|"),data.List[[i]]$ED, value=FALSE))
      resdf<-data.List[[i]][Ind.List[[i]], c(4,13,14,19)]
      resdf<-arrange(resdf, ED)
      Res.List[[i]]<-resdf
      rm(resdf)
    }
    else
    {
      Res.List[[i]]<-NA
    }

  }
  return(Res.List)
}


#




