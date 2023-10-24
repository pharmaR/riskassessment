
subtraction.matrix<-function(avec,bvec){
  na<-length(avec)
  nb<-length(bvec)
  out<-matrix(NA,nrow=na,ncol=nb)
  for(i in seq_len(na)){
    out[i,]<-avec[i]-bvec
  }
  return(out)
}

meanpercentile<-function(sample,population){
  sample %>% sapply(function(x) mean(x<population,na.rm=T)) %>% mean(na.rm=T)
}

#' Compute stimulus-rest correlations of double-difference scores
#' This function provides a statistic that can give an indication of how deviant
#' the responses to specific stimuli are, in comparison to the rest of the stimulus set.
#' The algorithm computes stimulus-rest correlations of stimulus-specific double-difference scores.
#' It takes single-difference approach-avoidance scores for each stimulus, and computes
#' every possible subtraction between individual stimuli from both stimulus categories.
#' It then computes correlations between every such subtraction of stimuli on one hand, and
#' the mean double difference score of all other stimuli. Stimulus-rest correlations are then
#' computed by averaging every such subtraction-rest correlation involving a specific stimulus.
#'
#' @param ds a \code{data.frame}
#' @param subjvar the label of the participant identifier variable
#' @param stimvar the label of the stimulus identifier variable
#' @param pullvar the label of the movement direction identifier variable
#' @param targetvar the label of the stimulus category identifier variable
#' @param rtvar the label of the reaction time variable
#' @param method Optional, the correlation method to be used (pearson, spearman, kendall)
#'
#' @return Returns a \code{aat_stimulus_rest} object containing statistics for each stimulus.
#' Stats include the average stimulus-rest correlation (mcor); the standard deviation of
#' dyad-rest correlations for this stimulus (sdcor);
#' the number of valid correlations involved in these statistic (n);
#' the average percentile of dyad-rest correlations involving the stimulus within
#' the distribution of all other dyad-rest correlations (restpercentile);
#' as well as z-scores (zpercentile) and p-values for this percentile (pval).
#'
#' @export
#'
#' @examples
#'
#' ds<-aat_simulate()
#' stimrest<-aat_stimulus_rest(ds,subjvar="subj",stimvar="stim",pullvar="is_pull",
#'                      targetvar="is_target",rtvar="rt")
#' plot(stimrest)
#' print(stimrest)
aat_stimulus_rest<-function(ds,subjvar,stimvar,pullvar,targetvar,rtvar,method=c("pearson","spearman","kendall")){
  method<-match.arg(method)
  # check data
  ds<-aat_preparedata(ds,subjvar,pullvar,targetvar,rtvar,stimvar=stimvar)

  #compute single-difference scores
  biasset<-ds%>%group_by(!!sym(subjvar),!!sym(stimvar),!!sym(targetvar))%>%
    summarise(bias=mean(subset(!!sym(rtvar),!!sym(pullvar)==0),na.rm=T)-
                mean(subset(!!sym(rtvar),!!sym(pullvar)==1),na.rm=T),.groups="drop")
  stimset<-biasset%>%select(!!sym(stimvar),!!sym(targetvar))%>%distinct()
  stimset$mcor<-NA

  for(i in seq_len(nrow(stimset))){
    iterset<-biasset%>%group_by(!!sym(subjvar))%>%
      summarise(stimbias=.data$bias[which(!!sym(stimvar)==stimset[[stimvar]][i])],
                restbias=mean(.data$bias[!!sym(stimvar) != stimset[[stimvar]][i] &
                                           !!sym(targetvar) == stimset[[targetvar]][i] ]),
                counterbias=mean(.data$bias[!!sym(targetvar) != stimset[[targetvar]][i] ]),
                .groups="drop")
    stimset$mcor[i]<-cor(iterset$stimbias-iterset$counterbias,iterset$restbias-iterset$counterbias,
                         use="complete.obs",method=method)
  }
  return(structure(stimset,class=c("aat_stimulus_rest","data.frame")))
}

#' @rdname aat_stimulus_rest
#' @param x an \code{aat_stimulus_rest} object
#' @param ... Ignored.
#' @export
plot.aat_stimulus_rest<-function(x,...){
  x<-x[!is.na(x$mcor),]
  ranks<-rank(x$mcor)
  wideness<-max(x$mcor)-min(x$mcor)
  plot(x=x$mcor,y=ranks,
       xlim=c(min(x$mcor)-.5*wideness*strwidth(s=x$mcor[min(ranks)],cex=.5,font=2,units="figure"),
              max(x$mcor)+.5*wideness*strwidth(s=x$mcor[max(ranks)],cex=.5,font=2,units="figure")),
       xlab="Stimulus-rest correlation",main=paste0("Stimulus-rest correlations"),
       yaxt="n")
  segments(x0=mean(x$mcor),x1=x$mcor,y0=ranks,y1=ranks)
  text(x=x$mcor,y=ranks,labels=x$stim,
       pos=3+sign(x$mcor-mean(x$mcor)),offset=0.5,cex=.5,font=2)
  abline(v=mean(x$mcor))
  axis(2, labels=x$img,at=ranks,las=1,cex.axis=.5)
}

