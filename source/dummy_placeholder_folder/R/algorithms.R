
# Score computation algorithms ####

#' @title AAT score computation algorithms
#' @name Algorithms
#' @description AAT score computation algorithms
#' @param ds A long-format data.frame
#' @param subjvar Column name of the participant identifier variable
#' @param pullvar Column name of the movement variable (0: avoid; 1: approach)
#' @param targetvar Column name of the stimulus category variable (0: control stimulus; 1: target stimulus)
#' @param rtvar Column name of the reaction time variable
#' @param ... Other arguments passed on by functions (ignored)
#'
#' @return A data.frame containing participant number and computed AAT score.
NULL

#' @describeIn Algorithms computes a mean-based double-difference score:
#' \code{(mean(push_target) - mean(pull_target)) - (mean(push_control) - mean(pull_control))}
#'
#' @export
aat_doublemeandiff<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[targetvar]],ds[[pullvar]]),
            mean.default,na.rm=TRUE)
  b<-apply(a,1,function(x){x[2,1]-x[2,2]-(x[1,1]-x[1,2]) })
  setNames(data.frame(id=names(b),ab=b,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_doublemeandiff_old<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0 & ds[[targetvar]]==1)
  idx2<-which(ds[[pullvar]]==1 & ds[[targetvar]]==1)
  idx3<-which(ds[[pullvar]]==0 & ds[[targetvar]]==0)
  idx4<-which(ds[[pullvar]]==1 & ds[[targetvar]]==0)

  ab<-(tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],mean.default,na.rm=TRUE) -
       tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],mean.default,na.rm=TRUE))-
      (tapply(ds[[rtvar]][idx3],ds[[subjvar]][idx3],mean.default,na.rm=TRUE) -
       tapply(ds[[rtvar]][idx4],ds[[subjvar]][idx4],mean.default,na.rm=TRUE))
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}

aat_doublemeandiff_older<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  group_by(ds,!!sym(subjvar)) %>%
    summarise(ab=(mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 1),na.rm=TRUE) -
                  mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 1),na.rm=TRUE)) -
                 (mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 0),na.rm=TRUE) -
                  mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 0),na.rm=TRUE)))
}

#' @export
#' @describeIn Algorithms computes a median-based double-difference score:
#' \code{(median(push_target) - median(pull_target)) - (median(push_control) - median(pull_control))}
aat_doublemediandiff<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[targetvar]],ds[[pullvar]]),
            median.default,na.rm=TRUE)
  b<-apply(a,1,function(x){x[2,1]-x[2,2]-(x[1,1]-x[1,2]) })
  setNames(data.frame(id=names(b),ab=b,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_doublemediandiff_old<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0 & ds[[targetvar]]==1)
  idx2<-which(ds[[pullvar]]==1 & ds[[targetvar]]==1)
  idx3<-which(ds[[pullvar]]==0 & ds[[targetvar]]==0)
  idx4<-which(ds[[pullvar]]==1 & ds[[targetvar]]==0)

  ab<-(tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],median.default,na.rm=TRUE) -
       tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],median.default,na.rm=TRUE))-
      (tapply(ds[[rtvar]][idx3],ds[[subjvar]][idx3],median.default,na.rm=TRUE) -
       tapply(ds[[rtvar]][idx4],ds[[subjvar]][idx4],median.default,na.rm=TRUE))
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}

aat_doublemediandiff_older<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  group_by(ds,!!sym(subjvar)) %>%
    summarise(ab=(median(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 1),na.rm=TRUE) -
                  median(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 1),na.rm=TRUE)) -
                 (median(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 0),na.rm=TRUE) -
                  median(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 0),na.rm=TRUE)))
}

#' @export
#' @describeIn Algorithms computes D-scores for a 2-block design (see Greenwald, Nosek, and Banaji, 2003):
#' \code{((mean(push_target) - mean(pull_target)) - (mean(push_control) - mean(pull_control))) / sd(participant_reaction_times)}
aat_dscore<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[targetvar]],ds[[pullvar]]),
            mean.default,na.rm=TRUE)
  b<-apply(a,1,function(x){x[2,1]-x[2,2]-(x[1,1]-x[1,2]) })
  sds<-tapply(ds[[rtvar]],ds[[subjvar]],vec.sd,na.rm=TRUE)
  c<-b/sds
  setNames(data.frame(id=names(c),ab=c,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_dscore_old<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0 & ds[[targetvar]]==1)
  idx2<-which(ds[[pullvar]]==1 & ds[[targetvar]]==1)
  idx3<-which(ds[[pullvar]]==0 & ds[[targetvar]]==0)
  idx4<-which(ds[[pullvar]]==1 & ds[[targetvar]]==0)

  ab<-((tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],mean.default,na.rm=TRUE) -
        tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],mean.default,na.rm=TRUE))-
       (tapply(ds[[rtvar]][idx3],ds[[subjvar]][idx3],mean.default,na.rm=TRUE) -
        tapply(ds[[rtvar]][idx4],ds[[subjvar]][idx4],mean.default,na.rm=TRUE)))/
        tapply(ds[[rtvar]],ds[[subjvar]],sd,na.rm=TRUE)
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}

aat_dscore_older<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  group_by(ds,!!sym(subjvar)) %>%
    summarise(ab=((mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 1),na.rm=TRUE) -
                   mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 1),na.rm=TRUE)) -
                  (mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 0),na.rm=TRUE) -
                   mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 0),na.rm=TRUE))) /
                   sd(!!sym(rtvar),na.rm=TRUE))
}

#' @export
#' @describeIn Algorithms computes a double-difference score usign medians,
#' and divides it by the median absolute deviation of the participant's overall reaction times:
#' \code{((median(push_target) - median(pull_target)) - (median(push_control) - median(pull_control))) / mad(participant_reaction_times)}
aat_mediandscore<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[targetvar]],ds[[pullvar]]),
            median.default,na.rm=TRUE)
  b<-apply(a,1,function(x){x[2,1]-x[2,2]-(x[1,1]-x[1,2]) })
  sds<-tapply(ds[[rtvar]],ds[[subjvar]],mad,na.rm=TRUE)
  c<-b/sds
  setNames(data.frame(id=names(c),ab=c,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_mediandscore_old<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0 & ds[[targetvar]]==1)
  idx2<-which(ds[[pullvar]]==1 & ds[[targetvar]]==1)
  idx3<-which(ds[[pullvar]]==0 & ds[[targetvar]]==0)
  idx4<-which(ds[[pullvar]]==1 & ds[[targetvar]]==0)

  ab<-((tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],median.default,na.rm=TRUE) -
        tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],median.default,na.rm=TRUE))-
       (tapply(ds[[rtvar]][idx3],ds[[subjvar]][idx3],median.default,na.rm=TRUE) -
        tapply(ds[[rtvar]][idx4],ds[[subjvar]][idx4],median.default,na.rm=TRUE)))/
        tapply(ds[[rtvar]],ds[[subjvar]],mad,na.rm=TRUE)
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}


aat_mediandscore_older<-function(ds,subjvar,pullvar,targetvar,rtvar,...){
  group_by(ds,!!sym(subjvar)) %>%
    summarise(ab=((median(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 1),na.rm=TRUE) -
                   median(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 1),na.rm=TRUE)) -
                  (median(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 0),na.rm=TRUE) -
                   median(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 0),na.rm=TRUE))) /
                   mad(!!sym(rtvar),na.rm=TRUE))
}

#' @param blockvar name of the variable indicating block number
#' @export
#' @describeIn Algorithms computes D-scores for pairs of sequential blocks
#' and averages the resulting score (see Greenwald, Nosek, and Banaji, 2003).
#' Requires extra \code{blockvar} argument, indicating the name of the block variable.
#note: this matches sequential blocks with one another.
aat_dscore_multiblock<-function(ds,subjvar,pullvar,targetvar,rtvar,blockvar,...){
  ds$.blockset<-floor((ds[[blockvar]]-min(ds[[blockvar]]))/2)
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds$.blockset,ds[[targetvar]],ds[[pullvar]]),
            mean.default,na.rm=TRUE)
  b<-apply(a,1:2,function(x){x[2,1]-x[2,2]-(x[1,1]-x[1,2]) })
  sds<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds$.blockset),vec.sd,na.rm=TRUE)
  c<-rowMeans(b/sds)
  setNames(data.frame(id=names(c),ab=c,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_dscore_multiblock_old<-function(ds,subjvar,pullvar,targetvar,rtvar,blockvar,...){
  ds %>% mutate(.blockset = floor((!!sym(blockvar) - min(!!sym(blockvar)))/2) ) %>%
    group_by(!!sym(subjvar),.data$.blockset) %>%
    summarise(ab=((mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 1),na.rm=TRUE) -
                   mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 1),na.rm=TRUE)) -
                  (mean(subset(!!sym(rtvar),!!sym(pullvar)==0 & !!sym(targetvar) == 0),na.rm=TRUE) -
                   mean(subset(!!sym(rtvar),!!sym(pullvar)==1 & !!sym(targetvar) == 0),na.rm=TRUE))) /
                   sd(!!sym(rtvar),na.rm=TRUE)) %>%
    group_by(!!sym(subjvar)) %>% summarise(ab=mean(ab,na.rm=TRUE))
}

#' @param formula A regression formula to fit to the data to compute an AAT score
#' @param aatterm A character naming the formula term representing the approach bias.
#' Usually this is the interaction of the movement-direction and stimulus-category terms.
#' @export
#' @describeIn Algorithms \code{aat_regression} and \code{aat_standardregression} fit regression models to participants' reaction times and extract a term that serves as AAT score.
#' \code{aat_regression} extracts the raw coefficient, equivalent to a mean difference score.
#' \code{aat_standardregression} extracts the t-score of the coefficient, standardized on the basis of the variability of the participant's reaction times.
#' These algorithms can be used to regress nuisance variables out of the data before computing AAT scores.
#' When using these functions, additional arguments must be provided:
#' \itemize{
#' \item \code{formula} - a formula to fit to the data
#' \item \code{aatterm} - the term within the formula that indicates the approach bias; this is usually the interaction of the pull and target terms.
#' }
aat_regression<-function(ds,subjvar,formula,aatterm,...){
  output<-data.frame(pp=unique(ds[[subjvar]]),ab=NA,var=NA)
  for(i in seq_len(nrow(output))){
    mod<-coef(summary(lm(formula,data=ds[ds[[subjvar]]==output[i,"pp"],])))
    if(aatterm %in% rownames(mod)){
      output[i,"ab"]<- -mod[rownames(mod)==aatterm,1]
      output[i,"var"]<- mod[rownames(mod)==aatterm,2]
    }
  }
  colnames(output)[colnames(output)=="pp"]<-subjvar
  return(output)
}

#' @export
#' @describeIn Algorithms See above
aat_standardregression<-function(ds,subjvar,formula,aatterm,...){
  output<-data.frame(pp=unique(ds[[subjvar]]),ab=NA,var=NA)
  for(i in seq_len(nrow(output))){
    mod<-coef(summary(lm(formula,data=ds[ds[[subjvar]]==output[i,"pp"],])))
    if(aatterm %in% rownames(mod)){
      output[i,"ab"]<- -mod[rownames(mod)==aatterm,1]
      output[i,"var"]<- mod[rownames(mod)==aatterm,2]
    }
  }
  colnames(output)[colnames(output)=="pp"]<-subjvar
  output$ab<-output$ab/output$var
  return(output)
}

#' @export
#' @describeIn Algorithms subtracts the mean approach reaction time from the mean avoidance reaction time.
#' Using this algorithm is only sensible if the supplied data contain a single stimulus category.
aat_singlemeandiff<-function(ds,subjvar,pullvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[pullvar]]),mean.default,na.rm=T)
  b<-apply(a,1,function(x){ x[1]-x[2] })
  setNames(data.frame(id=names(b),ab=b,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_singlemeandiff_old<-function(ds,subjvar,pullvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0)
  idx2<-which(ds[[pullvar]]==1)

  ab<-(tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],mean.default,na.rm=T) -
       tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],mean.default,na.rm=T))
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}

aat_singlemeandiff_older<-function(ds,subjvar,pullvar,rtvar,...){
  group_by(ds,!!sym(subjvar))%>%
    summarise(ab=mean(subset(!!sym(rtvar),!!sym(pullvar)==1)) -
                 mean(subset(!!sym(rtvar),!!sym(pullvar)==0)))
}

#' @export
#' @describeIn Algorithms subtracts the median approach reaction time from the median avoidance reaction time.
#' Using this algorithm is only sensible if the supplied data contain a single stimulus category.
aat_singlemediandiff<-function(ds,subjvar,pullvar,rtvar,...){
  a<-tapply(ds[[rtvar]],list(ds[[subjvar]],ds[[pullvar]]),median.default,na.rm=T)
  b<-apply(a,1,function(x){ x[1]-x[2] })
  setNames(data.frame(id=names(b),ab=b,stringsAsFactors=F),
           c(subjvar,"ab"))
}

aat_singlemediandiff_old<-function(ds,subjvar,pullvar,rtvar,...){
  idx1<-which(ds[[pullvar]]==0)
  idx2<-which(ds[[pullvar]]==1)
  ab<-(tapply(ds[[rtvar]][idx1],ds[[subjvar]][idx1],median.default,na.rm=T) -
       tapply(ds[[rtvar]][idx2],ds[[subjvar]][idx2],median.default,na.rm=T))
  setNames(data.frame(id=names(ab),ab=ab,stringsAsFactors=F),c(subjvar,"ab"))
}

aat_singlemediandiff_older<-function(ds,subjvar,pullvar,rtvar,...){
  group_by(ds,!!sym(subjvar))%>%
    summarise(ab=median(subset(!!sym(rtvar),!!sym(pullvar)==1)) -
                 median(subset(!!sym(rtvar),!!sym(pullvar)==0)))
}
