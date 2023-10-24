
# Outlier removing algorithms ####
#' @title Pre-processing rules
#' @description These are pre-processing rules that can be used in \link{aat_splithalf}, \link{aat_bootstrap}, and \link{aat_compute}.
#'
#' \itemize{
#' \item The following rules are to be used for the \code{trialdropfunc} argument.
#' The way you handle outliers for the reliability computation and bootstrapping more broadly
#' should mimic the way you do it in your regular analyses.
#' It is recommended to exclude outlying trials when computing AAT scores using the mean double-dfference scores and regression scoring approaches,
#' but not when using d-scores or median double-difference scores.
#' \itemize{
#' \item \code{prune_nothing} excludes no trials (default)
#' \item \code{trial_prune_3SD} excludes trials deviating more than 3SD from the mean per participant.
#' \item \code{trial_prune_3MAD} excludes trials deviating more than 3 median absolute deviations from the median per participant.
#' \item \code{trial_prune_grubbs} applies a Grubbs' test to the data, removing one outlier at a time until the test is no longer significant.
#' \item \code{trial_prune_SD_dropcases} removes trials deviating more than a specific number of standard deviations from the participant's mean,
#' and removes participants with an excessive percentage of outliers.
#' Required arguments:
#' \itemize{
#' \item \code{trialsd} - trials deviating more than \code{trialsd} standard deviations from the participant's mean are excluded (optional; default is 3)
#' \item \code{maxoutliers} - participants with a higher percentage of outliers are removed from the data. (optional; default is .15)
#' }
#' \item \code{trial_recode_SD} recodes outlying reaction times to the nearest non-outlying value,
#' with outliers defined as reaction times deviating more than a certain number of standard deviations from the participant's mean. Required argument:
#' \itemize{
#' \item \code{trialsd} - trials deviating more than this many standard deviations from the mean are classified as outliers.
#' }
#' \item \code{trial_prune_percent_subject} and \code{trial_prune_percent_sample} remove trials below and/or above certain percentiles,
#' on a subject-by-subject basis or sample-wide, respectively. The following arguments are available:
#' \itemize{
#' \item \code{lowerpercent} and \code{uppperpercent} (optional; defaults are .01 and .99).
#' }
#' }
#' \item The following pre-procesing rules are to be used for the \code{errortrialfunc} argument.
#' They determine what is to be done with errors - remove or recode?
#'
#' \itemize{
#' \item \code{prune_nothing} removes no errors (default).
#' \item \code{error_replace_blockmeanplus} replaces error trial reaction times with the block mean, plus an arbitrary extra quantity.
#' If used, the following additional arguments are required:
#' \itemize{
#' \item \code{blockvar} - Quoted name of the block variable (mandatory)
#' \item \code{errorvar} - Quoted name of the error variable, where errors are 1 or TRUE and correct trials are 0 or FALSE (mandatory)
#' \item \code{errorbonus} - Amount to add to the reaction time of error trials. Default is 0.6 (recommended by \code{Greenwald, Nosek, & Banaji, 2003})
#' }
#' \item \code{error_prune_dropcases} removes errors and drops participants if they have more errors than a given percentage. The following arguments are available:
#' \itemize{
#' \item \code{errorvar} - Quoted name of the error variable, where errors are 1 or TRUE and correct trials are 0 or FALSE (mandatory)
#' \item \code{maxerrors} - participants with a higher percentage of errors are excluded from the dataset. Default is .15.
#' }
#' }
#' \item These are pre-processing rules to be used for the \code{casedropfunc} argument.
#' The way you handle outliers here should mimic the way you do it in your regular analyses.
#' \itemize{
#' \item \code{prune_nothing} excludes no participants (default)
#' \item \code{case_prune_3SD} excludes participants deviating more than 3SD from the sample mean.
#' }
#' }
#' @param ds A data.frame.
#' @param subjvar The name of the subject variable.
#' @param rtvar The name of the reaction time variable.
#' @param blockvar The name of the block variable.
#' @param errorvar The name of the error variable.
#' @param lowerpercent,upperpercent for \code{trial_prune_percent_subject} and \code{trial_prune_percent_sample},
#' the lower and upper proportions beyond which trials are considered outliers and removed (defaults to .01 and .99).
#' @param trialsd The amount of deviation from the participant mean (in SD) after which a trial is considered an outlier and excluded (defaults to 3).
#' @param maxoutliers for \code{trial_prune_SD_dropcases}, the maximum percentage of outliers, after which a participant is excluded from the data.
#' @param errorbonus for \code{error_replace_blockmeanplus}, the amount of seconds to add to the block mean
#' and use as a replacement for error trial reaction times (default is 0.6).
#' @param maxerrors for \code{error_prune_dropcases}, the maximum percentage of errors, after which a participant is excluded from the data.
#' @param ... Other arguments (ignored).
#' @name Preprocessing
NULL

#' @export
#' @rdname Preprocessing
prune_nothing<-function(ds,...){
  ds
}

#' @export
#' @rdname Preprocessing
trial_prune_percent_subject<-function(ds,subjvar,rtvar,lowerpercent=.01,upperpercent=.99,...){
  ds %>% group_by(!!sym(subjvar),key) %>%
    mutate(percentile=(rank(!!sym(rtvar))-1)/(n()-1)) %>%
    filter(.data$percentile > lowerpercent & .data$percentile< upperpercent) %>% ungroup()
}

trial_prune_percent_subject_alt<-function(ds,subjvar,rtvar,lowerpercent=.01,upperpercent=.99,...){
  ds$percentile <- ave(ds[[rtvar]],ds[[subjvar]],ds[["key"]],FUN=function(x){ (rank(x)-1)/(length(x)-1) })
  ds[ds$percentile > lowerpercent & ds$percentile < upperpercent,]
}

#' @export
#' @rdname Preprocessing
trial_prune_percent_sample<-function(ds,rtvar,lowerpercent=.01,upperpercent=.99,...){
  ds %>% group_by(key) %>% mutate(percentile=(rank(!!sym(rtvar))-1)/(n()-1)) %>%
    filter(.data$percentile > lowerpercent & .data$percentile< upperpercent) %>% ungroup()
}

trial_prune_percent_sample_alt<-function(ds,rtvar,lowerpercent=.01,upperpercent=.99,...){
  ds$percentile <- ave(ds[[rtvar]],ds[["key"]],FUN=function(x){ (rank(x)-1)/(length(x)-1) })
  ds[(ds$percentile > lowerpercent & ds$percentile < upperpercent),]
}

#' @export
#' @rdname Preprocessing
trial_prune_3SD<-function(ds,subjvar,rtvar,...){
  ds %>% group_by(!!sym(subjvar),key) %>% filter(abs(vec.scale(!!sym(rtvar))) <3) %>% ungroup()
}

#' @export
#' @rdname Preprocessing
trial_prune_3MAD<-function(ds,subjvar,rtvar,...){
  ds %>% group_by(!!sym(subjvar),key) %>% filter(abs(vec.madscale(!!sym(rtvar))) <3) %>% ungroup()
}

trial_prune_3SD_alt<-function(ds,subjvar,rtvar,...){
  h<-ave(ds[[rtvar]],ds[[subjvar]],ds[["key"]],FUN=vec.scale)
  ds[which(abs(h)<3),]
}


#' @export
#' @rdname Preprocessing
trial_prune_SD_dropcases<-function(ds,subjvar,rtvar,trialsd=3,maxoutliers=.15,...){
  ds %>% group_by(!!sym(subjvar),key) %>%
    mutate(is.ol=as.numeric(abs(vec.scale(!!sym(rtvar))) >=trialsd),
           avg.ol=mean.default(.data$is.ol)) %>%
    ungroup() %>% filter(.data$is.ol==0 & .data$avg.ol<maxoutliers)
}

#' @export
#' @rdname Preprocessing
trial_recode_SD<-function(ds,subjvar,rtvar,trialsd=3,...){
  dsa<- ds %>% group_by(!!sym(subjvar),key) %>%
    mutate(ol.z.score=vec.scale(!!sym(rtvar)),
           ol.type=(.data$ol.z.score >= trialsd) - (.data$ol.z.score <= -trialsd),
           is.ol=abs(.data$ol.type),
           ol.max.rt=mean.default(!!sym(rtvar))+vec.sd(!!sym(rtvar))*trialsd,
           ol.min.rt=mean.default(!!sym(rtvar))-vec.sd(!!sym(rtvar))*trialsd)
  dsa[which(dsa$is.ol!=0),rtvar]<-ifelse(dsa[which(dsa$is.ol!=0),]$ol.type==1,
                                         dsa[which(dsa$is.ol!=0),]$ol.max.rt,
                                         dsa[which(dsa$is.ol!=0),]$ol.min.rt)
  #dsa %>% dplyr::select(-.data$ol.type,-.data$ol.max.rt,-.data$ol.min.rt,-.data$ol.z.score)
  dsa$ol.type<-dsa$ol.max.rt<-dsa$ol.min.rt<-dsa$ol.z.score<-NULL
  return(dsa)
}

trial_recode_SD_alt<-function(ds,subjvar,rtvar,trialsd=3,...){
  ds$ol.grmean<-ave(ds[[rtvar]],ds[[subjvar]],ds$key,FUN=mean.default)
  ds$ol.grsd<-ave(ds[[rtvar]],ds[[subjvar]],ds$key,FUN=vec.sd)
  ds$ol.z.score<-(ds[[rtvar]]-ds$ol.grmean)/ds$ol.grsd
  ds$is.ol<-abs(ds$ol.z.score)>=trialsd
  ds[[rtvar]]<-(!ds$is.ol)*ds[[rtvar]] + ds$is.ol*(ds$ol.grmean+sign(ds$ol.z.score)*ds$ol.grsd*trialsd)
  ds$ol.grmean<-ds$ol.grsd<-ds$ol.z.score<-ds$ol.type<-NULL
  ds
}

#' @export
#' @rdname Preprocessing
trial_prune_grubbs<-function(ds,subjvar,rtvar,...){
  ds %>% group_by(!!sym(subjvar)) %>% filter(!grubbsFilter(!!sym(rtvar))) %>% ungroup()
}

grubbsFilter<-function(x,alphalevel=.05){
  pval<-0
  is.ol<-rep(F,length(x))
  while(pval<alphalevel & sum(is.ol) < length(x)){
    scaled<-vec.scale(x[!is.ol])
    biggest<-which.max(abs(scaled))
    pval<-pgrubbs(scaled[biggest],sum(!is.ol))
    if(pval<alphalevel)
      is.ol[!is.ol][biggest]<-T
  }
  is.ol
}

# Borrowed from the {outliers} package
pgrubbs<-function(p,n){
  s <- (p^2 * n * (2 - n))/(p^2 * n - (n - 1)^2)
  t <- sqrt(s)
  if (is.nan(t)) {
    res <- 0
  }
  else {
    res <- n * (1 - pt(t, n - 2))
    res[res > 1] <- 1
  }
  return(res)
}

#' @export
#' @rdname Preprocessing
case_prune_3SD<-function(ds,...){
  ds[which(abs(vec.scale(ds$abhalf0))<3 & abs(vec.scale(ds$abhalf1))<3),]
}

#Replace error trial latencies with correct block mean RT + 600
#' @export
#' @rdname Preprocessing
error_replace_blockmeanplus<-function(ds,subjvar,rtvar,blockvar,errorvar,errorbonus, ...){
  if(!("is.ol" %in% colnames(ds))){ ds$is.ol<-0 }
  ds%<>%group_by(!!sym(subjvar),!!sym(blockvar), key)%>%
    mutate(newrt=mean.default((!!sym(rtvar))[!(!!sym(errorvar)) & .data$is.ol==0])+errorbonus)%>%ungroup()
  errids<-which(ds[[errorvar]]==1)
  ds[[rtvar]][errids]<-ds$newrt[errids]
  ds$newrt<-NULL
  ds
}

error_replace_blockmeanplus_alt<-function(ds,subjvar,rtvar,blockvar,errorvar,errorbonus, ...){
  if(!("is.ol" %in% colnames(ds))){ ds$is.ol<-0 }
  ds$.corrmean<-ave(ds$rt+ifelse(!ds[[errorvar]] & !ds$is.ol,0,NA),
                    ds[[rtvar]],ds[[blockvar]],ds[["key"]],
                   FUN=function(x){mean.default(x[!is.na(x)])})
  ds[[rtvar]][ds[[errorvar]]==TRUE]<-ds$.corrmean[ds[[errorvar]]==TRUE]+errorbonus
  ds$.corrmean<-NULL
  ds
}

#' @export
#' @rdname Preprocessing
error_prune_dropcases<-function(ds,subjvar, errorvar, maxerrors = .15, ...){
  ds%>%group_by(!!sym(subjvar), key)%>%
    filter(mean.default(!!sym(errorvar))<maxerrors & !!sym(errorvar) == FALSE)
}

error_prune_dropcases_alt<-function(ds,subjvar, errorvar, maxerrors = .15, ...){
  ds$merr<-ave(ds[[errorvar]],ds[[subjvar]],ds$key,FUN=mean.default)
  ds[which(ds$merr<maxerrors & !ds[[errorvar]]),]
}



