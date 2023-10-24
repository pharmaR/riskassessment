
#' @title Compute simple AAT scores
#' @description Compute simple AAT scores, with optional outlier exclusion and error trial recoding.
#' @param ds a long-format data.frame
#' @param subjvar column name of subject variable
#' @param pullvar column name of pull/push indicator variable, must be numeric or logical (where pull is 1 or TRUE)
#' @param targetvar column name of target stimulus indicator, must be numeric or logical (where target is 1 or TRUE)
#' @param rtvar column name of reaction time variable
#' @param algorithm Function (without brackets or quotes) to be used to compute AAT scores. See \link{Algorithms} for a list of usable algorithms.
#' @param trialdropfunc Function (without brackets or quotes) to be used to exclude outlying trials in each half.
#' The way you handle outliers for the reliability computation should mimic the way you do it in your regular analyses.
#' It is recommended to exclude outlying trials when computing AAT scores using the mean double-dfference scores and regression scoring approaches,
#' but not when using d-scores or median double-difference scores.
#' \itemize{
#' \item \code{prune_nothing} excludes no trials (default)
#' \item \code{trial_prune_3SD} excludes trials deviating more than 3SD from the mean per participant.
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
#' @param errortrialfunc Function (without brackets or quotes) to apply to an error trial.
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
#' @param ... Other arguments, to be passed on to the algorithm or outlier rejection functions (see arguments above)
#'
#' @export
#'
#' @examples
#' #Compute the correlation between relevant-feature and irrelevant-feature AAT scores
#' ds<-erotica[erotica$correct==1,]
#' relevant <- aat_compute(ds=ds[ds$is_irrelevant==0,],
#'                         pullvar="is_pull",targetvar="is_target",
#'                         rtvar="RT",subjvar="subject",
#'                         trialdropfunc="trial_prune_3SD",
#'                         algorithm="aat_doublemediandiff")
#'
#' irrelevant <- aat_compute(ds=ds[ds$is_irrelevant==1,],
#'                         pullvar="is_pull",targetvar="is_target",
#'                         rtvar="RT",subjvar="subject",
#'                         trialdropfunc="trial_prune_3SD",
#'                         algorithm="aat_doublemediandiff")
#'
#' comparison.df <- merge(relevant, irrelevant, by = "subject")
#' cor(comparison.df$ab.x, comparison.df$ab.y)
#' # 0.1145726
aat_compute<-function(ds,subjvar,pullvar,targetvar=NULL,rtvar,
                      algorithm=c("aat_doublemeandiff","aat_doublemediandiff",
                                  "aat_dscore","aat_dscore_multiblock",
                                  "aat_regression","aat_standardregression",
                                  "aat_doublemeanquotient","aat_doublemedianquotient",
                                  "aat_singlemeandiff","aat_singlemediandiff"),
                      trialdropfunc=c("prune_nothing","trial_prune_3SD","trial_prune_3MAD",
                                      "trial_prune_SD_dropcases","trial_recode_SD",
                                      "trial_prune_percent_subject","trial_prune_percent_sample",
                                      "trial_prune_grubbs"),
                      errortrialfunc=c("prune_nothing","error_replace_blockmeanplus","error_prune_dropcases"),
                      ...){
  #Handle arguments
  args<-list(...)
  algorithm<-ifelse(is.function(algorithm),deparse(substitute(algorithm)),match.arg(algorithm))
  if(!(algorithm %in% c("aat_singlemeandiff","aat_singlemediandiff","aat_regression","aat_standardregression")) & is.null(targetvar)){
    stop("Argument targetvar missing but required for algorithm!")
  }
  trialdropfunc<-ifelse(is.function(trialdropfunc),deparse(substitute(trialdropfunc)),match.arg(trialdropfunc))
  errortrialfunc<-ifelse(is.function(errortrialfunc),deparse(substitute(errortrialfunc)),match.arg(errortrialfunc))
  errorpenalizefunc<-ifelse(errortrialfunc=="error_replace_blockmeanplus",errortrialfunc,"prune_nothing")
  errorremovefunc<-ifelse(errortrialfunc=="error_replace_blockmeanplus","prune_nothing",errortrialfunc)
  if(errortrialfunc=="error_replace_blockmeanplus"){
    stopifnot(!is.null(args$blockvar),!is.null(args$errorvar))
    if(is.null(args$errorbonus)){ args$errorbonus<- 0.6 }
    if(is.null(args$blockvar)){ args$blockvar<- 0 }
    if(is.null(args$errorvar)){ args$errorvar<- 0 }
  }
  stopifnot(!(algorithm=="aat_dscore_multiblock" & is.null(args$blockvar)))
  if(algorithm %in% c("aat_regression","aat_standardregression")){
    if(!("formula" %in% names(args))){
      args$formula<-as.formula(paste0(rtvar,"~",pullvar,"*",targetvar))
      warning("No formula provided. Defaulting to formula ",form2char(args$formula))
    }else if(is.character(args$formula)){
      args$formula<-as.formula(args$formula)
    }
    if(!("aatterm" %in% names(args))){
      args$aatterm<-paste0(pullvar,":",targetvar)
      warning("No AAT-term provided. Defaulting to AAT-term ",args$aatterm)
    }
  }
  ds<-do.call(aat_preparedata,c(list(ds=ds,subjvar=subjvar,pullvar=pullvar,targetvar=targetvar,rtvar=rtvar),args)) %>% mutate(key=1)

  #Handle error removal
  ds<-do.call(errorremovefunc,c(args,list(ds=ds,subjvar=subjvar,rtvar=rtvar)))
  #Handle outlying trials
  ds<-do.call(trialdropfunc,c(args,list(ds=ds,subjvar=subjvar,rtvar=rtvar)))
  #Handle error penalization
  ds<-do.call(errorpenalizefunc,c(args,list(ds=ds,subjvar=subjvar,rtvar=rtvar)))

  abds<-do.call(algorithm,c(list(ds=ds,subjvar=subjvar,pullvar=pullvar,
                                 targetvar=targetvar,rtvar=rtvar),args))

  abds <- merge(x=abds,by=subjvar,all=TRUE,y=ds %>% group_by(!!sym(subjvar)) %>% summarise(trials=n()))
  return(abds)
}
