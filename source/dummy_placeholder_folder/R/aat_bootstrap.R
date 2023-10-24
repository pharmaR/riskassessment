#############################
# bootstrapped bias score computation
#' @title Compute bootstrapped approach-bias scores
#' @description Compute bootstrapped approach-bias scores with confidence intervals.
#' @param ds a longformat data.frame
#' @param subjvar Quoted name of the participant identifier column
#' @param pullvar Quoted name of the column indicating pull trials.
#' Pull trials should either be represented by 1, or by the second level of a factor.
#' @param targetvar Name of the column indicating trials featuring the target stimulus.
#' Target stimuli should either be represented by 1, or by the second level of a factor.
#' @param rtvar Name of the reaction time column.
#' @param iters Total number of desired iterations. At least 200 are required to get confidence intervals that make sense.
#' @param algorithm Function (without brackets or quotes) to be used to compute AAT scores. See \link{Algorithms} for a list of usable algorithms.
#' @param trialdropfunc Function (without brackets or quotes) to be used to exclude outlying trials in each half.
#' The way you handle outliers for the reliability computation should mimic the way you do it in your regular analyses.
#' It is recommended to exclude outlying trials when computing AAT scores using the mean double-dfference scores and regression scoring approaches,
#' but not when using d-scores or median double-difference scores.
#' \itemize{
#' \item \code{prune_nothing} excludes no trials (default)
#' \item \code{trial_prune_grubbs} applies a Grubbs' test to the data, removing one outlier at a time until the test is no longer significant.
#' \item \code{trial_prune_3SD} excludes trials deviating more than 3SD from the mean per participant.
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
#' @param plot Plot the bias scores and their confidence intervals after computation is complete. This gives a good overview of the data.
#' @param include.raw logical indicating whether raw split-half data should be included in the output object.
#' @param parallel If TRUE (default), will use parallel computing to compute results faster.
#' If a doParallel backend has not been registered beforehand,
#' this function will register a cluster and stop it after finishing, which takes some extra time.
#' @param ... Other arguments, to be passed on to the algorithm or outlier rejection functions (see arguments above)
#'
#'
#' @return A list, containing bootstrapped bias scores, their variance, bootstrapped 95 percent confidence intervals,
#' the number of iterations, and a matrix of bias scores for each iteration.
#'
#' @author Sercan Kahveci
#' @examples
#' # Compute 10 bootstrapped AAT scores.
#' boot<-aat_bootstrap(ds=erotica[erotica$is_irrelevant==0,], subjvar="subject",
#'                     pullvar="is_pull", targetvar="is_target",rtvar="RT",
#'                     iters=10,algorithm="aat_doublemediandiff",
#'                     trialdropfunc="trial_prune_3SD",
#'                     plot=FALSE, parallel=FALSE)
#' plot(boot)
#' print(boot)
#'
#' @export
aat_bootstrap<-function(ds,subjvar,pullvar,targetvar=NULL,rtvar,iters,
                        algorithm=c("aat_doublemeandiff","aat_doublemediandiff",
                                    "aat_dscore","aat_dscore_multiblock",
                                    "aat_regression","aat_standardregression",
                                    "aat_singlemeandiff","aat_singlemediandiff"),
                        trialdropfunc=c("prune_nothing","trial_prune_3SD","trial_prune_3MAD",
                                        "trial_prune_SD_dropcases","trial_recode_SD",
                                        "trial_prune_percent_subject","trial_prune_percent_sample",
                                        "trial_prune_grubbs"),
                        errortrialfunc=c("prune_nothing","error_replace_blockmeanplus","error_prune_dropcases"),
                        plot=TRUE,include.raw=FALSE,parallel=TRUE,...){
  packs<-c("magrittr","dplyr","AATtools")

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

  #Prepare the cluster
  if(parallel){
    `%dofunc%` <- `%dopar%`
    hasCluster<-getDoParRegistered()
    if(!hasCluster){
      cluster<-makeCluster(getOption("AATtools.workers"))
      registerDoParallel(cluster)
      on.exit(unregisterDoParallel(cluster))
    }
  }else{
    `%dofunc%` <- `%do%`
  }

  #bootstrap loop
  results<-
    foreach(iter = seq_len(iters), .packages=packs, .combine=cbind) %dofunc% {
      #Split data
      # iterds<-ds %>% group_by(!!sym(subjvar), !!sym(pullvar), !!sym(targetvar)) %>%
      #   sample_n(size=n(),replace=TRUE) %>% ungroup()
      iterds<-ds[unlist(lapply(split(x=seq_len(nrow(ds)),f=ds[c(subjvar,pullvar,targetvar)]),
                        FUN=function(x){ x[sample.int(length(x),replace=T)] })),]

      #Handle error removal
      iterds<-do.call(errorremovefunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))
      #Handle outlying trials
      iterds<-do.call(trialdropfunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))
      #Handle error penalization
      iterds<-do.call(errorpenalizefunc,c(args,list(ds=iterds,subjvar=subjvar,rtvar=rtvar)))

      abds<-do.call(algorithm,c(list(ds=iterds,subjvar=subjvar,pullvar=pullvar,
                                     targetvar=targetvar,rtvar=rtvar),args))

      #colnames(abds)<-c(subjvar,paste0("iter", formatC(iter, width = nchar(iters), format = "d", flag = "0")))
      outvar<-abds$ab
      names(outvar)<-abds[[subjvar]]
      outvar
    }

  #results<-results[!is.na(rownames(results)),]
  statset<-data.frame(ppidx=rownames(results),
                      bias=rowMeans(results,na.rm=TRUE),
                      var=apply(results,MARGIN = 1,FUN=var,na.rm=TRUE),
                      lowerci=apply(results,MARGIN=1,FUN=function(x){quantile(x,0.025,na.rm=TRUE)}),
                      upperci=apply(results,MARGIN=1,FUN=function(x){quantile(x,0.975,na.rm=TRUE)}),
                      stringsAsFactors=F)
  statset$ci<-statset$upperci-statset$lowerci

  #q-reliability
  bv<-var(statset$bias,na.rm=TRUE)
  wv<-mean(statset$var,na.rm=TRUE)
  q<-1-wv/bv

  output<-list(bias=statset,
               reliability=q,
               parameters=c(list(ds=ds,
                                 subjvar=subjvar,
                                 pullvar=pullvar,
                                 targetvar=targetvar,
                                 rtvar=rtvar,
                                 iters=iters,
                                 algorithm=algorithm,
                                 trialdropfunc=trialdropfunc,
                                 errortrialfunc=errortrialfunc),args)) %>%
    structure(class = "aat_bootstrap")
  if(include.raw){
    output$iterdata<-results
  }
  if(plot){ plot(output) }
  return(output)
}

#' @export
#' @rdname aat_bootstrap
#' @param x An \code{aat_bootstrap} object.
print.aat_bootstrap<-function(x,...){
  cat("Bootstrapped bias scores and confidence intervals",
      "\nMean bias score: ", mean(x$bias$bias,na.rm=TRUE),
      "\nMean confidence interval: ",mean(x$bias$ci,na.rm=TRUE),
      "\nreliability: q = ",x$reliability,
      "\nNumber of iterations: ",x$parameters$iters,sep="")
}

#' @export
#' @rdname aat_bootstrap
#' @param x An \code{aat_bootstrap} object.
plot.aat_bootstrap <- function(x,...){
  statset<-x$bias
  statset<-statset[!is.na(statset$bias) & !is.na(statset$upperci) & !is.na(statset$lowerci),]
  rank<-rank(statset$bias)
  wideness<-max(statset$upperci) - min(statset$lowerci)
  plot(x=statset$bias,y=rank,xlim=c(min(statset$lowerci)-0.01*wideness,max(statset$upperci)+0.01*wideness),
       xlab="Bias score",main=paste0("Individual bias scores with 95%CI",
                                     "\nEstimated reliability: q = ",x$reliability))
  segments(x0=statset$lowerci,x1=statset$bias-0.005*wideness,y0=rank,y1=rank)
  segments(x0=statset$bias+0.005*wideness,x1=statset$upperci,y0=rank,y1=rank)
  abline(v=0)
  #text(x=statset$bias,y=statset$rownr,labels=statset$ppidx,cex=0.5)
}
