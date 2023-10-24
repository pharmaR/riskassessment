#' Simulate AAT datasets and predict parameters
#'
#' \code{aat_simulate()} generates approach-avoidance task datasets.
#'
#' @param npps Number of participants
#' @param nstims Number of stimuli
#' @param stimreps Number of repetitions of each stimulus within each group
#' (i.e. within approach target, avoid target, approach control, avoid control)
#' @param meanrt Mean sample reaction time
#' @param meanrt_jitter Extent by which participants' mean RTs
#' deviate from mean sample RT.
#' @param sdrt Standard deviation of samplewide RTs,
#' ignoring effects of movement, stimulus, and approach bias.
#' In essence, this represents the amount of pure noise present in the data.
#' @param sdrt_jitter Extent by which standard deviations of individual participants' RTs
#' are larger or smaller than the samplewide SD.
#' @param pullfx size of the effect of approach-versus-avoidance, in milliseconds
#' @param pullfx_jitter Individual variation in the effect of approach-versus-avoidance
#' @param stimfx size of the effect of stimulus category, in milliseconds
#' @param stimfx_jitter Individual variation in the effect of stimulus category
#' @param biasfx Size of the approach bias effect, in milliseconds
#' @param biasfx_jitter Individual variation in the approach bias effect
#' @param empirical If TRUE, then effect sizes and standard deviations will be exact
#' @param ... Ignored.
#'
#' @return \code{aat_simulate()} returns a \code{data.frame} with the following columns:
#' subj (participant ID), stim (stimulus number), rep (stimulus repetition number),
#' is_pull (0 = avoid, 1 = approach), is_target (0 = control stimulus, 1 = target stimulus),
#' meanrt (participant's mean RT), sdrt (participant's residual standard deviation),
#' pullfx (participant approach-avoidance effect size in ms),
#' stimfx (participant stimulus category effect size in ms),
#' biasfx (participant approach bias effect size in ms),
#' and rt (trial reaction time).
#' Additionally, the data.frame has the attribute \code{population_reliability} which represents
#' the expected reliability of the data given the provided parameters.
#' @details Defaults of \code{aat_simulate()} are based on
#' Kahveci, Van Alebeek, Berking, & Blechert (2021).
#' @export
#'
#' @examples
#' ts<- aat_simulate(pullfx = 50, stimfx = 10, biasfx = 100)
#' mod<-lm(rt~is_pull*is_target,data=ts)
#' coef(mod) #these should be somewhat close to the provided coefficients
#'
#' # Here's how one might derive the parameters used in this function from a real dataset
#' \dontrun{
#' mod<-lmer(decisiontime ~ is_pull * is_food + (is_pull * is_food | subjectid),data=dsa)
#' fixef(mod) # from here, all the fx and mean RTs are derived
#' ranef(mod)$subjectid %>% apply(2,sd) #from here, all the fx jitters are derived
#' dsa %>% group_by(subjectid) %>% summarise(sd=sd(resid)) %>%
#' summarise(m=mean(sd),s=sd(sd)) # from here, sdrt_jitter is derived
#' }
aat_simulate<-function(npps=36,nstims=16,stimreps=4,
                       meanrt=632,meanrt_jitter=90.1,
                       sdrt=158,sdrt_jitter=49.9,
                       pullfx=-39.2,pullfx_jitter=40.5,
                       stimfx=-30.9,stimfx_jitter=32.5,
                       biasfx= 39.0,biasfx_jitter=60.1,
                       empirical=FALSE, ...){

  cond.scale<-function(x,emp){ if(emp){vec.scale(x)}else{x} }

  #set properties
  subjprops<-data.frame(subj=1:npps,
                        meanrt=meanrt+meanrt_jitter*cond.scale(rnorm(npps),empirical),
                        sdrt=sdrt+sdrt_jitter*cond.scale(rgamma2(n=npps,shape=3),empirical),
                        pullfx=pullfx+pullfx_jitter*cond.scale(rnorm(npps),empirical),
                        stimfx=stimfx+stimfx_jitter*cond.scale(rnorm(npps),empirical),
                        biasfx=biasfx+biasfx_jitter*cond.scale(rnorm(npps),empirical))

  #initialize dataset
  ds<-expand.grid(subj=1:npps,stim=1:nstims,rep=1:stimreps,is_pull=0:1,is_target=0:1)
  ds<-merge(ds,subjprops,by="subj",all.x=T)

  #fix stimulus names
  ds$stim<-paste0(ds$is_target,"-",ds$stim)

  #Generate RTs
  gshape<-3
  gscale<-1
  ds$rt<-rgamma2(n=nrow(ds),shape=gshape)
  if(empirical){
    ds$rt<-ave(ds$rt,ds[c("subj","is_pull","is_target")],FUN=vec.scale)
  }
  ds$rt<-ds$rt * ds$sdrt + ds$meanrt +
    (ds$is_pull-.5)*ds$pullfx + (ds$is_target-.5) * ds$stimfx +
    ((ds$is_pull==ds$is_target)-.5)*-.5 * ds$biasfx

  #compute true "population" reliability (Kahveci's Q)
  # alt_q <- (biasfx_jitter^2)/(biasfx_jitter^2 + sdrt^2 /(nstims*stimreps) *4)
  # attr(ds,"population_reliability")<-alt_q

  #output
  return(ds)
}

rgamma2<-function(n,shape,m=0,s=1){
  m + (rgamma(n=n,shape=shape,scale=1) - shape*1) *s/(sqrt(shape)*1)
}

aat_simulate_old<-function(npps=40,nstims=32,stimreps=2,
                  meanrt=743,meanrt_jitter=66,
                  sdrt=133,sdrt_jitter=38,
                  pullfx=25,pullfx_jitter=40,
                  stimfx=10,stimfx_jitter=35,
                  biasfx=35,biasfx_jitter=75){

  #set properties
  subjprops<-data.frame(subj=1:npps,
                        meanrt=meanrt+meanrt_jitter*rnorm(npps),
                        sdrt=sdrt+sdrt_jitter*rgamma2(n=npps,shape=3),
                        pullfx=pullfx+pullfx_jitter*rnorm(npps),
                        stimfx=stimfx+stimfx_jitter*rnorm(npps),
                        biasfx=biasfx+biasfx_jitter*rnorm(npps))

  #initialize dataset
  ds<-expand.grid(subj=1:npps,stim=1:nstims,rep=1:stimreps,is_pull=0:1,is_target=0:1)
  ds<-merge(ds,subjprops,by="subj",all.x=T)

  #Generate RTs
  gshape<-3
  gscale<-1
  ds$rt<-(rgamma(n=nrow(ds),shape=gshape,scale=gscale)-gshape*gscale) *
    ds$sdrt/(sqrt(gshape)*gscale) + ds$meanrt +
    (ds$is_pull-.5)*ds$pullfx + (ds$is_target-.5)*ds$stimfx +
    (ds$is_pull*ds$is_target-.25)*ds$biasfx

  #compute true "population" reliability (Kahveci's Q)
  alt_q <- (biasfx_jitter^2)/(biasfx_jitter^2 + sdrt^2 /(nstims*stimreps) *4)
  attr(ds,"population_reliability")<-alt_q

  #output
  return(ds)
}



#'
#' \code{aat_simulate2} offers defaults taken from different studies and allows inserting outliers.
#'
#' @param ... Any parameters of \code{aat_simulate} provided here will override the defaults
#' from the defaults parameter.
#' @param defaults Which set of default values should be used?
#' @param slowols Number of slow outliers to insert per participant
#' @param fastols Number of fats outliers to insert per participant
#' @param olsd Number of standard deviations by which (slow) outliers deviate
#'
#' @details "Lender2018" parameters are taken from the relevant-feature AAT of
#' Lender, Meule, Rinck, Brockmeyer, & Blechert (2018). "Kahveci2021" parameters
#' are taken from Kahveci, Van Alebeek, Berking, & Blechert (in review).
#'
#' Lender, A., Meule, A., Rinck, M., Brockmeyer, T., & Blechert, J. (2018).
#' Measurement of food-related approach–avoidance biases:
#' Larger biases when food stimuli are task relevant. Appetite, 125, 42-47.
#'
#' Kahveci, S., Van Alebeek, H., Berking, M., & Blechert, J. (in review).
#' Touchscreen based assessment of food approach biases: investigation of
#' reliability and stimulus-specific effects.
#' @export
#'
#' @examples
#' hist(aat_simulate2(defaults="Lender2018_relevant_raw",slowols=10,fastols=10)$rt)
#' @rdname aat_simulate
aat_simulate2<-function(..., defaults="none",
                        slowols=0,fastols=0,olsd=3){
  override.args<-list(...)

  if(defaults=="none"){
    args<-override.args
  }else{
    chosenset<-match.arg(defaults,choices=dataprops$setname)
    args<-as.list(dataprops[chosenset==dataprops$setname,])
  }
  args[names(override.args)]<-override.args

  ds<-do.call(aat_simulate,args)

  gshape<-3
  gscale<-1

  #slow OLs
  ds<-ds%>%group_by(.data$subj)%>%mutate(rownum=1:n())%>%
    mutate(rt = ifelse(!(.data$rownum %in% sample(.data$rownum,slowols)),.data$rt,
                       .data$rt+.data$sdrt*olsd))
  #fast OLs
  if(fastols>0){
    ds<-ds%>%group_by(.data$subj)%>%mutate(rownum=1:n(),eligible=.data$rt>.data$sdrt*olsd)%>%
      mutate(rt = ifelse(!(.data$rownum %in% sample(which(.data$eligible),fastols)),
                         .data$rt,.data$rt-.data$sdrt*olsd))
  }
  return(ds)
}

#experimental. Can currently only be used in datasets with approximately equal trials in all cells
aat_properties<-function(ds,subjvar,pullvar,targetvar,rtvar){
  ds<-aat_preparedata(ds=ds,subjvar=subjvar,pullvar=pullvar,targetvar=targetvar,rtvar=rtvar)

  ds%<>%group_by(!!sym(subjvar))%>%
    mutate(pulldiff=mean(subset(!!sym(rtvar),!!sym(pullvar)==1)) - mean(subset(!!sym(rtvar),!!sym(pullvar)==0)),
           targetdiff=mean(subset(!!sym(rtvar),!!sym(targetvar)==1)) - mean(subset(!!sym(rtvar),!!sym(targetvar)==0)),
           doublediff=2*mean(subset(!!sym(rtvar),!!sym(pullvar)!=!!sym(targetvar))) -
                      2*mean(subset(!!sym(rtvar),!!sym(pullvar)==!!sym(targetvar))))

  ds%<>%group_by(!!sym(subjvar)) %>%
    mutate(.residrt=!!sym(rtvar)+
             -(!!sym(pullvar)-mean(!!sym(pullvar)))*.data$pulldiff+
             -(!!sym(targetvar)-mean(!!sym(targetvar)))*.data$targetdiff+
             +.5*((!!sym(pullvar)==!!sym(targetvar))-mean(!!sym(pullvar)==!!sym(targetvar)))*.data$doublediff)

  ppstats<-ds%>%group_by(!!sym(subjvar))%>%
    summarise(.pullfx=first(.data$pulldiff),
              .targetfx=first(.data$targetdiff),
              .biasfx=first(.data$doublediff),
              .meanrt=mean(!!sym(rtvar)),
              .sdrt.full=sd(!!sym(rtvar)),
              .sdrt.resid=sd(.data$.residrt),
              ntrial=n(),
              .groups="drop")

  output<-ppstats %>% ungroup() %>%
    summarise(pullfx=mean(.data$.pullfx),pullfx_jitter=sd(.data$.pullfx),
              stimfx=mean(.data$.targetfx),stimfx_jitter=sd(.data$.targetfx),
              biasfx=mean(.data$.biasfx),biasfx_jitter=sd(.data$.biasfx),
              meanrt=mean(.data$.meanrt),meanrt_jitter=sd(.data$.meanrt),
              sdrt.full=mean(.data$.sdrt.full),sdrt.full_jitter=sd(.data$.sdrt.full),
              sdrt.resid=mean(.data$.sdrt.resid),sdrt.resid_jitter=sd(.data$.sdrt.resid),
              ntrial=mean(.data$ntrial),
              .groups="drop")

  return(list(dataprops=as.list(output),subjectprops=ppstats,ds=ds))
}

#' @rdname aat_simulate
#' @description \code{aat_getstudydata()} retrieves the properties of datasets from a number of pre-existing studies
#' @export
aat_getstudydata<-function(){
  dataprops
}
