setwd("//soliscom.uu.nl/uu/Users/Sahin010/My Documents/Research/Diversity and Inclusion/Frontiers/GU/Data")

#### The packages we need ####

library(haven)
library(data.table)
library(psych)
library(Hmisc)
library(summarytools)
library(sjstats)
library(car)
library(lavaan)
library(GPArotation)


## Load data ####
MyData <- read_sav("Ruwe Data Openheid voor Diversiteit Organisatie Onidentificieerbaar.sav")

#### First, cleaning and creating variables we need for analyses ####
#remove participants that did not finish
MyData <- MyData[!(MyData$Finish_Recode == 0),]
MyData <- MyData[-1,] ## somehow finish_recode was 1, but there was no data of this person
#View(MyData)

# new variabele names to make it easier to understand
setnames(MyData, old = c('Q30','Q31','Q32','Q34','Q34.0','Q35.0','Q41','Q42','Q33'), 
         new = c('sex','age','junior','tenure','senior','education' ,'vis_diss','inv_diss','hoursworkweek'))

# so that 1 is yes, and 0 is no on dissimilarity
MyData$inv_diss <- car::recode(MyData$inv_diss,"2=0;1=1")
MyData$vis_diss <- car::recode(MyData$vis_diss,"2=0;1=1")

MyData$education <- car::recode(MyData$education,"1=1;2=2;3=3;4=4;5=5;6=6;7=NA")
levels(MyData$education) <- list("elementary"=1,"highschool"=2,"lower vocational"=3,"middle vocational"=4,
                                "applied university"=5,"college"=6)
MyData$senior <-  car::recode(MyData$senior,"2=0;1=1")
MyData$junior <-  car::recode(MyData$junior,"2=0;1=1")
levels(MyData$senior) <- list("yes"=1,"no"=0)
levels(MyData$junior) <- list("yes"=1,"no"=0)
levels(MyData$sex) <- list("male"=1,"female"=0, "no answer" =3)
## score on climate for inclusion, using the diversity screener

MyData$DivScr <- rowMeans(MyData[c('Q47_1','Q47_2','Q47_3','Q47_4','Q47_5','Q47_6','Q49_1',
                                 'Q49_2','Q49_3','Q49_4','Q49_5','Q49_6')], na.rm = T)

## calculate alpha

alphaDivScr <- with(MyData,data.frame(Q47_1 ,Q47_2 ,Q47_3 ,Q47_3 ,Q47_4,Q47_5, Q47_6,Q49_1,Q49_2,Q49_3,
                               Q49_4,Q49_5,Q49_6))  

psych::alpha(alphaDivScr)


## authenticity and belonging, subcscales of inclusion
MyData$authenticity <- rowMeans(MyData[c('Q51_8','Q51_9','Q51_10','Q51_11','Q51_1','Q51_5','Q51_6',
                                   'Q51_7')], na.rm = T)

MyData$belonging <- rowMeans(MyData[c('Q52_1','Q52_5','Q52_6','Q52_7','Q52_8','Q52_9','Q52_10',
                                         'Q52_11')], na.rm = T)
#Inclusion variable
MyData$InclJan <- rowMeans(MyData[c('Q51_8','Q51_9','Q51_10','Q51_11','Q51_1','Q51_5','Q51_6','Q51_7',
                                    'Q52_1','Q52_5','Q52_6','Q52_7','Q52_8','Q52_9','Q52_10','Q52_11')], na.rm = T)

alphaIncl <- with(MyData,data.frame(Q51_8,Q51_9,Q51_10,Q51_11,Q51_1,Q51_5,Q51_6,Q51_7,
                                    Q52_1,Q52_5,Q52_6,Q52_7,Q52_8,Q52_9,Q52_10,Q52_11))  
psych::alpha(alphaIncl)


## Exploratory factor analysis
parallel.incl <- fa.parallel(alphaIncl, fm = 'pa', fa = 'fa')

EFA.incl <- fa(r=alphaIncl, nfactors=2, rotate="oblimin")
print.psych(EFA.incl, cut = 0.40)

#career commitment
MyData$commit <- rowMeans(MyData[c('Q52_1.0','Q53_1','Q54_1.0','Q55_1','Q56_1','Q57_1')], na.rm = T)

alphacommit <- with(MyData,data.frame(Q52_1.0,Q53_1,Q54_1.0,Q55_1,Q56_1,Q57_1))  
psych::alpha(alphacommit)
# motivation to grow
# fix wrong scoring
MyData$Q41_1<-car::recode(MyData$Q41_1,"1=1;15=2;16=3;17=4;18=5;19=6")
MyData$Q41_7<-car::recode(MyData$Q41_7,"1=1;15=2;16=3;17=4;18=5;19=6")
MyData$Q41_8<-car::recode(MyData$Q41_8,"1=1;15=2;16=3;17=4;18=5;19=6")
MyData$Q41_9<-car::recode(MyData$Q41_9,"1=1;15=2;16=3;17=4;18=5;19=6")
MyData$Q41_10<-car::recode(MyData$Q41_10,"1=1;15=2;16=3;17=4;18=5;19=6")

MyData$motivat <- rowMeans(MyData[c('Q41_1','Q41_7','Q41_8','Q41_9','Q41_10')], na.rm = T)

alphamotivat <- with(MyData,data.frame(Q41_1,Q41_7,Q41_8,Q41_9,Q41_10))  
psych::alpha(alphamotivat)
# job satisfaction
MyData$job_sat <- rowMeans(MyData[c('Q51_1.0','Q51_2','Q51_3')], na.rm = T)

alphajobsat <- with(MyData,data.frame(Q51_1.0,Q51_2,Q51_3))  
psych::alpha(alphajobsat)
# stress
#rescore so higher score is more stress
MyData$Q52_1.1 <- car::recode(MyData$Q52_1.1,"1=6;2=5;3=4;4=3;5=2;6=1")
MyData$Q52_2 <- car::recode(MyData$Q52_2,"1=6;2=5;3=4;4=3;5=2;6=1")
MyData$Q52_3 <- car::recode(MyData$Q52_3,"1=6;2=5;3=4;4=3;5=2;6=1")
MyData$stress <- rowMeans(MyData[c('Q52_1.1','Q52_2','Q52_3','Q52_4','Q52_5.0','Q52_6.0')], na.rm = T)

alphastress <- with(MyData,data.frame(Q52_1.1,Q52_2,Q52_3,Q52_4,Q52_5.0,Q52_6.0))  
psych::alpha(alphastress)
#turnover intentions
MyData$Q60_1<-car::recode(MyData$Q60_1,"2=0")
MyData$Q60_2<-car::recode(MyData$Q60_2,"2=0")
MyData$Q60_3<-car::recode(MyData$Q60_3,"2=0")
MyData$Q60_4<-car::recode(MyData$Q60_4,"2=0")
MyData$turnover <- rowMeans(MyData[c('Q60_1','Q60_2','Q60_3','Q60_4')], na.rm = T)

alphaturnover <- with(MyData,data.frame(Q60_1,Q60_2,Q60_3,Q60_4))  
alpha(alphaturnover)
# new variable about dissimilarity
MyData$dissimilarity[(MyData$inv_diss == 0) & (MyData$vis_diss == 0)] <- 1
MyData$dissimilarity[(MyData$inv_diss == 0) & (MyData$vis_diss == 1)] <- 2
MyData$dissimilarity[(MyData$inv_diss == 1) & (MyData$vis_diss == 0)] <- 3
MyData$dissimilarity[(MyData$inv_diss == 1) & (MyData$vis_diss == 1)] <- 4

MyData$dissimilarity <- factor(MyData$dissimilarity,
                    levels = c(1,2,3,4),
                    labels = c("similar", "only visibly diss", "only invisibly diss", 
                               "both visibly and invisibly diss"))


## compute correlations ####
correlations <- subset(MyData, select=c(vis_diss,inv_diss,DivScr,InclJan,job_sat,stress,turnover,commit,motivat))
rcorr(as.matrix(correlations), type = "pearson")

## show descriptives of variables ####
freq(MyData$sex)
freq(MyData$junior)
freq(MyData$senior)
freq(MyData$education)

mean(MyData$age, na.rm = T)
sd(MyData$age, na.rm = T)

## by visible dissimilarity
visible_dissimilar <- subset(MyData, select=c(vis_diss,DivScr,InclJan,job_sat,stress,turnover,commit,motivat))
describeBy(visible_dissimilar,visible_dissimilar$vis_diss)

## by invisible dissimilarity
invisible_dissimilar <- subset(MyData, select=c(inv_diss,DivScr,InclJan,job_sat,stress,turnover,commit,motivat))
describeBy(invisible_dissimilar,invisible_dissimilar$inv_diss)

## by all possible combinations of dissimilarity
dissimilar <- subset(MyData, select=c(dissimilarity,DivScr,InclJan,job_sat,stress,turnover,commit,motivat))
describeBy(dissimilar,dissimilar$dissimilarity)

## grand means
describe(correlations)

## inv_diss and vis_diss are factors and thus coded as 1 and 2 sometimes. That's why in the 
# descriptives it has a mean above 1 even though I coded it as 1 and 0


#### EFA analyses in response to editor
# first for those who feel similar
similardata <- MyData[which(MyData$inv_diss== 0 & MyData$vis_diss == 0),]

inclusion_climate <- with(similardata,data.frame(Q47_1 ,Q47_2 ,Q47_3 ,Q47_3 ,Q47_4,Q47_5, Q47_6,Q49_1,Q49_2,Q49_3,
                                      Q49_4,Q49_5,Q49_6,Q51_8,Q51_9,Q51_10,Q51_11,Q51_1,Q51_5,Q51_6,Q51_7,
                                      Q52_1,Q52_5,Q52_6,Q52_7,Q52_8,Q52_9,Q52_10,Q52_11))  
parallel.incl <- fa.parallel(inclusion_climate, fm = 'pa', fa = 'fa', n.iter = 20)

EFA.incl_climate <- fa(r=inclusion_climate, nfactors=4, rotate="oblimin")
print.psych(EFA.incl_climate, cut = 0.30)

## now for those who feel dissimilar in any way
dissimilardata <- MyData[which(MyData$inv_diss== 1 | MyData$vis_diss == 1),]

inclusion_climate_diss <- with(dissimilardata,data.frame(Q47_1 ,Q47_2 ,Q47_3 ,Q47_3 ,Q47_4,Q47_5, Q47_6,Q49_1,Q49_2,Q49_3,
                                                 Q49_4,Q49_5,Q49_6,Q51_8,Q51_9,Q51_10,Q51_11,Q51_1,Q51_5,Q51_6,Q51_7,
                                                 Q52_1,Q52_5,Q52_6,Q52_7,Q52_8,Q52_9,Q52_10,Q52_11))  
parallel.incl_diss <- fa.parallel(inclusion_climate_diss, fm = 'pa', fa = 'fa')

EFA.incl_climate_diss <- fa(r=inclusion_climate_diss, nfactors=5, rotate="oblimin")
print.psych(EFA.incl_climate_diss, cut = 0.30)

## climate for inclusion and felt inclusion are seperate factors in both samples

## Now to show that all measures are different from each other:
alphaIncl <- with(MyData,data.frame(Q51_8,Q51_9,Q51_10,Q51_11,Q51_1,Q51_5,Q51_6,Q51_7,
                                    Q52_1,Q52_5,Q52_6,Q52_7,Q52_8,Q52_9,Q52_10,Q52_11))  
alphacommit <- with(MyData,data.frame(Q52_1.0,Q53_1,Q54_1.0,Q55_1,Q56_1,Q57_1))  
alphamotivat <- with(MyData,data.frame(Q41_1,Q41_7,Q41_8,Q41_9,Q41_10))  
alphajobsat <- with(MyData,data.frame(Q51_1.0,Q51_2,Q51_3))  
alphastress <- with(MyData,data.frame(Q52_1.1,Q52_2,Q52_3,Q52_4,Q52_5.0,Q52_6.0))  
alphaturnover <- with(MyData,data.frame(Q60_1,Q60_2,Q60_3,Q60_4))  

turnovercommitmentmotivation <- with(MyData,data.frame(Q52_1.0,Q53_1,Q54_1.0,Q55_1,Q56_1,Q57_1,
                                                       Q41_1,Q41_7,Q41_8,Q41_9,Q41_10,
                                                       Q60_1,Q60_2,Q60_3,Q60_4))  

parallel.tcm <- fa.parallel(turnovercommitmentmotivation, fm = 'pa', fa = 'fa')

EFA.tcm <- fa(r=turnovercommitmentmotivation, nfactors=3, rotate="oblimin")
print.psych(EFA.tcm, cut = 0.30)

## mostly seperate, however slight overlap between ... and ...

EFA.all <- with(MyData,data.frame(Q51_8,Q51_9,Q51_10,Q51_11,Q51_1,Q51_5,Q51_6,Q51_7,
                                  Q52_1,Q52_5,Q52_6,Q52_7,Q52_8,Q52_9,Q52_10,Q52_11,
                                  Q52_1.0,Q53_1,Q54_1.0,Q55_1,Q56_1,Q57_1,
                                  Q41_1,Q41_7,Q41_8,Q41_9,Q41_10,
                                  Q51_1.0,Q51_2,Q51_3,
                                  Q52_1.1,Q52_2,Q52_3,Q52_4,Q52_5.0,Q52_6.0,
                                  Q60_1,Q60_2,Q60_3,Q60_4,
                                  vis_diss, inv_diss))  

parallel.tcm <- fa.parallel(EFA.all, fm = 'pa', fa = 'fa')
EFA.tcm <- fa(EFA.all, nfactors=9, rotate="oblimin")
print.psych(EFA.tcm, cut = 0.30)


### Hypothesis 1 ####
options(contrasts = c("contr.helmert", "contr.poly"))

## ANOVA testing whether invisible and visible dissimilarity differ on inclusion
fit <- aov(InclJan~as.factor(inv_diss)*as.factor(vis_diss),data=MyData)
anova_stats(Anova(fit, type =3))
summary.lm(fit)
confint(fit)

### Hypothesis 2 ####

### The MANOVA, testing differences between inv_diss and vis_diss on work outcomes
manova.type3 <- manova(cbind(job_sat,stress,turnover,commit,motivat) ~ as.factor(inv_diss)*as.factor(vis_diss), 
             data=MyData)

manova.fit <- Manova(manova.type3, type="III")
summary(manova.fit, multivariate=TRUE)

complete.job <- MyData[complete.cases(MyData$job_sat),]
univariate_job <- aov(job_sat ~ as.factor(inv_diss)*as.factor(vis_diss), 
                 data=complete.job)
anova_stats(Anova(univariate_job, type =3))

complete.stress <- MyData[complete.cases(MyData$stress),]
univariate_stress <- aov(stress ~ as.factor(inv_diss)*as.factor(vis_diss), 
                      data=complete.stress)
anova_stats(Anova(univariate_stress, type =3))

complete.turnover <- MyData[complete.cases(MyData$turnover),]
univariate_turnover<- aov(turnover ~ as.factor(inv_diss)*as.factor(vis_diss), 
                         data=complete.turnover)
anova_stats(Anova(univariate_turnover, type =3))

complete.commit <- MyData[complete.cases(MyData$commit),]
univariate_commit <- aov(commit ~ as.factor(inv_diss)*as.factor(vis_diss), 
                         data=complete.commit)
anova_stats(Anova(univariate_commit, type =3))

complete.motivat <- MyData[complete.cases(MyData$motivat),]
univariate_motivat <- aov(motivat ~ as.factor(inv_diss)*as.factor(vis_diss), 
                         data=complete.motivat)
anova_stats(Anova(univariate_motivat, type =3))



#### Hypotheses 3 and 4 ####
#### Moderation, Mediation and finally the moderated mediation analyses using lavaan 

## numeric needed for analyses
MyData$inv_diss <- as.numeric(MyData$inv_diss)

MyData$vis_diss <- as.numeric(MyData$vis_diss)
# center variables


center.var <- function(x){
  centered <- as.vector(scale(x = x, center = TRUE,
                              scale = FALSE))
  return(centered)
}

# apply function to many variables (_c for centered)
MyData[,c("inv_diss_c", "vis_diss_c", "DivScr_c", "InclJan_c", "commit_c", "motivat_c", "job_sat_c", "stress_c", "turnover_c")] <- 
  lapply(X = MyData[,c("inv_diss", "vis_diss", "DivScr", "InclJan", "commit", "motivat", "job_sat", "stress", "turnover")], FUN = function(x) center.var(x))

# means of the vars (centered variables should be 0, originals should have original means)
sapply(MyData[,c("inv_diss_c", "vis_diss_c", "DivScr_c", "InclJan_c", "commit_c", "motivat_c", "job_sat_c", "stress_c", "turnover_c",
                 "inv_diss", "vis_diss", "DivScr", "InclJan", "commit", "motivat", "job_sat", "stress", "turnover")],
       FUN = function(x){
         round(mean(x, na.rm = T), 2)
       }, simplify = TRUE,
       USE.NAMES = TRUE)

# SD should be same in original and centered variables
sapply(MyData[,c("inv_diss_c", "vis_diss_c", "DivScr_c", "InclJan_c", "commit_c", "motivat_c", "job_sat_c", "stress_c", "turnover_c",
                 "inv_diss", "vis_diss", "DivScr", "InclJan", "commit", "motivat", "job_sat", "stress", "turnover")],
       FUN = function(x){
         round(sd(x, na.rm = T), 2)
       }, simplify = TRUE,
       USE.NAMES = TRUE)

# create interaction variable 
MyData$inv_dissDivScr <- with(MyData, inv_diss*DivScr_c)
MyData$vis_dissDivScr <- with(MyData, vis_diss*DivScr_c)
MyData$inv_dissvis_diss <- with(MyData, inv_diss*vis_diss)
MyData$inv_dissvis_dissDivScr <- with(MyData, inv_diss*vis_diss*DivScr_c)
# mean and variation
mean_of_DivScr <- mean(MyData$DivScr_c, na.rm = T)
var_of_DivScr <- var(MyData$DivScr_c, na.rm = T)

#####################################         moderation
moderationModel <- "
InclJan ~ b1*inv_diss
InclJan ~ b2*vis_diss
InclJan ~ b3*DivScr_c
InclJan ~ b4*inv_dissDivScr
InclJan ~ b5*vis_dissDivScr
InclJan ~ b6*inv_dissvis_diss
InclJan ~ b7*inv_dissvis_dissDivScr

DivScr_c ~ mean_of_DivScr*1
DivScr_c ~~ var_of_DivScr*DivScr_c

InclJan ~ 1
inv_diss ~ 1
vis_diss ~ 1

SD.below := b1 + b4*(mean_of_DivScr - sqrt(var_of_DivScr))
mean := b1 + b4*(mean_of_DivScr)
SD.above := b1 + b4*(mean_of_DivScr + sqrt(var_of_DivScr))"

# fit the model (this takes some time)
sem1 <- sem(model = moderationModel,
            data = MyData,
            se = "bootstrap",
            bootstrap = 5000)

# fit measures
summary(sem1,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

parameterEstimates(sem1,
                   boot.ci.type = "bca.simple",
                   level = .95,
                   ci = TRUE,
                   standardized = FALSE)

##################################################    mediation
# parameters
MediationModel <- '
job_sat ~ c1*inv_diss_c
job_sat ~ c2*vis_diss_c
commit ~ c3*inv_diss_c
commit ~ c4*vis_diss_c
motivat ~ c5*inv_diss_c
motivat ~ c6*vis_diss_c
stress ~ c7*inv_diss_c
stress ~ c8*vis_diss_c
turnover~ c9*inv_diss_c
turnover~ c10*vis_diss_c
directjob_inv := c1
directjob_vis := c2
directcommit_inv := c3
directcommit_vis := c4
directmotivat_inv := c5
directmotivat_vis := c6
directstress_inv := c7
directstress_vis := c8
directturnover_inv := c9
directturnover_vis := c10

# regressions
InclJan ~ a1*inv_diss_c
InclJan ~ a2*vis_diss_c

job_sat ~ b1*InclJan
commit ~ b2*InclJan
motivat ~ b3*InclJan
stress ~ b4*InclJan
turnover~ b5*InclJan

# indirect effect (a*b)
indirectinv_job := a1*b1
indirectvis_job := a2*b1
indirectinv_commit := a1*b2
indirectvis_commit := a2*b2
indirectinv_motiv := a1*b3
indirectvis_motiv := a2*b3
indirectinv_stress := a1*b4
indirectvis_stress := a2*b4
indirectinv_turnover := a1*b5
indirectvis_turnover := a2*b5

# total effect
totalinv_job := c1 + (a1*b1)
totalvis_job := c2 + (a2*b1)
totalinv_commit := c3 + (a1*b2)
totalvis_commit := c4 + (a2*b2)
totalinv_motiv := c5 + (a1*b3)
totalvis_motiv := c6 + (a2*b3)
totalinv_stress := c7+ (a1*b4)
totalinv_stress := c8 + (a2*b4)
totalinv_turnover := c9+ (a1*b5)
totalinv_turnover := c10 + (a2*b5)'


# fit model
sem2 <- sem(model = MediationModel,
            data = MyData,
            se = "bootstrap",
            bootstrap = 5000)
# fit measures
summary(sem2,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

parameterEstimates(sem2,
                   boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE,
                   standardized = F)


################################################    moderated mediation

#c in the model are direct effects
modelMedMod <- "
InclJan ~ a1*inv_diss
InclJan ~ a2*vis_diss

job_sat ~ b1*InclJan
commit ~ b2*InclJan
motivat ~ b3*InclJan
stress ~ b4*InclJan
turnover~ b5*InclJan

InclJan ~ a3*DivScr_c
InclJan ~ a4*inv_dissDivScr
InclJan ~ a5*vis_dissDivScr
InclJan ~ a6*inv_dissvis_diss
InclJan ~ a7*inv_dissvis_dissDivScr

job_sat ~ c1*inv_diss
job_sat ~ c6*vis_diss
commit ~ c2*inv_diss
commit ~ c7*vis_diss
motivat ~ c3*inv_diss
motivat ~ c8*vis_diss
stress ~ c4*inv_diss
stress ~ c9*vis_diss
turnover~ c5*inv_diss
turnover~ c10*vis_diss

DivScr_c ~ mean_of_DivScr*1
DivScr_c ~~ var_of_DivScr*DivScr_c

SD.below_invis_incl := a1 + a4*(mean_of_DivScr - sqrt(var_of_DivScr))
mean_invis_incl := a1 + a4*(mean_of_DivScr)
SD.above_invis_incl := a1 + a4*(mean_of_DivScr + sqrt(var_of_DivScr))

invisible_job_satindirect.SDbelow := a1*b1 + a4*-sqrt(var_of_DivScr)*b1
invisible_job_satindirect.mean := a1*b2 + a4*(mean_of_DivScr)*b1
invisible_job_satindirect.SDabove := a1*b1 + a4*sqrt(var_of_DivScr)*b1

invisible_commitindirect.SDbelow := a1*b2 + a4*-sqrt(var_of_DivScr)*b2
invisible_commitindirect.mean := a1*b2 + a4*(mean_of_DivScr)*b2
invisible_commitindirect.SDabove := a1*b2 + a4*sqrt(var_of_DivScr)*b2

invisible_motivatindirect.SDbelow := a1*b3 + a4*-sqrt(var_of_DivScr)*b3
invisible_motivatindirect.mean := a1*b3 + a4*(mean_of_DivScr)*b3
invisible_motivatindirect.SDabove := a1*b3 + a4*sqrt(var_of_DivScr)*b3

invisible_stressindirect.SDbelow := a1*b4 + a4*-sqrt(var_of_DivScr)*b4
invisible_stressindirect.mean := a1*b4 + a4*(mean_of_DivScr)*b4
invisible_stressindirect.SDabove := a1*b4 + a4*sqrt(var_of_DivScr)*b4

invisible_turnoverindirect.SDbelow := a1*b5 + a4*-sqrt(var_of_DivScr)*b5
invisible_turnoverindirect.mean := a1*b5 + a4*(mean_of_DivScr)*b5
invisible_turnoverindirect.SDabove := a1*b5 + a4*sqrt(var_of_DivScr)*b5

SD.below_vis_incl := a2 + a5*(mean_of_DivScr - sqrt(var_of_DivScr))
mean_vis_incl := a2 + a5*(mean_of_DivScr)
SD.above_vis_incl := a2 + a5*(mean_of_DivScr + sqrt(var_of_DivScr))

visible_job_satindirect.SDbelow := a2*b1 + a5*-sqrt(var_of_DivScr)*b1
visible_job_satindirect.mean := a2*b2 + a5*(mean_of_DivScr)*b1
visible_job_satindirect.SDabove := a2*b1 + a5*sqrt(var_of_DivScr)*b1

visible_commitindirect.SDbelow := a2*b2 + a5*-sqrt(var_of_DivScr)*b2
visible_commitindirect.mean := a2*b2 + a5*(mean_of_DivScr)*b2
visible_commitindirect.SDabove := a2*b2 + a5*sqrt(var_of_DivScr)*b2

visible_motivatindirect.SDbelow := a2*b3 + a5*-sqrt(var_of_DivScr)*b3
visible_motivatindirect.mean := a2*b3 + a5*(mean_of_DivScr)*b3
visible_motivatindirect.SDabove := a2*b3 + a5*sqrt(var_of_DivScr)*b3

visible_stressindirect.SDbelow := a2*b4 + a5*-sqrt(var_of_DivScr)*b4
visible_stressindirect.mean := a2*b4 + a5*(mean_of_DivScr)*b4
visible_stressindirect.SDabove := a2*b4 + a5*sqrt(var_of_DivScr)*b4

visible_turnoverindirect.SDbelow := a2*b5 + a5*-sqrt(var_of_DivScr)*b5
visible_turnoverindirect.mean := a2*b5 + a5*(mean_of_DivScr)*b5
visible_turnoverindirect.SDabove := a2*b5 + a5*sqrt(var_of_DivScr)*b5"

# here are the results, ta-da!
fitMedMod <- sem(data=MyData, model=modelMedMod, se="bootstrap", bootstrap = 5000)
summary(fitMedMod, fit.measures=T, standardize=T, rsquare=T)
parameterEstimates(fitMedMod, boot.ci.type = "bca.simple", level = .95, ci=T,standardized = F)
