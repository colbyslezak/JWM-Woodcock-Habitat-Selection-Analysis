# install.packages("dplyr")
# install.packages("lme4")
# install.packages("optimx")
# install.packages("brglm2")
# install.packages("detectseparation")
# install.packages("interactions")
# install.packages("sjPlot")
# install.packages("sjmisc")
# install.packages("RVAideMemoire")
# install.packages("survMisc")
# install.packages("DHARMa")
# install.packages("devtools")
# install.packages("remotes")
# install.packages("ggbreak") 
# install.packages("ggsignif")
# install.packages("glmmTMB")
# devtools::install_github("kevinsblake/NatParksPalettes")
# remotes::install_github("BastilleRousseau/IndRSA")
# install.packages("rphylopic")

library(remotes)
library(devtools)
library(LogisticDx)
library(RVAideMemoire)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
library(dplyr)
library(Amelia) # for investigating missing data in dataframe
library(Hmisc) # for correlation matrices
library(lme4) # for logistic regressions
library(AICcmodavg) # for multimodel inference
library(ggplot2) # for visualizing predictive outputs
library(cowplot) # for combining individual plots in grids
library(ggpubr)
library(broom)
library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(visreg)      # for potting logodds and probability 
library(margins)     # to calculate Average Marginal Effects
library(rcompanion)  # to calculate pseudo R2
library(ROCR) 
library(optimx)
library(brglm2)
library(detectseparation)
library(interactions)
library(MuMIn)
library(survMisc)
library(DHARMa)
library(IndRSA)
library(lubridate)
library(NatParksPalettes) #national park ggplot colors
library(ggbreak) #to condense portion of ggplot axis
library(ggsignif) #ro add significance level to ggplot
library(glmmTMB) #alternative package for making mixed effects models, it runs faster but can't be used with k-fold cross package
library(rphylopic)

# read in csv with use and available points that have habitat class and site as a number instead of words per Martina's suggestion, 2010_2017malesusedandavailwhabitatandsiteasnumbersandvalues.csv is the correct version but same as 2010_2017usedandavailwhabitatandvalues.csv which has words
useandavailnumberhabitat <- read.csv("./Data/2010_2021malesusedandavailwsiteandhabitasnumber.csv", stringsAsFactors = TRUE)

# make certain columns factors
useandavailnumberhabitat$Habitat_Cl <- as.factor(useandavailnumberhabitat$Habitat_Cl)
useandavailnumberhabitat$use <- as.factor(useandavailnumberhabitat$use)
useandavailnumberhabitat$site <- as.factor(useandavailnumberhabitat$site)
useandavailnumberhabitat$year <- as.factor(useandavailnumberhabitat$year)
useandavailnumberhabitat$weights <- as.numeric(useandavailnumberhabitat$weights)

# remove Tillinghast, Nicholas Farm, and Carolina before making models because there are so few birds, there are now 146 birds in the dataset
useandavailnumberhabitatwoTil <- useandavailnumberhabitat %>% dplyr::filter(site== "1"|site=="2"|site=="3"|site=="7") %>% droplevels()
str(useandavailnumberhabitatwoTil)
levels(useandavailnumberhabitatwoTil$site)
levels(useandavailnumberhabitat$year)
unique(useandavailnumberhabitatwoTil$id)

# scale variables 
library(dplyr)
usedandavailnumberhabitatwoTilscaled <- useandavailnumberhabitatwoTil %>%
  dplyr::mutate_at(c(5,6,7,8,9,10), funs(c(scale(.))))

# make upland forest mixed the reference category for models
usedandavailnumberhabitatwoTilscaled$Habitat_Cl <- relevel(usedandavailnumberhabitatwoTilscaled$Habitat_Cl, ref="3")
unique(usedandavailnumberhabitatwoTilscaled$Habitat_Cl)

# test for correlations between all male continuous variables ----
## Pearson r < |0.5| = not correlated
#remove anything that has a correlation greater that 0.7 or 0.8
#no correlated variables

covariates <- usedandavailnumberhabitatwoTilscaled %>%
  select(d2ag,d2moistsoi,d2stream,d2upyoungf,elevation,slope)

# Generate correlation matrix, there is nothing correlated in the 10 year male dataset
flat_cor_mat <- function(cor_r, cor_p){
  library(tidyr)
  library(tibble)
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}

correlations <- rcorr(as.matrix(covariates[, 1:6]), type = "pearson")

cor_matrix <- flat_cor_mat(correlations$r, correlations$P)
head(cor_matrix)


# ###make plots of used points
# 
# # used points distance to young forest
# ggplot(usedpoints, aes(x = d2upyoungf)) +
#   geom_histogram(aes(color="use", fill="use"),alpha = 0.3) +
#   labs(x = "d 2 young f (m)") +
#   theme_bw()

# 
# # used points distance to ag
# ggplot(usedpoints, aes(x = d2ag)) +
#   geom_histogram(bins=100) +
#   labs(x = "d 2 ag (m)") +
#   theme_bw()
# 
# # used points distance to stream
# ggplot(usedpoints, aes(x = d2stream)) +
#   geom_histogram(bins=10) +
#   labs(x = "d 2 stream (m)") +
#   theme_bw()
# 
# # used points elevation
# ggplot(usedpoints, aes(x = elevation)) +
#   geom_histogram(bins=100) +
#   labs(x = "elevation") +
#   theme_bw()
# 
# # used points slope 
# ggplot(usedpoints, aes(x = slope)) +
#   geom_histogram(bins=10) +
#   labs(x = "slope") +
#   theme_bw()
# 
# # used points forest cover 
# ggplot(usedpoints, aes(x = Habitat_Cl)) +
#   geom_bar() +
#   labs(x = "Habitat cover") +
#   theme_bw()
# 
# ### make plots of available points 
# 
# # used points distance to young forest
# ggplot(availpoints, aes(x = d2upyoungf)) +
#   geom_histogram(bins=100) +
#   labs(x = "d 2 young f (m)") +
#   theme_bw()
# 
# # used points distance to ag
# ggplot(availpoints, aes(x = d2ag)) +
#   geom_histogram(bins=100) +
#   labs(x = "d 2 ag (m)") +
#   theme_bw()
# 
# # used points distance to stream
# ggplot(availpoints, aes(x = d2stream)) +
#   geom_histogram(bins=10) +
#   labs(x = "d 2 stream (m)") +
#   theme_bw()
# 
# # used points elevation
# ggplot(availpoints, aes(x = elevation)) +
#   geom_histogram(bins=100) +
#   labs(x = "elevation") +
#   theme_bw()
# 
# # used points slope 
# ggplot(availpoints, aes(x = slope)) +
#   geom_histogram(bins=10) +
#   labs(x = "slope") +
#   theme_bw()
# 
# # used points forest cover 
# ggplot(availpoints, aes(x = Habitat_Cl)) +
#   geom_bar() +
#   labs(x = "Habitat cover") +
#   theme_bw()

# # filter birds out of the formatted used and avail point csv that include 20 or greater points
# 
# used20102017forgreaterthan20used <- usedandavail20102017forusedonly %>%  group_by(Refid) %>% filter(n() >=20) %>% droplevels()
# 
# # see which birds have more than 20 used, there are 44 ids 
# unique(used20102017forgreaterthan20used$Refid)
# 
# # filer use and available sheet to only include birds from unique line of code above so that scaled use and available sheet includes only birds that had 20 or more used points
# usedandavailablegreaterthan2020102017 <- usedandavail20102017forscaled %>% filter(Refid==142|Refid==144|Refid==150|Refid==151|Refid==152|Refid==156|Refid==157||Refid==159|Refid==160|Refid==161|Refid==163|Refid==166|Refid==167|Refid==169|Refid==171|Refid==172|Refid==173||Refid==176|Refid==180|Refid==181|Refid==182|Refid==186|Refid==187|Refid==190|Refid==191|Refid==202|Refid==204|Refid==205|Refid==206|Refid==207|Refid==208|Refid==211|Refid==215|Refid==216|Refid==219|Refid==220|Refid==221|Refid==222|Refid==223|Refid==225|Refid==226|Refid==228|Refid==230|Refid==232|Refid==234|Refid==236|Refid==237|Refid==239|Refid==242|Refid==243|Refid==244|Refid==245|Refid==247|Refid==250|Refid==251|Refid==259|Refid==262|Refid==265|Refid==257|Refid==255|Refid==254|Refid==252)


# top model of Roger's now with site as a random effect, model converges!!, I also tried id as the random effect and model would converge when I changed it to the optimizer listed in this forum:https://stackoverflow.com/questions/33670628/solution-to-the-warning-message-using-glmer I weighted models and it didn't change coeffcients hardly at all so 5,000 avail is sufficient 

# reran my model with all variables with available sample weighted by 2 and 3 equivalent to 10,000 and 15,000 available points to see if model coefficients changed and that I had enough available points to accurately represent availability

######make male all year models without Carolina, Till, or Nick Farm 2010-2021###########
# without weights on available sample

# get help with package glmer
??glmer

lme1 <- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                elevation +
                slope + 
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(lme1)
# save model lme2 output (my top model) so that you don't have to run it again
saveRDS(lme1, file = "./ObjectsAndModels/lme1.rds")

# read model back in to environment
lme1 <- readRDS("./ObjectsAndModels/lme1.rds")



# # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# lme12w <- glmer(use ~ Habitat_Cl +
#                  d2ag +
#                  d2stream +
#                  d2upyoungf +
#                  elevation +
#                  slope +
#                  d2moistsoi+
#                  (1|id)+(1|site),
#                family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=TRUE, weights=weights)
# # 
# summary(lme12w)

# # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # change the weights column so that 2 is now 3
# usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# 
# # make weights numeric
# usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(usedandavailnumberhabitatwoTilscaled$weights)
# 
# class(usedandavailnumberhabitatwoTilscaled$weights)
# 
# lme13w <- glmer(use ~ Habitat_Cl +
#                   d2ag +
#                   d2stream +
#                   d2upyoungf +
#                   elevation +
#                   slope + 
#                   d2moistsoi+
#                   (1|id)+(1|site),
#                 family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme13w)


# Roger's second top model now with id and slope as a random effect, model converges  
lme2 <- glmer(use ~ Habitat_Cl +
                 d2ag +
                 d2stream +
                 d2upyoungf +
                 slope +
                 d2moistsoi+
                 (1|id)+(1|site),
               family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme2)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme2, file = "./ObjectsAndModels/lme2.rds")
# read model back in to environment
lme2 <- readRDS("./ObjectsAndModels/lme2.rds")

# Roger's third top model now with id and slope as random effect 
lme3<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                elevation +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme3)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme3, file = "./ObjectsAndModels/lme3.rds")
# read model back in to environment
lme3 <- readRDS("./ObjectsAndModels/lme3.rds")

# Roger's fourth top model now with id and slope as a random effect 
lme4<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                d2moistsoi +
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme4)
# save model lme28 output (my top model) so that you don't have to run it again
saveRDS(lme4, file = "./ObjectsAndModels/lme4.rds")
# read model back in to environment
lme4 <- readRDS("./ObjectsAndModels/lme4.rds")

# Roger's fifth top model now with id and slope as a random effect
lme5<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme5)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme5, file = "./ObjectsAndModels/lme5.rds")
# read model back in to environment
lme5 <- readRDS("./ObjectsAndModels/lme5.rds")

# Roger's sixth top model now with id and slope as a random effect 
lme6<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2upyoungf +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme6)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme6, file = "./ObjectsAndModels/lme6.rds")
# read model back in to environment
lme6 <- readRDS("./ObjectsAndModels/lme6.rds")

# Roger's seventh top model now with id and slope as a random effect 
lme7<- glmer(use ~ Habitat_Cl +
                elevation+
                d2ag +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme7)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme7, file = "./ObjectsAndModels/lme7.rds")
# read model back in to environment
lme7 <- readRDS("./ObjectsAndModels/lme7.rds")

# Roger's eigth top model now with id and slope as a random effect 
lme8<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme8)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme8, file = "./ObjectsAndModels/lme8.rds")
# read model back in to environment
lme8 <- readRDS("./ObjectsAndModels/lme8.rds")

# Roger's ninth top model now with id and slope as a random effect 
lme9<- glmer(use ~ 
                elevation+
                slope+
                d2ag +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme9)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme9, file = "./ObjectsAndModels/lme9.rds")
# read model back in to environment
lme9 <- readRDS("./ObjectsAndModels/lme9.rds")

# Check singularity on this one bc there was a warning 
tt <- getME(lme9,"theta")
ll <- getME(lme9,"lower")
min(tt[ll==0])   ## equal to 0 (good!)


# Roger's tenth top model now with id and slope as a random effect 
lme10<- glmer(use ~ 
                slope+
                d2ag +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme10)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme10, file = "./ObjectsAndModels/lme10.rds")
# read model back in to environment
lme10 <- readRDS("./ObjectsAndModels/lme10.rds")

# Roger's eleventh top model now with id and slope as a random effect

# add line below to end of model if needed----
# , verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000))
lme11<- glmer(use ~ 
                d2ag +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme11)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme11, file = "./ObjectsAndModels/lme11.rds")
# read model back in to environment
lme11 <- readRDS("./ObjectsAndModels/lme11.rds")

# Check singularity on this one bc there was a warning 
tt2 <- getME(lme11,"theta")
ll2 <- getME(lme11,"lower")
min(tt2[ll2==0])   ## equal to 0 (good!)

# Roger's twelfth top model now with id and site as a random effect 
lme12<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme12)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme12, file = "./ObjectsAndModels/lme12.rds")
# read model back in to environment
lme12 <- readRDS("./ObjectsAndModels/lme12.rds")

# Roger's thirteenth top model now with id and slope as a random effect 
lme13<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme13)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme13, file = "./ObjectsAndModels/lme13.rds")
# read model back in to environment
lme13 <- readRDS("./ObjectsAndModels/lme13.rds")

# Roger's fourteenth top model now with id and slope as a random effect 
lme14<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme14)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme14, file = "./ObjectsAndModels/lme14.rds")
# read model back in to environment
lme14 <- readRDS("./ObjectsAndModels/lme14.rds")

# Roger's fifteenth top model now with id and slope as a random effect 
lme15<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled)

summary(lme15)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme15, file = "./ObjectsAndModels/lme15.rds")
# read model back in to environment
lme15 <- readRDS("./ObjectsAndModels/lme15.rds")

# make list of models
u_modroglme4random <- list(lme1, lme2, lme3, lme4, lme5, lme6, lme7, lme8, lme9, lme10, lme11, lme12,lme13, lme14, lme15)
#
# name models
u_modnameroglme4random <- c("lme1", "lme2", "lme3", "lme4", "lme5", "lme6", "lme7", "lme8", "lme9", "lme10", "lme11", "lme12","lme13", "lme14", "lme15")
#
# run AIC on models
aictab(u_modroglme4random, modnames = u_modnameroglme4random, second.ord = TRUE)

# there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 

# output AIC table
aicoutputlme4random <- data.frame(aictab(u_modroglme4random, modnames = u_modnameroglme4random))

# write aic table 
# write.table(aicoutputlme4random, file="./Figures/maleallyearsaictable.csv", sep=',')

# need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC


# kfold cross validation for top model lme2, works with 10 folds 1 rep
??kfoldRSF

kfoldlme2 <- kfoldRSF(
  lme2,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save kfoldlme2 output (my top model) so that you don't have to run it again
saveRDS(kfoldlme2, file = "./ObjectsAndModels/kfoldlme2.rds")

# read model back in to environment
kfoldlme2 <- readRDS("./ObjectsAndModels/kfoldlme2.rds")
# check summary
summary(kfoldlme2)

# kfold cross validation for second top model lme 1, does not work with 10 folds 1 rep,  works with 5 folds 1 rep
kfoldlme1 <- kfoldRSF(
  lme1,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save modelkfold lme2 output (my second top model) so that you don't have to run it again
saveRDS(kfoldlme1, file = "./ObjectsAndModels/kfoldlme1.rds")

# read model back in to environment
kfoldlme1 <- readRDS("./ObjectsAndModels/kfoldlme1.rds")
# check summary
summary(kfoldlme1)

# make kfold summaries into a dataframe
kfoldlme1summ <- data.frame(kfoldlme1)
# calculate mean kfold 0.90
mean(kfoldlme1$kfold[kfoldlme1$type=="obs"])

kfoldlme2summ <- data.frame(kfoldlme2)
# calculate mean kfold 0.90
mean(kfoldlme2$kfold[kfoldlme2$type=="obs"])


#######Model Averaging/Multimodel Inference for Male All Years Models########
success_topmods <- aicoutputlme4random %>% 
  filter(Delta_AICc < 2)

# Model Averaged Estimates with Shrinkage
## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
success_topmods
cands.success <- list(lme2, lme1)
candname.success <- c("lme2","lme1")
summary(lme1)
summary(lme2)
s_ModAvgCoefficients <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8",  "slope", "d2stream", "d2moistsoi", "d2upyoungf", "d2ag","elevation" )
success_int.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "(Intercept)")
success_habitatcl1.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl1")
success_habitatcl2.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl2")
success_habitatcl4.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl4")
success_habitatcl5.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl5")
success_habitatcl6.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl6")
success_habitatcl7.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl7")
success_habitatcl8.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "Habitat_Cl8")
success_slope.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "slope")
success_d2stream.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "d2stream")
success_d2moistsoi.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "d2moistsoi")
success_d2upyoungf.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "d2upyoungf")
success_d2ag.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "d2ag")
success_elevation.shrink <- modavgShrink(cand.set = cands.success, modnames = candname.success, parm = "elevation")

success_est.shrink <- c(success_int.shrink$Mod.avg.beta, success_habitatcl1.shrink$Mod.avg.beta, success_habitatcl2.shrink$Mod.avg.beta,success_habitatcl4.shrink$Mod.avg.beta, success_habitatcl5.shrink$Mod.avg.beta,success_habitatcl6.shrink$Mod.avg.beta, success_habitatcl7.shrink$Mod.avg.beta,success_habitatcl8.shrink$Mod.avg.beta,success_slope.shrink$Mod.avg.beta, success_d2stream.shrink$Mod.avg.beta,success_d2moistsoi.shrink$Mod.avg.beta,success_d2upyoungf.shrink$Mod.avg.beta,success_d2ag.shrink$Mod.avg.beta,success_elevation.shrink$Mod.avg.beta)

success_SE.shrink <- c(success_int.shrink$Uncond.SE, success_habitatcl1.shrink$Uncond.SE, success_habitatcl2.shrink$Uncond.SE,success_habitatcl4.shrink$Uncond.SE, success_habitatcl5.shrink$Uncond.SE,success_habitatcl6.shrink$Uncond.SE, success_habitatcl7.shrink$Uncond.SE,success_habitatcl8.shrink$Uncond.SE,success_slope.shrink$Uncond.SE, success_d2stream.shrink$Uncond.SE,success_d2moistsoi.shrink$Uncond.SE,success_d2upyoungf.shrink$Uncond.SE,success_d2ag.shrink$Uncond.SE,success_elevation.shrink$Uncond.SE)

success_LowCL.shrink <- c(success_int.shrink$Lower.CL, success_habitatcl1.shrink$Lower.CL, success_habitatcl2.shrink$Lower.CL,success_habitatcl4.shrink$Lower.CL, success_habitatcl5.shrink$Lower.CL,success_habitatcl6.shrink$Lower.CL, success_habitatcl7.shrink$Lower.CL,success_habitatcl8.shrink$Lower.CL, success_slope.shrink$Lower.CL,success_d2stream.shrink$Lower.CL, success_d2moistsoi.shrink$Lower.CL,success_d2upyoungf.shrink$Lower.CL,success_d2ag.shrink$Lower.CL,success_elevation.shrink$Lower.CL)

success_UpperCL.shrink <- c(success_int.shrink$Upper.CL, success_habitatcl1.shrink$Upper.CL, success_habitatcl2.shrink$Upper.CL,success_habitatcl4.shrink$Upper.CL, success_habitatcl5.shrink$Upper.CL,success_habitatcl6.shrink$Upper.CL, success_habitatcl7.shrink$Upper.CL,success_habitatcl8.shrink$Upper.CL, success_slope.shrink$Upper.CL,success_d2stream.shrink$Upper.CL, success_d2moistsoi.shrink$Upper.CL,success_d2upyoungf.shrink$Upper.CL,success_d2ag.shrink$Upper.CL,success_elevation.shrink$Upper.CL)

success_modavgcoefficients.shrink <- data.frame(s_ModAvgCoefficients, success_est.shrink, success_SE.shrink, success_LowCL.shrink, success_UpperCL.shrink)

# write_csv(success_modavgcoefficients.shrink, "ObjectsAndModels/malesallyearsmodelaverage.csv")


###########male models checking for year differences between the 2 time periods at 3 main sites 2010/2011 and 2020/2021 and check coefficients with CIs#########

#########male first 2010/2011 models w/ 3 main sites######### 

# checking to see if difference in d2 young forest between 2 periods----

# without weights on available sample 
lme1first <- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                elevation +
                slope + 
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme1first)
# save model lme2 output (my top model) so that you don't have to run it again
saveRDS(lme1first, file = "./ObjectsAndModels/lme1first.rds")

# read model back in to environment
lme1first <- readRDS("./ObjectsAndModels/lme1first.rds")


# # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# lme12wfirst <- glmer(use ~ Habitat_Cl +
#                  d2ag +
#                  d2stream +
#                  d2upyoungf +
#                  elevation +
#                  slope + 
#                  d2moistsoi+
#                  (1|id)+(1|site),
#                family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme12wfirst)

# # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # change the weights column so that 2 is now 3
# usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# 
# # make weights numeric
# usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(usedandavailnumberhabitatwoTilscaled$weights)
# 
# class(usedandavailnumberhabitatwoTilscaled$weights)
# 
# lme13wfirst <- glmer(use ~ Habitat_Cl +
#                   d2ag +
#                   d2stream +
#                   d2upyoungf +
#                   elevation +
#                   slope + 
#                   d2moistsoi+
#                   (1|id)+(1|site),
#                 family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme13wfirst)


# Roger's second top model now with id and slope as a random effect  
lme2first <- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                slope +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(lme2first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme2first, file = "./ObjectsAndModels/lme2first.rds")
# read model back in to environment
lme2first <- readRDS("./ObjectsAndModels/lme2first.rds")

# Roger's third top model now with id and slope as random effect
lme3first<- glmer(use ~ Habitat_Cl +
               d2ag +
               d2stream +
               d2upyoungf +
               elevation +
               d2moistsoi+
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(lme3first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme3first, file = "./ObjectsAndModels/lme3first.rds")
# read model back in to environment
lme3first <- readRDS("./ObjectsAndModels/lme3first.rds")

# Roger's fourth top model now with id and slope as a random effect 
lme4first<- glmer(use ~ Habitat_Cl +
               d2ag +
               d2stream +
               d2upyoungf +
               d2moistsoi +
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(lme4first)
# save model lme28 output (my top model) so that you don't have to run it again
saveRDS(lme4first, file = "./ObjectsAndModels/lme4first.rds")
# read model back in to environment
lme4first <- readRDS("./ObjectsAndModels/lme4first.rds")

# Roger's fifth top model now with id and slope as a random effect did not converge unless I used nloptwrap optimizer
lme5first<- glmer(use ~ Habitat_Cl +
               d2ag +
               d2stream +
               d2upyoungf +
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

# ss_lme5first <- getME(lme5first,c("theta","fixef"))
# lme5first_r3_v2 <- update(lme5first, start = ss_lme5first, control = glmerControl(optCtrl = list(maxfun = 2e4)))

summary(lme5first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme5first, file = "./ObjectsAndModels/lme5first.rds")
# read model back in to environment
lme5first <- readRDS("./ObjectsAndModels/lme5first.rds")

# Roger's sixth top model now with id and slope as a random effect 
lme6first<- glmer(use ~ Habitat_Cl +
               d2ag +
               d2upyoungf +
               d2moistsoi+
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme6first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme6first, file = "./ObjectsAndModels/lme6first.rds")
# read model back in to environment
lme6first <- readRDS("./ObjectsAndModels/lme6first.rds")

# Roger's seventh top model now with id and slope as a random effect 
lme7first<- glmer(use ~ Habitat_Cl +
               elevation+
               d2ag +
               d2moistsoi+
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme7first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme7first, file = "./ObjectsAndModels/lme7first.rds")
# read model back in to environment
lme7first <- readRDS("./ObjectsAndModels/lme7first.rds")

# Roger's eigth top model now with id and slope as a random effect 
lme8first<- glmer(use ~ Habitat_Cl +
               d2ag +
               d2upyoungf+
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme8first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme8first, file = "./ObjectsAndModels/lme8first.rds")
# read model back in to environment
lme8first <- readRDS("./ObjectsAndModels/lme8first.rds")

# Roger's ninth top model now with id and slope as a random effect 
lme9first<- glmer(use ~ 
               elevation+
               slope+
               d2ag +
               d2stream +
               d2upyoungf+
               d2moistsoi+
               (1|id)+(1|site),
             family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(lme9first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme9first, file = "./ObjectsAndModels/lme9first.rds")
# read model back in to environment
lme9first <- readRDS("./ObjectsAndModels/lme9first.rds")

# Roger's tenth top model now with id and slope as a random effect 
lme10first<- glmer(use ~ 
                slope+
                d2ag +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"),control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

summary(lme10first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme10first, file = "./ObjectsAndModels/lme10first.rds")
# read model back in to environment
lme10first <- readRDS("./ObjectsAndModels/lme10first.rds")

# Roger's eleventh top model now with id and slope as a random effect 
lme11first<- glmer(use ~ 
                d2ag +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))
summary(lme11first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme11first, file = "./ObjectsAndModels/lme11first.rds")
# read model back in to environment
lme11first <- readRDS("./ObjectsAndModels/lme11first.rds")

# Roger's twelfth top model now with id and site as a random effect 
lme12first<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme12first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme12first, file = "./ObjectsAndModels/lme12first.rds")
# read model back in to environment
lme12first <- readRDS("./ObjectsAndModels/lme12first.rds")

# Roger's thirteenth top model now with id and slope as a random effect 
lme13first<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme13first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme13first, file = "./ObjectsAndModels/lme13first.rds")
# read model back in to environment
lme13first <- readRDS("./ObjectsAndModels/lme13first.rds")

# Roger's fourteenth top model now with id and slope as a random effect 
lme14first<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme14first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme14first, file = "./ObjectsAndModels/lme14first.rds")
# read model back in to environment
lme14first <- readRDS("./ObjectsAndModels/lme14first.rds")

# Roger's fifteenth top model now with id and slope as a random effect 
lme15first<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), year=="2010"|year=="2011"))

summary(lme15first)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme15first, file = "./ObjectsAndModels/lme15first.rds")
# read model back in to environment
lme15first <- readRDS("./ObjectsAndModels/lme15first.rds")

# make list of models
u_modroglme4randomfirst <- list(lme1first, lme2first, lme3first, lme4first, lme5first, lme6first, lme7first, lme8first, lme9first, lme10first, lme11first, lme12first,lme13first, lme14first, lme15first)
#
# name models
u_modnameroglme4randomfirst <- c("lme1first", "lme2first", "lme3first", "lme4first", "lme5first_r3_v2", "lme6first", "lme7first", "lme8first", "lme9first", "lme10first", "lme11first", "lme12first","lme13first", "lme14first", "lme15first")
#
# run AIC on models
aictab(u_modroglme4randomfirst, modnames = u_modnameroglme4randomfirst, second.ord = TRUE)

# there are 3 top competing models (roger's model 2, model 13, and model 12) 

# output AIC table
aicoutputlme4randomfirst <- data.frame(aictab(u_modroglme4randomfirst, modnames = u_modnameroglme4randomfirst))

# write aic table 
# write.table(aicoutputlme4randomfirst, file="./Figures/malefirstperiodaictable.csv", sep=',')

# # need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC
# 
# 
# # kfold cross validation for top model lme2first
# kfoldlme2first <- kfoldRSF(
#   lme2first,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save kfold lme2first output so that you don't have to run it again
# saveRDS(kfoldlme2first, file = "./ObjectsAndModels/kfoldlme2first.rds")
# 
# # read model back in to environment
# kfoldlme2first <- readRDS("./ObjectsAndModels/kfoldlme2first.rds")
# # check summary
# summary(kfoldlme2first)
# 
# # kfold cross validation for lme 1first
# kfoldlme1first <- kfoldRSF(
#   lme1first,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save model kfold lme1first output so that you don't have to run it again
# saveRDS(kfoldlme1first, file = "./ObjectsAndModels/kfoldlme1first.rds")
# 
# # read model back in to environment
# kfoldlme1first <- readRDS("./ObjectsAndModels/kfoldlme1first.rds")
# # check summary
# summary(kfoldlme1first)
# 
# # kfold cross validation for top model lme12first
# kfoldlme12first <- kfoldRSF(
#   lme12first,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save kfoldlme12first output so that you don't have to run it again
# saveRDS(kfoldlme12first, file = "./ObjectsAndModels/kfoldlme12first.rds")
# summary(kfoldlme12first)
# # read model back in to environment
# kfoldlme12first <- readRDS("./ObjectsAndModels/kfoldlme12first.rds")
# # check summary
# summary(kfoldlme12first)
# 
# 
# # kfold cross validation for second top model lme 1
# kfoldlme13first <- kfoldRSF(
#   lme13first,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save model kfoldlme2 output (my second top model) so that you don't have to run it again
# saveRDS(kfoldlme13first, file = "./ObjectsAndModels/kfoldlme13first.rds")
# 
# # read model back in to environment
# kfoldlme13first <- readRDS("./ObjectsAndModels/kfoldlme13first.rds")
# # check summary
# summary(kfoldlme13first)
# 
# # make kfold summaries into a dataframe
# kfoldlme1firstsumm <- data.frame(kfoldlme1first)
# # calculate mean kfold
# mean(kfoldlme1first$kfold[kfoldlme1first$type=="obs"])
# 
# kfoldlme2firstsumm <- data.frame(kfoldlme2first)
# # calculate mean kfold
# mean(kfoldlme2first$kfold[kfoldlme2first$type=="obs"])
# 
# # make kfold summaries into a dataframe
# kfoldlme12firstsumm <- data.frame(kfoldlme12first)
# # calculate mean kfold
# mean(kfoldlme12first$kfold[kfoldlme12first$type=="obs"])
# 
# kfoldlme13firstsumm <- data.frame(kfoldlme13first)
# # calculate mean kfold
# mean(kfoldlme13first$kfold[kfoldlme13first$type=="obs"])

#######Model Averaging/Multimodel Inference for Male First Models########
success_topmodsfirst <- aicoutputlme4randomfirst %>% 
  filter(Delta_AICc < 2)

# Model Averaged Estimates with Shrinkage
## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
success_topmodsfirst
cands.successfirst <- list(lme2first, lme12first, lme14first)
candname.successfirst <- c("lme2first","lme12first","lme14first")
summary(lme13first)
s_ModAvgCoefficientsfirst <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8","slope", "d2stream", "d2upyoungf","d2ag","d2moistsoi")
success_int.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "(Intercept)")
success_habitatcl1.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl1")
success_habitatcl2.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl2")
success_habitatcl4.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl4")
success_habitatcl5.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl5")
success_habitatcl6.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl6")
success_habitatcl7.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl7")
success_habitatcl8.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "Habitat_Cl8")
success_slope.shrinkfirst <-      modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "slope")
success_d2stream.shrinkfirst <-   modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "d2stream")
success_d2upyoungf.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "d2upyoungf")
success_d2ag.shrinkfirst <-       modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "d2ag")
success_d2moistsoi.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "d2moistsoi")
success_d2moistsoi.shrinkfirst <- modavgShrink(cand.set = cands.successfirst, modnames = candname.successfirst, parm = "d2moistsoi")

success_est.shrinkfirst <- c(success_int.shrinkfirst$Mod.avg.beta, success_habitatcl1.shrinkfirst$Mod.avg.beta, success_habitatcl2.shrinkfirst$Mod.avg.beta,success_habitatcl4.shrinkfirst$Mod.avg.beta, success_habitatcl5.shrinkfirst$Mod.avg.beta,success_habitatcl6.shrinkfirst$Mod.avg.beta, success_habitatcl7.shrinkfirst$Mod.avg.beta,success_habitatcl8.shrinkfirst$Mod.avg.beta,success_slope.shrinkfirst$Mod.avg.beta, success_d2stream.shrinkfirst$Mod.avg.beta,success_d2upyoungf.shrinkfirst$Mod.avg.beta,success_d2ag.shrinkfirst$Mod.avg.beta,success_d2moistsoi.shrinkfirst$Mod.avg.beta)

success_SE.shrinkfirst <- c(success_int.shrinkfirst$Uncond.SE, success_habitatcl1.shrinkfirst$Uncond.SE, success_habitatcl2.shrinkfirst$Uncond.SE,success_habitatcl4.shrinkfirst$Uncond.SE, success_habitatcl5.shrinkfirst$Uncond.SE,success_habitatcl6.shrinkfirst$Uncond.SE, success_habitatcl7.shrinkfirst$Uncond.SE,success_habitatcl8.shrinkfirst$Uncond.SE,success_slope.shrinkfirst$Uncond.SE, success_d2stream.shrinkfirst$Uncond.SE,success_d2upyoungf.shrinkfirst$Uncond.SE,success_d2ag.shrinkfirst$Uncond.SE,success_d2moistsoi.shrinkfirst$Uncond.SE)

success_LowCL.shrinkfirst <- c(success_int.shrinkfirst$Lower.CL, success_habitatcl1.shrinkfirst$Lower.CL, success_habitatcl2.shrinkfirst$Lower.CL,success_habitatcl4.shrinkfirst$Lower.CL, success_habitatcl5.shrinkfirst$Lower.CL,success_habitatcl6.shrinkfirst$Lower.CL, success_habitatcl7.shrinkfirst$Lower.CL,success_habitatcl8.shrinkfirst$Lower.CL, success_slope.shrinkfirst$Lower.CL,success_d2stream.shrinkfirst$Lower.CL, success_d2upyoungf.shrinkfirst$Lower.CL,success_d2ag.shrinkfirst$Lower.CL,success_d2moistsoi.shrinkfirst$Lower.CL)

success_UpperCL.shrinkfirst <- c(success_int.shrinkfirst$Upper.CL, success_habitatcl1.shrinkfirst$Upper.CL, success_habitatcl2.shrinkfirst$Upper.CL,success_habitatcl4.shrinkfirst$Upper.CL, success_habitatcl5.shrinkfirst$Upper.CL,success_habitatcl6.shrinkfirst$Upper.CL, success_habitatcl7.shrinkfirst$Upper.CL,success_habitatcl8.shrinkfirst$Upper.CL, success_slope.shrinkfirst$Upper.CL,success_d2stream.shrinkfirst$Upper.CL,success_d2upyoungf.shrinkfirst$Upper.CL,success_d2ag.shrinkfirst$Upper.CL,success_d2moistsoi.shrinkfirst$Upper.CL)

success_modavgcoefficients.shrinkfirst <- data.frame(s_ModAvgCoefficientsfirst, success_est.shrinkfirst, success_SE.shrinkfirst, success_LowCL.shrinkfirst, success_UpperCL.shrinkfirst)

# write_csv(success_modavgcoefficients.shrinkfirst, "ObjectsAndModels/20102011modelaverage.csv")


#########male second 2020/2021 models w/ 3 main sites##########
# filter out just arcadia, big river, and great swamp to match what Roger had in 2010/2011
filteredsites <-usedandavailnumberhabitatwoTilscaled %>% filter(site=="1"|site=="2"|site=="3") %>% droplevels()

 #without weights on availbale sample 
lme1second <- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2stream +
                     d2upyoungf +
                     elevation +
                     slope + 
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))
summary(lme1second)
# save model lme2 output (my top model) so that you don't have to run it again
saveRDS(lme1second, file = "./ObjectsAndModels/lme1second.rds")

# read model back in to environment
lme1second <- readRDS("./ObjectsAndModels/lme1second.rds")



# # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# lme12wsecond <- glmer(use ~ Habitat_Cl +
#                  d2ag +
#                  d2stream +
#                  d2upyoungf +
#                  elevation +
#                  slope + 
#                  d2moistsoi+
#                  (1|id)+(1|site),
#                family = binomial(link = "logit"), data = filteredsites, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme12wsecond)

# # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # change the weights column so that 2 is now 3
# usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# 
# # make weights numeric
# usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(filteredsites$weights)
# 
# class(usedandavailnumberhabitatwoTilscaled$weights)
# 
# lme13wsecond <- glmer(use ~ Habitat_Cl +
#                   d2ag +
#                   d2stream +
#                   d2upyoungf +
#                   elevation +
#                   slope + 
#                   d2moistsoi+
#                   (1|id)+(1|site),
#                 family = binomial(link = "logit"), data = filteredsites, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme13wsecond)


# Roger's second top model now with id and slope as a random effect, model converges  
lme2second <- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2stream +
                     d2upyoungf +
                     slope +
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme2second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme2second, file = "./ObjectsAndModels/lme2second.rds")
# read model back in to environment
lme2second <- readRDS("./ObjectsAndModels/lme2second.rds")

# Roger's third top model now with id and slope as random effect 
lme3second<- glmer(use ~ Habitat_Cl +
                    d2ag +
                    d2stream +
                    d2upyoungf +
                    elevation +
                    d2moistsoi+
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme3second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme3second, file = "./ObjectsAndModels/lme3second.rds")
# read model back in to environment
lme3second <- readRDS("./ObjectsAndModels/lme3second.rds")

# Roger's fourth top model now with id and slope as a random effect 
lme4second<- glmer(use ~ Habitat_Cl +
                    d2ag +
                    d2stream +
                    d2upyoungf +
                    d2moistsoi +
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme4second)
# save model lme28 output (my top model) so that you don't have to run it again
saveRDS(lme4second, file = "./ObjectsAndModels/lme4second.rds")
# read model back in to environment
lme4second <- readRDS("./ObjectsAndModels/lme4second.rds")

# Roger's fifth top model now with id and slope as a random effect
lme5second<- glmer(use ~ Habitat_Cl +
                    d2ag +
                    d2stream +
                    d2upyoungf +
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme5second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme5second, file = "./ObjectsAndModels/lme5second.rds")
# read model back in to environment
lme5second <- readRDS("./ObjectsAndModels/lme5second.rds")

# Roger's sixth top model now with id and slope as a random effect 
lme6second<- glmer(use ~ Habitat_Cl +
                    d2ag +
                    d2upyoungf +
                    d2moistsoi+
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme6second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme6second, file = "./ObjectsAndModels/lme6second.rds")
# read model back in to environment
lme6second <- readRDS("./ObjectsAndModels/lme6second.rds")

# Roger's seventh top model now with id and slope as a random effect 
lme7second<- glmer(use ~ Habitat_Cl +
                    elevation+
                    d2ag +
                    d2moistsoi+
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme7second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme7second, file = "./ObjectsAndModels/lme7second.rds")
# read model back in to environment
lme7second <- readRDS("./ObjectsAndModels/lme7second.rds")

# Roger's eigth top model now with id and slope as a random effect 
lme8second<- glmer(use ~ Habitat_Cl +
                    d2ag +
                    d2upyoungf+
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme8second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme8second, file = "./ObjectsAndModels/lme8second.rds")
# read model back in to environment
lme8second <- readRDS("./ObjectsAndModels/lme8second.rds")

# Roger's ninth top model now with id and slope as a random effect 
lme9second<- glmer(use ~ 
                    elevation+
                    slope+
                    d2ag +
                    d2stream +
                    d2upyoungf+
                    d2moistsoi+
                    (1|id)+(1|site),
                  family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme9second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme9second, file = "./ObjectsAndModels/lme9second.rds")
# read model back in to environment
lme9second <- readRDS("./ObjectsAndModels/lme9second.rds")

# Roger's tenth top model now with id and slope as a random effect 
lme10second<- glmer(use ~ 
                     slope+
                     d2ag +
                     d2stream +
                     d2upyoungf+
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme10second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme10second, file = "./ObjectsAndModels/lme10second.rds")
# read model back in to environment
lme10second <- readRDS("./ObjectsAndModels/lme10second.rds")

# Roger's eleventh top model now with id and slope as a random effect 
lme11second<- glmer(use ~ 
                     d2ag +
                     d2upyoungf+
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme11second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme11second, file = "./ObjectsAndModels/lme11second.rds")
# read model back in to environment
lme11second <- readRDS("./ObjectsAndModels/lme11second.rds")

# Roger's twelfth top model now with id and site as a random effect 
lme12second<- glmer(use ~ 
                     slope+
                     Habitat_Cl +
                     d2stream +
                     d2upyoungf+
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme12second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme12second, file = "./ObjectsAndModels/lme12second.rds")
# read model back in to environment
lme12second <- readRDS("./ObjectsAndModels/lme12second.rds")

# Roger's thirteenth top model now with id and slope as a random effect 
lme13second<- glmer(use ~ 
                     slope+
                     Habitat_Cl +
                     d2stream +
                     d2upyoungf+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme13second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme13second, file = "./ObjectsAndModels/lme13second.rds")
# read model back in to environment
lme13second <- readRDS("./ObjectsAndModels/lme13second.rds")

# Roger's fourteenth top model now with id and slope as a random effect 
lme14second<- glmer(use ~ 
                     Habitat_Cl+
                     d2stream +
                     d2upyoungf+
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme14second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme14second, file = "./ObjectsAndModels/lme14second.rds")
# read model back in to environment
lme14second <- readRDS("./ObjectsAndModels/lme14second.rds")

# Roger's fifteenth top model now with id and slope as a random effect 
lme15second<- glmer(use ~ 
                     Habitat_Cl+
                     d2stream +
                     d2upyoungf+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((filteredsites), year=="2020"|year=="2021"))

summary(lme15second)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme15second, file = "./ObjectsAndModels/lme15second.rds")
# read model back in to environment
lme15second <- readRDS("./ObjectsAndModels/lme15second.rds")

# make list of models
u_modroglme4randomsecond <- list(lme1second, lme2second, lme3second, lme4second, lme5second, lme6second, lme7second, lme8second, lme9second, lme10second, lme11second, lme12second,lme13second, lme14second, lme15second)
#
# name models
u_modnameroglme4randomsecond <- c("lme1second", "lme2second", "lme3second", "lme4second", "lme5second", "lme6second", "lme7second", "lme8second", "lme9second", "lme10second", "lme11second", "lme12second","lme13second", "lme14second", "lme15second")
#
# run AIC on models
aictab(u_modroglme4randomsecond, modnames = u_modnameroglme4randomsecond, second.ord = TRUE)

# there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 

# output AIC table
aicoutputlme4randomsecond <- data.frame(aictab(u_modroglme4randomsecond, modnames = u_modnameroglme4randomsecond))

# write aic table 
# write.table(aicoutputlme4randomsecond, file="./Figures/maleseconperiodaictable.csv", sep=',')

# # need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC
# 
# 
# # kfold cross validation for model lme2second
# kfoldlme2second <- kfoldRSF(
#   lme2second,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save kfoldlme2second output so that you don't have to run it again
# saveRDS(kfoldlme2second, file = "./ObjectsAndModels/kfoldlme2second.rds")
# 
# # read model back in to environment
# kfoldlme2second <- readRDS("./ObjectsAndModels/kfoldlme2second.rds")
# # check summary
# summary(kfoldlme2second)
# 
# # kfold cross validation for lme1second
# kfoldlme1second <- kfoldRSF(
#   lme1second,
#   k = 5,
#   nrepet = 1,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save model kfoldlme1second so that you don't have to run it again
# saveRDS(kfoldlme1second, file = "./ObjectsAndModels/kfoldlme1second.rds")
# 
# # read model back in to environment
# kfoldlme1second <- readRDS("./ObjectsAndModels/kfoldlme1second.rds")
# # check summary
# summary(kfoldlme1second)
# 
# # make kfold summaries into a dataframe
# kfoldlme1secondsumm <- data.frame(kfoldlme1second)
# # calculate mean kfold 
# mean(kfoldlme1second$kfold[kfoldlme1second$type=="obs"])
# 
# kfoldlme2secondsumm <- data.frame(kfoldlme2second)
# # calculate mean kfold 
# mean(kfoldlme2second$kfold[kfoldlme2second$type=="obs"])

#######Model Averaging/Multimodel Inference for Male Second Models########
success_topmodssecond <- aicoutputlme4randomsecond %>% 
  filter(Delta_AICc < 2)

# Model Averaged Estimates with Shrinkage
## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
success_topmodssecond
cands.successsecond <- list(lme2second, lme12second)
candname.successsecond <- c("lme2second","lme12second")
summary(lme13first)

s_ModAvgCoefficientssecond <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8", "slope", "d2stream", "d2upyoungf", "d2ag","d2moistsoi")
success_int.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "(Intercept)")
success_habitatcl1.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl1")
success_habitatcl2.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl2")
success_habitatcl4.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl4")
success_habitatcl5.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl5")
success_habitatcl6.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl6")
success_habitatcl7.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl7")
success_habitatcl8.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "Habitat_Cl8")
success_slope.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "slope")
success_d2stream.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "d2stream")
success_d2upyoungf.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "d2upyoungf")
success_d2ag.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "d2ag")
success_d2moistsoi.shrinksecond <- modavgShrink(cand.set = cands.successsecond, modnames = candname.successsecond, parm = "d2moistsoi")


success_est.shrinksecond <- c(success_int.shrinksecond$Mod.avg.beta, success_habitatcl1.shrinksecond$Mod.avg.beta, success_habitatcl2.shrinksecond$Mod.avg.beta,success_habitatcl4.shrinksecond$Mod.avg.beta, success_habitatcl5.shrinksecond$Mod.avg.beta,success_habitatcl6.shrinksecond$Mod.avg.beta, success_habitatcl7.shrinksecond$Mod.avg.beta,success_habitatcl8.shrinksecond$Mod.avg.beta,success_slope.shrinksecond$Mod.avg.beta, success_d2stream.shrinksecond$Mod.avg.beta,success_d2upyoungf.shrinksecond$Mod.avg.beta,success_d2ag.shrinksecond$Mod.avg.beta,success_d2moistsoi.shrinksecond$Mod.avg.beta)

success_SE.shrinksecond <- c(success_int.shrinksecond$Uncond.SE, success_habitatcl1.shrinksecond$Uncond.SE, success_habitatcl2.shrinksecond$Uncond.SE,success_habitatcl4.shrinksecond$Uncond.SE, success_habitatcl5.shrinksecond$Uncond.SE,success_habitatcl6.shrinksecond$Uncond.SE, success_habitatcl7.shrinksecond$Uncond.SE,success_habitatcl8.shrinksecond$Uncond.SE,success_slope.shrinksecond$Uncond.SE, success_d2stream.shrinksecond$Uncond.SE,success_d2upyoungf.shrinksecond$Uncond.SE,success_d2ag.shrinksecond$Uncond.SE,success_d2moistsoi.shrinksecond$Uncond.SE)

success_LowCL.shrinksecond <- c(success_int.shrinksecond$Lower.CL, success_habitatcl1.shrinksecond$Lower.CL, success_habitatcl2.shrinksecond$Lower.CL,success_habitatcl4.shrinksecond$Lower.CL, success_habitatcl5.shrinksecond$Lower.CL,success_habitatcl6.shrinksecond$Lower.CL, success_habitatcl7.shrinksecond$Lower.CL,success_habitatcl8.shrinksecond$Lower.CL, success_slope.shrinksecond$Lower.CL,success_d2stream.shrinksecond$Lower.CL,success_d2upyoungf.shrinksecond$Lower.CL,success_d2ag.shrinksecond$Lower.CL,success_d2moistsoi.shrinksecond$Lower.CL)

success_UpperCL.shrinksecond <- c(success_int.shrinksecond$Upper.CL, success_habitatcl1.shrinksecond$Upper.CL, success_habitatcl2.shrinksecond$Upper.CL,success_habitatcl4.shrinksecond$Upper.CL, success_habitatcl5.shrinksecond$Upper.CL,success_habitatcl6.shrinksecond$Upper.CL, success_habitatcl7.shrinksecond$Upper.CL,success_habitatcl8.shrinksecond$Upper.CL, success_slope.shrinksecond$Upper.CL,success_d2stream.shrinksecond$Upper.CL, success_d2upyoungf.shrinksecond$Upper.CL,success_d2ag.shrinksecond$Upper.CL,success_d2moistsoi.shrinksecond$Upper.CL)

success_modavgcoefficients.shrinksecond <- data.frame(s_ModAvgCoefficientssecond, success_est.shrinksecond, success_SE.shrinksecond, success_LowCL.shrinksecond, success_UpperCL.shrinksecond)

# write_csv(success_modavgcoefficients.shrinksecond, "ObjectsAndModels/20202021modelavergae.csv")

######make male comp models 2020/2021 for comparison w/ females (Arcadia, Big River, G Swamp, F carter)##############
# there is only one female from Carter so may have to drop Carter later on
malesitefilter <- usedandavailnumberhabitatwoTilscaled %>% filter(site=="1"|site=="2"|site=="3"|site=="7") %>% droplevels()
str(malesitefilter)
#without weights on available sample 
lme1malecomp <- glmer(use ~ Habitat_Cl +
                      d2ag +
                      d2stream +
                      d2upyoungf +
                      elevation +
                      slope + 
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(lme1malecomp)
# save model lme2 output (my top model) so that you don't have to run it again
saveRDS(lme1malecomp, file = "./ObjectsAndModels/lme1malecomp.rds")

# read model back in to environment
lme1malecomp <- readRDS("./ObjectsAndModels/lme1malecomp.rds")



# # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# lme12wmalecomp <- glmer(use ~ Habitat_Cl +
#                  d2ag +
#                  d2stream +
#                  d2upyoungf +
#                  elevation +
#                  slope + 
#                  d2moistsoi+
#                  (1|id)+(1|site),
#                family = binomial(link = "logit"), data = filteredsites, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme12wmalecomp)

# # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # change the weights column so that 2 is now 3
# usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# 
# # make weights numeric
# usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(filteredsites$weights)
# 
# class(usedandavailnumberhabitatwoTilscaled$weights)
# 
# lme13wmalecomp <- glmer(use ~ Habitat_Cl +
#                   d2ag +
#                   d2stream +
#                   d2upyoungf +
#                   elevation +
#                   slope + 
#                   d2moistsoi+
#                   (1|id)+(1|site),
#                 family = binomial(link = "logit"), data = filteredsites, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# 
# summary(lme13wmalecomp)


# Roger's malecomp top model now with id and slope as a random effect, model converges  
lme2malecomp <- glmer(use ~ Habitat_Cl +
                      d2ag +
                      d2stream +
                      d2upyoungf +
                      slope +
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme2malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme2malecomp, file = "./ObjectsAndModels/lme2malecomp.rds")
# read model back in to environment
lme2malecomp <- readRDS("./ObjectsAndModels/lme2malecomp.rds")

# Roger's third top model now with id and slope as random effect 
lme3malecomp<- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2stream +
                     d2upyoungf +
                     elevation +
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme3malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme3malecomp, file = "./ObjectsAndModels/lme3malecomp.rds")
# read model back in to environment
lme3malecomp <- readRDS("./ObjectsAndModels/lme3malecomp.rds")

# Roger's fourth top model now with id and slope as a random effect 
lme4malecomp<- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2stream +
                     d2upyoungf +
                     d2moistsoi +
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme4malecomp)
# save model lme28 output (my top model) so that you don't have to run it again
saveRDS(lme4malecomp, file = "./ObjectsAndModels/lme4malecomp.rds")
# read model back in to environment
lme4malecomp <- readRDS("./ObjectsAndModels/lme4malecomp.rds")

# Roger's fifth top model now with id and slope as a random effect
lme5malecomp<- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2stream +
                     d2upyoungf +
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme5malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme5malecomp, file = "./ObjectsAndModels/lme5malecomp.rds")
# read model back in to environment
lme5malecomp <- readRDS("./ObjectsAndModels/lme5malecomp.rds")

# Roger's sixth top model now with id and slope as a random effect 
lme6malecomp<- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2upyoungf +
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme6malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme6malecomp, file = "./ObjectsAndModels/lme6malecomp.rds")
# read model back in to environment
lme6malecomp <- readRDS("./ObjectsAndModels/lme6malecomp.rds")

# Roger's seventh top model now with id and slope as a random effect 
lme7malecomp<- glmer(use ~ Habitat_Cl +
                     elevation+
                     d2ag +
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme7malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme7malecomp, file = "./ObjectsAndModels/lme7malecomp.rds")
# read model back in to environment
lme7malecomp <- readRDS("./ObjectsAndModels/lme7malecomp.rds")

# Roger's eigth top model now with id and slope as a random effect 
lme8malecomp<- glmer(use ~ Habitat_Cl +
                     d2ag +
                     d2upyoungf+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme8malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme8malecomp, file = "./ObjectsAndModels/lme8malecomp.rds")
# read model back in to environment
lme8malecomp <- readRDS("./ObjectsAndModels/lme8malecomp.rds")

# Roger's ninth top model now with id and slope as a random effect 
lme9malecomp<- glmer(use ~ 
                     elevation+
                     slope+
                     d2ag +
                     d2stream +
                     d2upyoungf+
                     d2moistsoi+
                     (1|id)+(1|site),
                   family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme9malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme9malecomp, file = "./ObjectsAndModels/lme9malecomp.rds")
# read model back in to environment
lme9malecomp <- readRDS("./ObjectsAndModels/lme9malecomp.rds")

# Roger's tenth top model now with id and slope as a random effect 
lme10malecomp<- glmer(use ~ 
                      slope+
                      d2ag +
                      d2stream +
                      d2upyoungf+
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme10malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme10malecomp, file = "./ObjectsAndModels/lme10malecomp.rds")
# read model back in to environment
lme10malecomp <- readRDS("./ObjectsAndModels/lme10malecomp.rds")

# Roger's eleventh top model now with id and slope as a random effect 
lme11malecomp<- glmer(use ~ 
                      d2ag +
                      d2upyoungf+
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme11malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme11malecomp, file = "./ObjectsAndModels/lme11malecomp.rds")
# read model back in to environment
lme11malecomp <- readRDS("./ObjectsAndModels/lme11malecomp.rds")

# Roger's twelfth top model now with id and site as a random effect 
lme12malecomp<- glmer(use ~ 
                      slope+
                      Habitat_Cl +
                      d2stream +
                      d2upyoungf+
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme12malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme12malecomp, file = "./ObjectsAndModels/lme12malecomp.rds")
# read model back in to environment
lme12malecomp <- readRDS("./ObjectsAndModels/lme12malecomp.rds")

# Roger's thirteenth top model now with id and slope as a random effect 
lme13malecomp<- glmer(use ~ 
                      slope+
                      Habitat_Cl +
                      d2stream +
                      d2upyoungf+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme13malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme13malecomp, file = "./ObjectsAndModels/lme13malecomp.rds")
# read model back in to environment
lme13malecomp <- readRDS("./ObjectsAndModels/lme13malecomp.rds")

# Roger's fourteenth top model now with id and slope as a random effect 
lme14malecomp<- glmer(use ~ 
                      Habitat_Cl+
                      d2stream +
                      d2upyoungf+
                      d2moistsoi+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme14malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme14malecomp, file = "./ObjectsAndModels/lme14malecomp.rds")
# read model back in to environment
lme14malecomp <- readRDS("./ObjectsAndModels/lme14malecomp.rds")

# Roger's fifteenth top model now with id and slope as a random effect 
lme15malecomp<- glmer(use ~ 
                      Habitat_Cl+
                      d2stream +
                      d2upyoungf+
                      (1|id)+(1|site),
                    family = binomial(link = "logit"), data = filter((malesitefilter), year=="2020"|year=="2021"))

summary(lme15malecomp)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme15malecomp, file = "./ObjectsAndModels/lme15malecomp.rds")
# read model back in to environment
lme15malecomp <- readRDS("./ObjectsAndModels/lme15malecomp.rds")

# make list of models
u_modroglme4randommalecomp <- list(lme1malecomp, lme2malecomp, lme3malecomp, lme4malecomp, lme5malecomp, lme6malecomp, lme7malecomp, lme8malecomp, lme9malecomp, lme10malecomp, lme11malecomp, lme12malecomp,lme13malecomp, lme14malecomp, lme15malecomp)
#
# name models
u_modnameroglme4randommalecomp <- c("lme1malecomp", "lme2malecomp", "lme3malecomp", "lme4malecomp", "lme5malecomp", "lme6malecomp", "lme7malecomp", "lme8malecomp", "lme9malecomp", "lme10malecomp", "lme11malecomp", "lme12malecomp","lme13malecomp", "lme14malecomp", "lme15malecomp")
#
# run AIC on models
aictab(u_modroglme4randommalecomp, modnames = u_modnameroglme4randommalecomp, second.ord = TRUE)

# there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 

# output AIC table
aicoutputlme4randommalecomp <- data.frame(aictab(u_modroglme4randommalecomp, modnames = u_modnameroglme4randommalecomp))

# write aic table 
# write.table(aicoutputlme4randommalecomp, file="./Figures/malecompaictable.csv", sep=',')

# need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC


# kfold cross validation for model lme2malecomp
kfoldlme2malecomp <- kfoldRSF(
  lme2malecomp,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save kfoldlme2malecomp output so that you dont have to run it again
saveRDS(kfoldlme2malecomp, file = "./ObjectsAndModels/kfoldlme2malecomp.rds")

# read model back in to environment
kfoldlme2malecomp <- readRDS("./ObjectsAndModels/kfoldlme2malecomp.rds")
# check summary
summary(kfoldlme2malecomp)

# kfold cross validation for model lme1malecomp
kfoldlme1malecomp <- kfoldRSF(
  lme1malecomp,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save model kfoldlme1malecomp output so that you don't have to run it again
saveRDS(kfoldlme1malecomp, file = "./ObjectsAndModels/kfoldlme1malecomp.rds")

# read model back in to environment
kfoldlme1malecomp <- readRDS("./ObjectsAndModels/kfoldlme1malecomp.rds")
# check summary
summary(kfoldlme1malecomp)

# kfold cross validation for model lme12malecomp
kfoldlme12malecomp <- kfoldRSF(
  lme12malecomp,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save kfoldlme12malecomp output so that you don't have to run it again
saveRDS(kfoldlme12malecomp, file = "./ObjectsAndModels/kfoldlme12malecomp.rds")

# read model back in to environment
kfoldlme12malecomp <- readRDS("./ObjectsAndModels/kfoldlme12malecomp.rds")
# check summary
summary(kfoldlme12malecomp)


# make kfold summaries into a dataframe
kfoldlme1malecompsumm <- data.frame(kfoldlme1malecomp)
# calculate mean kfold 0.9757576
mean(kfoldlme1malecomp$kfold[kfoldlme1malecomp$type=="obs"])

kfoldlme2summ <- data.frame(kfoldlme2malecomp)
# calculate mean kfold 0.8328306
mean(kfoldlme2malecomp$kfold[kfoldlme2malecomp$type=="obs"])

kfoldlme12summ <- data.frame(kfoldlme12malecomp)
# calculate mean kfold 0.9817256
mean(kfoldlme12malecomp$kfold[kfoldlme12malecomp$type=="obs"])


#######Model Averaging/Multimodel Inference for Success Models########
success_topmodsmalecomp <- aicoutputlme4randommalecomp %>% 
  filter(Delta_AICc < 2)

# Model Averaged Estimates with Shrinkage
## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
success_topmodsmalecomp
cands.successmalecomp <- list(lme2malecomp, lme1malecomp, lme12malecomp)
candname.successmalecomp <- c("lme2malecomp","lme1malecomp","lme12malecomp")
summary(lme1fem)
s_ModAvgCoefficientsmalecomp <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8", "slope", "d2stream", "d2moistsoi", "d2upyoungf","d2ag","elevation" )
success_int.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "(Intercept)")
success_habitatcl1.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl1")
success_habitatcl2.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl2")
success_habitatcl4.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl4")
success_habitatcl5.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl5")
success_habitatcl6.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl6")
success_habitatcl7.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl7")
success_habitatcl8.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "Habitat_Cl8")
success_slope.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "slope")
success_d2stream.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "d2stream")
success_d2moistsoi.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "d2moistsoi")
success_d2upyoungf.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "d2upyoungf")
success_d2ag.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "d2ag")
success_elevation.shrinkmalecomp <- modavgShrink(cand.set = cands.successmalecomp, modnames = candname.successmalecomp, parm = "elevation")

success_est.shrinkmalecomp <- c(success_int.shrinkmalecomp$Mod.avg.beta, success_habitatcl1.shrinkmalecomp$Mod.avg.beta, success_habitatcl2.shrinkmalecomp$Mod.avg.beta,success_habitatcl4.shrinkmalecomp$Mod.avg.beta, success_habitatcl5.shrinkmalecomp$Mod.avg.beta,success_habitatcl6.shrinkmalecomp$Mod.avg.beta, success_habitatcl7.shrinkmalecomp$Mod.avg.beta,success_habitatcl8.shrinkmalecomp$Mod.avg.beta,success_slope.shrinkmalecomp$Mod.avg.beta, success_d2stream.shrinkmalecomp$Mod.avg.beta,success_d2moistsoi.shrinkmalecomp$Mod.avg.beta,success_d2upyoungf.shrinkmalecomp$Mod.avg.beta,success_d2ag.shrinkmalecomp$Mod.avg.beta,success_elevation.shrinkmalecomp$Mod.avg.beta)

success_SE.shrinkmalecomp <- c(success_int.shrinkmalecomp$Uncond.SE, success_habitatcl1.shrinkmalecomp$Uncond.SE, success_habitatcl2.shrinkmalecomp$Uncond.SE,success_habitatcl4.shrinkmalecomp$Uncond.SE, success_habitatcl5.shrinkmalecomp$Uncond.SE,success_habitatcl6.shrinkmalecomp$Uncond.SE, success_habitatcl7.shrinkmalecomp$Uncond.SE,success_habitatcl8.shrinkmalecomp$Uncond.SE,success_slope.shrinkmalecomp$Uncond.SE, success_d2stream.shrinkmalecomp$Uncond.SE,success_d2moistsoi.shrinkmalecomp$Uncond.SE,success_d2upyoungf.shrinkmalecomp$Uncond.SE,success_d2ag.shrinkmalecomp$Uncond.SE,success_elevation.shrinkmalecomp$Uncond.SE)

success_LowCL.shrinkmalecomp <- c(success_int.shrinkmalecomp$Lower.CL, success_habitatcl1.shrinkmalecomp$Lower.CL, success_habitatcl2.shrinkmalecomp$Lower.CL,success_habitatcl4.shrinkmalecomp$Lower.CL, success_habitatcl5.shrinkmalecomp$Lower.CL,success_habitatcl6.shrinkmalecomp$Lower.CL, success_habitatcl7.shrinkmalecomp$Lower.CL,success_habitatcl8.shrinkmalecomp$Lower.CL, success_slope.shrinkmalecomp$Lower.CL,success_d2stream.shrinkmalecomp$Lower.CL, success_d2moistsoi.shrinkmalecomp$Lower.CL,success_d2upyoungf.shrinkmalecomp$Lower.CL,success_d2ag.shrinkmalecomp$Lower.CL,success_elevation.shrinkmalecomp$Lower.CL)

success_UpperCL.shrinkmalecomp <- c(success_int.shrinkmalecomp$Upper.CL, success_habitatcl1.shrinkmalecomp$Upper.CL, success_habitatcl2.shrinkmalecomp$Upper.CL,success_habitatcl4.shrinkmalecomp$Upper.CL, success_habitatcl5.shrinkmalecomp$Upper.CL,success_habitatcl6.shrinkmalecomp$Upper.CL, success_habitatcl7.shrinkmalecomp$Upper.CL,success_habitatcl8.shrinkmalecomp$Upper.CL, success_slope.shrinkmalecomp$Upper.CL,success_d2stream.shrinkmalecomp$Upper.CL, success_d2moistsoi.shrinkmalecomp$Upper.CL,success_d2upyoungf.shrinkmalecomp$Upper.CL,success_d2ag.shrinkmalecomp$Upper.CL,success_elevation.shrinkmalecomp$Upper.CL)

success_modavgcoefficients.shrinkmalecomp <- data.frame(s_ModAvgCoefficientsmalecomp, success_est.shrinkmalecomp, success_SE.shrinkmalecomp, success_LowCL.shrinkmalecomp, success_UpperCL.shrinkmalecomp)

# write_csv(success_modavgcoefficients.shrinkmalecomp, "ObjectsAndModels/20102012malecompmodelaverage.csv")



# #########make models for each study site over the 10 year period###############
# 
# #####Arcadia######
# lme1arcadia <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       elevation +
#                       slope + 
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# summary(lme1arcadia)
# # save model lme2 output (my top model) so that you don't have to run it again
# saveRDS(lme1arcadia, file = "./ObjectsAndModels/lme1arcadia.rds")
# 
# # read model back in to environment
# lme1arcadia <- readRDS("./ObjectsAndModels/lme1arcadia.rds")
# 
# 
# 
# # # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# # lme12warcadia <- glmer(use ~ Habitat_Cl +
# #                  d2ag +
# #                  d2stream +
# #                  d2upyoungf +
# #                  elevation +
# #                  slope + 
# #                  d2moistsoi+
# #                  (1|id),
# #                family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme12warcadia)
# 
# # # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # # change the weights column so that 2 is now 3
# # usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# # 
# # # make weights numeric
# # usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # class(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # lme13warcadia <- glmer(use ~ Habitat_Cl +
# #                   d2ag +
# #                   d2stream +
# #                   d2upyoungf +
# #                   elevation +
# #                   slope + 
# #                   d2moistsoi+
# #                   (1|id),
# #                 family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme13warcadia)
# 
# 
# # Roger's second top model now with id and slope as a random effect, model converges  
# lme2arcadia <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       slope +
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme2arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme2arcadia, file = "./ObjectsAndModels/lme2arcadia.rds")
# # read model back in to environment
# lme2arcadia <- readRDS("./ObjectsAndModels/lme2arcadia.rds")
# 
# # Roger's third top model now with id and slope as random effect 
# lme3arcadia<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      elevation +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme3arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme3arcadia, file = "./ObjectsAndModels/lme3arcadia.rds")
# # read model back in to environment
# lme3arcadia <- readRDS("./ObjectsAndModels/lme3arcadia.rds")
# 
# # Roger's fourth top model now with id and slope as a random effect
# lme4arcadia<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      d2moistsoi +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme4arcadia)
# # save model lme28 output (my top model) so that you don't have to run it again
# saveRDS(lme4arcadia, file = "./ObjectsAndModels/lme4arcadia.rds")
# # read model back in to environment
# lme4arcadia <- readRDS("./ObjectsAndModels/lme4arcadia.rds")
# 
# # Roger's fifth top model now with id and slope as a random effect
# lme5arcadia<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme5arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme5arcadia, file = "./ObjectsAndModels/lme5arcadia.rds")
# # read model back in to environment
# lme5arcadia <- readRDS("./ObjectsAndModels/lme5arcadia.rds")
# 
# # Roger's sixth top model now with id and slope as a random effect 
# lme6arcadia<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme6arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme6arcadia, file = "./ObjectsAndModels/lme6arcadia.rds")
# # read model back in to environment
# lme6arcadia <- readRDS("./ObjectsAndModels/lme6arcadia.rds")
# 
# # Roger's seventh top model now with id and slope as a random effect 
# lme7arcadia<- glmer(use ~ Habitat_Cl +
#                      elevation+
#                      d2ag +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme7arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme7arcadia, file = "./ObjectsAndModels/lme7arcadia.rds")
# # read model back in to environment
# lme7arcadia <- readRDS("./ObjectsAndModels/lme7arcadia.rds")
# 
# # Roger's eigth top model now with id and slope as a random effect 
# lme8arcadia<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme8arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme8arcadia, file = "./ObjectsAndModels/lme8arcadia.rds")
# # read model back in to environment
# lme8arcadia <- readRDS("./ObjectsAndModels/lme8arcadia.rds")
# 
# # Roger's ninth top model now with id and slope as a random effect 
# lme9arcadia<- glmer(use ~ 
#                      elevation+
#                      slope+
#                      d2ag +
#                      d2stream +
#                      d2upyoungf+
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme9arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme9arcadia, file = "./ObjectsAndModels/lme9arcadia.rds")
# # read model back in to environment
# lme9arcadia <- readRDS("./ObjectsAndModels/lme9arcadia.rds")
# 
# # Roger's tenth top model now with id and slope as a random effect 
# lme10arcadia<- glmer(use ~ 
#                       slope+
#                       d2ag +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme10arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme10arcadia, file = "./ObjectsAndModels/lme10arcadia.rds")
# # read model back in to environment
# lme10arcadia <- readRDS("./ObjectsAndModels/lme10arcadia.rds")
# 
# # Roger's eleventh top model now with id and slope as a random effect 
# lme11arcadia<- glmer(use ~ 
#                       d2ag +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme11arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme11arcadia, file = "./ObjectsAndModels/lme11arcadia.rds")
# # read model back in to environment
# lme11arcadia <- readRDS("./ObjectsAndModels/lme11arcadia.rds")
# 
# # Roger's twelfth top model now with id and site as a random effect 
# lme12arcadia<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme12arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme12arcadia, file = "./ObjectsAndModels/lme12arcadia.rds")
# # read model back in to environment
# lme12arcadia <- readRDS("./ObjectsAndModels/lme12arcadia.rds")
# 
# # Roger's thirteenth top model now with id and slope as a random effect 
# lme13arcadia<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme13arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme13arcadia, file = "./ObjectsAndModels/lme13arcadia.rds")
# # read model back in to environment
# lme13arcadia <- readRDS("./ObjectsAndModels/lme13arcadia.rds")
# 
# # Roger's fourteenth top model now with id and slope as a random effect 
# lme14arcadia<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme14arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme14arcadia, file = "./ObjectsAndModels/lme14arcadia.rds")
# # read model back in to environment
# lme14arcadia <- readRDS("./ObjectsAndModels/lme14arcadia.rds")
# 
# # Roger's fifteenth top model now with id and slope as a random effect 
# lme15arcadia<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="1"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme15arcadia)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme15arcadia, file = "./ObjectsAndModels/lme15arcadia.rds")
# # read model back in to environment
# lme15arcadia <- readRDS("./ObjectsAndModels/lme15arcadia.rds")
# 
# # make list of models
# u_modroglme4randomarcadia <- list(lme1arcadia, lme2arcadia, lme3arcadia, lme4arcadia, lme5arcadia, lme6arcadia, lme7arcadia, lme8arcadia, lme9arcadia, lme10arcadia, lme11arcadia, lme12arcadia,lme13arcadia, lme14arcadia, lme15arcadia)
# #
# # name models
# u_modnameroglme4randomarcadia <- c("lme1arcadia", "lme2arcadia", "lme3arcadia", "lme4arcadia", "lme5arcadia", "lme6arcadia", "lme7arcadia", "lme8arcadia", "lme9arcadia", "lme10arcadia", "lme11arcadia", "lme12arcadia","lme13arcadia", "lme14arcadia", "lme15arcadia")
# #
# # run AIC on models
# aictab(u_modroglme4randomarcadia, modnames = u_modnameroglme4randomarcadia, second.ord = TRUE)
# 
# # there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 
# 
# # output AIC table
# aicoutputlme4randomarcadia <- data.frame(aictab(u_modroglme4randomarcadia, modnames = u_modnameroglme4randomarcadia))
# 
# need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC
#
#
# # kfold cross validation for model lme2arcadia
# kfoldlme2arcadia <- kfoldRSF(
#   lme2arcadia,
#   k = 5,
#   nrepet = 3,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
#
# # save kfoldlme2arcadia output so that you don't have to run it again
# saveRDS(kfoldlme2arcadia, file = "./ObjectsAndModels/kfoldlme2arcadia.rds")
#
# # read model back in to environment
# kfoldlme2arcadia <- readRDS("./ObjectsAndModels/kfoldlme2arcadia.rds")
# # check summary
# summary(kfoldlme2arcadia)
#
# # kfold cross validation for model lme1arcadia
# kfoldlme1arcadia <- kfoldRSF(
#   lme1arcadia,
#   k = 5,
#   nrepet = 3,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
#
# # save model kfoldlme1arcadia so that you don't have to run it again
# saveRDS(kfoldlme1arcadia, file = "./ObjectsAndModels/kfoldlme1arcadia.rds")
#
# # read model back in to environment
# kfoldlme1arcadia <- readRDS("./ObjectsAndModels/kfoldlme1arcadia.rds")
# # check summary
# summary(kfoldlme1arcadia)
#
# # make kfold summaries into a dataframe
# kfoldlme1arcadiasumm <- data.frame(kfoldlme1arcadia)
# # calculate mean kfold
# mean(kfoldlme1arcadia$kfold[kfoldlme1arcadia$type=="obs"])
#
# kfoldlme2arcadiasumm <- data.frame(kfoldlme2arcadia)
# # calculate mean kfold
# mean(kfoldlme2arcadia$kfold[kfoldlme2arcadia$type=="obs"])

# #######Model Averaging/Multimodel Inference for Success Models########
# success_topmodsarcadia <- aicoutputlme4randomarcadia %>% 
#   filter(Delta_AICc < 2)
# 
# # Model Averaged Estimates with Shrinkage
# ## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
# success_topmodsarcadia
# cands.successarcadia <- list(lme2arcadia, lme1arcadia)
# candname.successarcadia <- c("lme2arcadia","lme1arcadia")
# 
# s_ModAvgCoefficientsarcadia <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8","slope", "d2stream", "d2moistsoi", "d2upyoungf","d2ag","elevation" )
# success_int.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "(Intercept)")
# success_habitatcl1.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl1")
# success_habitatcl2.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl2")
# success_habitatcl4.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl4")
# success_habitatcl5.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl5")
# success_habitatcl6.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl6")
# success_habitatcl7.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl7")
# success_habitatcl8.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "Habitat_Cl8")
# success_slope.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "slope")
# success_d2stream.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "d2stream")
# success_d2moistsoi.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "d2moistsoi")
# success_d2upyoungf.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "d2upyoungf")
# success_d2ag.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "d2ag")
# success_elevation.shrinkarcadia <- modavgShrink(cand.set = cands.successarcadia, modnames = candname.successarcadia, parm = "elevation")
# 
# success_est.shrinkarcadia <- c(success_int.shrinkarcadia$Mod.avg.beta, success_habitatcl1.shrinkarcadia$Mod.avg.beta, success_habitatcl2.shrinkarcadia$Mod.avg.beta,success_habitatcl4.shrinkarcadia$Mod.avg.beta, success_habitatcl5.shrinkarcadia$Mod.avg.beta,success_habitatcl6.shrinkarcadia$Mod.avg.beta, success_habitatcl7.shrinkarcadia$Mod.avg.beta,success_habitatcl8.shrinkarcadia$Mod.avg.beta,success_slope.shrinkarcadia$Mod.avg.beta, success_d2stream.shrinkarcadia$Mod.avg.beta,success_d2moistsoi.shrinkarcadia$Mod.avg.beta,success_d2upyoungf.shrinkarcadia$Mod.avg.beta,success_d2ag.shrinkarcadia$Mod.avg.beta,success_elevation.shrinkarcadia$Mod.avg.beta)
# 
# success_SE.shrinkarcadia <- c(success_int.shrinkarcadia$Uncond.SE, success_habitatcl1.shrinkarcadia$Uncond.SE, success_habitatcl2.shrinkarcadia$Uncond.SE,success_habitatcl4.shrinkarcadia$Uncond.SE, success_habitatcl5.shrinkarcadia$Uncond.SE,success_habitatcl6.shrinkarcadia$Uncond.SE, success_habitatcl7.shrinkarcadia$Uncond.SE,success_habitatcl8.shrinkarcadia$Uncond.SE,success_slope.shrinkarcadia$Uncond.SE, success_d2stream.shrinkarcadia$Uncond.SE,success_d2moistsoi.shrinkarcadia$Uncond.SE,success_d2upyoungf.shrinkarcadia$Uncond.SE,success_d2ag.shrinkarcadia$Uncond.SE,success_elevation.shrinkarcadia$Uncond.SE)
# 
# success_LowCL.shrinkarcadia <- c(success_int.shrinkarcadia$Lower.CL, success_habitatcl1.shrinkarcadia$Lower.CL, success_habitatcl2.shrinkarcadia$Lower.CL,success_habitatcl4.shrinkarcadia$Lower.CL, success_habitatcl5.shrinkarcadia$Lower.CL,success_habitatcl6.shrinkarcadia$Lower.CL, success_habitatcl7.shrinkarcadia$Lower.CL,success_habitatcl8.shrinkarcadia$Lower.CL, success_slope.shrinkarcadia$Lower.CL,success_d2stream.shrinkarcadia$Lower.CL, success_d2moistsoi.shrinkarcadia$Lower.CL,success_d2upyoungf.shrinkarcadia$Lower.CL,success_d2ag.shrinkarcadia$Lower.CL,success_elevation.shrinkarcadia$Lower.CL)
# 
# success_UpperCL.shrinkarcadia <- c(success_int.shrinkarcadia$Upper.CL, success_habitatcl1.shrinkarcadia$Upper.CL, success_habitatcl2.shrinkarcadia$Upper.CL,success_habitatcl4.shrinkarcadia$Upper.CL, success_habitatcl5.shrinkarcadia$Upper.CL,success_habitatcl6.shrinkarcadia$Upper.CL, success_habitatcl7.shrinkarcadia$Upper.CL,success_habitatcl8.shrinkarcadia$Upper.CL, success_slope.shrinkarcadia$Upper.CL,success_d2stream.shrinkarcadia$Upper.CL, success_d2moistsoi.shrinkarcadia$Upper.CL,success_d2upyoungf.shrinkarcadia$Upper.CL,success_d2ag.shrinkarcadia$Upper.CL,success_elevation.shrinkarcadia$Upper.CL)
# 
# success_modavgcoefficients.shrinkarcadia <- data.frame(s_ModAvgCoefficientsarcadia, success_est.shrinkarcadia, success_LowCL.shrinkarcadia, success_UpperCL.shrinkarcadia,success_SE.shrinkarcadia)
# 
# # write_csv(success_modavgcoefficients.shrinkarcadia, "ObjectsAndModels/arcadiamodelaverage.csv")
# 
# ########Big River#########
# lme1bigriver <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       elevation +
#                       slope + 
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# summary(lme1bigriver)
# # save model lme2 output (my top model) so that you don't have to run it again
# saveRDS(lme1bigriver, file = "./ObjectsAndModels/lme1bigriver.rds")
# 
# # read model back in to environment
# lme1bigriver <- readRDS("./ObjectsAndModels/lme1bigriver.rds")
# 
# 
# 
# # # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# # lme12wbigriver <- glmer(use ~ Habitat_Cl +
# #                  d2ag +
# #                  d2stream +
# #                  d2upyoungf +
# #                  elevation +
# #                  slope + 
# #                  d2moistsoi+
# #                  (1|id),
# #                family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme12wbigriver)
# 
# # # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # # change the weights column so that 2 is now 3
# # usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# # 
# # # make weights numeric
# # usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # class(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # lme13wbigriver <- glmer(use ~ Habitat_Cl +
# #                   d2ag +
# #                   d2stream +
# #                   d2upyoungf +
# #                   elevation +
# #                   slope + 
# #                   d2moistsoi+
# #                   (1|id),
# #                 family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme13wbigriver)
# 
# 
# # Roger's bigriver top model now with id and slope as a random effect, model converges  
# lme2bigriver <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       slope +
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme2bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme2bigriver, file = "./ObjectsAndModels/lme2bigriver.rds")
# # read model back in to environment
# lme2bigriver <- readRDS("./ObjectsAndModels/lme2bigriver.rds")
# 
# # Roger's third top model now with id and slope as random effect 
# lme3bigriver<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      elevation +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme3bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme3bigriver, file = "./ObjectsAndModels/lme3bigriver.rds")
# # read model back in to environment
# lme3bigriver <- readRDS("./ObjectsAndModels/lme3bigriver.rds")
# 
# # Roger's fourth top model now with id and slope as a random effect 
# lme4bigriver<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      d2moistsoi +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme4bigriver)
# # save model lme28 output (my top model) so that you don't have to run it again
# saveRDS(lme4bigriver, file = "./ObjectsAndModels/lme4bigriver.rds")
# # read model back in to environment
# lme4bigriver <- readRDS("./ObjectsAndModels/lme4bigriver.rds")
# 
# # Roger's fifth top model now with id and slope as a random effect
# lme5bigriver<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme5bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme5bigriver, file = "./ObjectsAndModels/lme5bigriver.rds")
# # read model back in to environment
# lme5bigriver <- readRDS("./ObjectsAndModels/lme5bigriver.rds")
# 
# # Roger's sixth top model now with id and slope as a random effect 
# lme6bigriver<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme6bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme6bigriver, file = "./ObjectsAndModels/lme6bigriver.rds")
# # read model back in to environment
# lme6bigriver <- readRDS("./ObjectsAndModels/lme6bigriver.rds")
# 
# # Roger's seventh top model now with id and slope as a random effect 
# lme7bigriver<- glmer(use ~ Habitat_Cl +
#                      elevation+
#                      d2ag +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme7bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme7bigriver, file = "./ObjectsAndModels/lme7bigriver.rds")
# # read model back in to environment
# lme7bigriver <- readRDS("./ObjectsAndModels/lme7bigriver.rds")
# 
# # Roger's eigth top model now with id and slope as a random effect 
# lme8bigriver<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme8bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme8bigriver, file = "./ObjectsAndModels/lme8bigriver.rds")
# # read model back in to environment
# lme8bigriver <- readRDS("./ObjectsAndModels/lme8bigriver.rds")
# 
# # Roger's ninth top model now with id and slope as a random effect 
# lme9bigriver<- glmer(use ~ 
#                      elevation+
#                      slope+
#                      d2ag +
#                      d2stream +
#                      d2upyoungf+
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme9bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme9bigriver, file = "./ObjectsAndModels/lme9bigriver.rds")
# # read model back in to environment
# lme9bigriver <- readRDS("./ObjectsAndModels/lme9bigriver.rds")
# 
# # Roger's tenth top model now with id and slope as a random effect 
# lme10bigriver<- glmer(use ~ 
#                       slope+
#                       d2ag +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme10bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme10bigriver, file = "./ObjectsAndModels/lme10bigriver.rds")
# # read model back in to environment
# lme10bigriver <- readRDS("./ObjectsAndModels/lme10bigriver.rds")
# 
# # Roger's eleventh top model now with id and slope as a random effect 
# lme11bigriver<- glmer(use ~ 
#                       d2ag +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme11bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme11bigriver, file = "./ObjectsAndModels/lme11bigriver.rds")
# # read model back in to environment
# lme11bigriver <- readRDS("./ObjectsAndModels/lme11bigriver.rds")
# 
# # Roger's twelfth top model now with id and site as a random effect 
# lme12bigriver<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme12bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme12bigriver, file = "./ObjectsAndModels/lme12bigriver.rds")
# # read model back in to environment
# lme12bigriver <- readRDS("./ObjectsAndModels/lme12bigriver.rds")
# 
# # Roger's thirteenth top model now with id and slope as a random effect 
# lme13bigriver<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme13bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme13bigriver, file = "./ObjectsAndModels/lme13bigriver.rds")
# # read model back in to environment
# lme13bigriver <- readRDS("./ObjectsAndModels/lme13bigriver.rds")
# 
# # Roger's fourteenth top model now with id and slope as a random effect 
# lme14bigriver<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme14bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme14bigriver, file = "./ObjectsAndModels/lme14bigriver.rds")
# # read model back in to environment
# lme14bigriver <- readRDS("./ObjectsAndModels/lme14bigriver.rds")
# 
# # Roger's fifteenth top model now with id and slope as a random effect 
# lme15bigriver<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="2"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme15bigriver)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme15bigriver, file = "./ObjectsAndModels/lme15bigriver.rds")
# # read model back in to environment
# lme15bigriver <- readRDS("./ObjectsAndModels/lme15bigriver.rds")
# 
# # make list of models
# u_modroglme4randombigriver <- list(lme1bigriver, lme2bigriver, lme3bigriver, lme4bigriver, lme5bigriver, lme6bigriver, lme7bigriver, lme8bigriver, lme9bigriver, lme10bigriver, lme11bigriver, lme12bigriver,lme13bigriver, lme14bigriver, lme15bigriver)
# #
# # name models
# u_modnameroglme4randombigriver <- c("lme1bigriver", "lme2bigriver", "lme3bigriver", "lme4bigriver", "lme5bigriver", "lme6bigriver", "lme7bigriver", "lme8bigriver", "lme9bigriver", "lme10bigriver", "lme11bigriver", "lme12bigriver","lme13bigriver", "lme14bigriver", "lme15bigriver")
# #
# # run AIC on models
# aictab(u_modroglme4randombigriver, modnames = u_modnameroglme4randombigriver, second.ord = TRUE)
# 
# # there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 
# 
# # output AIC table
# aicoutputlme4randombigriver <- data.frame(aictab(u_modroglme4randombigriver, modnames = u_modnameroglme4randombigriver))
# 
# #######Great Swamp#########
# lme1gswamp <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       elevation +
#                       slope + 
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="nloptwrap",optCtrl=list("NLOPT_LN_BOBYQA")))
# 
# # try pickinng up where model did not converge, still did not converge
# ss_lme1gswamp <- getME(lme1gswamp,c("theta","fixef"))
# lme1gswampv2 <- update(lme1gswamp, start = ss_lme1gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# # try again picking up from last try, model converged
# ss_lme1gswampv3 <- getME(lme1gswampv2,c("theta","fixef"))
# lme1gswampv3 <- update(lme1gswampv2, start = ss_lme1gswampv3, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme1gswampv3)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme1gswampv3, file = "./ObjectsAndModels/lme1gswampv3.rds")
# # read model back in to environment
# lme1gswampv3 <- readRDS("./ObjectsAndModels/lme1gswampv3.rds")
# 
# 
# # # with 2x weights on available sample, coefficient estimates do not move besides like the thousandths place
# # lme12wgswamp <- glmer(use ~ Habitat_Cl +
# #                  d2ag +
# #                  d2stream +
# #                  d2upyoungf +
# #                  elevation +
# #                  slope + 
# #                  d2moistsoi+
# #                  (1|id),
# #                family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme12wgswamp)
# 
# # # with 3x weights on available sample, coefficients do not move besides like the thousandths place.... I didn't bother doing 3x this time around
# # # change the weights column so that 2 is now 3
# # usedandavailnumberhabitatwoTilscaled %>% weights== ("2"<-"3")
# # 
# # # make weights numeric
# # usedandavailnumberhabitatwoTilscaled$weights <- as.numeric(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # class(usedandavailnumberhabitatwoTilscaled$weights)
# # 
# # lme13wgswamp <- glmer(use ~ Habitat_Cl +
# #                   d2ag +
# #                   d2stream +
# #                   d2upyoungf +
# #                   elevation +
# #                   slope + 
# #                   d2moistsoi+
# #                   (1|id),
# #                 family = binomial(link = "logit"), data = usedandavailnumberhabitatwoTilscaled, verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)), weights=weights)
# # 
# # summary(lme13wgswamp)
# 
# 
# # Roger's gswamp second top model now with id and slope as a random effect, model converges  
# lme2gswamp <- glmer(use ~ Habitat_Cl +
#                       d2ag +
#                       d2stream +
#                       d2upyoungf +
#                       slope +
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# # try pickinng up where model did not converge
# ss_lme2gswamp <- getME(lme2gswamp,c("theta","fixef"))
# lme2gswampv2 <- update(lme2gswamp, start = ss_lme2gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme2gswampv2)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme2gswampv2, file = "./ObjectsAndModels/lme2gswampv2.rds")
# # read model back in to environment
# lme2gswampv2 <- readRDS("./ObjectsAndModels/lme2gswampv2.rds")
# 
# # Roger's third top model now with id and slope as random effect 
# lme3gswamp<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      elevation +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# # try picking up where model did not converge
# ss_lme3gswamp <- getME(lme3gswamp,c("theta","fixef"))
# lme3gswampv2 <- update(lme3gswamp, start = ss_lme3gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme3gswampv2)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme3gswampv2, file = "./ObjectsAndModels/lme3gswampv2.rds")
# # read model back in to environment
# lme3gswampv2 <- readRDS("./ObjectsAndModels/lme3gswampv2.rds")
# 
# # Roger's fourth top model now with id and slope as a random effect 
# lme4gswamp<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      d2moistsoi +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme4gswamp)
# # save model lme28 output (my top model) so that you don't have to run it again
# saveRDS(lme4gswamp, file = "./ObjectsAndModels/lme4gswamp.rds")
# # read model back in to environment
# lme4gswamp <- readRDS("./ObjectsAndModels/lme4gswamp.rds")
# 
# # Roger's fifth top model now with id and slope as a random effect
# lme5gswamp<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2stream +
#                      d2upyoungf +
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme5gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme5gswamp, file = "./ObjectsAndModels/lme5gswamp.rds")
# # read model back in to environment
# lme5gswamp <- readRDS("./ObjectsAndModels/lme5gswamp.rds")
# 
# # Roger's sixth top model now with id and slope as a random effect 
# lme6gswamp<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme6gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme6gswamp, file = "./ObjectsAndModels/lme6gswamp.rds")
# # read model back in to environment
# lme6gswamp <- readRDS("./ObjectsAndModels/lme6gswamp.rds")
# 
# # Roger's seventh top model now with id and slope as a random effect 
# lme7gswamp<- glmer(use ~ Habitat_Cl +
#                      elevation+
#                      d2ag +
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme7gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme7gswamp, file = "./ObjectsAndModels/lme7gswamp.rds")
# # read model back in to environment
# lme7gswamp <- readRDS("./ObjectsAndModels/lme7gswamp.rds")
# 
# # Roger's eigth top model now with id and slope as a random effect 
# lme8gswamp<- glmer(use ~ Habitat_Cl +
#                      d2ag +
#                      d2upyoungf+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme8gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme8gswamp, file = "./ObjectsAndModels/lme8gswamp.rds")
# # read model back in to environment
# lme8gswamp <- readRDS("./ObjectsAndModels/lme8gswamp.rds")
# 
# # Roger's ninth top model now with id and slope as a random effect 
# lme9gswamp<- glmer(use ~ 
#                      elevation+
#                      slope+
#                      d2ag +
#                      d2stream +
#                      d2upyoungf+
#                      d2moistsoi+
#                      (1|id),
#                    family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme9gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme9gswamp, file = "./ObjectsAndModels/lme9gswamp.rds")
# # read model back in to environment
# lme9gswamp <- readRDS("./ObjectsAndModels/lme9gswamp.rds")
# 
# # Roger's tenth top model now with id and slope as a random effect 
# lme10gswamp<- glmer(use ~ 
#                       slope+
#                       d2ag +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme10gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme10gswamp, file = "./ObjectsAndModels/lme10gswamp.rds")
# # read model back in to environment
# lme10gswamp <- readRDS("./ObjectsAndModels/lme10gswamp.rds")
# 
# # Roger's eleventh top model now with id and slope as a random effect 
# lme11gswamp<- glmer(use ~ 
#                       d2ag +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme11gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme11gswamp, file = "./ObjectsAndModels/lme11gswamp.rds")
# # read model back in to environment
# lme11gswamp <- readRDS("./ObjectsAndModels/lme11gswamp.rds")
# 
# # Roger's twelfth top model now with id and site as a random effect 
# lme12gswamp<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# # try picking up where model did not converge
# ss_lme12gswamp <- getME(lme12gswamp,c("theta","fixef"))
# lme12gswampv2 <- update(lme12gswamp, start = ss_lme12gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme12gswampv2)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme12gswampv2, file = "./ObjectsAndModels/lme12gswampv2.rds")
# # read model back in to environment
# lme12gswampv2 <- readRDS("./ObjectsAndModels/lme12gswampv2.rds")
# 
# # Roger's thirteenth top model now with id and slope as a random effect 
# lme13gswamp<- glmer(use ~ 
#                       slope+
#                       Habitat_Cl +
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme13gswamp)
# 
# # try picking up where model did not converge
# ss_lme13gswamp <- getME(lme13gswamp,c("theta","fixef"))
# lme13gswampv2 <- update(lme13gswamp, start = ss_lme13gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme13gswampv2)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme13gswampv2, file = "./ObjectsAndModels/lme13gswampv2.rds")
# # read model back in to environment
# lme13gswampv2 <- readRDS("./ObjectsAndModels/lme13gswampv2.rds")
# 
# # Roger's fourteenth top model now with id and slope as a random effect 
# lme14gswamp<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       d2moistsoi+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# # try picking up where model did not converge
# ss_lme14gswamp <- getME(lme14gswamp,c("theta","fixef"))
# lme14gswampv2 <- update(lme14gswamp, start = ss_lme14gswamp, control = glmerControl(optCtrl = list(maxfun = 2e4)))
# 
# summary(lme14gswampv2)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme14gswampv2, file = "./ObjectsAndModels/lme14gswampv2.rds")
# # read model back in to environment
# lme14gswampv2 <- readRDS("./ObjectsAndModels/lme14gswampv2.rds")
# 
# # Roger's fifteenth top model now with id and slope as a random effect 
# lme15gswamp<- glmer(use ~ 
#                       Habitat_Cl+
#                       d2stream +
#                       d2upyoungf+
#                       (1|id),
#                     family = binomial(link = "logit"), data = filter((usedandavailnumberhabitatwoTilscaled), site=="3"), verbose=1, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
# 
# summary(lme15gswamp)
# # save model lme26 output (my top model) so that you don't have to run it again
# saveRDS(lme15gswamp, file = "./ObjectsAndModels/lme15gswamp.rds")
# # read model back in to environment
# lme15gswamp <- readRDS("./ObjectsAndModels/lme15gswamp.rds")
# 
# # make list of models
# u_modroglme4randomgswamp <- list(lme1gswampv3, lme2gswampv2, lme3gswampv2, lme4gswamp, lme5gswamp, lme6gswamp, lme7gswamp, lme8gswamp, lme9gswamp, lme10gswamp, lme11gswamp, lme12gswampv2,lme13gswampv2, lme14gswampv2, lme15gswamp)
# #
# # name models
# u_modnameroglme4randomgswamp <- c("lme1gswampv3", "lme2gswampv2", "lme3gswampv2", "lme4gswamp", "lme5gswamp", "lme6gswamp", "lme7gswamp", "lme8gswamp", "lme9gswamp", "lme10gswamp", "lme11gswamp", "lme12gswampv2","lme13gswampv2", "lme14gswampv2", "lme15gswamp")
# #
# # run AIC on models
# aictab(u_modroglme4randomgswamp, modnames = u_modnameroglme4randomgswamp, second.ord = TRUE)
# 
# # there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 
# 
# # output AIC table
# aicoutputlme4randomgswamp <- data.frame(aictab(u_modroglme4randomgswamp, modnames = u_modnameroglme4randomgswamp))


# # need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC
# 
# 
# # kfold cross validation for model lme1gswampv3
# kfoldlme1gswampv3 <- kfoldRSF(
#   lme1gswampv3,
#   k = 5,
#   nrepet = 3,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save model kfoldlme1gswampv3 so that you don't have to run it again
# saveRDS(kfoldlme1gswampv3, file = "./ObjectsAndModels/kfoldlme1gswampv3.rds")
# 
# # read model back in to environment
# kfoldlme1gswampv3 <- readRDS("./ObjectsAndModels/kfoldlme1gswampv3.rds")
# # check summary
# summary(kfoldlme1gswampv3)
# 
# # kfold cross validation for model lme2gswampv2 
# kfoldlme2gswampv2 <- kfoldRSF(
#   lme2gswampv2,
#   k = 5,
#   nrepet = 3,
#   nbins = 10,
#   jitter = TRUE,
#   random = TRUE,
#   method = method,
#   x = m,
#   form_ls = ls,
#   reproducible = TRUE)
# 
# # save model kfoldlme2gswampv2 so that you don't have to run it again
# saveRDS(kfoldlme2gswampv2, file = "./ObjectsAndModels/kfoldlme2gswampv2.rds")
# 
# # read model back in to environment
# kfoldlme2gswampv2 <- readRDS("./ObjectsAndModels/kfoldlme2gswampv2.rds")
# # check summary
# summary(kfoldlme2gswampv2)
# 
# # make kfold summaries into a dataframe
# kfoldlme1gswampv3summ <- data.frame(kfoldlme1gswampv3)
# # calculate mean kfold 
# mean(kfoldlme1gswampv3$kfold[kfoldlme1gswampv3$type=="obs"])
# 
# kfoldlme2gswampv2summ <- data.frame(kfoldlme2gswampv2)
# # calculate mean kfold 
# mean(kfoldlme2gswampv2$kfold[kfoldlme2gswampv2$type=="obs"])

# #######Model Averaging/Multimodel Inference for Success Models########
# success_topmodsgswamp <- aicoutputlme4randomgswamp %>% 
#   filter(Delta_AICc < 2)
# 
# # Model Averaged Estimates with Shrinkage
# ## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
# success_topmodsgswamp
# cands.successgswamp <- list(lme2gswampv2, lme1gswampv3)
# candname.successgswamp <- c("lme2gswampv2","lme1gswampv3")
# 
# s_ModAvgCoefficientsgswamp <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8", "slope", "d2stream", "d2moistsoi", "d2upyoungf", "d2ag","elevation" )
# success_int.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "(Intercept)")
# success_habitatcl1.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl1")
# success_habitatcl2.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl2")
# success_habitatcl4.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl4")
# success_habitatcl5.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl5")
# success_habitatcl6.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl6")
# success_habitatcl7.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl7")
# success_habitatcl8.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "Habitat_Cl8")
# success_slope.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "slope")
# success_d2stream.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "d2stream")
# success_d2moistsoi.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "d2moistsoi")
# success_d2upyoungf.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "d2upyoungf")
# success_d2ag.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "d2ag")
# success_elevation.shrinkgswamp <- modavgShrink(cand.set = cands.successgswamp, modnames = candname.successgswamp, parm = "elevation")
# 
# success_est.shrinkgswamp <- c(success_int.shrinkgswamp$Mod.avg.beta, success_habitatcl1.shrinkgswamp$Mod.avg.beta, success_habitatcl2.shrinkgswamp$Mod.avg.beta,success_habitatcl4.shrinkgswamp$Mod.avg.beta, success_habitatcl5.shrinkgswamp$Mod.avg.beta,success_habitatcl6.shrinkgswamp$Mod.avg.beta, success_habitatcl7.shrinkgswamp$Mod.avg.beta,success_habitatcl8.shrinkgswamp$Mod.avg.beta,success_slope.shrinkgswamp$Mod.avg.beta, success_d2stream.shrinkgswamp$Mod.avg.beta,success_d2moistsoi.shrinkgswamp$Mod.avg.beta,success_d2upyoungf.shrinkgswamp$Mod.avg.beta,success_d2ag.shrinkgswamp$Mod.avg.beta,success_elevation.shrinkgswamp$Mod.avg.beta)
# 
# success_SE.shrinkgswamp <- c(success_int.shrinkgswamp$Uncond.SE, success_habitatcl1.shrinkgswamp$Uncond.SE, success_habitatcl2.shrinkgswamp$Uncond.SE,success_habitatcl4.shrinkgswamp$Uncond.SE, success_habitatcl5.shrinkgswamp$Uncond.SE,success_habitatcl6.shrinkgswamp$Uncond.SE, success_habitatcl7.shrinkgswamp$Uncond.SE,success_habitatcl8.shrinkgswamp$Uncond.SE,success_slope.shrinkgswamp$Uncond.SE, success_d2stream.shrinkgswamp$Uncond.SE,success_d2moistsoi.shrinkgswamp$Uncond.SE,success_d2upyoungf.shrinkgswamp$Uncond.SE,success_d2ag.shrinkgswamp$Uncond.SE,success_elevation.shrinkgswamp$Uncond.SE)
# 
# success_LowCL.shrinkgswamp <- c(success_int.shrinkgswamp$Lower.CL, success_habitatcl1.shrinkgswamp$Lower.CL, success_habitatcl2.shrinkgswamp$Lower.CL,success_habitatcl4.shrinkgswamp$Lower.CL, success_habitatcl5.shrinkgswamp$Lower.CL,success_habitatcl6.shrinkgswamp$Lower.CL, success_habitatcl7.shrinkgswamp$Lower.CL,success_habitatcl8.shrinkgswamp$Lower.CL, success_slope.shrinkgswamp$Lower.CL,success_d2stream.shrinkgswamp$Lower.CL, success_d2moistsoi.shrinkgswamp$Lower.CL,success_d2upyoungf.shrinkgswamp$Lower.CL,success_d2ag.shrinkgswamp$Lower.CL,success_elevation.shrinkgswamp$Lower.CL)
# 
# success_UpperCL.shrinkgswamp <- c(success_int.shrinkgswamp$Upper.CL, success_habitatcl1.shrinkgswamp$Upper.CL, success_habitatcl2.shrinkgswamp$Upper.CL,success_habitatcl4.shrinkgswamp$Upper.CL, success_habitatcl5.shrinkgswamp$Upper.CL,success_habitatcl6.shrinkgswamp$Upper.CL, success_habitatcl7.shrinkgswamp$Upper.CL,success_habitatcl8.shrinkgswamp$Upper.CL, success_slope.shrinkgswamp$Upper.CL,success_d2stream.shrinkgswamp$Upper.CL, success_d2moistsoi.shrinkgswamp$Upper.CL,success_d2upyoungf.shrinkgswamp$Upper.CL,success_d2ag.shrinkgswamp$Upper.CL,success_elevation.shrinkgswamp$Upper.CL)
# 
# success_modavgcoefficients.shrinkgswamp <- data.frame(s_ModAvgCoefficientsgswamp, success_est.shrinkgswamp, success_LowCL.shrinkgswamp, success_UpperCL.shrinkgswamp,success_SE.shrinkgswamp)
# 
# write_csv(success_modavgcoefficients.shrinkgswamp, "ObjectsAndModels/greatswampmodelaverage.csv")


###########make female comp models 2020/2021 for comparison w/ females (Arcadia, Big River, G Swamp, F carter)################
# the tryfemalesep contains the female tracked 2 years but entered correctly into the csv, it did not change estimates.
useandavailnumberhabitatfemales <- read.csv("./Data/20202021usedandavailpointsfemales.csv", stringsAsFactors = TRUE)
unique(useandavailnumberhabitatfemales$site)
# make certain columns factors
useandavailnumberhabitatfemales$Habitat_Cl <- as.factor(useandavailnumberhabitatfemales$Habitat_Cl)
useandavailnumberhabitatfemales$use <- as.numeric(useandavailnumberhabitatfemales$use)
useandavailnumberhabitatfemales$site <- as.factor(useandavailnumberhabitatfemales$site)
useandavailnumberhabitatfemales$year <- as.factor(useandavailnumberhabitatfemales$year)
useandavailnumberhabitatfemales$weights <- as.numeric(useandavailnumberhabitatfemales$weights)

# scale variables 
library(dplyr)
usedandavailnumberfemalesscaled <- useandavailnumberhabitatfemales %>%
  dplyr::mutate_at(c(5,6,7,8,9,10), funs(c(scale(.))))

# sites, 1,2,3,7 are included 
unique(usedandavailnumberfemalesscaled$site)

# make upland forest mixed the reference category for models
usedandavailnumberfemalesscaled$Habitat_Cl <- relevel(usedandavailnumberfemalesscaled$Habitat_Cl, ref="3")
unique(usedandavailnumberfemalesscaled$Habitat_Cl)

# test for correlations between all female continuous variables ----
## Pearson r < |0.5| = not correlated
#remove anything that has a correlation greater that 0.7 or 0.8
#no correlated variables

covariates <- usedandavailnumberfemalesscaled %>%
  select(d2ag,d2moistsoi,d2stream,d2upyoungf,elevation,slope)

# Generate correlation matrix, there is nothing correlated in the 2 year female dataset
flat_cor_mat <- function(cor_r, cor_p){
  library(tidyr)
  library(tibble)
  cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- gather(cor_r, column, cor, -1)
  cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- gather(cor_p, column, p, -1)
  cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
  cor_p_matrix
}

correlations <- rcorr(as.matrix(covariates[, 1:6]), type = "pearson")

cor_matrix <- flat_cor_mat(correlations$r, correlations$P)
head(cor_matrix)


# Roger's top model without weights on available sample, weighting by 2 only moved estimates by hundredths place
lme1fem <- glmer(use ~ Habitat_Cl +
                 d2ag +
                 d2stream +
                 d2upyoungf +
                 elevation +
                 slope + 
                 d2moistsoi+
                 (1|id)+(1|site),
               family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)
summary(lme1fem)
# save model lme2 output (my top model) so that you don't have to run it again
saveRDS(lme1fem, file = "./ObjectsAndModels/lme1fem.rds")

# read model back in to environment
lme1fem <- readRDS("./ObjectsAndModels/lme1fem.rds")
summary(lme1fem)

# Roger's second top model now with id and slope as a random effect, model converges  
lme2fem <- glmer(use ~ Habitat_Cl +
                 d2ag +
                 d2stream +
                 d2upyoungf +
                 slope +
                 d2moistsoi+
                 (1|id)+(1|site),
               family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme2fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme2fem, file = "./ObjectsAndModels/lme2fem.rds")
# read model back in to environment
lme2fem <- readRDS("./ObjectsAndModels/lme2fem.rds")

# Roger's third top model now with id and slope as random effect 
lme3fem<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                elevation +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme3fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme3fem, file = "./ObjectsAndModels/lme3fem.rds")
# read model back in to environment
lme3fem <- readRDS("./ObjectsAndModels/lme3fem.rds")

# Roger's fourth top model now with id and slope as a random effect 
lme4fem<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                d2moistsoi +
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme4fem)
# save model lme28 output (my top model) so that you don't have to run it again
saveRDS(lme4fem, file = "./ObjectsAndModels/lme4fem.rds")
# read model back in to environment
lme4fem <- readRDS("./ObjectsAndModels/lme4fem.rds")

# Roger's fifth top model now with id and slope as a random effect
lme5fem<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2stream +
                d2upyoungf +
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme5fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme5fem, file = "./ObjectsAndModels/lme5fem.rds")
# read model back in to environment
lme5fem <- readRDS("./ObjectsAndModels/lme5fem.rds")

# Roger's sixth top model now with id and slope as a random effect 
lme6fem<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2upyoungf +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme6fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme6fem, file = "./ObjectsAndModels/lme6fem.rds")
# read model back in to environment
lme6fem <- readRDS("./ObjectsAndModels/lme6fem.rds")

# Roger's seventh top model now with id and slope as a random effect 
lme7fem<- glmer(use ~ Habitat_Cl +
                elevation+
                d2ag +
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme7fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme7fem, file = "./ObjectsAndModels/lme7fem.rds")
# read model back in to environment
lme7fem <- readRDS("./ObjectsAndModels/lme7fem.rds")

# Roger's eigth top model now with id and slope as a random effect 
lme8fem<- glmer(use ~ Habitat_Cl +
                d2ag +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme8fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme8fem, file = "./ObjectsAndModels/lme8fem.rds")
# read model back in to environment
lme8fem <- readRDS("./ObjectsAndModels/lme8fem.rds")

# Roger's ninth top model now with id and slope as a random effect 
lme9fem<- glmer(use ~ 
                elevation+
                slope+
                d2ag +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme9fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme9fem, file = "./ObjectsAndModels/lme9fem.rds")
# read model back in to environment
lme9fem <- readRDS("./ObjectsAndModels/lme9fem.rds")

# Roger's tenth top model now with id and slope as a random effect 
lme10fem<- glmer(use ~ 
                slope+
                d2ag +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme10fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme10fem, file = "./ObjectsAndModels/lme10fem.rds")
# read model back in to environment
lme10fem <- readRDS("./ObjectsAndModels/lme10fem.rds")

# Roger's eleventh top model now with id and slope as a random effect 
lme11fem<- glmer(use ~ 
                d2ag +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme11fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme11fem, file = "./ObjectsAndModels/lme11fem.rds")
# read model back in to environment
lme11fem <- readRDS("./ObjectsAndModels/lme11fem.rds")

# Roger's twelfth top model now with id and site as a random effect 
lme12fem<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme12fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme12fem, file = "./ObjectsAndModels/lme12fem.rds")
# read model back in to environment
lme12fem <- readRDS("./ObjectsAndModels/lme12fem.rds")

# Roger's thirteenth top model now with id and slope as a random effect 
lme13fem<- glmer(use ~ 
                slope+
                Habitat_Cl +
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme13fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme13fem, file = "./ObjectsAndModels/lme13fem.rds")
# read model back in to environment
lme13fem <- readRDS("./ObjectsAndModels/lme13fem.rds")

# Roger's fourteenth top model now with id and slope as a random effect 
lme14fem<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                d2moistsoi+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme14fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme14fem, file = "./ObjectsAndModels/lme14fem.rds")
# read model back in to environment
lme14fem <- readRDS("./ObjectsAndModels/lme14fem.rds")

# Roger's fifteenth top model now with id and slope as a random effect 
lme15fem<- glmer(use ~ 
                Habitat_Cl+
                d2stream +
                d2upyoungf+
                (1|id)+(1|site),
              family = binomial(link = "logit"), data = usedandavailnumberfemalesscaled)

summary(lme15fem)
# save model lme26 output (my top model) so that you don't have to run it again
saveRDS(lme15fem, file = "./ObjectsAndModels/lme15fem.rds")
# read model back in to environment
lme15fem <- readRDS("./ObjectsAndModels/lme15fem.rds")

# make list of models
u_modroglme4randomfemales <- list(lme1fem, lme2fem, lme3fem, lme4fem, lme5fem, lme6fem, lme7fem, lme8fem, lme9fem, lme10fem, lme11fem, lme12fem,lme13fem, lme14fem, lme15fem)
#
# name models
u_modnameroglme4randomfemales <- c("lme1fem", "lme2fem", "lme3fem", "lme4fem", "lme5fem", "lme6fem", "lme7fem", "lme8fem", "lme9fem", "lme10fem", "lme11fem", "lme12fem","lme13fem", "lme14fem", "lme15fem")
#
# run AIC on models
aictab(u_modroglme4randomfemales, modnames = u_modnameroglme4randomfemales, second.ord = TRUE)

# there are two top competing models (roger's model 2 and model 1), Roger's model 2 holds most of the weight 

# output AIC table
aicoutputlme4randomfemales <- data.frame(aictab(u_modroglme4randomfemales, modnames = u_modnameroglme4randomfemales))

# write aic table 
# write.table(aicoutputlme4randomfemales, file="./Figures/femaleaictable.csv", sep=',')


# need to do K-fold cross validation on top models to evaluate "the accuracy" of the models, there is the package https://rdrr.io/github/BastilleRousseau/IndRSA/man/kfoldRSF.html that can do this or you can use Liam Berigan's way which evaluates the AUC

# if using glmmTMB you need to use cvFit function in cvTools package because kfoldRSF can only take models built using glm or glmer, but I couldn't fugure it out so just went back to glmer (which is wayyyy slower btw)

summary(lme1fem)
summary(lme3fem)

# kfold cross validation for model lme3fem
kfoldlme2fem <- kfoldRSF(
  lme2fem,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save kfoldlme3fem so that you don't have to run it again
saveRDS(kfoldlme2fem, file = "./ObjectsAndModels/kfoldlme2fem.rds")

# read model back in to environment
kfoldlme2fem <- readRDS("./ObjectsAndModels/kfoldlme2fem.rds")
# check summary
summary(kfoldlme2fem)

# kfold cross validation for model lme1fem
kfoldlme3fem <- kfoldRSF(
  lme3fem,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save model kfoldlme1fem output so that you don't have to run it again
saveRDS(kfoldlme3fem, file = "./ObjectsAndModels/kfoldlme3fem.rds")

# read model back in to environment
kfoldlme3fem <- readRDS("./ObjectsAndModels/kfoldlme3fem.rds")
# check summary
summary(kfoldlme3fem)

# kfold cross validation for model lme4fem
kfoldlme4fem <- kfoldRSF(
  lme4fem,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save model kfoldlme1fem output so that you don't have to run it again
saveRDS(kfoldlme4fem, file = "./ObjectsAndModels/kfoldlme4fem.rds")

# read model back in to environment
kfoldlme4fem <- readRDS("./ObjectsAndModels/kfoldlme4fem.rds")
# check summary
summary(kfoldlme4fem)

# kfold cross validation for model lme14fem
kfoldlme14fem <- kfoldRSF(
  lme14fem,
  k = 5,
  nrepet = 1,
  nbins = 10,
  jitter = TRUE,
  random = TRUE,
  method = method,
  x = m,
  form_ls = ls,
  reproducible = TRUE)

# save model kfoldlme1fem output so that you don't have to run it again
saveRDS(kfoldlme14fem, file = "./ObjectsAndModels/kfoldlme14fem.rds")

# read model back in to environment
kfoldlme14fem <- readRDS("./ObjectsAndModels/kfoldlme14fem.rds")
# check summary
summary(kfoldlme14fem)

# make kfold summaries into a dataframe
kfoldlme2femsumm <- data.frame(kfoldlme2fem)
# calculate mean kfold .65
mean(kfoldlme2fem$kfold[kfoldlme2fem$type=="obs"])

kfoldlme3femsumm <- data.frame(kfoldlme3fem)
# calculate mean kfold .75
mean(kfoldlme3fem$kfold[kfoldlme3fem$type=="obs"])

kfoldlme4femsumm <- data.frame(kfoldlme4fem)
# calculate mean kfold .75
mean(kfoldlme4fem$kfold[kfoldlme4fem$type=="obs"])

kfoldlme14femsumm <- data.frame(kfoldlme3fem)
# calculate mean kfold .61
mean(kfoldlme14fem$kfold[kfoldlme14fem$type=="obs"])

#######Model Averaging/Multimodel Inference for Female only models########
success_topmodsfem <- aicoutputlme4randomfemales %>% 
  filter(Delta_AICc < 2)

# Model Averaged Estimates with Shrinkage
## Burnham and Anderson 2002 pg. 152, Anderson 2008 pg. 130-132, Lukacs et al. 2010
success_topmodsfem
cands.successfem <- list(lme4fem, lme14fem, lme3fem, lme2fem)
candname.successfem <- c("lme4fem","lme14fem","lme3fem","lme2fem")

s_ModAvgCoefficientsfem <- c("(Intercept)", "Habitat_Cl1", "Habitat_Cl2", "Habitat_Cl4", "Habitat_Cl5", "Habitat_Cl6", "Habitat_Cl7", "Habitat_Cl8", "d2stream", "d2moistsoi", "d2upyoungf", "d2ag","elevation","slope" )
success_int.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "(Intercept)")
success_habitatcl1.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl1")
success_habitatcl2.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl2")
success_habitatcl4.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl4")
success_habitatcl5.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl5")
success_habitatcl6.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl6")
success_habitatcl7.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl7")
success_habitatcl8.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "Habitat_Cl8")
success_d2stream.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "d2stream")
success_d2moistsoi.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "d2moistsoi")
success_d2upyoungf.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "d2upyoungf")
success_d2ag.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "d2ag")
success_elevation.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "elevation")
success_slope.shrinkfem <- modavgShrink(cand.set = cands.successfem, modnames = candname.successfem, parm = "slope")

success_est.shrinkfem <- c(success_int.shrinkfem$Mod.avg.beta, success_habitatcl1.shrinkfem$Mod.avg.beta, success_habitatcl2.shrinkfem$Mod.avg.beta,success_habitatcl4.shrinkfem$Mod.avg.beta, success_habitatcl5.shrinkfem$Mod.avg.beta,success_habitatcl6.shrinkfem$Mod.avg.beta, success_habitatcl7.shrinkfem$Mod.avg.beta,success_habitatcl8.shrinkfem$Mod.avg.beta,success_d2stream.shrinkfem$Mod.avg.beta,success_d2moistsoi.shrinkfem$Mod.avg.beta,success_d2upyoungf.shrinkfem$Mod.avg.beta,success_d2ag.shrinkfem$Mod.avg.beta,success_elevation.shrinkfem$Mod.avg.beta,success_slope.shrinkfem$Mod.avg.beta)

success_SE.shrinkfem <- c(success_int.shrinkfem$Uncond.SE, success_habitatcl1.shrinkfem$Uncond.SE, success_habitatcl2.shrinkfem$Uncond.SE,success_habitatcl4.shrinkfem$Uncond.SE, success_habitatcl5.shrinkfem$Uncond.SE,success_habitatcl6.shrinkfem$Uncond.SE, success_habitatcl7.shrinkfem$Uncond.SE,success_habitatcl8.shrinkfem$Uncond.SE,success_d2stream.shrinkfem$Uncond.SE,success_d2moistsoi.shrinkfem$Uncond.SE,success_d2upyoungf.shrinkfem$Uncond.SE,success_d2ag.shrinkfem$Uncond.SE,success_elevation.shrinkfem$Uncond.SE,success_slope.shrinkfem$Uncond.SE)

success_LowCL.shrinkfem <- c(success_int.shrinkfem$Lower.CL, success_habitatcl1.shrinkfem$Lower.CL, success_habitatcl2.shrinkfem$Lower.CL,success_habitatcl4.shrinkfem$Lower.CL, success_habitatcl5.shrinkfem$Lower.CL,success_habitatcl6.shrinkfem$Lower.CL, success_habitatcl7.shrinkfem$Lower.CL,success_habitatcl8.shrinkfem$Lower.CL,success_d2stream.shrinkfem$Lower.CL, success_d2moistsoi.shrinkfem$Lower.CL,success_d2upyoungf.shrinkfem$Lower.CL,success_d2ag.shrinkfem$Lower.CL,success_elevation.shrinkfem$Lower.CL,success_slope.shrinkfem$Lower.CL)

success_UpperCL.shrinkfem <- c(success_int.shrinkfem$Upper.CL, success_habitatcl1.shrinkfem$Upper.CL, success_habitatcl2.shrinkfem$Upper.CL,success_habitatcl4.shrinkfem$Upper.CL, success_habitatcl5.shrinkfem$Upper.CL,success_habitatcl6.shrinkfem$Upper.CL, success_habitatcl7.shrinkfem$Upper.CL,success_habitatcl8.shrinkfem$Upper.CL,success_d2stream.shrinkfem$Upper.CL, success_d2moistsoi.shrinkfem$Upper.CL,success_d2upyoungf.shrinkfem$Upper.CL,success_d2ag.shrinkfem$Upper.CL,success_elevation.shrinkfem$Upper.CL,success_slope.shrinkfem$Upper.CL)

success_modavgcoefficients.shrinkfem <- data.frame(s_ModAvgCoefficientsfem, success_est.shrinkfem, success_SE.shrinkfem, success_LowCL.shrinkfem, success_UpperCL.shrinkfem)

# write_csv(success_modavgcoefficients.shrinkfem, "ObjectsAndModels/.20202021femalemodelaverage.csv")

#########make plots of all years of males 2010-2021############
male10yrs <- data.frame(cbind(success_modavgcoefficients.shrink$success_est.shrink,success_modavgcoefficients.shrink$success_LowCL.shrink,success_modavgcoefficients.shrink$success_UpperCL.shrink,success_modavgcoefficients.shrink$success_SE.shrink))
colnames(male10yrs) <- c("estimate", "lower.bound", "upper.bound","SE")
rownames(male10yrs) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")

# make column with coefficient names
male10yrs$coefficient <- as.factor(rep(c("Intercept","Upland Coniferous Forest","Upland Deciduous Forest","Upland Young Forest","Wetland Coniferous Forest","Wetland Deciduous Forest","Wetland Mixed Forest","Wetland Young Forest","Slope","Distance to stream","Distance to moist soils","Distance to upland young forest","Distance to agriculture","Elevation")))

# make column with significance 95% CI
# male10yrs$significance <- as.factor(rep(c("significant","not significant","significant","significant","not significant","significant","significant","significant","significant","not significant","significant","significant","significant","not significant")))

# write csv to export model average results from the first and second periods
# write.csv(male10yrs, "./Figures/male11yrsmodelaverageoutput.csv", row.names = FALSE)

# make ggplot with 95% CIs and include picture of woodcock using r phylopic 
remotes::install_github("sckott/rphylopic")
citation("rphylopic")
library(rphylopic)
uuid <- get_uuid(name = "Scolopax", n = 1)
img <- pick_phylopic(name = "Scolopax")

library(dplyr)
allyear <- ggplot(male10yrs %>% filter(coefficient!="Intercept"), aes(x = estimate, y = coefficient)) +
  geom_vline(xintercept = 0)+
  guides(col= guide_legend(title= "95% Significance"))+
  geom_errorbar(aes(xmin = lower.bound, xmax = upper.bound), position = position_dodge2(width = 0.4, preserve = "single"), width = 0.5, size=.8,color=natparks.pals("Triglav",1)) +
  geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2,color=natparks.pals("CraterLake",1))+
  geom_hline(yintercept = 0) +
  xlab(element_text("Coefficient effect size (logit scale)")) +
  ylab(element_text("Environmental covariates")) +
  xlim(-1,1.5)+
  theme(axis.text.x = element_text(angle = 360)) +theme(legend.position = "right")+
  theme(legend.title.align=0.5)+
  scale_y_discrete(labels = c('Distance to agriculture','Distance to moist soils','Distance to streams',"Distance to upland young forest", "Elevation", "Slope", "Upland coniferous forest", "Upland deciduous forest", "Upland young forest", "Wetland coniferous forest", "Wetland deciduous forest", "Wetland mixed forest", "Wetland young forest"))+
  theme_pubr(base_size=14)+
  theme(text=element_text(family="serif"))+
  # theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 14))+
  theme(axis.title=element_text(size=14))+
  # theme(plot.title=element_text(size=20))+
  theme(legend.text=element_text(size=14))+
  add_phylopic(img = img, color = "darkgray", alpha = .4)+
  labs(caption = "Silhouette added via the rphylopic package (Gearty et al. 2023)")

ggsave(plot = allyear, filename = "./Figures/11yearsofmales.jpg", dpi = 400, width = 12, height = 8)
  
#########make plots of male and female comparison for 2020 and 2021#######
# top models
success_modavgcoefficients.shrinkfem
success_modavgcoefficients.shrinkmalecomp

# make dataframe of the word male so I can call estimates from the sex in ggplot
maleword <- data.frame(c(rep("male", times=14)))
colnames(maleword) <- c("sex")

# make dataframe for males
malesex <- data.frame(cbind(success_modavgcoefficients.shrinkmalecomp$success_est.shrinkmalecomp,success_modavgcoefficients.shrinkmalecomp$success_LowCL.shrinkmalecomp,success_modavgcoefficients.shrinkmalecomp$success_UpperCL.shrinkmalecomp,success_modavgcoefficients.shrinkmalecomp$success_SE.shrinkmalecomp,maleword))
colnames(malesex) <- c("estimate", "lower.bound", "upper.bound","SE","sex")
rownames(malesex) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")

# make column with coefficient names
malesex$coefficient <- as.factor(rep(c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")))

# make dataframe of the word female so I can call estimates from the sex period in ggplot
femaleword <- data.frame(c(rep("female", times=14)))
colnames(femaleword) <- c("sex")

# make dataframe for second period of years
femalesex <- data.frame(cbind(success_modavgcoefficients.shrinkfem$success_est.shrinkfem,success_modavgcoefficients.shrinkfem$success_LowCL.shrinkfem,success_modavgcoefficients.shrinkfem$success_UpperCL.shrinkfem,success_modavgcoefficients.shrinkfem$success_SE.shrinkfem,femaleword))
colnames(femalesex) <- c("estimate", "lower.bound", "upper.bound","SE","sex")
rownames(femalesex) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation","slope")

# make column with coefficient names
femalesex$coefficient <- as.factor(rep(c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation","slope")))
                                       
# combine these 2 dataframes
femaleandmalesex <- rbind(malesex,femalesex)

# write csv to export model average results from the first and second periods
# write.csv(femaleandmalesex, "./Figures/femaleandmalesexmodelaverageoutput.csv", row.names = FALSE)

# make ggplot with 95% CIs
# nice color options: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
malefemaleplot <- ggplot(femaleandmalesex %>% filter(coefficient!="Intercept"), aes(x = estimate, y = coefficient, color = sex, group = coefficient)) +
  geom_vline(xintercept = 0)+
  geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
  geom_errorbar(aes(xmin = lower.bound, xmax = upper.bound), position = position_dodge2(width = 0.4, preserve = "single"), width = 0.5, size=.8) +
  geom_hline(yintercept = 0) +
  guides(col= guide_legend(title= "Sex"))+
  ylab(element_text("Environmental covariates")) +
  xlab(element_text("Coefficient effect size (logit scale)"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 360))+
  scale_y_discrete(labels = c('Distance to agriculture','Distance to moist soils','Distance to streams',"Distance to upland young forest", "Elevation", "Slope", "Upland coniferous forest", "Upland deciduous forest", "Upland young forest", "Wetland coniferous forest", "Wetland deciduous forest", "Wetland mixed forest", "Wetland young forest"))+
  annotate("text", x = 0.139, y = 6, label = "***")+
  annotate("text", x = .19, y = 3, label = "***")+
  annotate("text", x = -.31, y = 4, label = "***")+
  theme_pubr(base_size=14)+
  scale_color_manual(values=natparks.pals("CraterLake",2),labels = c("Female", "Male"))+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size = 14))+
  theme(axis.title=element_text(size=14))+
  # theme(plot.title=element_text(size=20))+
  theme(legend.text=element_text(size=14))+
  add_phylopic(img = img, color = "darkgray", alpha = .4)+
  labs(caption = "Silhouette added via the rphylopic package (Gearty et al. 2023)")

ggsave(plot = malefemaleplot, filename = "./Figures/malefemalecoeff.jpg", dpi = 400, width = 12, height = 8)

# check for statistical differences between males and females when it isn't very clear by subtracting upper and lower CIs----

# wetland forest deciduous is not statistically different
wetforestdeciddiffupperfem <- femaleandmalesex$upper.bound[20]-femaleandmalesex$lower.bound[6]
margerrorwetforestdecidfirstfem <- 0.15253745*1.96
margerrorwetforestdecidsecondfem <- 0.25830950 *1.96
avgmargerrorwetforestdecidfem <- (margerrorwetforestdecidfirstfem+margerrorwetforestdecidsecondfem)/2
propwetforestdecidfem <- wetforestdeciddiffupperfem/avgmargerrorwetforestdecidfem

# upland young forest is not statistically different 
upyoungfdiffupperfem <- femaleandmalesex$upper.bound[18]-femaleandmalesex$lower.bound[4]
margerrorupyoungffirstfem <- 0.15250449*1.96
margerrorupyoungfsecondfem <- 0.26658689 *1.96
avgmargerrorupyoungffem <- (margerrorupyoungffirstfem+margerrorupyoungfsecondfem)/2
propupyoungffem <- upyoungfdiffupperfem/avgmargerrorupyoungffem

# upland forest coniferous is not statistically different
upforestconifdiffupperfem <- femaleandmalesex$upper.bound[16]-femaleandmalesex$lower.bound[2]
margerrorupforestconiffirstfem <- 0.15250449*1.96
margerrorupforestconifsecondfem <- 0.47570368 *1.96
avgmargerrorupforestconiffem <- (margerrorupforestconiffirstfem+margerrorupforestconifsecondfem)/2
propupforestconiffem <- upforestconifdiffupperfem/avgmargerrorupforestconiffem

# d2moistsoi is not statistically different
d2moistsoidiffupperfem <- femaleandmalesex$upper.bound[24]-femaleandmalesex$lower.bound[11]
margerrord2moistsoifirstfem <- 0.05097057*1.96
margerrord2moistsoisecondfem <- 0.06393737 *1.96
avgmargerrord2moistsoifem <- (margerrord2moistsoifirstfem+margerrord2moistsoisecondfem)/2
propd2moistsoifem <- d2moistsoidiffupperfem/avgmargerrord2moistsoifem

# d2ag is not statistically different
d2agdiffupperfem <- femaleandmalesex$upper.bound[26]-femaleandmalesex$lower.bound[13]
margerrord2agfirstfem <- 0.02649004*1.96
margerrord2agsecondfem <- 0.04815618 *1.96
avgmargerrord2agfem <- (margerrord2agfirstfem+margerrord2agsecondfem)/2
propd2agfem <- d2agdiffupperfem/avgmargerrord2agfem


#########make plots of first period of males and second period of males######
# top models
success_modavgcoefficients.shrinkfirst
success_modavgcoefficients.shrinksecond

# make dataframe of the word first so I can call estimates from the first period in ggplot
firstword <- data.frame(c(rep("first", times=13)))
colnames(firstword) <- c("period")

# make dataframe for first period of years
firstperiod <- data.frame(cbind(success_modavgcoefficients.shrinkfirst$success_est.shrinkfirst,success_modavgcoefficients.shrinkfirst$success_LowCL.shrinkfirst,success_modavgcoefficients.shrinkfirst$success_UpperCL.shrinkfirst,success_modavgcoefficients.shrinkfirst$success_SE.shrinkfirst,firstword))
colnames(firstperiod) <- c("estimate","lower.bound", "upper.bound","SE","period")
rownames(firstperiod) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2upyoungf","d2ag","d2moistsoi")

# make dataframe of the word second so I can call estimates from the first period in ggplot
secondword <- data.frame(c(rep("second", times=13)))
colnames(secondword) <- c("period")

# make dataframe for second period of years
secondperiod <- data.frame(cbind(success_modavgcoefficients.shrinksecond$success_est.shrinksecond,success_modavgcoefficients.shrinksecond$success_LowCL.shrinksecond,success_modavgcoefficients.shrinksecond$success_UpperCL.shrinksecond,success_modavgcoefficients.shrinksecond$success_SE.shrinksecond,secondword))
colnames(secondperiod) <- c("estimate", "lower.bound", "upper.bound","SE","period")
rownames(secondperiod) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","Slope","d2stream","d2upyoungf","d2ag","d2moistsoi")

# combine these 2 dataframes
firstandsecondperiods <- rbind(firstperiod,secondperiod)


# make column with coefficient names 
firstandsecondperiods$coefficient <- as.factor(rep(c("Intercept","Upland forest coniferous","Upland forest deciduous","Upland young forest","Wetland forest coniferous","Wetland forest deciduous","Wetland forest mixed","Wetland young forest","Slope","Distance to streams","Distance to upland young forest","Distance to agriculture","Distance to moist soils","Intercept","Upland forest coniferous","Upland forest deciduous","Upland young forest","Wetland forest coniferous","Wetland forest deciduous","Wetland forest mixed","Wetland young forest","Slope","Distance to streams","Distance to upland young forest","Distance to agriculture","Distance to moist soils")))

# write csv to export model average results from the first and second periods 
# write.csv(firstandsecondperiods, "./Figures/firstandsecondperiodstopmodelaverageoutput.csv", row.names = FALSE)

# make ggplot with 95% CIs
firstandsecondcoeff <- ggplot(firstandsecondperiods %>% filter(coefficient!="Intercept"), aes(x = estimate, y = coefficient, color = period, group = coefficient)) +
  geom_vline(xintercept = 0)+
  guides(col= guide_legend(title= "Study period"))+
  geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
  geom_errorbar(aes(xmin = lower.bound, xmax = upper.bound), position = position_dodge2(width = 0.4, preserve = "single"), width = 0.5, size=.8) +
  geom_hline(yintercept = 0) +
  ylab(element_text("Environmental covariates")) +
  xlab(element_text("Coefficient effect size (logit scale)")) +
  theme(axis.text.x = element_text(angle = 360))+
  scale_y_discrete(labels = c('Distance to agriculture','Distance to moist soils','Distance to streams',"Distance to upland young forest", "Slope", "Upland coniferous forest", "Upland deciduous forest", "Upland young forest", "Wetland coniferous forest", "Wetland deciduous forest", "Wetland mixed forest", "Wetland young forest"))+
  theme(plot.title = element_text(hjust = 0.5))+
  # scale_x_break(c(-2, -10), scales = 2)+
  annotate("text", x = 0.25, y = 2, label = "***")+
  annotate("text", x = 1.36, y = 8, label = "***")+
  annotate("text", x = 1.23, y = 12, label = "***")+
  annotate("text", x = .13, y = 5, label = "***")+
  annotate("text", x = 0.17, y = 3, label = "***")+
  annotate("text", x = 0.17, y = 4, label = "***")+
    scale_color_manual(values=natparks.pals("CraterLake",2),labels=c("First (2010-11)", "Second (2020-21)"))+
  theme_pubr(base_size=14)+
  theme(text=element_text(family="serif"))+
  theme(plot.title = element_text(hjust = 0.5))+
  # theme(text = element_text(size = 12))+
  theme(axis.title=element_text(size=14))+
  # theme(plot.title=element_text(size=20))+
  theme(legend.text=element_text(size=14))+
  add_phylopic(img = img, color = "darkgray", alpha = .4)+
  labs(caption = "Silhouette added via the rphylopic package (Gearty et al. 2023)")

ggsave(plot = firstandsecondcoeff, filename = "./Figures/firstandsecondcoeff.jpg", dpi = 400, width = 12, height = 8)

# check for statistical differences in variables that just barely overlap by subtracting the upper of the lesser variable by the lower of the greater variable and the dividing by the average margin of error----In the majority of cases that have proportion overlap of .50 and meet the stated conditions—sample sizes of at least 10 and w1 (margin of error) and w2 (margin fo error) not differing by more than a factor of 2—the p value is between .04 and .05 and, in virtually every case that meets the conditions, p is between .03 and .05 (Cumming, 2004). so look for ca. % overlap of 0.5 or less in order to be signficantly different 

# d2 ag is not statistically different 
d2agdiffupperf <- firstandsecondperiods$upper.bound[25]-firstandsecondperiods$lower.bound[12]
margerrord2agfirst <- 0.03271194*1.96
margerrord2agsecond <- 0.02006882 *1.96
avgmargerrord2agperiods <- (margerrord2agfirst+margerrord2agsecond)/2
propd2agperiods <- d2agdiffupperf/avgmargerrord2agperiods

# d2moist soi is statistically different
d2moistsoidiffupperf <- firstandsecondperiods$upper.bound[26]-firstandsecondperiods$lower.bound[13]
margerrord2moistsoifirst <- 0.03609382  *1.96
margerrord2moistsoisecond <- 0.04942948 *1.96
avgmargerrord2moistsoiperiods <- (margerrord2moistsoifirst+margerrord2moistsoisecond)/2
propd2moistsoiperiods <- d2moistsoidiffupperf/avgmargerrord2moistsoiperiods

# d2 stream is statistically different
d2streamsdiffupperf <- firstandsecondperiods$upper.bound[10]-firstandsecondperiods$lower.bound[23]
margerrord2streamsfirst <- 0.04783304  *1.96
margerrord2streamssecond <- 0.03243381  *1.96
avgmargerrord2streamsperiods <- (margerrord2streamsfirst+margerrord2streamssecond)/2
propd2streamsperiods <- d2streamsdiffupperf/avgmargerrord2streamsperiods

# upland forest deciduous is not stastically different 
upfordeciddiffupperf <- firstandsecondperiods$upper.bound[16]-firstandsecondperiods$lower.bound[3]
margerrorupfordecidfirst <- 0.10442224  *1.96
margerrorupfordecidsecond <- 0.15017100  *1.96
avgmargerrorupfordecidperiods <- (margerrorupfordecidfirst+margerrorupfordecidsecond)/2
propupfordecidperiods <- upfordeciddiffupperf/avgmargerrorupfordecidperiods

# upland forest coniferous is not stastically different 
upforconifdiffupperf <- firstandsecondperiods$upper.bound[15]-firstandsecondperiods$lower.bound[2]
margerrorupforconiffirst <- 0.30423189    *1.96
margerrorupforconifsecond <- 0.18862080   *1.96
avgmargerrorupforconifperiods <- (margerrorupforconiffirst+margerrorupforconifsecond)/2
propupforconifperiods <- upforconifdiffupperf/avgmargerrorupforconifperiods

# upland young forest is just barely stastically different (0.59)
upyoungfdiffupperf <- firstandsecondperiods$upper.bound[17]-firstandsecondperiods$lower.bound[4]
margerrorupyoungffirst <- 0.15903545  *1.96
margerrorupyoungfsecond <- 0.15238035  *1.96
avgmargerrorupyoungfperiods <- (margerrorupyoungffirst+margerrorupyoungfsecond)/2
propupyoungfperiods <- upyoungfdiffupperf/avgmargerrorupyoungfperiods

# wetland forest deciduous is not stastically different
wetfordeciddiffupperf <- firstandsecondperiods$upper.bound[6]-firstandsecondperiods$lower.bound[19]
margerrorwetfordecidfirst <- 0.13631220  *1.96
margerrorwetfordecidsecond <- 0.15362181  *1.96
avgmargerrorwetfordecidperiods <- (margerrorwetfordecidfirst+margerrorwetfordecidsecond)/2
propwetfordecidperiods <- wetfordeciddiffupperf/avgmargerrorwetfordecidperiods

# wetland forest coniferous is not stastically different 
wetforconifdiffupperf <- firstandsecondperiods$upper.bound[5]-firstandsecondperiods$lower.bound[18]
margerrorwetforconiffirst <- 0.30423189    *1.96
margerrorwetforconifsecond <- 0.25893041   *1.96
avgmargerrorwetforconifperiods <- (margerrorwetforconiffirst+margerrorwetforconifsecond)/2
propwetforconifperiods <- wetforconifdiffupperf/avgmargerrorwetforconifperiods

# wetland young forest is stastically different 
wetyoungfdiffupperf <- firstandsecondperiods$upper.bound[21]-firstandsecondperiods$lower.bound[8]
margerrorwetyoungffirst <- 0.14594773 *1.96
margerrorwetyoungfsecond <- 0.17639769 *1.96
avgmargerrorwetyoungfperiods <- (margerrorwetyoungffirst+margerrorwetyoungfsecond)/2
propwetyoungfperiods <- wetyoungfdiffupperf/avgmargerrorwetyoungfperiods

# ##########make plots of Arcadia, Big River, and Great Swamp######
# # top models
# success_modavgcoefficients.shrinkarcadia
# summary(lme1bigriver)
# success_modavgcoefficients.shrinkgswamp
# 
# # make dataframe of the word arcadia so I can call estimates from the first period in ggplot
# arcadiaword <- data.frame(c(rep("arcadia", times=14)))
# colnames(arcadiaword) <- c("site")
# 
# # make dataframe for site arcadia
# arcadiasite <- data.frame(cbind(success_modavgcoefficients.shrinkarcadia$success_est.shrinkarcadia,success_modavgcoefficients.shrinkarcadia$success_LowCL.shrinkarcadia,success_modavgcoefficients.shrinkarcadia$success_UpperCL.shrinkarcadia,success_modavgcoefficients.shrinkarcadia$success_SE.shrinkarcadia,arcadiaword))
# colnames(arcadiasite) <- c("estimate", "lower.bound", "upper.bound","SE","site")
# rownames(arcadiasite) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")
# 
# arcadiasite$coefficient <- as.factor(rep(c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope", "d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")))
# 
# # make dataframe of the word greatswamp so I can call estimates from the first period in ggplot
# gswampword <- data.frame(c(rep("great swamp", times=14)))
# colnames(gswampword) <- c("site")
# 
# # make dataframe for site great swamp
# gswampsite <- data.frame(cbind(success_modavgcoefficients.shrinkgswamp$success_est.shrinkgswamp,success_modavgcoefficients.shrinkgswamp$success_LowCL.shrinkgswamp,success_modavgcoefficients.shrinkgswamp$success_UpperCL.shrinkgswamp,success_modavgcoefficients.shrinkgswamp$success_SE.shrinkgswamp,gswampword))
# colnames(gswampsite) <- c("estimate", "lower.bound", "upper.bound","SE","site")
# rownames(gswampsite) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope","d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")
# gswampsite$coefficient <- as.factor(rep(c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","slope", "d2stream","d2moistsoi","d2upyoungf","d2ag","elevation")))
# 
# # make dataframe for Big River
# 
# # calculate 95% CI from SE and estimate
# bigriversumm <- data.frame(summary(lme1bigriver)$coefficients)
# bigriversumm$lower.bound <- bigriversumm$Estimate - 1.96*bigriversumm$Std..Error
# bigriversumm$upper.bound <- bigriversumm$Estimate + 1.96*bigriversumm$Std..Error
# 
# # make dataframe of the word greatswamp so I can call estimates from the first period in ggplot
# briverword <- data.frame(c(rep("big river", times=14)))
# colnames(briverword) <- c("site")
# 
# briversite <- data.frame(cbind(bigriversumm$Estimate,bigriversumm$lower.bound, bigriversumm$upper.bound,bigriversumm$Std..Error,briverword))
# colnames(briversite) <- c("estimate", "lower.bound", "upper.bound","SE","site")
# rownames(briversite) <- c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","d2ag","d2stream","d2upyoungf","elevation","slope","d2moistsoi")
# 
# briversite$coefficient <- as.factor(rep(c("Intercept","Upland Forest Coniferous","Upland Forest Deciduous","Upland Young Forest","Wetland Forest Coniferous","Wetland Forest Deciduous","Wetland Forest Mixed","Wetland Young Forest","d2ag", "d2stream","d2upyoungf","elevation","slope","d2moistsoi")))
# 
# # combine 3 sites into one dataframe
# allsites10yrs <- rbind(arcadiasite,gswampsite,briversite)
# 
# # make ggplots 
# # Plot estimates and 95% CIs 
# ggplot(allsites10yrs, aes(x = coefficient, y = estimate, color = site, group = coefficient)) +
#   geom_point(position = position_dodge2(width = 0.5, preserve = "single"), size=2) +
#   geom_errorbar(aes(ymin = lower.bound, ymax = upper.bound), position = position_dodge2(width = 0.4, preserve = "single"), width = 0.5, size=1) +
#   geom_hline(yintercept = 0) +
#   xlab(element_blank()) +
#   ylab(element_text("Coefficient Effect Size (log scale)")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90))

# check for percent change of forests in each mgmt area in the availability areas----

arcadiaoldforest<- read.csv("./Data/percent change forests/arcadiaoldhabitat.csv", stringsAsFactors = TRUE)
summaryarcadiaoldforest <- arcadiaoldforest %>% group_by(GRIDCODE) %>% summarise(sum=sum(Area))

arcadianewforest <- read.csv("./Data/percent change forests/arcadianewhabitat.csv", stringsAsFactors = TRUE)
summaryarcadianewforest <- arcadianewforest %>% group_by(gridcode) %>% summarise(sum=sum(Area))

briveroldforest<- read.csv("./Data/percent change forests/briveroldhabitat.csv", stringsAsFactors = TRUE)
summarybriveroldforest <- briveroldforest %>% group_by(GRIDCODE) %>% summarise(sum=sum(Area))

brivernewforest <- read.csv("./Data/percent change forests/brivernewhabitat.csv", stringsAsFactors = TRUE)
summarybrivernewforest <- brivernewforest %>% group_by(gridcode) %>% summarise(sum=sum(Acres))

gswampoldforest<- read.csv("./Data/percent change forests/gswampoldhabitat.csv", stringsAsFactors = TRUE)
summarygswampoldforest <- gswampoldforest %>% group_by(GRIDCODE) %>% summarise(sum=sum(Area))

gswampnewforest <- read.csv("./Data/percent change forests/gswampnewhabitat.csv", stringsAsFactors = TRUE)
summarygswampnewforest <- gswampnewforest %>% group_by(gridcode) %>% summarise(sum=sum(Acres))


# check the percent change in moist soils in each mgmt area----
arcadiaoldmoistsoi<- read.csv("./Data/percent change forests/arcadiaoldmoistsoi.csv", stringsAsFactors = TRUE)
summaryarcadiaoldmoistsoi <- arcadiaoldmoistsoi %>% summarise(sum=sum(Area))

arcadianewmoistsoi <- read.csv("./Data/percent change forests/arcadianewmoistsoi.csv", stringsAsFactors = TRUE)
summaryarcadianewmoistsoi <- arcadianewmoistsoi %>% summarise(sum=sum(Area))

briveroldmoistsoi<- read.csv("./Data/percent change forests/briveroldmoistsoi.csv", stringsAsFactors = TRUE)
summarybriveroldmoistsoi <- briveroldmoistsoi %>% summarise(sum=sum(Area))

brivernewmoistsoi <- read.csv("./Data/percent change forests/brivernewmoistsoi.csv", stringsAsFactors = TRUE)
summarybrivernewmoistsoi <- brivernewmoistsoi %>% summarise(sum=sum(Area))

gswampoldmoistsoi<- read.csv("./Data/percent change forests/gswampoldmoistsoi.csv", stringsAsFactors = TRUE)
summarygswampoldmoistsoi <- gswampoldmoistsoi %>% summarise(sum=sum(Area))

gswampnewmoistsoi<- read.csv("./Data/percent change forests/gswampnewmoistsoi.csv", stringsAsFactors = TRUE)
summarygswampnewmoistsoi <- gswampnewmoistsoi %>% summarise(sum=sum(Area))


# see if there are differences in KDEs by site, age, and period for males during first and second periods, there is no statistical differences in any of the variables we looked at 
# read csv for males
malekdedata<- read.csv("./Data/2010_2021malehomerangesizekde95.csv", stringsAsFactors = TRUE)
str(malekdedata)
malekdedataonlydata <- malekdedata %>% filter(age=="ASY"|age=="SY") %>% filter(site=="arcadia"|site=="greatswamp"|site=="bigriver") %>% droplevels()
malekdeonlydata2 <-  malekdedata %>% filter(site=="arcadia"|site=="greatswamp"|site=="bigriver") %>% droplevels()

# Run mixed model to see whether the periods differ in how period and site predict home range size
# the data wasn't normally distributed so I log transformed and it became more normal 
malekdedataonlydata$area <- log(malekdedataonlydata$area)
hist(malekdedataonlydata$area)
shapiro.test(malekdedataonlydata$area)

# linear mixed model shows there are no significant predictors of home range size in either period 
firstsecondlm <- lm(area~period+site+period*site, data=malekdedataonlydata)
summary(firstsecondlm)
# plot(firstsecondlm)

# first period only make sure you don't use log transformmed version
malekdedataonlydatafirst <- malekdeonlydata2 %>% filter(period=="first")
# first period only 
malekdedataonlydatasecond <- malekdeonlydata2 %>% filter(period=="second")

# BELOW IS AN ALTERNATIVE ANOVA WORk FLOW IF YOU WANTED TO DO EACH VARIABLE SEPARATE BUT WE DECIDED AGAINST
# malekdedataonlydatafirst <- malekdedataonlydata %>% filter(period=="first")
# malekdedataonlydatasecond <- malekdedataonlydata %>% filter(period=="second")

# # ANOVA for first period home range size, it is not predicted by site or age 
# malehomerangeaovfirst <- aov(area~site+age,data=malekdedataonlydatafirst)
# summary(malehomerangeaovfirst)
# 
# # ANOVA for second period home range size, it is not predicted by site, age, or year
# malehomerangeaovsecond <- aov(area~site+age+year,data=malekdedataonlydatasecond)
# summary(malehomerangeaovsecond)
# 
# # ANOVA for difference between periods, there is no difference betwwen the periods (0.79)
# malehomerangeaov <- aov(area~period,data=malekdedataonlydata)
# summary(malehomerangeaov)
# 
# # run nested Anova on kde home ranges between two periods with site nested, there is no difference 
# homerangeaovcomparefirstsecondsite <- aov(area ~ period + Error(site), data=malekdedataonlydata)
# summary(homerangeaovcomparefirstsecondsite)
# 
# # run nested Anova on kde home ranges between two periods with year nested, there is no difference 
# homerangeaovcomparefirstsecondyear <- aov(area ~ period + Error(year), data=malekdedataonlydata)
# summary(homerangeaovcomparefirstsecondyear)
# 
# # run nested Anova on kde home ranges between two periods with age nested, there is no difference 
# homerangeaovcomparefirstsecondage <- aov(area ~ period + Error(age), data=malekdedataonlydata)
# summary(homerangeaovcomparefirstsecondage)

# calculate summary stats, range for first period kdes is 1.34-223.82 ha
range(malekdedataonlydatafirst$area)
# mean kde is 39.76 hectares first period
mean(malekdedataonlydatafirst$area)
# median kde 14.03 hectares first period
median(malekdedataonlydatafirst$area)

# calculate summary stats, range for second period kdes is 1.02-472.97 ha
range(malekdedataonlydatasecond$area)
# mean kde is 41.50 hectares second period
mean(malekdedataonlydatasecond$area)
# median kde 9.67 hectares second period
median(malekdedataonlydatasecond$area)

# calculate standard error first period 9.05
sd(malekdedataonlydatafirst$area)/sqrt(length((malekdedataonlydatafirst$area)))

# calculate standard error second period 13.35
sd(malekdedataonlydatasecond$area)/sqrt(length((malekdedataonlydatasecond$area)))


# see if there are differences in KDEs by sex (male v/female), there is no difference (0.189)----
# read csv
femalemalekdedata<- read.csv("./Data/2020_2021femalemalehomerangesizekde95.csv", stringsAsFactors = TRUE)
str(femalemalekdedata)
unique(femalemalekdedata$site)
femalemalekdedataonlydata <- femalemalekdedata %>% filter(age=="ASY"|age=="SY") %>% filter(site=="arcadia"|site=="greatswamp"|site=="bigriver"|site=="franciscarter") %>% droplevels()

# filter females to get range of kde home range values, range for females is 2.16-773.13 hectares 
femalerange <- femalemalekdedataonlydata %>% filter(sex=="female")
range(femalerange$area)
# mean kde is 78.72 hectares females
mean(femalerange$area)

# median kde 19.96 hectares females
median(femalerange$area)

# calculate standard error females 46.35
sd(femalerange$area)/sqrt(length((femalerange$area)))

# filter males to get range of kde home range values, range for females is 1.02-472.97 hectares
malerange <- femalemalekdedataonlydata %>% filter(sex=="male")
range(malerange$area)
# mean kde is 35.01 hectares males
mean(malerange$area)
# median kde 12.05 hectares males
median(malerange$area)

# calculate standard error males from male-female comparison 10.66
sd(malerange$area)/sqrt(length((malerange$area)))

# Run mixed model to see whether the sexes differ in how age and site predict home range size
# the data wasn't normally distributed so I log transformed and it became normal 
femalemalekdedataonlydata$area <- log(femalemalekdedataonlydata$area)
shapiro.test(femalemalekdedataonlydata$area)

# linear mixed model excluding frnacis carter and big river because only one female at each, but still only 2 ASY at arcadia and 5 females in total there
malefemalelm <- lm(area~sex+site+age+site*age+site*sex, data=femalemalekdedataonlydata %>% filter(site=="arcadia"|site=="greatswamp"))
summary(malefemalelm)


# BELOW IS AN ALTERNATIVE ANOVA WOR FLOW IF YOU WANTED TO DO EACH VARIABLE SEPARATE BUT WE DECIDED AGAINST
# # run Anova on kde home ranges between sexes, sex is not a predictor of kde home range size (0.189)
# levels(femalemalekdedataonlydata$sex)
# femalehomerangeaov <- aov(area~sex,data=femalemalekdedataonlydata)
# summary(femalehomerangeaov)
# 
# # run ANOVA on males to see if site and age predict kde home range size, it does not
# malekdeaov <- aov(area~site+age,data=malerange)
# summary(malekdeaov)
# 
# # run ANOVA on males to see if year predicts kde home range size, it does not
# malekdeaovyear <- aov(area~year,data=malerange)
# summary(malekdeaovyear)
# 
# # run ANOVA on females to see if site, age, and year predicts kde home range size, it does not even when I just include great swamp and arcadia so I used all sites
# femalekdeaov <- aov(area~site+age+year,data=femalerange)
# summary(femalekdeaov)
# 
# # run nested Anova on kde home ranges between sexes with site nested, there is no difference 
# homerangeaovcomparemalefemalesite <- aov(area ~ sex + Error(site), data=femalemalekdedataonlydata)
# summary(homerangeaovcomparemalefemalesite)
# 
# # run nested Anova on kde home ranges between sexes with year nested, there is no difference 
# homerangeaovcomparemalefemaleyear <- aov(area ~ sex + Error(year), data=femalemalekdedataonlydata)
# summary(homerangeaovcomparemalefemaleyear)
# 
# # run nested Anova on kde home ranges between sexes with age nested, there is no difference 
# homerangeaovcomparemalefemaleage <- aov(area ~ sex + Error(age), data=femalemalekdedataonlydata)
# summary(homerangeaovcomparemalefemaleage)


# calculate average and median MCP size by site, year, age, sex for males and females. I did not use the cut down versions of MCPs (removing unavailable habitat because older studies did not do this...and gives you an idea of area they covered in their summer because 100% MCP)----

# male MCPs summary
malemcpdata<- read.csv("./Data/2010_2021maleshomerangesizemcp.csv", stringsAsFactors = TRUE)
malemcpdatawant <- malemcpdata %>% filter(year=="2020"|year=="2021") %>% droplevels()
malemcpdatawant2 <- malemcpdata %>% filter(year=="2010"|year=="2011") %>% droplevels()

# mean mcp is 38.79 hectares for males first period
mean(malemcpdatawant$Area,na.rm=TRUE)
# median mcp is 26.70 hectares for males first period
median(malemcpdatawant$Area,na.rm=TRUE)
# range mcp is 0.69-153.10 hectares for males first period
range(malemcpdatawant$Area)

# calculate standard error males mcp 6.05 first period
sd(malemcpdatawant$Area)/sqrt(length((malemcpdatawant$Area)))

# mean mcp is 48.07 hectares for males second period 
mean(malemcpdatawant2$Area,na.rm=TRUE)
# median mcp is 18.03 hectares for males
median(malemcpdatawant2$Area,na.rm=TRUE)
# range mcp is 0.68-295.3 hectares for males second period
range(malemcpdatawant2$Area)

# calculate standard error males mcp 9.6 males second period
sd(malemcpdatawant2$Area)/sqrt(length((malemcpdatawant$Area)))


# now female MCPs summary
femalemcpdata<- read.csv("./Data/2020_2021femaleshomerangesizemcp.csv", stringsAsFactors = TRUE)
femalemcpdatawant <- femalemcpdata %>% filter(year=="2020"|year=="2021") %>% filter(sex=="female") %>% droplevels()

# mean mcp is 61.46 hectares for females
mean(femalemcpdatawant$Area,na.rm=TRUE)
# median mcp is 20.14 hectares for females
median(femalemcpdatawant$Area,na.rm=TRUE)
# range mcp is 1.15-263.71 hectares for females
range(femalemcpdatawant$Area)

# calculate standard error females mcp 21.18
sd(femalemcpdatawant$Area)/sqrt(length((femalemcpdatawant$Area)))

# Run Anovas on MCPs for males v females, sex is not a predictor of mcp size (0.161)
femalemalesmcpdata<- read.csv("./Data/2020_2021femalemaleshomerangesizemcp.csv", stringsAsFactors = TRUE)
femalemalesmcpdatawant <- femalemalesmcpdata %>% filter(year=="2020"|year=="2021") %>% droplevels()

femalemalemcphomerangeaov <- aov(Area~sex,data=femalemalesmcpdatawant)
summary(femalemalemcphomerangeaov)

# Run Anovas to see if site and age predict female kde size, it does not for site or age, when I include only arcadia and great swamp just barely insignficant for site (0.058) so used all sites
femalesmcps <- femalemalesmcpdatawant %>% filter(sex=="female")
femalemcphomerangeaov <- aov(Area~site+age,data=femalesmcps)
summary(femalemcphomerangeaov)

# Run Anovas to see if site and age predict male kde size, it does not for site or age
malesmcps <- femalemalesmcpdatawant %>% filter(sex=="male ")
malemcphomerangeaov <- aov(Area~site+age,data=malesmcps)
summary(malemcphomerangeaov)

# see if there were differences in total precip and average monthly temp 2010-11 and 202-21: https://www.weather.gov/wrh/climate
weather<- read.csv("./Data/weatherdata201011202021.csv", stringsAsFactors = TRUE)


# the total rainfall was 28.18 inches for summer 2010-11
# the mean daily rainfall was 0.13 inches 
weather201011 <- weather %>% filter(year=="2010"|year=="2011")
sum(weather201011$totalprecip)
mean(weather201011$totalprecip)


# the total rainfall was 30.66 inches for summer 2020-11
# the mean daily rainfall was 0.14 inches
weather202021 <- weather %>% filter(year=="2020"|year=="2021")
sum(weather202021$totalprecip)
mean(weather202021$totalprecip)

# see if there is statistical difference in total summer rainfall between two periods, there is not (0.64)
rainfallaov <- aov(totalprecip~period, data=weather)
summary(rainfallaov)

# see if there is a statistical difference in average daily temp for the summer period between two periods, there is not (0.25)
tempaov <- aov(avgtemp~period, data=weather)
summary(tempaov)

# the average temp for summer 2010-11 was 71.11
mean(weather201011$avgtemp)

# the average temp for summer 2020-21 was 72.01
mean(weather202021$avgtemp)

# combining RSF images into panel plot for TWS publication----
install.packages("magick")
img1<- magick::image_read("./Figures/male11yearsrsf.jpg") 
img2<- magick::image_read("./Figures/females2yearsrsf.jpg")

# combined read in images
combined <- magick::image_append(c(img1, img2))

# add annotations to panels
combineda <- magick::image_annotate(combined, "A.", size = 150, color = "black", location = "+300+300", font="Times")
combinedab <- magick::image_annotate(combineda, "B.", size = 150, color = "black", location = "+4500+300", font="Times")

# write combined image to new jpg
magick::image_write(combinedab, path = "./Figures/combinedmale11andfemale1.jpg", format = "jpg", density=400)
citation("Hmisc")
