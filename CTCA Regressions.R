#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Coding Alone - Trust and Digital Innovation #
# 15-01-2019, Fabian Stephany #
# Output: 
# - Stepwise model table (Figure Appendix 1A)
# - Table model comparison (Figure Appendix 1B)
# - Figure Cross validation performance (Figure Appendix 1C)
# - Table final regression (Figure 2B)
# - Figure Dot-Whisker plot (Figure 2C)

#%#%#%#%
# PLEASE NOTE THAT WE HAVE USED A SAMPLE OF 500,000 SO CONTRIBUTIONS
# TO CALCULATE THE REGIONAL REGRESSIONS;
# THEREFORE THE RESULTS MIGHT SLIGHTLY DIFFER FROM THE FINAL TABLES IN THE PAPER.
#%#%#%#%

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
## Read packages
library(readxl)
library(reshape2)
library("plyr")
library(tidyverse)
library(xlsx)
library("lme4")
library(dplyr)
library("rjson")
library("fuzzyjoin")
library("PerformanceAnalytics")
library("stargazer")
library(MASS)
library("pscl")
library("pglm")
library("stringr")
library("ggplot2")
library("plm")
library("tidyr")
library("texreg")
library(caret)
library(ape)
library(spdep)
library("nlme")
library("MuMIn")
library(sp)
library(gstat)
library(mgcv)
library(RColorBrewer)


Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Inverse Hyperbolic Sine Transformation
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

# Create function to access mode by group
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
## EXAMINE DATA
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
load(paste(getwd(),"/Data/coding_alone.RData", sep = ""))
# Summary of outcome
summary(coding_alone$activity)
summary(coding_alone$activity_zero)
summary(coding_alone$activity_ihs)

# Compare activity with clicks
ggplot(subset(coding_alone, region_trust>0.5), aes(x=log(uniques), y=activity_ihs)) +
  geom_point() + geom_smooth() + geom_text(aes(label=iso2))

# Compare activity with clicks
ggplot(coding_alone, aes(x=log(city_pop), y=activity_ihs)) +
  geom_point() + geom_smooth()

# Compare activity with clicks
ggplot(coding_alone, aes(x=log(city_gdp_pc), y=activity_ihs)) +
  geom_point() + geom_smooth()


### Categorise trust in quantiles
coding_alone$trust_qntls <- 1
coding_alone$trust_qntls[coding_alone$region_trust>=quantile(coding_alone$region_trust)[2]] <- 2
coding_alone$trust_qntls[coding_alone$region_trust>=quantile(coding_alone$region_trust)[3]] <- 3
coding_alone$trust_qntls[coding_alone$region_trust>=quantile(coding_alone$region_trust)[4]] <- 4

## Plot activity_zero by trust
ggplot(coding_alone, aes(mean(activity_zero), fill = trust_qntls)) +
  geom_histogram(bins=100) +  scale_x_log10() +
  facet_grid(trust_qntls ~ ., margins=TRUE, scales="free_y")


# How is data dispersion?
mean(coding_alone$activity_zero) # 877
var(coding_alone$activity_zero) # 3760036
# ---> Data are overdispersed (mean << variance)

p <- ggplot(cod, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE)

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
## COMPARE MODELS - We first pick OLS (IHS)
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
### Stepwise model selection
for (l in c("log(affinity)", "log(city_pop)", "log(city_gdp_pc)", "log(city_pop_dens)", "log(city_age_dep)", "log(region_edu)", "log(region_bband)", 
            "region_civic", "log(city_emp_r)")){
  
  summary(mstep <- lm(as.formula(paste("activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic "  , l)),
                        data=coding_alone))

  print(l)
  print(summary(mstep)$adj.r.squared)
}


mstep1 <- lm(activity_ihs ~ log(affinity),
             data=coding_alone)
mstep2 <- lm(activity_ihs ~ log(affinity) + log(city_pop),
             data=coding_alone)
mstep3 <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens),
             data=coding_alone)
mstep4 <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc),
             data=coding_alone)
mstep5 <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r),
             data=coding_alone)

optimal <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
             data=coding_alone)

mstep7 <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust + log(region_edu),
              data=coding_alone)
mstep8<- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust + log(region_edu) + log(city_age_dep),
              data=coding_alone)
mstep9 <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust + log(region_edu) + log(city_age_dep) + log(region_bband),
              data=coding_alone)


stargazer(mstep1, mstep2, mstep3, mstep4, mstep5, optimal, mstep7, mstep8, mstep9, type="text", title="Coding Alone - Forward and Backward Model Selection",
          column.labels=c("Forward 1","Forward 2", "Forward 3", "Forward 4", "Forward 5", "OPTIMAL", "Backward3", "Backward2", "Backward1"))

### Compare with NB
## Activity with ZERO Values
### OLS
summary(mA_ols <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
                     data=coding_alone))

### OLS NAs
summary(mA_ols_na <- lm(log(activity) ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
                        data=coding_alone))

### GLM Negative Binomial
summary(mA_nbin <- glm.nb(activity_zero ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
                          data=coding_alone))

### GLM Negative Binomial
summary(mA_nbin_na <- glm.nb(activity ~ log(affinity) + log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
                             data=coding_alone))


stargazer(mA_ols, mA_ols_na, mA_nbin, mA_nbin_na, title="Coding Alone", type="text")


#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
## CROSS VALIDATION
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%


# These are my self-derived 10-fold CV loops. I think they do a good job in more or less automatically do CV and produce good figures.
df <- coding_alone

set.seed(17) # Random seed
folder <- 10 # 10-fold
a <- round(nrow(df)/folder)
# One vector per model and CV measure (Mean absolute error and Pearson-rho)
cv.error.10.mA_ols = rep(0, folder)
cv.rho.10.mA_ols = rep(0, folder)
list_mA_ols <- list()


cv.error.10.mA_ols_na = rep(0, folder)
cv.rho.10.mA_ols_na = rep(0, folder)
list_mA_ols_na <- list()

cv.error.10.mA_nbin = rep(0, folder)
cv.rho.10.mA_nbin = rep(0, folder)
list_mA_nbin <- list()

cv.error.10.mA_nbin_na = rep(0, folder)
cv.rho.10.mA_nbin_na = rep(0, folder)
list_mA_nbin_na <- list()


df <- df %>% sample_frac(size = 1)
for (i in 1:folder){
  test <- df[((i-1)*a+1):(i*a),]
  train = df %>% filter(!city_id %in% test$city_id)
  
  # Run each model in the training data
  mA_ols.t <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic,
                 data=train)
  # Run each model in the training data
  mA_ols_na.t <- lm(log(activity) ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic,
                    data=train)
  ### GLM Negative Binomial
  mA_nbin.t <- glm.nb(activity_zero ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic,
                      data=train)
  ### GLM Negative Binomial
  mA_nbin_na.t <- glm.nb(activity ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic,
                         data=train)
  
  
  # Store the coefficients
  list_mA_ols[[i]] <- summary(mA_ols.t)
  list_mA_ols_na[[i]] <- summary(mA_ols_na.t)
  list_mA_nbin[[i]] <- summary(mA_nbin.t)
  list_mA_nbin_na[[i]] <- summary(mA_nbin_na.t)
  
  # Out of sample predictions
  cv.error.10.mA_ols[i] = mean(abs(test$activity_ihs - predict(mA_ols.t,test)), na.rm=TRUE)
  cv.rho.10.mA_ols[i] = cor(test$activity_ihs,predict(mA_ols.t,test), use = "complete.obs")
  
  cv.error.10.mA_ols_na[i] = mean(abs(log(test$activity) - predict(mA_ols_na.t,test)), na.rm=TRUE)
  cv.rho.10.mA_ols_na[i] = cor(log(test$activity),predict(mA_ols.t,test), use = "complete.obs")
  
  cv.error.10.mA_nbin[i] = mean(abs(test$activity_zero - predict(mA_nbin.t,test)), na.rm=TRUE)
  cv.rho.10.mA_nbin[i] = cor(test$activity_zero,predict(mA_nbin.t,test), use = "complete.obs")
  
  cv.error.10.mA_nbin_na[i] = mean(abs(test$activity - predict(mA_nbin_na.t,test)), na.rm=TRUE)
  cv.rho.10.mA_nbin_na[i] = cor(test$activity,predict(mA_nbin_na.t,test), use = "complete.obs")
  
}

# Produce fancy plot for model performance
testPerformance <- data.frame(cbind(log(cv.error.10.mA_ols), log(cv.error.10.mA_ols_na), log(cv.error.10.mA_nbin), log(cv.error.10.mA_nbin_na), 
                                    cv.rho.10.mA_ols, cv.rho.10.mA_ols_na, cv.rho.10.mA_nbin, cv.rho.10.mA_nbin_na))

testPerformance <- gather(testPerformance, key, value)

testPerformance[testPerformance==0] <- NA

testPerformance$measure <- c(rep(c("Mean Absolute Error"),40),rep(c("Pearson Correlation Coefficient"),40))
testPerformance$model <- rep(c(c(rep("OLS (IHS)",10),rep("OLS (NAs)",10),rep("Neg. Binomial",10), rep("Neg. Binomial (NAs)",10))),2)

testPerformance$model <- factor(testPerformance$model, levels = c("OLS (IHS)","OLS (NAs)", "Neg. Binomial", "Neg. Binomial (NAs)"))

cv.plot <- ggplot(testPerformance, aes(x = model, y = value, col = model, fill = model)) + 
  geom_point(position = position_jitter(0.1), shape = 21, col = "black", size = 3, stroke = 0.2) + 
  stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.y=mean, geom="point", size=1, col = "white") + 
  facet_wrap(~ measure, scales = "free") +
  scale_colour_manual(values = c(brewer.pal(5,"Blues")[4],brewer.pal(5,"Greens")[4],brewer.pal(5,"Oranges")[4],brewer.pal(5,"BuPu")[4])) +
  scale_fill_manual(values = c(brewer.pal(5,"Blues")[4],brewer.pal(5,"Greens")[4],brewer.pal(5,"Oranges")[4],brewer.pal(5,"BuPu")[4])) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2), width = 0.2, lwd = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", fun.args = list(mult = 2), width = 0.18, lwd = 0.1, col = "white") +
  scale_x_discrete(labels = c("OLS\n(IHS)","OLS\n(log)", "Neg.\nBinom.", "Neg. Binom.\n(NAs removed)")) +
  labs(x = "Model", y = "Cross-validated MAE / Correlation") + 
  theme_bw() + theme(panel.grid = element_blank(),
    legend.position = "none", text = element_text(size = 14))


# Perform LOOCV
loocv.predicted_ols = rep(0, length(df$activity))
loocv.true_ols = rep(0, length(df$activity))
loocv.predicted_nb = rep(0, length(df$activity))
loocv.true_nb = rep(0, length(df$activity))

df <- coding_alone
for (i in 1:length(df$activity)){

  model.ols <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic, data=df[-i,])
  model.nb <- glm.nb(activity_zero ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic, data=df[-i,])
  
  loocv.predicted_ols[i] = predict(model.ols,df[i,])
  loocv.true_ols[i] = df$activity_ihs[i]
  
  loocv.predicted_nb[i] = predict(model.nb,df[i,])
  loocv.true_nb[i] = df$activity_zero[i]
}

# Store results
loocv.performance <- data.frame(loocv.predicted_ols, loocv.true_ols, loocv.predicted_nb, loocv.true_nb)

# Create Annotation with R?? for GAM (LOESS) Fit
round(summary(gam(loocv.true_ols ~ loocv.predicted_ols, data = loocv.performance))$r.sq,2)
pwcorr_ols = expression(paste("R"^"2"," = 0.74"))
round(summary(gam(loocv.true_nb ~ loocv.predicted_nb, data = loocv.performance))$r.sq,2)
pwcorr_nb = expression(paste("              R"^"2"," = 0.33"))

abc <- c(pwcorr_ols,pwcorr_nb)

loocv1 <- loocv.performance[,c(1:2)]
colnames(loocv1) <- c("predicted","true")
loocv1$type <- "OLS (IHS-transf.)"
loocv2 <- loocv.performance[,c(3:4)]
colnames(loocv2) <- c("predicted","true")
loocv2$type <- "Neg. Binom."

loocv <- rbind(loocv1, loocv2)

loocv$type <- factor(loocv$type, levels = c("OLS (IHS-transf.)", "Neg. Binom."))

# Plot LOOCV Fits
loocv.ols <- ggplot(loocv, aes(y = predicted, x=true, col = type)) +
  geom_point() + facet_wrap(~type, scale = "free") + 
  scale_color_manual(values = c(brewer.pal(5,"Blues")[4],brewer.pal(5,"Oranges")[4])) +
  geom_smooth(alpha = 0.2) + 
  #annotate("text", x = 1.25, y = 13, label = abc) +
    labs(x = "LOOCV true observations", y = "LOOCV predicted observations") +
    theme_bw() +
    theme(panel.grid = element_blank(), 
          text = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.position = "none",
          legend.title = element_blank(),
          legend.background = element_rect(colour = "grey", size = 0.5))

loocv.nb <- ggplot(loocv.performance, aes(x=loocv.true_nb, y=loocv.predicted_nb), palette = "jco") +
  geom_point(palette = "jco") + geom_smooth() + annotate("text", x = 30000, y = 5, label = pwcorr_nb)

library(ggpubr)
# Produce Graphic for CV Summary
ggarrange(cv.plot, loocv.ols, nrow = 2)      

#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
## FINAL TABLE
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Compare production and consumption
mA_ols <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_trust,
             data=coding_alone)

summary(mA_ols)

#mA_ols_space <- lm(activity_ihs ~ log(affinity) + log(city_pop) + log(city_pop_dens) + log(city_gdp_pc) + log(city_emp_r) + region_civic,
#             data=subset(coding_alone, distance>40000))

mB_ols <- lm(log(uniques) ~ log(city_pop) + log(affinity) + log(city_emp_r),
             data=coding_alone)

mB_ols_civic <- lm(log(uniques) ~ log(city_pop) + log(affinity) + log(city_emp_r) + region_trust,
                     data=coding_alone)

stargazer(mA_ols, mB_ols, mB_ols_civic, type="text", align=TRUE, 
          dep.var.labels=c("OLS", "OLS"), column.labels=c("Production", "Consumption", "Consumption"),
          title="Coding Alone - Explaining Digital Content Production and Consumption")

# Summary table of cities
stargazer(coding_alone[c("activity_zero", "activity", "activity_ihs", "uniques", "affinity", "region_trust", "city_pop","city_pop_dens","city_gdp_pc","city_emp_r", 
                         "city_age_dep", "region_edu", "region_bband")], summary=TRUE, rownames=FALSE, median = TRUE, omit.summary.stat = c("sd", "p25", "p75"))


# Coef plot
library("dotwhisker")

#mA_ols$coefficients
#summary(mA_ols)[2]
library(broom)
mA_ols2 <- tidy(mA_ols)
mA_ols2$model <- "(1) Contributions"
mA_ols2$model2 <- "(1) Contributions"
mB_ols2 <- tidy(mB_ols)
mB_ols2$model <- "(2) Clicks"
mB_ols2$model2 <- "(2, 3) Clicks"
mB_ols_civic2 <- tidy(mB_ols_civic)
mB_ols_civic2$model <- "(3) Clicks"
mB_ols_civic2$model2 <- "(2, 3) Clicks"

models <- rbind(mA_ols2, mB_ols2, mB_ols_civic2)

x <- dwplot(models, by_2sd = F,
       #style = "distribution",
       dist_args = list(alpha = 0.5),
       dot_args = list(size = 3, alpha = 0.8),
       whisker_args = list()
       #dodge_size = 0.01)
)

#x$data$model <- ifelse(x$data$model == "Model 1", "(1) Contributions",
#                       ifelse(x$data$model == "Model 2", "(2) Clicks", "(3) Clicks"))#
#

x$data$model2 <- ifelse(x$data$model == "(1) Contributions", "(1) Contributions","(2, 3) Clicks")
y <- x$data

x$data <- x$data %>% filter(!is.na(x$data$estimate))
x$data$model <- as.character(x$data$model)
x$data$model2 <- as.character(x$data$model2)
x$data$model2 <- ifelse(x$data$model2 == "(1) Contributions", "(1) Contributions" ,"(2, 3) Clicks")

x %>% #filter(!is.na(x$data$estimate)) %>% 
  relabel_predictors(`log(affinity)` = "Affinity",
                     `log(city_pop)` = "Population",
                    `log(city_gdp_pc)` = "GDP",
                    region_charity = "Charity",
                     `log(city_pop_dens)` = "Pop.\ndensity",
                    `log(city_emp_r)` = "Employment\nratio") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  #coord_fixed(0.6) +
  #facet_wrap(~model2,nrow = 2) +
  coord_flip() +
  facet_grid(model2 ~.) +
  scale_colour_manual(values = c(brewer.pal(5,"Blues")[4],
                                 brewer.pal(5,"Oranges")[4],
                                 brewer.pal(5,"Greens")[4]),
                      name = "Model") +
  scale_fill_manual(values = c(brewer.pal(5,"Blues")[4],
                                 brewer.pal(5,"Oranges")[4],
                               brewer.pal(5,"Greens")[4])) +
  theme_bw() +
  labs(x = "Coefficient", y = "", colour = "", fill = "") +
  theme(legend.position = c(0.35,0.99),
        
        #legend.position = "none",
        legend.justification = c(1, 1),
        panel.grid = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background = element_blank(),
        text = element_text(size = 14),
        legend.title.align = .5)

