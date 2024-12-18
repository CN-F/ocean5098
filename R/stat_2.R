#xaringanExtra::use_clipboard()

library(gvlma)
library(Hmisc)
library(corrplot)
library(mvabund)
library(ggplot2)
library (lme4)
library(mgcv)
rm(list=ls())

#knitr::include_graphics("illustrations/linear-regression.png")
desktop_path<- "C:\\Users\\user\\Desktop"
Plant_height <- read.csv(file = paste0(desktop_path,"\\Plant_height.csv"), header = TRUE)

mod1_plant<- lm(loght ~ temp, data = Plant_height)

mod1_plant$coefficients

summary(mod1_plant)

confint(mod1_plant)

sigma(mod1_plant)*100/mean(Plant_height$loght)

## fitted(mod1_plant) # predicted values
## residuals(mod1_plant) # residuals
## anova(mod1_plant) # anova table
## vcov(mod1_plant) # covariance matrix for model parameters
## influence(mod1_plant) # regression diagnostics

plot(mod1_plant, which = 1)

par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(mod1_plant$residuals) # Histogram of residuals
plot(mod1_plant, which = 2) # Quantile plot

gvmodel <- gvlma(mod1_plant)
summary(gvmodel)

mod2_plant<- lm(loght ~ temp + alt + rain, data = Plant_height)


plant_predictor<-data.frame(Plant_height$temp,Plant_height$alt,Plant_height$rain)
corr<-cor(plant_predictor)
p.val<-rcorr(as.matrix(plant_predictor))$P
corrplot(corr,type='upper',method='color', addCoef.col = "black",  p.mat=as.matrix(p.val), sig.level = 0.05,diag=F)

# Evaluate Collinearity
car::vif(mod2_plant) # variance inflation factors
sqrt(car::vif(mod2_plant)) > 2 # problem?

summary(mod2_plant)
par(mfrow = c(2, 2))
plot(mod2_plant)


mod3_plant<-lm(formula = loght ~ temp + rain, data = Plant_height)
BIC(mod1_plant); BIC(mod2_plant); BIC(mod3_plant)
AIC(mod1_plant); AIC(mod2_plant); AIC(mod3_plant)

turtles <- read.csv(file = paste0(desktop_path,"\\turtles.csv"), header = TRUE)
str(turtles)

turtles$Temperature <- factor(turtles$Temperature)
boxplot(Days ~ Temperature, data = turtles, ylab = "Hatching time (days)", xlab = "Temperature (C)")

turtles.aov <- aov(Days ~ Temperature, data = turtles)
summary(turtles.aov)

turtles.lm <- lm(Days ~ Temperature, data = turtles)
anova(turtles.lm) 

summary(turtles.lm)

TukeyHSD(turtles.aov)

par(mfrow = c(1, 3)) # This code put two plots in the same window
hist(turtles.aov$residuals)
plot(turtles.aov, which = 2)
plot(turtles.aov, which = 1)

sessile <- read.csv(file = paste0(desktop_path,"\\sessile.csv"), header = TRUE)
# check your predictors with `str`
boxplot(Richness ~ Copper * Orientation, data = sessile, names = c("High.H", "Low.H", "None.H", "High.V", "Low.V", "None.V"), ylab = "Species richness", xlab = "Copper/Orientation", ylim = c(0, 80))

sessile.aov <- aov(Richness ~ Copper * Orientation, data = sessile)
# same as:
sessile.aov <- aov(Richness ~ Copper + Orientation + Copper:Orientation, data = sessile)
summary(sessile.aov)

# same as:
sessile.lm <- lm(Richness ~ Copper * Orientation, data = sessile)
anova(sessile.lm)

interaction.plot(sessile$Copper, sessile$Orientation, sessile$Richness)

# Input data file and check the structure of the data
mink <- read.csv(file = paste0(desktop_path,"\\mink.csv"), header = TRUE)
# grouped boxplot
ggplot(mink, aes(x=Treatment, y=Voles, fill=Area)) + 
    geom_boxplot()

Mink.nested <- aov(Voles ~ Treatment + Area %in% Treatment, data = mink)
summary(Mink.nested)

Crab_PA <- read.csv(paste0(desktop_path,"\\crabs.csv"), header = T)
head(Crab_PA)

ft.crab <- glm(CrabPres ~ Time * Dist, family = binomial, data = Crab_PA)

plot(ft.crab, which = 1)

ft.crab.many <- manyglm(CrabPres ~ Time * Dist, family = "binomial", data = Crab_PA)
plot(ft.crab.many)

anova(ft.crab, test = "Chisq")

anova(ft.crab.many)

summary(ft.crab)

Reveg <- read.csv("data/revegetation.csv", header = T)
head(Reveg)

hist(Reveg$Soleolifera)

ft.sol.pois <- manyglm(Soleolifera ~ Treatment, family = "poisson", data = Reveg)

plot(ft.sol.pois)

ft.sol.nb <- manyglm(Soleolifera ~ Treatment, family = "negative binomiale", data = Reveg)

plot(ft.sol.nb)

anova(ft.sol.nb)
summary(ft.sol.nb)

ft.sol.nb$coefficients

boxplot(Soleolifera ~ Treatment, ylab = "Count", xlab = "Treatment", data = Reveg)

Estuaries <- read.csv("data/estuaries.csv", header = T)

ft.estu <- lmer(Total ~ Modification + (1 | Estuary), data = Estuaries, REML = T)

qqnorm(residuals(ft.estu))

scatter.smooth(residuals(ft.estu) ~ fitted(ft.estu))

ft.estu <- lmer(Total ~ Modification + (1 | Estuary), data = Estuaries, REML = F)
ft.estu.0 <- lmer(Total ~ (1 | Estuary), data = Estuaries, REML = F)

anova(ft.estu.0, ft.estu)

confint(ft.estu)

nBoot <- 1000
lrStat <- rep(NA, nBoot)
ft.null <- lm(Total ~ Modification, data = Estuaries) # null model
ft.alt <- lmer(Total ~ Modification + (1 | Estuary), data = Estuaries, REML = F) # alternate model
lrObs <- 2 * logLik(ft.alt) - 2 * logLik(ft.null) # observed test stat
for (iBoot in 1:nBoot)
{
  Estuaries$TotalSim <- unlist(simulate(ft.null)) # resampled data
  bNull <- lm(TotalSim ~ Modification, data = Estuaries) # null model
  bAlt <- lmer(TotalSim ~ Modification + (1 | Estuary), data = Estuaries, REML = F) # alternate model
  lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
}
mean(lrStat > lrObs) # P-value for test of Estuary effect

ModEst <- unique(Estuaries[c("Estuary", "Modification")]) # find which Estuaries are modified

# Prepare a vector of colors with specific color by modification levels
myColors <- ifelse(unique(ModEst$Modification) == "Modified", rgb(0.1, 0.1, 0.7, 0.5),
  ifelse(unique(ModEst$Modification) == "Pristine", rgb(0.8, 0.1, 0.3, 0.6),
    "grey90"
  )
)

boxplot(Total ~ Estuary, data = Estuaries, col = myColors, xlab = "Estuary", ylab = "Total invertebrates")
legend("bottomleft",
  inset = .02,
  c(" Modified ", " Pristine "), fill = unique(myColors), horiz = TRUE, cex = 0.8
)

# 0 if Modified, 1 if Pristine
is.mod <- ifelse(unique(ModEst$Modification) == "Modified", 0,
  ifelse(unique(ModEst$Modification) == "Pristine", 1, NA)
)

Est.means <- coef(ft.estu)$Estuary[, 1] + coef(ft.estu)$Estuary[, 2] * is.mod # Model means

stripchart(Est.means ~ sort(unique(Estuary)), data = Estuaries, pch = 18, col = "red", vertical = TRUE, add = TRUE)

Estuaries[1:10, ]
xtabs(~ Estuary + Site, Estuaries, sparse = TRUE)

Estuaries$Site <- as.factor(Estuaries$Site)
Estuaries$SiteWithin <- paste0(Estuaries$Estuary, "_", Estuaries$Site)

xtabs(~ Estuary + SiteWithin, Estuaries, sparse = TRUE)

fit.mod <- lmer(Total ~ Modification + (1 | Estuary) + (1 | SiteWithin), data = Estuaries)
# same as: lmer(Total ~ Modification + (1 | Estuary/SiteWithin), data = Estuaries)
summary(fit.mod)

fit.wrong <- lmer(Total ~ Modification + (1 | Estuary) + (1 | Site), data = Estuaries)
summary(fit.wrong)

par(mfrow = c(1, 2))
qqnorm(residuals(fit.mod))
scatter.smooth(residuals(fit.mod) ~ fitted(fit.mod)) # residual plot

fit.mod <- lmer(log(Total) ~ Modification + (1 | Estuary) + (1 | SiteWithin), data = Estuaries)
par(mfrow = c(1, 2))
qqnorm(residuals(fit.mod))
scatter.smooth(residuals(fit.mod) ~ fitted(fit.mod)) # residual plot

ft.mod <- lmer(log(Total) ~ Modification + (1 | Estuary) + (1 | SiteWithin), data = Estuaries, REML = F)
ft.mod.0 <- lmer(log(Total) ~ (1 | Estuary) + (1 | SiteWithin), data = Estuaries, REML = F)
anova(ft.mod.0, ft.mod)

nBoot <- 1000
lrStat <- rep(NA, nBoot)
ft.null <- lmer(log(Total) ~ Modification + (1 | Estuary), Estuaries, REML = F) # null model
ft.alt <- lmer(log(Total) ~ Modification + (1 | Estuary) + (1 | SiteWithin), Estuaries, REML = F) # alternate model
lrObs <- 2 * logLik(ft.alt) - 2 * logLik(ft.null) # observed test stat
for (iBoot in 1:nBoot)
{
  Estuaries$logTotalSim <- unlist(simulate(ft.null)) # resampled data
  bNull <- lmer(logTotalSim ~ Modification + (1 | Estuary), Estuaries, REML = F) # null model
  bAlt <- lmer(logTotalSim ~ Modification + (1 | Estuary) + (1 | SiteWithin), Estuaries, REML = F) # alternate model
  lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
}
mean(lrStat > lrObs) # P-value for test of Estuary effect

Estuaries$Hydroid

Estuaries$HydroidPres <- Estuaries$Hydroid > 0

fit.bin <- glmer(HydroidPres ~ Modification + (1 | Estuary), family = binomial, data = Estuaries)

par(mfrow = c(1, 2))
plot(residuals(fit.bin) ~ fitted(fit.bin), main = "residuals v.s. Fitted")
qqnorm(residuals(fit.bin))

nBoot <- 1000
lrStat <- rep(NA, nBoot)
ft.null <- glmer(HydroidPres ~ 1 + (1 | Estuary), family = binomial, data = Estuaries) # null model
ft.alt <- glmer(HydroidPres ~ Modification + (1 | Estuary), family = binomial, data = Estuaries) # alternate model

lrObs <- 2 * logLik(ft.alt) - 2 * logLik(ft.null) # observed test stat

for (iBoot in 1:nBoot)
{
  Estuaries$HydroidPresSim <- unlist(simulate(ft.null)) # resampled data
  tryCatch(
    { # sometimes the glmer code doesn't converge

      bNull <- glmer(HydroidPresSim ~ 1 + (1 | Estuary), family = binomial, data = Estuaries) # null model
      bAlt <- glmer(HydroidPresSim ~ Modification + (1 | Estuary), family = binomial, data = Estuaries) # alternate model
      lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
    },
    warning = function(war) {
      lrStat[iBoot] <- NA
    },
    error = function(err) {
      lrStat[iBoot] <- NA
    }
  ) # if code doesn't converge skip sim
}
mean(lrStat > lrObs, na.rm = T) # P-value for test of Estuary effect

fit.pois <- glmer(Hydroid ~ Modification + (1 | Estuary), family = poisson, data = Estuaries)

par(mfrow = c(1, 2))
plot(residuals(fit.pois) ~ fitted(fit.pois), main = "Residuals vs. Fitted")
qqnorm(residuals(fit.pois))

nBoot <- 1000
lrStat <- rep(NA, nBoot)
ft.null <- glmer(Hydroid ~ 1 + (1 | Estuary), family = poisson, data = Estuaries) # null model
ft.alt <- glmer(Hydroid ~ Modification + (1 | Estuary), family = poisson, data = Estuaries) # alternate model

lrObs <- 2 * logLik(ft.alt) - 2 * logLik(ft.null) # observed test stat
for (iBoot in 1:nBoot)
{
  Estuaries$HydroidSim <- unlist(simulate(ft.null)) # resampled data
  tryCatch(
    {
      bNull <- glmer(HydroidSim ~ 1 + (1 | Estuary), family = poisson, data = Estuaries) # null model
      bAlt <- glmer(HydroidSim ~ Modification + (1 | Estuary), family = poisson, data = Estuaries) # alternate model
      lrStat[iBoot] <- 2 * logLik(bAlt) - 2 * logLik(bNull) # resampled test stat
    },
    warning = function(war) {
      lrStat[iBoot] <- NA
    },
    error = function(err) {
      lrStat[iBoot] <- NA
    }
  ) # if code doesn't converge skip sim#   lrStat[iBoot]
}

fit.pois2 <- glmer(Schizoporella.errata ~ Modification + (1 | Estuary), family = poisson, data = Estuaries)
par(mfrow = c(1, 2))
plot(residuals(fit.pois) ~ fitted(fit.pois), main = "residuals vs. Fitted")
qqnorm(residuals(fit.pois))

fit.pois <- glmer(Hydroid ~ Modification + (1 | Estuary), family = poisson, data = Estuaries)
means <- fitted(fit.pois) # this will give the estimate at each data point
ModEst <- unique(Estuaries[c("Estuary", "Modification")]) # find which Estuaries are modified
cols <- as.numeric(ModEst[order(ModEst[, 1]), 2]) + 3 # Assign colour by modification
boxplot(Hydroid ~ Estuary, data = Estuaries, col = cols, xlab = "Estuary", ylab = "Count of hydroids")
legend("topleft",
  inset = .02,
  c("Modified", "Pristine"), fill = unique(cols), horiz = TRUE, cex = 0.8
)

Est.means <- summarize(means, Estuaries$Estuary, mean)$means # extract means by Estuary
stripchart(Est.means ~ sort(unique(Estuary)), data = Estuaries, pch = 18, col = "red", vertical = TRUE, add = TRUE) # plot means by estuary

x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)
y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))
Sample_data <- data.frame(y, x)
ggplot(Sample_data, aes(x, y)) +
  geom_point()

lm_y <- lm(y ~ x, data = Sample_data)
ggplot(Sample_data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = lm)
plot(lm_y, which = 1)

gam_y <- gam(y ~ s(x), method = "REML")

x_new <- seq(0, max(x), length.out = 100)
y_pred <- predict(gam_y, data.frame(x = x_new))

ggplot(Sample_data, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x))

par(mfrow = c(2, 2))
gam.check(gam_y)

CO2 <- read.csv("data/mauna_loa_co2.csv")
CO2$time <- as.integer(as.Date(CO2$Date, format = "%d/%m/%Y"))
CO2_dat <- CO2
CO2 <- CO2_dat[CO2_dat$year %in% (2000:2010), ]

ggplot(CO2_dat, aes(time, co2)) +
  geom_line()

CO2_time <- gam(co2 ~ s(time), data = CO2, method = "REML")

plot(CO2_time)

par(mfrow = c(2, 2))
gam.check(CO2_time)

CO2_season_time <- gam(co2 ~ s(month, bs = "cc", k = 12) + s(time), data = CO2, method = "REML")

par(mfrow = c(1, 2))
plot(CO2_season_time)

par(mfrow = c(2, 2))
gam.check(CO2_season_time)

CO2_season_time <- gam(co2 ~ s(month, bs = "cc", k = 12) + s(time), data = CO2_dat, method = "REML")
par(mfrow = c(1, 2))
plot(CO2_season_time)

CO2_pred <- data.frame(
  time = CO2_dat$time,
  co2 = CO2_dat$co2,
  predicted_values = predict(CO2_season_time, newdata = CO2_dat)
)
ggplot(CO2_pred, aes(x = time)) +
  geom_point(aes(y = co2), size = 1, alpha = 0.5) +
  geom_line(aes(y = predicted_values), colour = "red")

