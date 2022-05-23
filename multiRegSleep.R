sleepData <- read.csv("sleep.csv")
str(sleepData)


colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')

sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)
attach(sleepData)
kruskal.test(SleepHrs, StressCat)

install.packages("psych")
library(psych)

pairs.panels(sleepData,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# blood oxygen and body temperature have a perfect correlation with sleep hours, 
# therefore may violate assumptions and be dropped

# if we ask r to build the modelat this point, 
# rwill automatically split the factor variables
# alternatively we want to control the model
attach(sleepData)
set.seed(1)

# the thing we are trying to predict goes on the left of the ~
# we dont have full control when we create our mode this way. 
# it does n-1 and removes one of your categories
# dont build your model this way
model <- lm(formula = SleepHrs ~ SnoringRate + RespirationRate + 
              BodyTemp + Movement + BloodOxygen + 
              REM + HeartRate + StressCat,
            data = sleepData)
summary(model)
# why is HeartRate and stressCat^4 NA?
# the model is giving an adjusted R-squared = 1. This suggests there is something wrong. 
# Probably violates assumptions

# instead we control the variables
# we will create our own new variables
# if sex contains male insert 1 otherwise insert 0
sleepData$noStress <- ifelse(sleepData$StressCat == "no stress", 1, 0)
sleepData$mildStress <- ifelse(sleepData$StressCat == "mild stress", 1, 0)
sleepData$moderateStress <- ifelse(sleepData$StressCat == "moderated stress", 1, 0)
sleepData$highStress <- ifelse(sleepData$StressCat == "high stress", 1, 0)
sleepData$extremeStress <- ifelse(sleepData$StressCat == "extreme stress", 1, 0)

names(sleepData)
# drop unneeded variable names
# dont need stressLevel or stressCat anymore
sleepData <- sleepData[c(1:8, 11:15)]

# round variables to 1 decimal place
sleepData$SnoringRate <- round(sleepData$SnoringRate, 1)
sleepData$RespirationRate <- round(sleepData$RespirationRate, 1)
sleepData$BodyTemp <- round(sleepData$BodyTemp, 1)
sleepData$Movement <- round(sleepData$Movement, 1)
sleepData$BloodOxygen <- round(sleepData$BloodOxygen, 1)
sleepData$REM <- round(sleepData$REM, 1)
sleepData$SleepHrs <- round(sleepData$SleepHrs, 1)
sleepData$HeartRate <- round(sleepData$HeartRate, 1)

opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 3)) # divide graph area in 2 columns
# Check assumptions
# correlation
# there must be a correlation between sleepHrs and the other variables
scatter.smooth(x=SnoringRate,
               y = SleepHrs,
               main = "Sleep duration ~ snoring rate",
               xlab = "Snoring rate",
               ylab = "Sleep duration (hours)")
# looks like their are categories of snoring rate to sleep duration
# within each category there is a positive correlation
# however, there is a negative correlation overall

scatter.smooth(x=RespirationRate,
               y = SleepHrs,
               main = "Sleep duration ~ respiration rate",
               xlab = "respiration rate (breaths per min)",
               ylab = "Sleep duration in hours")

scatter.smooth(x=BodyTemp,
               y = SleepHrs,
               main = "Sleep duration ~ body temperature",
               xlab = "Body temp (Deg F.)",
               ylab = "Sleep duration in hours")

# there seems to be an issue with those who did not sleep
# it may be an option to remove those who did not sleep as this is a sleep study
# if they did not sleep then perhaps they should be removed.

scatter.smooth(x=Movement,
               y = SleepHrs,
               main = "Sleep duration ~ movement",
               xlab = "Movement",
               ylab = "Sleep duration in hours")

scatter.smooth(x=BloodOxygen,
               y = SleepHrs,
               main = "Sleep duration ~ blood O2",
               xlab = "Blood O2",
               ylab = "Sleep duration in hours")

scatter.smooth(x=REM,
               y = SleepHrs,
               main = "Sleep duration ~ REM",
               xlab = "REM",
               ylab = "Sleep duration in hours")

scatter.smooth(x=HeartRate,
               y = SleepHrs,
               main = "Sleep duration ~ Heart Rate",
               xlab = "Heart Rate (BPM)",
               ylab = "Sleep duration in hours")

scatter.smooth(x=sleepData$noStress,
               y = SleepHrs,
               main = "Sleep duration ~ no Stress",
               xlab = "No Stress",
               ylab = "Sleep duration in hours")

#sleepData$noStress <- factor(sleepData$noStress, 
#                              labels = c("no stress", "stress"),
#                              ordered = TRUE)

par(mfrow = c(1, 1))
par <- opar
boxplot(SleepHrs[noStress==1], SleepHrs[mildStress==1], SleepHrs[moderateStress==1], SleepHrs[highStress==1], SleepHrs[extremeStress==1], 
        main = "Sleep duration ~ Stress level",
        xlab = "Stress level",
        ylab = "Sleep duration in hours",
        col = "blue")
axis(1,
     at = 1:5,
     labels = c("no stress", "mild", "moderate", "high", "extreme"),
     las = 1)


# test for correlation
# anything below 0.3 is week
cor (SleepHrs, SnoringRate) # -0.92
cor(SleepHrs, RespirationRate)# -0.89
cor(SleepHrs, BodyTemp)# 0.95
cor(SleepHrs, Movement)# -0.9
cor(SleepHrs, BloodOxygen)# 0.95
cor (SleepHrs, REM)# -0.89
cor(SleepHrs, HeartRate)# -0.89
cor(SleepHrs, sleepData$noStress)# 0.7
cor(SleepHrs, sleepData$mildStress)# 0.37
cor(SleepHrs, sleepData$moderateStress)# -0.03
cor(SleepHrs, sleepData$highStress)# -0.44
cor(SleepHrs, sleepData$extremeStress)# -0.6

# dump mild, moderate and high stress from the model as they have a low correlation
sleepData <- sleepData[c(1:9, 13)]
# rebuild model
model <- lm(formula = SleepHrs ~ SnoringRate + RespirationRate + 
              BodyTemp + Movement + BloodOxygen + 
              REM + HeartRate + noStress + extremeStress,
            data = sleepData)
summary(model) # Adjusted R-squared = 0.9996

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(4,2)) # plot side by side

# testing for normality
hist(SleepHrs, 
     main = "Sleep hours", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Sleep hours")

hist(Movement, 
     main = "Movement", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Movement")

hist(REM, 
     main = "REM in minutes", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM")

hist(HeartRate, 
     main = "Heart rate in BPM", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Heart rate")

names(sleepData)
hist(SnoringRate, 
     main = "Snoring rate", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Snoring rate")

hist(RespirationRate, 
     main = "Respiration in breaths per minute", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Respiration rate")

hist(BodyTemp, 
     main = "Body temperature in deg F", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Body tem in F")

hist(BloodOxygen, 
     main = "Blood Oxygen level in %", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Blood oxygen level")
par(opar)
par(mfrow = c(3, 3))
# normality
with(sleepData, {
  qqnorm(SnoringRate, 
         main = "Normality analysis of Snoring Rate data")
  qqline(SnoringRate)
})

with(sleepData, {
  qqnorm(RespirationRate, 
         main = "Normality analysis of Respiration Rate data")
  qqline(RespirationRate)
})

with(sleepData, {
  qqnorm(BodyTemp, 
         main = "Normality analysis of Body Temp data")
  qqline(BodyTemp)
})

with(sleepData, {
  qqnorm(Movement, 
         main = "Normality analysis of Movement data")
  qqline(Movement)
})

with(sleepData, {
  qqnorm(BloodOxygen, 
         main = "Normality analysis of Blood Oxygen data")
  qqline(BloodOxygen)
})

with(sleepData, {
  qqnorm(REM, 
         main = "Normality analysis of REM data")
  qqline(REM)
})

with(sleepData, {
  qqnorm(HeartRate, 
         main = "Normality analysis of REM data")
  qqline(HeartRate)
})

with(sleepData, {
  qqnorm(SleepHrs, 
         main = "Normality analysis of Sleep duration")
  qqline(HeartRate)
})

# normality of categorical data
with(sleepData, {
  qqnorm(SleepHrs[noStress == 1], 
         main = "Normality analysis of REM data")
  qqline(SleepHrs[noStress == 1])
})

with(sleepData, {
  qqnorm(SleepHrs[noStress == 0], 
         main = "Normality analysis of REM data")
  qqline(SleepHrs[noStress == 0])
})

normality_test <- shapiro.test(SnoringRate)
normality_test$p.value # 5.150978e-21

normality_test <- shapiro.test(RespirationRate)
normality_test$p.value # 1.579149e-15

normality_test <- shapiro.test(SleepHrs)
normality_test$p.value # 2.166796e-20

normality_test <- shapiro.test(REM)
normality_test$p.value # 1.315245e-15

normality_test <- shapiro.test(Movement)
normality_test$p.value # 3.218008e-14

normality_test <- shapiro.test(HeartRate)
normality_test$p.value # 1.570963e-15

normality_test <- shapiro.test(BodyTemp)
normality_test$p.value # 7.014186e-09

normality_test <- shapiro.test(BloodOxygen)
normality_test$p.value # 1.347201e-11

normality_test <- shapiro.test(SleepHrs[noStress == 1])
normality_test$p.value # 0.0002949122

normality_test <- shapiro.test(SleepHrs[noStress == 0])
normality_test$p.value # 6.815756e-20

normality_test <- shapiro.test(SleepHrs[extremeStress == 1])
normality_test$p.value # 6.815756e-20

normality_test <- shapiro.test(SleepHrs[extremeStress == 0])
normality_test$p.value # 1.792434e-13