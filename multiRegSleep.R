sleepData <- read.csv("sleep.csv")
str(sleepData)


colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')

sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

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

boxplot(sleepData$noStress, SleepHrs, main = "Sleep duration ~ no Stress",
     xlab = "No Stress",
     ylab = "Sleep duration in hours")

boxplot(sleepData$mildStress, SleepHrs, main = "Sleep duration ~ mild Stress",
        xlab = "mild Stress",
        ylab = "Sleep duration in hours")

plot(sleepData$moderateStress, SleepHrs, main = "Sleep duration ~ moderate Stress",
        xlab = "moderate Stress",
        ylab = "Sleep duration in hours")

boxplot(sleepData$highStress, SleepHrs, main = "Sleep duration ~ high Stress",
        xlab = "high Stress",
        ylab = "Sleep duration in hours")

boxplot(sleepData$extremeStress, SleepHrs, main = "Sleep duration ~ extreme Stress",
        xlab = "extreme Stress",
        ylab = "Sleep duration in hours")

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


