# 
sleepData <- read.csv("sleep.csv")
str(sleepData)


colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')

sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

# examine relationships between variables

# check for linearity
variables_of_interest <- c('SnoringRate', 'RespirationRate', 
                           'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                           'SleepHrs', 'HeartRate')
pairs(sleepData[variables_of_interest], cex.labels = 1.5)


attach(sleepData)
# scatterplots
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 3)) # divide graph area in 2 columns
# Check assumptions
# correlation
# there must be a correlation between sleepHrs and the other variables
scatter.smooth(x=SnoringRate,
               y = SleepHrs,
               main = "Sleep duration ~ snoring rate",
               xlab = "Snoring rate",
               ylab = "Sleep duration (hours)",
               col = "blue")

cor (SleepHrs, SnoringRate) # -0.92
# looks like their are categories of snoring rate to sleep duration
# within each category there is a positive correlation
# however, there is a negative correlation overall

scatter.smooth(x=RespirationRate,
               y = SleepHrs,
               main = "Sleep duration ~ respiration rate",
               xlab = "respiration rate (breaths per min)",
               ylab = "Sleep duration in hours",
               col = "blue")
cor(SleepHrs, RespirationRate)

scatter.smooth(x=BodyTemp,
               y = SleepHrs,
               main = "Sleep duration ~ body temperature",
               xlab = "Body temp (Deg F.)",
               ylab = "Sleep duration in hours",
               col = "blue")
cor(SleepHrs, BodyTemp)
# there seems to be an issue with those who did not sleep
# it may be an option to remove those who did not sleep as this is a sleep study
# if they did not sleep then perhaps they should be removed.

scatter.smooth(x=Movement,
               y = SleepHrs,
               main = "Sleep duration ~ movement",
               xlab = "Movement",
               ylab = "Sleep duration in hours", col = "blue")
cor(SleepHrs, Movement)

scatter.smooth(x=BloodOxygen,
               y = SleepHrs,
               main = "Sleep duration ~ blood O2",
               xlab = "Blood O2 level (%)",
               ylab = "Sleep duration in hours",
               col = "blue")
cor(SleepHrs, BloodOxygen)

scatter.smooth(x=REM,
               y = SleepHrs,
               main = "Sleep duration ~ REM",
               xlab = "REM",
               ylab = "Sleep duration in hours",
               col = "blue")
cor (SleepHrs, REM)

scatter.smooth(x=HeartRate,
               y = SleepHrs,
               main = "Sleep duration ~ Heart Rate",
               xlab = "Heart Rate (BPM)",
               ylab = "Sleep duration in hours",
               col = "BLUE")
cor(SleepHrs, HeartRate)
