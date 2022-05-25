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


# checking for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 3)) # divide graph area in 3 rows by 2 columns
names(sleepData)
boxplot(SnoringRate,
        main = "Snoring Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(SnoringRate)$out)) # box plot for 'SnoringRate'
boxplot(RespirationRate,
        main = "Respiration Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(RespirationRate)$out)) # box plot for 'RespirationRate'
boxplot(BodyTemp,
        main = "Body Temperature",
        sub = paste("Outlier rows: ",
                    boxplot.stats(BodyTemp)$out)) # box plot for 'BodyTemp'
boxplot(Movement,
        main = "Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Movement)$out)) # box plot for 'Movement'
boxplot(BloodOxygen,
        main = "Blood Oxygen",
        sub = paste("Outlier rows: ",
                    boxplot.stats(BloodOxygen)$out)) # box plot for 'BloodOxygen'
boxplot(REM,
        main = "REM",
        sub = paste("Outlier rows: ",
                    boxplot.stats(REM)$out)) # box plot for 'REM'
boxplot(SleepHrs,
        main = "SleepHrs",
        sub = paste("Outlier rows: ",
                    boxplot.stats(SleepHrs)$out)) # box plot for 'SleepHrs'
boxplot(HeartRate,
        main = "Heart Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(HeartRate)$out)) # box plot for 'HeartRate'
detach(sleepData)
par(opar)

#####################################################################################################
# Training and testing

set.seed(1) # ensures we get the same random sample every time
no_rows_data <- nrow(sleepData) # gets number of rows in sleepData
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) # 70% of dataset
training_data <- sleepData[sample, ] # assigns 70% of sleepData to training_data
testing_data <- sleepData[-sample, ] # assigns the remainder to testing_data


# building MLR model
attach(sleepData)
fit <- lm(SleepHrs ~ HeartRate + RespirationRate + BodyTemp + Movement + BloodOxygen + REM + SnoringRate, data=sleepData)# 

# model evaluation
summary(fit)
# heart rate or respiration rate produce NA. This means that they are colinear and one of them adds nothing to the model
# Therefore one of them must be removed from the model

fit <- lm(SleepHrs ~ HeartRate + BodyTemp + Movement + BloodOxygen + REM + SnoringRate, data=sleepData)
summary(fit)

confint(fit)
?confint

# normality and studentized residuals
library(car)
par(mfrow = c(1, 1))
qqPlot(fit, labels=row.names(sleepData), id.method="identify", simulate=TRUE, main="Q-Q Plot")

training_data["35",]
training_data["253",]

fitted(fit)["35"]
fitted(fit)["253"]

student_fit <- rstudent(fit)
hist(student_fit,
     breaks=10,
     freq=FALSE,
     xlab="Studentized Residual",
     main="Distribution of Errors")
rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(fit) # identified 35 as an outlier
# sleepData <- sleepData[-c(35),] # remove row 35

############################################################

# linearity

crPlots(fit)

# influencial observations
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff, col = "blue")
abline(h = cutoff, lty = 2, col = "red")

avPlots(fit, ask=FALSE)

influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

################################################################

# Homoscedasticity

ncvTest(fit)

spreadLevelPlot(fit)

########################################################

# global validation of linear model assumptions

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)


###################################################

# multicolinearity
vif(fit)
sqrt(vif(fit)) > 2

# to determine which variables are collinear
myCors <- data.frame(cor(sleepData[1:8], method = "spearman"))



