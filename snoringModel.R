sleepData <- read.csv("sleep.csv")
str(sleepData)


colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')

sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

# instead we control the variables
# we will create our own new variables
# if sex contains male insert 1 otherwise insert 0
sleepData$noStress <- ifelse(sleepData$StressCat == "no stress", 1, 0)
sleepData$mildStress <- ifelse(sleepData$StressCat == "mild stress", 1, 0)
sleepData$moderateStress <- ifelse(sleepData$StressCat == "moderated stress", 1, 0)
sleepData$highStress <- ifelse(sleepData$StressCat == "high stress", 1, 0)
sleepData$extremeStress <- ifelse(sleepData$StressCat == "extreme stress", 1, 0)

sleepData <- sleepData[c(1:8, 11:15)]

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
names(sleepData)
scatter.smooth(x=SleepHrs,
               y = SnoringRate,
               main = "Sleep duration ~ snoring rate",
               xlab = "Sleep duration",
               ylab = "SnoringRate",
               col = "blue")

cor (SleepHrs, SnoringRate) # -0.92
# looks like their are categories of snoring rate to sleep duration
# within each category there is a positive correlation
# however, there is a negative correlation overall

scatter.smooth(x=RespirationRate,
               y = SnoringRate,
               main = "Snoring Rate ~ respiration rate",
               xlab = "respiration rate (breaths per min)",
               ylab = "Snoring Rate",
               col = "blue")
cor(SnoringRate, RespirationRate)

scatter.smooth(x=BodyTemp,
               y = SnoringRate,
               main = "Snoring rate ~ body temperature",
               xlab = "Body temp (Deg F.)",
               ylab = "Snoring rate",
               col = "blue")
cor(SnoringRate, BodyTemp)
# there seems to be an issue with those who did not sleep
# it may be an option to remove those who did not sleep as this is a sleep study
# if they did not sleep then perhaps they should be removed.

scatter.smooth(x=Movement,
               y = SnoringRate,
               main = "Snoring rate ~ movement",
               xlab = "Movement",
               ylab = "Snoring rate", col = "blue")
cor(SnoringRate, Movement)

scatter.smooth(x=BloodOxygen,
               y = SnoringRate,
               main = "Snoring Rate ~ blood O2",
               xlab = "Blood O2 level (%)",
               ylab = "Snoring Rate",
               col = "blue")
cor(SnoringRate, BloodOxygen)

scatter.smooth(x=REM,
               y = SnoringRate,
               main = "Snoring Rate ~ REM",
               xlab = "REM",
               ylab = "Snoring Rate",
               col = "blue")
cor (SnoringRate, REM)

scatter.smooth(x=HeartRate,
               y = SnoringRate,
               main = "Snoring Rate ~ Heart Rate",
               xlab = "Heart Rate (BPM)",
               ylab = "Snoring Rate",
               col = "BLUE")
cor(SnoringRate, HeartRate)

################################################################

# Training and testing

set.seed(1) # ensures we get the same random sample every time
no_rows_data <- nrow(sleepData) # gets number of rows in sleepData
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) # 70% of dataset
training_data <- sleepData[sample, ] # assigns 70% of sleepData to training_data
testing_data <- sleepData[-sample, ] # assigns the remainder to testing_data
typeof(training_data)
training_data <- as.data.frame(do.call(cbind, training_data))
typeof(training_data)
# building MLR model

attach(training_data)
fit <- lm(SnoringRate ~ HeartRate + RespirationRate + BodyTemp + Movement + 
            BloodOxygen + REM + SleepHrs + noStress + mildStress +
            moderateStress + highStress + extremeStress, data=training_data)

# model evaluation
summary(fit)
# heart rate or respiration rate produce NA. This means that they are collinear and one of them adds nothing to the model
# Therefore one of them must be removed from the model

#fit <- lm(SnoringRate ~ HeartRate + BodyTemp + Movement + BloodOxygen + REM + SleepHrs + noStress + mildStress +
 #           highStress, data=training_data)
#summary(fit)

confint(fit)
?confint

# to determine which variables are collinear
myCors <- data.frame(cor(training_data, method = "spearman"))

fit <- lm(SnoringRate ~ BodyTemp +  
             mildStress +
            highStress, data=training_data)
summary(fit)
vif(fit)
sqrt(vif(fit)) > 2
names(training_data)

myCors <- data.frame(cor(training_data[c(1,3,7,9:12)], method = "spearman"))
confint(fit)

# normality and studentized residuals
library(car)
par(mfrow = c(1, 1))
qqPlot(fit, labels=row.names(sleepData), id.method="identify", simulate=TRUE, main="Q-Q Plot")

training_data["272",]
training_data["384",]

fitted(fit)["272"]
fitted(fit)["384"]



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

# Transforming variables

library(car)
summary(powerTransform(training_data$SnoringRate)) # cant be done as many values in sleepHrs are zero
# from spreadLevelPlot(fit) - suggested power transformation is 1.497262


log_transform_snore <- log(training_data$SnoringRate)
training_data$snore_log <- log_transform_snore

 
fit_model1 <- lm(SnoringRate ~ BodyTemp +  
                   mildStress +
                   highStress, data=training_data)
fit_model2 <- lm(snore_log ~ BodyTemp +  
                   mildStress +
                   highStress, data=training_data)
AIC(fit_model1,fit_model2)

####################################################

# stepwise regression

library(MASS)
fit_test <- lm(SnoringRate ~ BodyTemp +  
                 mildStress +
                 highStress, data=training_data)
stepAIC(fit_test, direction="backward")

summary(fit_model2)

# all subset regression
install.packages("leaps")
library(leaps)
leaps <-regsubsets(SnoringRate ~ BodyTemp +  
                     mildStress +
                     highStress, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

leaps <-regsubsets(snore_log ~ BodyTemp +  
                     mildStress +
                     highStress, data=training_data, nbest=4)
plot(leaps, scale="adjr2", main = "All subsets regression - log transformed")

####################################################
# testing how to inverse a log transform
nums <- c(2,3,4,5,6)
nums_log <- log(nums)
nums_log
converted_nums <- exp(nums_log)
converted_nums

# model training vs testing

predicted_snoring <- predict(fit_model1, testing_data)
predicted_logSnore <- predict(fit_model2, testing_data)
converted_log_snore <- exp(predicted_logSnore)



actuals_predictions <- data.frame(cbind(actuals = testing_data$SnoringRate, predicted = predicted_snoring))
head(actuals_predictions)

actuals_predictions_log <- data.frame(cbind(actuals = testing_data$SnoringRate, predicted = converted_log_snore))
head(actuals_predictions_log)

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

correlation_accuracy_log <- cor(actuals_predictions_log)
correlation_accuracy_log

min_max_accuracy <- mean(apply(actuals_predictions, 1, min) /apply(actuals_predictions, 1, max))
min_max_accuracy

min_max_accuracy <- mean(apply(actuals_predictions_log, 1, min) /apply(actuals_predictions_log, 1, max))
min_max_accuracy

sigma(fit_model1)/ mean(testing_data$SnoringRate)
sigma(fit_model2)/ mean(testing_data$SnoringRate)

##########################################################

# Run some output with the final model

summary(sleepData$SnoringRate)
summary(sleepData$mildStress)
summary(sleepData$highStress)
summary(sleepData$BodyTemp)

# test 1 low bodyTemp, mildStress
df <- data.frame(BodyTemp = c(85), mildStress = c(1), highStress = c(0))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore

# test 1 low bodyTemp, highStress
df <- data.frame(BodyTemp = c(85), mildStress = c(0), highStress = c(1))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore

minTemp <- subset(sleepData, BodyTemp <= 88 & mildStress == 1)
summary(minTemp$SnoringRate)
minTemp <- subset(sleepData, BodyTemp <= 88 & highStress == 1)
summary(minTemp$SnoringRate)

# test 2 90 bodyTemp, mildStress
df <- data.frame(BodyTemp = c(90), mildStress = c(1), highStress = c(0))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore


df <- data.frame(BodyTemp = c(90), mildStress = c(0), highStress = c(1))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore


minMidTemp <- subset(sleepData, BodyTemp >= 88 & BodyTemp <= 92.5 & mildStress == 1)
summary(minMidTemp$SnoringRate) # 0 obs
minMidTemp <- subset(sleepData, BodyTemp >= 88 & BodyTemp <= 92.5 & highStress == 1)
summary(minMidTemp$SnoringRate) # 126
minMidTemp <- subset(sleepData, BodyTemp >= 88 & BodyTemp <= 92.5)
summary(minMidTemp$SnoringRate) # 209 obs

# test 2.5 92.5 bodyTemp, mildStress
df <- data.frame(BodyTemp = c(92.5), mildStress = c(1), highStress = c(0))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore


df <- data.frame(BodyTemp = c(92.5), mildStress = c(0), highStress = c(1))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore

meanTemp <- subset(sleepData, BodyTemp >= 90 & BodyTemp <= 95 & mildStress == 1)
summary(meanTemp$SnoringRate) # 63
meanTemp <- subset(sleepData, BodyTemp >= 90 & BodyTemp <= 95 & highStress == 1)
summary(meanTemp$SnoringRate) # 126
meanTemp <- subset(sleepData, BodyTemp >= 90 & BodyTemp <= 95)
summary(meanTemp$SnoringRate) #127

# test 3 95 bodyTemp, mildStress
df <- data.frame(BodyTemp = c(95), mildStress = c(1), highStress = c(0))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore


df <- data.frame(BodyTemp = c(95), mildStress = c(0), highStress = c(1))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore

training_data
MidHighTemp <- subset(sleepData, BodyTemp >= 92.5 & BodyTemp <= 97 & mildStress == 1)
summary(MidHighTemp$SnoringRate)# 127 obs
MidHighTemp <- subset(sleepData, BodyTemp >= 92.5 & BodyTemp <= 97 & highStress == 1)
summary(MidHighTemp$SnoringRate) # 0 obs
MidHighTemp <- subset(sleepData, BodyTemp >= 92.5 & BodyTemp <= 97)
summary(MidHighTemp$SnoringRate) # 262 obs
MidHighTemp <- subset(training_data, BodyTemp >= 92.5 & BodyTemp <= 97 & highStress == 1)
summary(MidHighTemp$SnoringRate)


# test 4 99 bodyTemp, mildStress
df <- data.frame(BodyTemp = c(99), mildStress = c(1), highStress = c(0))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore


df <- data.frame(BodyTemp = c(99), mildStress = c(0), highStress = c(1))
predicted_snore <- predict(fit_model2, df)
predicted_snore <- exp(predicted_snore)
predicted_snore

MidHighTemp <- subset(sleepData, BodyTemp >= 97 & mildStress == 1)
summary(MidHighTemp$SnoringRate)
MidHighTemp <- subset(sleepData, BodyTemp >= 97 & highStress == 1)
summary(MidHighTemp$SnoringRate)
MidHighTemp <- subset(sleepData, BodyTemp >= 97)
summary(MidHighTemp$SnoringRate)

mean(sleepData$BodyTemp)

par(mfrow = c(3, 2))
hist(training_data$BodyTemp, main = "Distribution of body temperature in training data", xlab = "Body temperature in F")
hist(testing_data$BodyTemp, main = "Distribution of body temperature in testing data", xlab = "Body temperature in F")

hist(training_data$BodyTemp[training_data$mildStress == 1], main = "Distribution of body temperature with mild stress(training)", xlab = "Body temperature in F")
hist(testing_data$BodyTemp[testing_data$mildStress == 1], main = "Distribution of body temperature with mild stress(testing)", xlab = "Body temperature in F")

hist(training_data$BodyTemp[training_data$highStress == 1], main = "Distribution of body temperature with high stress(training)", xlab = "Body temperature in F")
hist(testing_data$BodyTemp[testing_data$highStress == 1], main = "Distribution of body temperature with high stress(testing)", xlab = "Body temperature in F")
