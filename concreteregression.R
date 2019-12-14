library(leaps)
library(MASS)

data=read.table("Concrete_Data.txt", sep="\t", header=TRUE)
names(data)<-c("Cement", "Blast Furnace", "Ash", "Water", "SuperPlasticizer", "CoarseAgg",
               "FineAgg", "Age", "Strength")

cement=data$Cement
blast=data$`Blast Furnace`
ash=data$Ash
water=data$Water
super=data$SuperPlasticizer
coarse=data$CoarseAgg
fine=data$FineAgg
age=data$Age
strength=data$Strength

# Scatterplot Matrix of predictors
pairs(~strength + age + fine + coarse + super + water + ash + blast + cement)

# Check the correlation matrix for any high correlation values
cor(data)

# Apply stepwise regression
basemod=lm(strength~1)
stepmod=lm(strength ~ age + fine + coarse + super + water + ash + blast + cement)
step(basemod, scope = list(lower=basemod, upper=stepmod))

stepwise=lm(strength ~ cement + super + age + blast + water + 
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

summary(stepwise)

# Check for interaction terms
super2=super^2
superage=super*age
cementwater=cement*water
cementage=cement*age

estimate=fitted(stepwise)
errors=strength-estimate

plot(stepwise)
plot(estimate, errors, ylab="Residuals", xlab="Fitted")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)
plot(super2, errors)
plot(superage, errors)
shapiro.test(errors)
qqnorm(errors)
qqline(errors)

# F-tests to see which terms to add
add1(stepwise, ~.+super2+superage+cementwater+cementage, test="F")

# From the general linear F Test we add super^2 and apply again
stepwise=lm(strength ~ cement + super + age + blast + water + 
              ash+ super*age)

# F-tests to see which terms to add
add1(stepwise, ~.+super2+superage+cementwater+cementage, test="F")

stepwise=lm(strength ~ cement + super + age + blast + water + 
              ash+ super*age + I(super^2))

estimate=fitted(stepwise)
errors=strength-estimate

plot(stepwise)
plot(estimate, errors, ylab="Residuals", xlab="Fitted")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)
plot(super2, errors)
plot(superage, errors)
shapiro.test(errors)
qqnorm(errors)
qqline(errors)

# Log transform age because non-linear
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ super*log(age) + I(super^2))

estimate=fitted(stepwise)
errors=strength-estimate

plot(stepwise)
plot(estimate, errors, ylab="Residuals", xlab="Fitted")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)
plot(super2, errors)
plot(superage, errors)
shapiro.test(errors)
qqnorm(errors)
qqline(errors)


# Try Box-Cox transform for fanning effect
trans=boxcox(strength ~ cement + super + log(age) + blast + water + 
              ash+ super*log(age)+ I(super^2), data=data, lambda=seq(0, 1, length=10))

str=strength^(0.6)
stepwise2=lm(str ~ cement + super + log(age) + blast + water + 
              ash+ super*log(age)+ I(super^2) )
estimate=fitted(stepwise2)
errors=strength-estimate
plot(estimate, errors, xlab="Fitted", ylab="Residuals")
plot(stepwise2)

qqnorm(errors)
qqline(errors)

# Check Cook's Distance for influential points
cookdist=(cooks.distance(stepwise))
plot(cookdist)

# DFFITS Line for influential points
dfvals=(dffits(stepwise))
cutoff=2*sqrt((10/(n-10-1)))
plot(dfvals)
abline(h=0.1981267, col='red')

# Find externally studentized residual
r_ext=sort(rstudent(stepwise))
n<-length(strength)
r_ext[n]

# Delete externally studentized residual
data<-data[-382,]
rownames(data) <- 1:nrow(data)

cement=data$Cement
blast=data$`Blast Furnace`
ash=data$Ash
water=data$Water
super=data$SuperPlasticizer
coarse=data$CoarseAgg
fine=data$FineAgg
age=data$Age
strength=data$Strength

stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ super*log(age)+ I(super^2) )

# Check whether interaction term cement*water has effect on concrete strength
stepwise2=lm(strength ~ cement + super + log(age) + blast + water + 
               ash+ super*log(age)+ I(super^2) + cement*water )

anova(stepwise, stepwise2)

fit=fitted(stepwise)
errors=strength-fit
shapiro.test(errors)

# Prediction Interval for strength
p1=data.frame(blast=0, ash=0, cement=mean(cement), water=mean(water), super=mean(super),
              age=mean(age))

predictstr=predict(stepwise, p1, se.fit = TRUE, interval = "prediction", level = 0.95)
print(predictstr$fit)

# Confidence interval for strength
p2=data.frame(blast=mean(blast), ash=mean(ash), cement=mean(cement), water=mean(water), super=mean(super), age=365)

confstr=predict(stepwise, p2, se.fit = TRUE, interval = "confidence", level = 0.95)
print(confstr$fit)
