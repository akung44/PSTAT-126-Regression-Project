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

#Stepwise Regression
basemod=lm(strength~1)
stepmod=lm(strength ~ age + fine + coarse + super + water + ash + blast + cement)
step(basemod, scope = list(lower=basemod, upper=stepmod))

stepwise=lm(strength ~ cement + super + age + blast + water + 
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

summary(stepwise)

# Check residuals vs predictor for any transformations that need to be made to meet LINE conditions

plot(fittedstep, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

# Check for normality
qqnorm(errors)
qqline(errors)

shapiro.test(errors)

# Try Best Subset Regression to see if there is a better model
reg=regsubsets(subset(data, select=-Strength), strength, nvmax=10)
mod=summary(reg)
mod$which
mod$adjr2

r2mod=lm(strength ~ age + fine + coarse + super + water + ash + blast + cement)
summary(r2mod)

# Check LINE conditions of Best Subset with adjusted R2

fittedr2=fitted(r2mod)
errors=strength-fittedr2

plot(fittedr2, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

qqnorm(errors)
qqline(errors)

# Check which model has the best Mallow's Cp
mod$cp
# Since none of our Mallow's Cp is close to p, we will not consider this measure

# We then see that our best subset model by adjusted R2 (full model) has more predictors, looks similar, and has a lower adjusted R2 then our stepwise regression
# As a result, we will choose our stepwise regression model

# Log transform age from our stepwise regression model
library(MASS)
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

plot(fittedstep, errors, xlab="fitted", ylab="residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

qqnorm(errors)
qqline(errors)

shapiro.test(errors)

# Studentized residuals outliers

r_ext=sort(rstudent(stepwise))
n<-length(strength)
r_ext[n]

# Delete points individually, rerun regression and check for influential y values
data<-data[-382,]
rownames(data) <- 1:nrow(data)
# Check our regression with the studentized residuals deleted 382, 384, 15, 9, 405
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
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

# Check the LINE conditions in residual vs predictor

plot(fittedstep, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

# Check the normality condition
qqnorm(errors)
qqline(errors)

shapiro.test(errors)
hist(errors)

# Delete another point
r_ext=sort(rstudent(stepwise))
n<-length(strength)
r_ext[n]

data<-data[-383,]
rownames(data) <- 1:nrow(data)

#Check again to see if normality is achieved

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
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

qqnorm(errors)
qqline(errors)

shapiro.test(errors)

# We delete 2 externally studentized points and now our regression achieves normality by the Shapiro Test


# We now check residuals vs predictor for other variables we did not include with general linear F tests
combined1=cement*water
combined2=cement*age
combined3=cement^2
combined4=cement*coarse
combined5=water*coarse
combined6=water*fine
combined8=blast*fine
combined9=water^2
combined10=blast*cement
combined11=blast*ash
combined12=super*water
combined13=super*cement
combined14=super^2

add1(stepwise, ~.+combined1+combined2+combined3+combined4+combined5+combined6+combined8+combined9+combined10+combined11+combined12+combined13+combined14 , test="F")

# From the general linear F Test we add super^2 and apply again
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ I(super^2))

add1(stepwise, ~.+combined1+combined2+combined3+combined4+combined5+combined6+combined8+combined9+combined10+combined11+combined12+combined13+combined14 , test="F")

# From the general linear F test we add cement*age and apply again
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ I(super^2) + cement*age)

add1(stepwise, ~.+combined1+combined2+combined3+combined4+combined5+combined6+combined8+combined9+combined10+combined11+combined12+combined13+combined14 , test="F")

# From the general linear F test we add super*water and apply again
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ I(super^2) + cement*age + super*water)


# We again check the LINE conditions with our new parameters that we added
cement=data$Cement
blast=data$`Blast Furnace`
ash=data$Ash
water=data$Water
super=data$SuperPlasticizer
coarse=data$CoarseAgg
fine=data$FineAgg
age=data$Age
strength=data$Strength

fittedstep=fitted(stepwise)
errors=strength-fittedstep

qqnorm(errors)
qqline(errors)

shapiro.test(errors)

plot(fittedstep, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

# Our final model is the following below
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash+ I(super^2) + cement*age + super*water)