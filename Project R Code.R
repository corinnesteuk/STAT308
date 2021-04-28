### WINE QUALITY R CODE ###

# This data set is describing variants of the Portuguese "Vinho Verde" red wine. The original data set has attributes 
# 1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulfates
# 11 - alcohol (response)
# 12- quality

#reads the excel file 
d<-read.csv(file.choose())
attach(d)
head(d)

#Min/1st Qu/MEDIAN/mean/3rd Qu/max
dim(d)
row.names(d) = 1:1599
summary(d)

#Standard Deviations
sd(alcohol)
sd(fixed.acidity)
sd(volatile.acidity)
sd(citric.acid)
sd(residual.sugar)
sd(chlorides)
sd(free.sulfur.dioxide)
sd(total.sulfur.dioxide)
sd(density)
sd(pH)
sd(sulphates)
sd(quality)

#Correlation Plot
library(corrplot)
corr = cor(d) 
print(round(corr,digits = 3))
corrplot(corr, method="circle")

#Scatterplot Matrix
pairs(d)
plot(d)


#preliminary observations and plots of the entire data set
f = lm(alcohol~fixed.acidity + volatile.acidity +citric.acid +residual.sugar +chlorides + free.sulfur.dioxide + total.sulfur.dioxide +density+pH+sulphates+quality, data = d)
summary(f)
par(mfrow = c(2,2))
plot(f)

#Check for Mulitcollinearity
vif(f)

#Predictor Significance
library(car)
Anova(f, type = 'III')

#Reduced model without free.sulfur.dioxide
#Coefficients, R.Squared
f2 = lm(alcohol~fixed.acidity + volatile.acidity +citric.acid +residual.sugar +chlorides + total.sulfur.dioxide +density+pH+sulphates+factor(quality), data = d)
Anova(f2, type = 'III')
summary(f2)

#Histograms of f2
par(mfrow = c(2,2))
hist(residuals(f2), main="Histogram of f2", breaks=20)

#Partial F test with a further reduced model
full_model = f2
reduced_model = lm(alcohol~fixed.acidity+volatile.acidity+residual.sugar+total.sulfur.dioxide+density+pH+factor(quality), data = d)
anova(reduced_model, full_model)

#Comparing ALternative models with Stepwise AIC Selection
library(MASS)
fit0= lm(alcohol~1, data = d)
step3 = stepAIC(fit0, scope=list(lower=fit0, upper =f2), direction="both")
step3$anova

#Multicollinearlity
vif(f2)

#Y-graphs
par(mfrow = c(1,2))
hist(alcohol)
boxplot(alcohol)
summary(alcohol)

#Creating the model without the 'quality' predictor
f3 = lm(alcohol~fixed.acidity + volatile.acidity +citric.acid +residual.sugar +chlorides + total.sulfur.dioxide +density+pH+sulphates, data = d)
f3
vif(f3)
anova(f3)

#Partial F test with and without the quality predictor
anova(f3, f2)

#Studentized Residuals
res= rstudent(f2)
plot(res, ylab = "Jackknife residuals")
which(abs(res)>3)
which.max(abs(res))

#Cook's Distance Plot
cutoff <- 4/((nrow(data)-length(f2$coefficients)-2))
plot(f2, which=4, cook.levels=cutoff)

#High Leverage, n=1599, k = 9
Lev = lm.influence(f2)$hat
sort(Lev, decreasing = T)[1:10]
2*(9+1)/1599
leveragePlots(f2)
plot(f2,4)

#Dffits
Dff = dffits(f2)
#plot(dff,ylab="Dffits")
which(abs(Dff) > 2*sqrt((9+1)/1599))
which(abs(Dff) > 1)
sort(Dff, decreasing = T)[1:5]
#Studentized - Multiple points > 3.
#Cook's - Point 900 = 0.9345763 > 0.5, but < 1, so not an influential point.
#Leverage - 
#DFFits - no values are bigger than 1.

#Residuals Diagnostic for f2
par(mfrow = c(2,2))
plot(f2)

# Test constant variance
library(lmtest)
library(zoo)
bptest(f2) 

# Test normality
library(nortest)
ad.test(f2$residuals) 
shapiro.test(f2$residuals) 


#Box-Cox Transformation Method
boxcox(f2, lambda = seq(-2.5, 4, 1/10))
  #instantiating variables for the new, transformed model
alcohol = d$alcohol
alcohol_bc = alcohol^(-1.5)
fixed.acidity = d$fixed.acidity
volatile.acidity = d$volatile.acidity
citric.acid = d$citric.acid
residual.sugar = d$residual.sugar
chlorides = d$chlorides
total.sulfur.dioxide = d$total.sulfur.dioxide
density = d$density
pH = d$pH
sulphates = d$sulphates
quality = d$quality
  #creating new data.frame for the new, transformed model
d2 = data.frame(fixed.acidity,volatile.acidity, citric.acid, residual.sugar, chlorides, total.sulfur.dioxide, density, pH, sulphates,quality, alcohol_bc)
f2_bc = lm(alcohol_bc~fixed.acidity + volatile.acidity +citric.acid +residual.sugar +chlorides + total.sulfur.dioxide +density+pH+sulphates+factor(quality), data = d2)
  #plotting transformed models residuals and performing numerical diagnostic
par(mfrow = c(2,2))
plot(f2_bc)
summary(f2_bc)

# Test constant variance
bptest(f2_bc) 

# Test normality
ad.test(f2_bc$residuals) 
shapiro.test(f2_bc$residuals) 

#Prediction for fixed acidity
new_fixed_acidity = 7.4
pred = predict(f2, data.frame(fixed.acidity = new_fixed_acidity,volatile.acidity, citric.acid, residual.sugar, chlorides, total.sulfur.dioxide, density, pH, sulphates,factor(quality)), interval = 'prediction', level = .95)
pred

#prediction for pH
new_pH = 3.4
pred2 = predict(f2, data.frame(fixed.acidity,volatile.acidity, citric.acid, residual.sugar, chlorides, total.sulfur.dioxide, density, pH = new_pH, sulphates,factor(quality)), se.fit = TRUE, interval = 'prediction', level = .95)
pred2

new_pH = 3.4 
pred2 = predict(f2, data.frame(fixed.acidity,volatile.acidity, citric.acid, residual.sugar, chlorides, total.sulfur.dioxide, density, pH = new_pH, sulphates,factor(quality)), se.fit = FALSE, interval = 'prediction', level = .95)
pred2

#Prediction for means, Quality = 6
pred3 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 6), se.fit = TRUE, interval = 'prediction', level = .95)
pred3

#Prediction for means, Quality = 4 
pred4 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 4), se.fit = TRUE, interval = 'prediction', level = .95)
pred4

#Prediction for means, Quality = 5 
pred5 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 5), se.fit = TRUE, interval = 'prediction', level = .95)
pred5

#Prediction for means, Quality = 7 
pred7 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 7), se.fit = TRUE, interval = 'prediction', level = .95)
pred7

#Confidence Intervals
#Confidence Intervals for means, Quality = 4 
con4 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 4), se.fit = TRUE, interval = 'confidence', level = .95)
con4


#Confidence Intervals for means, Quality = 5 
con5 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 5), se.fit = TRUE, interval = 'confidence', level = .95)
con5

#Confidence Intervals for means, Quality = 6 
con6 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 6), se.fit = TRUE, interval = 'confidence', level = .95)
con6

#Confidence Intervals for means, Quality = 7 
con7 = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), density = mean(density), pH = mean(pH), sulphates = mean(sulphates),quality = 7), se.fit = TRUE, interval = 'confidence', level = .95)
con7

#Prediction for means, density = 0.997
predden = predict(f2, data.frame(fixed.acidity = mean(fixed.acidity),volatile.acidity = mean(volatile.acidity), citric.acid = mean(citric.acid), residual.sugar = mean(residual.sugar), chlorides = mean(chlorides), total.sulfur.dioxide = mean(total.sulfur.dioxide), quality = 5, pH = mean(pH), sulphates = mean(sulphates),density = 0.997), se.fit = TRUE, interval = 'prediction', level = .95)
predden


#Mulitcollinearity for transformed model
vif(f2_bc)

#correlation pH
cor(alcohol_bc, pH)

#Plots and GGplot with fixed acidity, quality, and density against transformed response alcohol_bc
library(ggplot2)

plot(alcohol_bc~fixed.acidity)
ggplot(data=d2, aes(x=fixed.acidity, y=alcohol_bc))+geom_point()+geom_smooth(method=lm, se=TRUE)

plot(alcohol_bc~quality)
ggplot(data=d2, aes(x=quality, y=alcohol_bc))+geom_point()+geom_smooth(method=lm, se=TRUE)

plot(alcohol_bc~density)
ggplot(data=d2, aes(x=density, y=alcohol_bc))+geom_point()+geom_smooth(method=lm, se=TRUE)

#correlation plot for transformed(f2) model
corrplot(cor(d2),
         method = "circle",       
         addrect = 2,              
         rect.col = 3,            
         rect.lwd = 3) 