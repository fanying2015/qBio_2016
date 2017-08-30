oxygen <- c(2.01, 6.98, 12.09, 17.03, 22.01, 27.06, 32.06, 36.91, 42.08, 46.99, 52.05, 56.92)
saturation <- c(2.42, 11.37, 24.33, 31.03, 48.57, 49.41, 56.66, 64.29, 72.36, 69.94, 77.94, 76.28)
d <- data.frame(oxygen, saturation)

#1. plotting the data
plot(saturation ~ oxygen, data = d, xlim = c(0, 60), ylim = c(0, 80))
# it may have cooperativity. The curve seems to increase faster than expected when the oxygen concentration is high.

#2. Fit the data to Michelis-Menten model
m0.oxytrans <- nls(saturation ~ Vmax * oxygen / (Km + oxygen),
                   start = list(Vmax = 80, Km = 20),
                   data = d)
summary(m0.oxytrans)

# plot the regressed curve against the original data points
x <- 0:80
lines(x, predict(m0.oxytrans, newdata = data.frame(oxygen = x)), col = "blue", lwd = 2)

# do the QC of residuals
plot(residuals(m0.oxytrans) ~ predict(m0.oxytrans))
qqnorm(residuals(m0.oxytrans))
qqline(residuals(m0.oxytrans))
shapiro.test(residuals(m0.oxytrans))
# p-value = 0.4136 # it seems the residuals follow normal distribution

# compute the 95% CI for the paramters
confint(m0.oxytrans)
##      2.5%    97.5%
#Vmax 137.23915 284.6987
#Km    44.58951 134.9181

#Comment: it seems that the model underestimates the saturation when oxygen level is low, and the CI ranges are wide. The model is not good enough.

#3. Fit the data to a Hill model
m1.oxytrans <- nls(saturation ~ Vmax * oxygen ^ n / (Km ^ n + oxygen ^ n),
                   start = list(Vmax = 80, Km = 20, n = 1),
                   data = d)
summary(m1.oxytrans)

# plot the regressed curve against the original data points
plot(saturation ~ oxygen, data = d, xlim = c(0, 60), ylim = c(0, 80))
x <- 0:80
lines(x, predict(m1.oxytrans, newdata = data.frame(oxygen = x)), col = "red", lwd = 2)

# do the QC of residuals
plot(residuals(m1.oxytrans) ~ predict(m1.oxytrans))
qqnorm(residuals(m1.oxytrans))
qqline(residuals(m1.oxytrans))
shapiro.test(residuals(m1.oxytrans))
# p-value = 0.4063 # it seems the residuals follow normal distribution

# compute the 95% CI for the paramters
confint(m1.oxytrans)
##        2.5%      97.5%
#Vmax 83.848271 156.016477
#Km   20.131075  53.139889
#n     1.101171   2.103657

#Comment: The data fits the Hill model better. n > 1 shows a positive cooperativity. The CI ranges are narrower than the previous one.

#4. F-test to compare the two models
anova(m0.oxytrans, m1.oxytrans)
#   Res.Df Res.Sum Sq Df Sum Sq F value  Pr(>F)  
#1     10    138.382                            
#2      9     73.513  1 64.869  7.9417 0.02011 *
# F value > 1, p < 0.05, showing Hill model explains better than Michelis-Menten model

#5. repeat the analysis with a subset of the data
oxygen.s <- c(2.01, 12.09, 22.01, 32.06, 42.08, 52.05)
saturation.s <- c(2.42, 24.33, 48.57, 56.66, 72.36, 77.94)
d.s <- data.frame(oxygen.s, saturation.s)

plot(saturation.s ~ oxygen.s, data = d.s, xlim = c(0, 60), ylim = c(0, 80))

# Fit the data to Michelis-Menten model
m0.oxytrans.s <- nls(saturation.s ~ Vmax * oxygen.s / (Km + oxygen.s),
                     start = list(Vmax = 80, Km = 20),
                     data = d.s)
summary(m0.oxytrans.s)

# plot the regressed curve against the original data points
x <- 0:80
lines(x, predict(m0.oxytrans.s, newdata = data.frame(oxygen.s = x)), col = "blue", lwd = 2)

# do the QC of residuals
plot(residuals(m0.oxytrans.s) ~ predict(m0.oxytrans.s))
qqnorm(residuals(m0.oxytrans.s))
qqline(residuals(m0.oxytrans.s))
shapiro.test(residuals(m0.oxytrans.s))
# p-value = 0.1121 

confint(m0.oxytrans.s)
##        2.5%    97.5%
#Vmax 122.78224 494.7246
#Km    34.40765 252.1068

#Fit the data to a Hill model
m1.oxytrans.s <- nls(saturation.s ~ Vmax * oxygen.s ^ n / (Km ^ n + oxygen.s ^ n),
                     start = list(Vmax = 80, Km = 20, n = 1),
                     data = d.s)
summary(m1.oxytrans.s)

# plot the regressed curve against the original data points
plot(saturation.s ~ oxygen.s, data = d.s, xlim = c(0, 60), ylim = c(0, 80))
x <- 0:80
lines(x, predict(m1.oxytrans.s, newdata = data.frame(oxygen.s = x)), col = "red", lwd = 2)

# do the QC of residuals
plot(residuals(m1.oxytrans.s) ~ predict(m1.oxytrans.s))
qqnorm(residuals(m1.oxytrans.s))
qqline(residuals(m1.oxytrans.s))
shapiro.test(residuals(m1.oxytrans.s))
# p-value =  0.876 

# compute the 95% CI for the paramters
confint(m1.oxytrans.s)
#Waiting for profiling to be done...
#Error in prof$getProfile() : 
# step factor 0.000488281 reduced below 'minFactor' of 0.000976562

# switch the algorithm: don't do much help here.
m1.oxytrans.s1 <- nls(saturation.s ~ Vmax * oxygen.s ^ n / (Km ^ n + oxygen.s ^ n),
                      start = list(Vmax = 80, Km = 20, n = 1),
                      data = d.s, algorithm = "plinear") # show an error
summary(m1.oxytrans.s1)
confint(m1.oxytrans.s1)

m1.oxytrans.s2 <- nls(saturation.s ~ Vmax * oxygen.s ^ n / (Km ^ n + oxygen.s ^ n),
                      start = list(Vmax = 80, Km = 20, n = 1),
                      data = d.s, algorithm = "port")
summary(m1.oxytrans.s2)
confint(m1.oxytrans.s2)


#  2.5%    97.5%
#Vmax 81.69619       NA
#Km   20.51137       NA
#n          NA 2.792693

# F-test to compare the two models
anova(m0.oxytrans.s, m1.oxytrans.s)
#   Res.Df Res.Sum Sq Df Sum Sq F value Pr(>F)
#1      4     44.972                         
#2      3     26.341  1 18.631  2.1219 0.2412

# Comment: F value is not significant larger than 1. P value > 0.05, suggesting the Michelis-Menten model works better

# It's difficult to estimate the CI because there are not enough degrees of freedom (df = 3 for the Hill Model). In t test, with few degrees of freedom, the values of t distribution are much higher than the corresponding values for a normal distribution. 
# My interpretation: the data is noisy, t is too large to the CI. We need to collect more data points to get a (narrower) CI.
