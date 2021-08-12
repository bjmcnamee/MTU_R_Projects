#load in data
setwd("/home/barnyard/rstudio/scripts/college/R/Ass2")
df <- read.csv("process_sim.csv", header=TRUE) # read data file into dataframe 'df'
str(df)
round(sum(is.na(df$tconc))/sum(!is.na(df$tconc))*100,4)
df <- na.omit(df)

# check correlations of numeric varaibles
round(cor(df[-10], use = "c"),1)
# does not include 1. factor variable feed, 2. observation number No

# fit model to predict tconc, with all numeric variables
summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year+month+day+hour, data = df))
# all p values < 0.05 so all variables significant
# Multiple R squared value quite low

# simplified model with just Year as the only time variable
summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = df))
# all p values < 0.05 so all variables significant
# Multiple R squared value quite low but not much lower without dropped time variables

# store model coefficients in object
model <- (summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = df))$coeff)

# use median absolute deviation instead of sd to avoid outliers
disp <- mad(summary(lm(tconc~Ph+TEMP+PRES+undesP+udt+pdt+year, data = df))$resid)

#index of last datapoint
last <- nrow(df)
# get last year
unique(df$year)
# number of rows in one year (2014)
nyear <- sum(df$year==2014)

#set up simulation using model coefficients, where we simulate using the last data point
simA <- NULL
for(i in 1:nyear){
  simA=cbind(ysim, model[1] + model[2]*df$Ph[last]+ 
               model[3]*df$TEMP[last] + model[4]*df$PRES[last]+
               model[5]*df$undesP[last]+ model[6]*df$udt[last]+
               model[7]*df$pdt[last] + 
               model[8]*2015 +  #index year forward to desired prediction date
               rnorm(1,0, disp)) #simulate residuals from normal distribution
}
str(simA)
# distribution of simulated prediction
hist(simA)
# compare this with the histogram of existing data
hist(df$tconc)
# existing data is skewed and not normally distributed like simulated data

# compare with existing variable
max <- max(df$tconc, na.rm = TRUE)
min <- min(df$tconc, na.rm = TRUE)
par(mfrow=c(1,2))
plot(df$tconc, col="green")
#plot of simulated data
plot(simA[1,],  ylim = c(min,max), xlim = c(0,length(df$tconc)), col="red")

#create dataset with just last years data
library(dplyr)
df2014 <- df %>% filter(year==2014)
# plots showing last years tconc
plot(df2014$tconc,  ylim = c(min,max), xlim = c(0,length(df$tconc)), col="green")

# new simulation using 2014 variable means instead of the last value
simB <- NULL
for(i in 1:nyear){
  simB = cbind (ysim, model[1] + model[2]*mean(df2014$Ph) + model[3]*mean(df2014$TEMP) + 
               model[4]*mean(df2014$PRES) + model[5]*mean(df2014$undesP) + 
               model[6]*mean(df2014$udt) + model[7]*mean(df2014$pdt) + model[8]*2015 +  #index year forward to desired prediction date
               rnorm(1,0, disp)) #simulate residuals from normal distribution
}

# plot new sim
plot(simB[1,],  ylim = c(min,max), xlim = c(0,length(df$tconc)), col="red")
# no difference

# using all last years data for all independent variables
simC <- NULL
for(i in 1:nyear){
  simC = cbind (ysim, model[1] + model[2]*df2014$Ph[i] + model[3]*df2014$TEMP[i] + 
                  model[4]*df2014$PRES[i] + model[5]*df2014$undesP[i] + 
                  model[6]*df2014$udt[i] + model[7]*df2014$pdt[i] + model[8]*2015 +  #index year forward to desired prediction date
                  rnorm(1,0, disp)) #simulate residuals from normal distribution
}

plot(simC[1,],  ylim = c(min,max), xlim = c(0,length(df$tconc)), col="red")
# plots showing last years tconc
plot(df2014$tconc,  ylim = c(min,max), xlim = c(0,length(df$tconc)), col="green")

