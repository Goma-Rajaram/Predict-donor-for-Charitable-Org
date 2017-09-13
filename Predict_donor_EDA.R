###################################
## Name   : Goma Rajaram
## Project: Predict Donor - EDA
####################################  

############################################
## load the data

charity <- read.csv(file.choose()) # load the "charity.csv" file
str(charity)
dim(charity)
attach(charity)

##Data quality check
library(fBasics)
df.numeric = data.frame(damt,
				agif,
				avhv,
				chld,
				inca,
				incm,
				lgif,
				npro,
				plow,
				rgif,
				tdon,
				tgif,
				tlag)
df.categorical = data.frame(donr, genf, home, hinc,reg1, reg2,reg3, reg4, wrat)
data.frame(basicStats(df.numeric))
data.frame(basicStats(df.categorical))
summary(df.numeric)
summary(df.categorical)

## predictor transformations

charity.t <- charity
##charity.t$avhv <- log(charity.t$avhv)
# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed

## set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

## EDA
attach(x.train)
df.numeric.x.train = data.frame(agif,
				avhv,
				chld,
				inca,
				incm,
				lgif,
				npro,
				plow,
				rgif,
				tdon,
				tgif,
				tlag)
df.categorical.x.train = data.frame(genf, home, hinc,reg1, reg2,reg3, reg4, wrat)
data.frame(basicStats(df.numeric.x.train))

table(c.train)
data.frame(basicStats(c.train))

range(charity$damt, na.rm = TRUE)
range(y.train)
summary(y.train)
data.frame(basicStats(charity$damt))
data.frame(basicStats(y.train))
hist(y.train, freq=TRUE,  col="green", 
      xlim = c(5,25),ylim=c(0,500),  
	xlab = "Donation amount", ylab = "Freq", main = "Histogram of DAMT")
qqnorm(y.train, col="turquoise")
qqline(y.train, col="purple")
t.test(y.train)
boxplot(y.train,
	col = 'blue', ylab = 'Donation amount', main = 'Boxplot of DAMT' )


cor(x.train,data.train[,23])
pairs(df.numeric.x.train)

##############################################

## Homeowner (1 - Homeowner, 0 - Not a Homeowner )
hist(x.train$home[c.train==0], freq=TRUE,  col=rgb(1,0,0,4/5), 
      xlim = c(0,1),ylim=c(0,2000),  
	xlab = "Home (0 - Not a Homeowner, 1 - Homeowner)", ylab = "Freq", main = "Home ownership by Donor")
hist(x.train$home[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)

## Chi-square GOF test allows you to compare categorical data with
## theoretical distribution. It has the null hypotheses that the data follows
## the specified distribution, and the alternative hypotheses that it does not
chisq.test(table(x.train$home))
chisq.test(table(x.train$home,c.train))

## Chi-square test of association (aka test of independence)
## In the same way that we can look at the association between continuous variables
## using statistics such as covariance and correlation, there are methods available
## to help you determine whether there is an association between categorical variables.
## The test has the null hypotheses that the variables are independent and
## the alternative hypothese that they are not independent
summary(table(x.train$home,c.train))

## Average home value
hist(x.train$avhv[c.train==0],   col=rgb(1,0,0,4/5), 
	xlim = c(0,600), ylim=c(0,700),
      xlab = "Average home value", ylab = "Freq", main = "Average home value by Donor")
hist(x.train$avhv[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$avhv, col="turquoise")
qqline(x.train$avhv, col="purple")
t.test(x.train$avhv)

## Log transformation of average home value
x.train$log_avhv = log(x.train$avhv)
hist(x.train$log_avhv[c.train==0], col=rgb(1,0,0,8/9), 
	xlab = "Log of average home value", main = "Average home value by Donor")
hist(x.train$log_avhv[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$log_avhv, col="turquoise")
qqline(x.train$log_avhv, col="purple")
t.test(x.train$log_avhv)


## Wealth rating (0 to 9)
hist(x.train$wrat[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9),xlim=c(0,10),ylim=c(0,1000),  
	xlab = "Wealth Rating", ylab = "Freq", main = "Wealth Rating by Donor")
hist(x.train$wrat[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$wrat,c.train))

## Number of children 
hist(x.train$chld[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9), ylim=c(0,1200), 
	xlab = "# of children",ylab = "Freq", main = "# of children by Donor")
hist(x.train$chld[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$chld,c.train))

## Check if there is benefit in creating a separate variable to indicate 
## zero children
x.train$have_chld  = 1 
for (i in 1:nrow(x.train)){
     if (x.train$chld[i] == 0) {
		x.train$have_chld[i] = 0
	}
}
sum(x.train$have_chld == 0)  ## (35% no children)
sum(x.train$chld == 0)

hist(x.train$have_chld[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9), ylim=c(0,1600), 
	xlab = "Have children 1 - Yes, 0 -No" ,ylab = "Freq", main = "children presence")
hist(x.train$have_chld[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$have_chld,c.train))


## Merging reg1, reg2, reg3, reg4 as a single variable region
x.train$region = 0
for (i in 1:nrow(x.train)){
     if (x.train$reg1[i] == 1) {
		x.train$region[i] = 1
	} else if (x.train$reg2[i] == 1) {
		x.train$region[i] = 2
	} else if (x.train$reg3[i] == 1) {
		x.train$region[i] = 3
	} else if (x.train$reg4[i] == 1) {
		x.train$region[i] = 4
	} else {
		x.train$region[i] = 5	
	}
}

sum(x.train$region == 1)
sum(x.train$reg1 == 1)
sum(x.train$region == 2)
sum(x.train$reg2 == 1)
sum(x.train$region == 3)
sum(x.train$reg3 == 1)
sum(x.train$region == 4)
sum(x.train$reg4 == 1)

sum(x.train$region == 5)
region5 = 0
for (i in 1:nrow(x.train)) {
	if ((x.train$reg1[i] == 0) && (x.train$reg2[i] == 0) && 
     (x.train$reg3[i] == 0) && (x.train$reg4[i] == 0)) {
		region5 = region5 + 1
	} 
}
region5 

## region
hist(x.train$region[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9),ylim = c(0,1000),  
	xlab = "Region",ylab = "Freq", main = "Region by Donor")
hist(x.train$region[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$region,c.train))

##reg2
hist(x.train$reg2[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9),ylim = c(0,1600),  
	xlab = "Region 2 (0 - other regions 1 - reg2)",ylab = "Freq",
	main = "Region 2 by Donor")
hist(x.train$reg2[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$reg2,c.train))   ## 242.7 , p low
chisq.test(table(x.train$reg1,c.train))   ## 12.42 , p low
chisq.test(table(x.train$reg3,c.train))   ## 42.74 , p low
chisq.test(table(x.train$reg4,c.train))   ## 62.79 , p low

## Median family income in $ thousands
hist(x.train$incm[c.train==0], col=rgb(1,0,0,8/9), ylim = c(0,1000), 
	xlab = "Median family income in $ thousands", main = "Median family income by Donor")
hist(x.train$incm[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$incm, col="turquoise")
qqline(x.train$incm, col="purple")
t.test(x.train$incm)

## Log transformation of Median family income
x.train$log_incm = log(x.train$incm)
hist(x.train$log_incm[c.train==0], col=rgb(1,0,0,8/9), ylim = c(0,800), 
	xlab = "Log of Median family income in $ thousands", main = "Median family income by Donor")
hist(x.train$log_incm[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$log_incm, col="turquoise")
qqline(x.train$log_incm, col="purple")
t.test(x.train$log_incm)

## Average family income in $ thousands
hist(x.train$inca[c.train==0], col=rgb(1,0,0,8/9), ylim = c(0,800), 
	xlab = "Average family income in $ thousands", main = "Average family income by Donor")
hist(x.train$inca[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$inca, col="turquoise")
qqline(x.train$inca, col="purple")
t.test(x.train$inca)

## Log transformation of Average family income
x.train$log_inca = log(x.train$inca)
hist(x.train$log_inca[c.train==0], col=rgb(1,0,0,8/9), ylim = c(0,500), 
	xlab = "Log of Average family income in $ thousands", main = "Average family income (Log form)  by Donor")
hist(x.train$log_inca[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$log_inca, col="turquoise")
qqline(x.train$log_inca, col="purple")
t.test(x.train$log_inca)

## Lifetime number of promotions received to date
hist(x.train$npro[c.train==0], freq=TRUE,  col=rgb(1,0,0,8/9),xlim=c(0,200), ylim=c(0,250), 
	xlab = "# of promotions",ylab = "Freq", main = "# of promotions by Donor")
hist(x.train$npro[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$npro,c.train)) 
summary(table(x.train$npro,c.train))
table(x.train$npro,c.train)

## tgif Dollar amount of lifetime gifts to date
hist(x.train$agif[c.train==0], col=rgb(1,0,0,8/9),
	xlim = c(0,100), ylim = c(0,1000),
	xlab = "Dollar amount of lifetime gifts",ylab = "Freq", main = "Lifetime gifts by Donor")
hist(x.train$agif[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
qqnorm(x.train$tgif, col="turquoise")
qqline(x.train$tgif, col="purple")
t.test(x.train$agif)

## Binning recent gift and average gift
x.train$f_agif = round(x.train$rgif,0)
hist(x.train$f_agif[c.train==0], col=rgb(1,0,0,8/9),
	xlim = c(0,66), ylim = c(0,1000),
	xlab = "Average gift amount",ylab = "Freq", main = "Average gifts by Donor")
hist(x.train$f_agif[c.train==1], col=rgb(0,1,0,8/9),add=T)
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$f_agif,c.train))


boxplot(x.train$tgif~c.train, 
		col = 'blue', xlab='0 - Non-donor, 1 - Donor',
		ylab = '$ amount', main = 'Gifts to date' )
IQR(x.train$tgif[c.train==1])

## 1 quartile = 63
## 3 quartile = 137

## lgif Dollar amount of largest gift to date
boxplot(x.train$lgif~c.train,
	col = 'blue', xlab='0 - Non-donor, 1 - Donor',
		ylab = '$ amount', main = 'Largest gift' )

IQR(x.train$lgif)
range(x.train$lgif)
## 1 quartile = 10
## 3 quartile = 25

## rgif Dollar amount of most recent gift
boxplot(x.train$rgif~c.train,
	col = 'blue', xlab='0 - Non-donor, 1 - Donor',
		ylab = '$ amount', main = 'Most recent gift' )


IQR(x.train$rgif)
range(x.train$rgif)
## 1 quartile = 7
## 3 quartile = 20

## agif Average Dollar amount of gifts to date
boxplot(x.train$agif~c.train,
	col = 'blue', xlab='0 - Non-donor, 1 - Donor',
		ylab = '$ amount', main = 'Average $ amt ' )



IQR(x.train$agif)
range(x.train$agif)
## 1 quartile = 6.97
## 3 quartile = 14.80

## Household income 7 categories
hist(x.train$hinc[c.train==0], freq=TRUE,  col=rgb(1,0,0,4/5),ylim=c(0,1400), 
     	xlab = "Household income", ylab = "Freq", main = "Household income categories by Donor")
hist(x.train$hinc[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$hinc))
summary(table(x.train$hinc,c.train))

## Genf MAle = 0, Female = 1  - Not significant
hist(x.train$genf[c.train==0], freq=TRUE,  col=rgb(1,0,0,4/5),
     	xlab = "Gender 0-Male, 1-Female", ylab = "Freq", main = "Gender by Donor")
hist(x.train$genf[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$genf))
summary(table(x.train$genf,c.train))

## Tdon Number of months since last donation - no correlation
hist(x.train$tdon[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,800),
     	xlab = "# of months since last donation",ylab= "Freq", main = "# of months")
hist(x.train$tdon[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("topright", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$tdon,c.train))

t.test(x.train$tdon[c.train==1])
t.test(x.train$tdon[c.train==0])

plot(x.train$tdon[c.train==1],y.train, 
col = 'blue', xlab='Number of months since last donation for donors',
		ylab = 'damt', main = 'Amount donated by tdon ' )

## Tlag Number of months between first and second donation
hist(x.train$tlag[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,800),xlim=c(0,30),
     	xlab = "# of months since last donation",ylab= "Freq", main = "# of months")
hist(x.train$tlag[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(x.train$tlag,c.train))
t.test(x.train$tlag)

plot(x.train$tlag[c.train==1],y.train,
col = 'blue', xlab='Duration (Number of months between first and second donation)',
		ylab = 'damt', main = 'Amount donated by tlag ' )


## Plow
hist(x.train$plow[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,1000),
     	xlab = "Low income percent",ylab= "Freq", main = "Percent of low income by donor")
hist(x.train$plow[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$plow),c.train))

##Interaction term between gender and number of promotions
x.train$gender_npro = x.train$genf * x.train$npro
hist(x.train$gender_npro[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,1000),
     	xlab = "Female and # of promotions",ylab= "Freq", 
	main = "Female + # of promotions by donor")
hist(x.train$gender_npro[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$gender_npro),c.train))

## Interaction term between homeowner and plow
x.train$homeowner_plow = x.train$home * x.train$plow
hist(x.train$homeowner_plow[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,1000),
     	xlab = "Homeowner and % catergorized as low income",ylab= "Freq", 
	main = "Homeowner + plow by donor")
hist(x.train$homeowner_plow[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$homeowner_plow),c.train))

## Interaction term between homeowner and tlag
x.train$home_tlag = x.train$home * x.train$tlag
hist(x.train$home_tlag[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,1200),
     	xlab = "Homeowner and tlag",ylab= "Freq", 
	main = "Homeowner + tlag by donor")
hist(x.train$home_tlag[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$home_tlag),c.train))

## Interaction term between reg2 and plow
x.train$reg2_plow = x.train$reg2 * x.train$plow
hist(x.train$reg2_plow[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,2000),
     	xlab = "Region 2 and plow",ylab= "Freq", 
	main = "Region 2 + plow by donor")
hist(x.train$reg2_plow[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$reg2_plow),c.train))


## Interaction term between home and agif
x.train$home_agif = x.train$home * round(x.train$agif,0)
hist(x.train$home_agif[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,2000),
     	xlab = "Being Homeowner and Avg. gift amount",ylab= "Freq", 
	main = "Home * agif by donor")
hist(x.train$home_agif[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$home_agif),c.train))

## Interaction term between genf and tgif
x.train$genf_tgif = x.train$genf * x.train$tgif
hist(x.train$genf_tgif[c.train==0],  col=rgb(1,0,0,4/5),ylim = c(0,2000),
     	xlab = "Being Female and Lifetime gifts to date",ylab= "Freq", 
	main = "genf * tgif by donor")
hist(x.train$genf_tgif[c.train==1], col=rgb(0,1,0,4/5),add=T)  	
legend("center", c("Donor", "Non-donor"), col=c( "green","red"),
 		lwd=10)
chisq.test(table(as.factor(x.train$genf_tgif),c.train))

###### END OF EDA ################################
