#####################################################
#####################################################
########### Solo 2 - Hierarchichal Bayes Logistic Model
#####################################################
#####################################################

#####################################################
#####################################################
########### Importing Libraries and Data
#####################################################
#####################################################
options(repr.plot.width=8, repr.plot.height=4)
library(plotly)
library(ggplot2)
library(tidyr)
getwd()
setwd("D:\\OneDrive\\MSDS\\450\\Solo 2")


# stc-cbc-respondents-v3.RData – This is the respondent data; R file format. The variables in
#   it are documented in the file stc-v3-datamap.txt.
load("stc-cbc-respondents-v3(1).RData")
ls()

#####################################################
#####################################################
########### Chapter 1 - Setting up the data
#####################################################
#####################################################

# Str(resp.data.v3)
#   § Look at the structure, str(resp.data.v3)
#   § 424 obs of 55 variables
#   § Only response data
#   § Discrete choice model 1_1-36, through 36 choice sets
# Values of 1,2,3 they chose for each choice set

str(resp.data.v3)
head(resp.data.v3)
colnames(resp.data.v3)
ncol(resp.data.v3)

##Plotting Data
# library(Hmisc)
# library(reshape2)
# plot.data <- melt(resp.data.v3[,c(4:55)])
# ggplot(plot.data,aes(x = value)) +
#     facet_wrap(~variable,scales = "free_x") +
#     geom_histogram()

# Plotting Data
options(repr.plot.width=8, repr.plot.height=8)
resp.data.v3[4:55] %>%
  gather() %>%
  ggplot(aes(value))+facet_wrap(~key,scales="free") + geom_histogram(binwidth=1)

options(repr.plot.width=8, repr.plot.height=4)

# Taskv3
  #   § Choice set 1 - 36
  #   § Choice id = 1,2,3
  #   § Screen 0,1,2
  #   § RAM 0,1,2
  #   § Processor 0,1,2
  #   § Price 0,1,2
  #   Brand 0

  # >  load("stc-cbc-respondents-v3(1).RData")
  # stb-dc-task-cbc-v3.csv – This describes the choice task in attributes and levels; it's a tab- delimited csv file with a header record. The attribute levels are coded 0,1,2,3. They correspond to the descriptions of the attributes and levels provided in the assignment
taskV3 <- read.csv("stc-dc-task-cbc -v3(1).csv", sep="\t")
ncol(taskV3)
colnames(taskV3)
str(taskV3)
head(taskV3)
# install.packages("dummies")

require(dummies)


# To make life a little easier for you, I've provided some R code to make the coding easier for you in efCode.RData. Once you imported this into your R session you'll see you have two R functions, efcode.att.f() and efcode.attmat.f(). (Note: I use the parens '()' to signify something that's an R function.)
#
# The function efcode.att.f() accepts as input a numeric vector, xvec, and produces an effects
#       coded version of it. This function requires the 'dummies' package. It uses the dummy() function in it.
#       It assumes that the lowest value of xvec represents the reference category, the one that's coded
#       with -1's. You can see what this function does by providing it with some vector input, e.g.
#       xvec=c(0,1,2,3), efcode.att.f(xvec).
load("efCode.RData")

ls()

# The function efcode.attmat.f() is a wrapper of sorts for efcode.att.f(). You can provide it with a
#       matrix describing your choice design (like a matrix of the attribute columns in stb-dc-task-cbc-v3.csv),
#       and it will produce an effects coded version of it. As indicated above, it's up to you to make sure you
#       understand what this function is supposed to do and whether it is working correctly for you.
str(efcode.att.f)

# Here's how to use effcode.attmat.f(). Let's assume you have read in the choice task design that it's in
#       stb -dc-task-cbc-v3.csv and that it's in a data frame called taskV3. Select the attribute variables in
#       this data frame into a matrix we'll call task.mat. Then, do (where “>” is the R command prompt):
str(efcode.attmat.f)

apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))})


# Task.Mat
#   § First two columns are label columns
#   § Important columns are processor, price, and brand
#   § 108 rows and 108 columns for design matrix
#   § Convert categorical variables into dummy variables
#   § X.mat = efcode.attmat.f(task.mat)
#   § Baseline case for 5 inch screen.  X1 and x2 = -1
#   RAM converted to x3,x4  8gb ram

task.mat <- as.matrix(taskV3[, c("screen", "RAM", "processor", "price", "brand")])
dim(task.mat)
head(task.mat)
task.mat
X.mat=efcode.attmat.f(task.mat)  # Here is where we do effects coding
dim(X.mat)
head(X.mat)


# PriceVec
#   Price -mean
# Note that the R scale() function could be used to do this, scale(taskV3$price,scale=FALSE), but the above is more transparent. Note also that the above is taking the element-wise difference of two numeric vectors.
pricevec=taskV3$price-mean(taskV3$price)

head(pricevec)
str(pricevec)

# Next, we'll get the columns from X.mat that represent brand. They should be the last three columns (check to be sure of this)
#       >X.brands=X.mat[,9:11]
X.brands=X.mat[,9:11]

dim(X.brands)
str(X.brands)


# X.Brands
#   § X.brands = X.mat[,9:11]
#   § Brands columns
#   X.brandsbyprice = x.brands * price
# Next, we're going to multiply each column in X.brands by pricevec. This isn't matrix multiplication. It's taking the inner product of each column and pricevec:
X.BrandByPrice = X.brands*pricevec

dim(X.BrandByPrice)
str(X.BrandByPrice)


# X.matrix
#   § 11 columns before and have columns 12,13,14 = 108 rows
#   § Matrix with 108 rows
#   § Prepared the data for the x matrix you want
#   Take determinant (t(x.matrix) = transpose %*% x.matrix))
# Now we'll combine X.mat and X.BrandsByPrice to get the X matrix we'll use for choice
#       modeling:
# >X.matrix=cbind(X.mat,X.BrandByPrice)
# This combines the columns of X.mat and X.BrandByPrice.
X.matrix=cbind(X.mat,X.BrandByPrice)

dim(X.matrix)
str(X.matrix)
head(X.matrix)
X2.matrix=X.matrix[,1:2]
dim(X2.matrix)

# You should get a positive number, maybe a very big one. If you don't, go back
# through the steps above and check your results for each one.
# Note that the %*% above means matrix multiplication in R.
det(t(X.matrix) %*% X.matrix)

# Next, you need to get the responses that STC's survey participants provided to the 36 choice questions they answered. The are in the data frame resp.data-v3 that's in the R data file stc-cbc-respondents-v3.RData. The variable names are DCM1_1 through DCM1_36. They are probably the 4th through the 39th variables in resp.data-v3.
# Let's get these responses out into their own data frame:
#  >ydata=resp.data[,4:39]

ydata=resp.data.v3[,4:39]
names(ydata)
str(ydata)
head(ydata)

# Make sure you have no missing data:
ydata=na.omit(ydata)
ydatadf <- ydata
head(ydatadf)

# Now, convert ydata to matrix
# > ydata=as.matrix(ydata)
ydata=as.matrix(ydata)
dim(ydata)

# For one of the assignment's objectives you'll need a “covariate” (a variable) that indicates whether a survey respondent has owned a STC product. There's a variable in the respondent data set called vList3. Recode this into a variable that is equal to 1 if a respondent has ever owned an STC product. Otherwise, make it equal to zero.
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) )
head(zowner)


# Lgtdata
#   § Way the package wants the input
#   Lgtdata is a list of more than one object
# For one of the assignment's objectives you'll need a “covariate” (a variable) that indicates whether a survey respondent has owned a STC product. There's a variable in the respondent data set called vList3. Recode this into a variable that is equal to 1 if a respondent has ever owned an STC product. Otherwise, make it equal to zero.
lgtdata = NULL

for (i in 1:424) { lgtdata[[i]]=list( y=ydata[i,],X=X.matrix )}
length(lgtdata)
str(lgtdata)

#####################################################
#####################################################
########### Chapter 2 - Fitting a Hierarchical Bayes Multi-Nomial Logistic
#####################################################
#####################################################

# Bayesm
#   § MCMC test=list(r=5000,keep=5)
#   § Every 5th data point keep
#   § Delete 4000, hold 1000
require(bayesm)

# The function in bayesm you're going to use is rhierMnlDP(), which uses MCMC to fit a hierarchical MNL model. Type help(rhierMnlDP) to see what it does, how it works, and what it outputs. It produces a list of results, so when you run it, you are going to assign what it outputs to a name. Note that the DP part of the name refers to “Dirichlet Process.” This code uses A Dirichlet prior to add additional “fatness” as needed to the tails of the multivariate normal (MVN) distribution, which can be a little too thin in this kind of choice modeling application.

# mcmctest=list(R=5000, keep=5)
mcmctest=list(R=20000, keep=5)

#   § Data1
#     □ P=3, lgtdata input which is a list x data y data
#     □ 1 argument
#   § Spit out results

Data1=list(p=3,lgtdata=lgtdata)

options(warn=-1)
# testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
options(warn=0)
names(testrun1)

# Betadraw
#   § Alpha, beta, delta, gamma - hierarchical things you don't worry about
#   § Important think Is beta draw
#   § Beta1:beta14 is what you want
#   § Betadraw1 - what is the dimension of that?  Matrix object.  First dimension is 424, next is 14, next is 1000
#   § 5000 simulations, keep every 5th value.
#   § 1000 values are simulated values
#   §
#   § Betadraw1 is the matrix object, first dimension 1 = 1 person 1
#   § Second dimension is beta1 which corresponds to dummy value x1 which corresponds to screen 7
#   § Third dimension is iteration.  You have 1000 iterations
#   § You look at person 1, screen 1, looking at simulated values for those betas

     # “betadraw” is an array that has the draws (i.e. samples from marginal posterior distributions)         for the regression coefficients:
betadraw1=testrun1$betadraw

# You'll see that “loglike” is the loglikelihood of the model at each iteration. You already know what “betadraw” is. “nmix” includes estimates of the the parameters of the MVN distribution on the betas. “Istardraw” summarizes the distribution of the number Dirichlet process (DP) components used per iteration. We've mentioned that the DP is used here to “fatten” MVN tails as needed. If you do:
table(testrun1$Istardraw)
###################

dim(betadraw1)
options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw1[50,1,]),betadraw1[50,1,])
plot(1:length(betadraw1[50,2,]),betadraw1[50,2,])
mtext("Respondent 50: Screen 7 vs 10", outer=TRUE,  cex=1, line=-2)

options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw1[50,3,]),betadraw1[50,3,])
plot(1:length(betadraw1[50,4,]),betadraw1[50,4,])
mtext("Respondent 50: Ram 16 vs 32", outer=TRUE,  cex=1, line=-2)

options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw1[50,7,]),betadraw1[50,7,])
plot(1:length(betadraw1[50,8,]),betadraw1[50,8,])
mtext("Respondent 50: Price 299 vs 399", outer=TRUE,  cex=1, line=-2)
options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw1[50,5,]),betadraw1[50,5,])
plot(1:length(betadraw1[50,6,]),betadraw1[50,6,])
mtext("Respondent 50: Processor 2 vs 2.5", outer=TRUE,  cex=1, line=-2)

# plot(1:length(betadraw1[20,5,]),betadraw1[20,5,])
# plot(1:length(betadraw1[50,10,]),betadraw1[50,10,])

# § What is this?
#   □ 14-15 dimension bumpy surface
#   □ You are just looking at a bread slice
#   □ It has looked at 15 dimensional surface, does a random walk
#   □ First couple of iterations was way off, need to remove those so it stabilizes.
#   □ Burning period up to 700.  beyond 700 it is converging to a surface
#   □ Density function takes histogram and smooths it out

param_1 = 2000
param_2 = 4000

plot.new()
plot(density(betadraw1[1,1,param_1:param_2],width=2))
abline(v=0) ## vertical line

#plot(density(betadraw1[1,2,701:1000],width=2))
#abline(v=0) ## vertical line
## abline(h=0.010) ## horizontal line

#### Compute the probability that person 1 beta 1 > 0 ####
#### Assumes Normal distribution of beta 1 ######

# Summary command gives you the exact statistics
#   Mean = beta1 for person1 is .98
#   300 simulated points to summarize
#   That means beta value .98
#   This is the preference
#   Beta1 is positive, they prefer 7 inch screen
#   Do they like 10  inch screen?
#     Mean is -.33, do not like it
#   Each individual person, you will know whether they like it or not.  So much detail out of this

mn <- mean(betadraw1[1,1,param_1:param_2])
sd <- sd(betadraw1[1,1,param_1:param_2])
mn
sd
plot.new()
plot(density(betadraw1[1,1,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean

plot.new()
plot(density(betadraw1[1,2,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean

plot.new()
plot(density(betadraw1[1,3,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean

plot.new()
plot(density(betadraw1[5,1,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean
plot.new()
plot(density(betadraw1[5,2,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean
plot.new()
plot(density(betadraw1[5,3,param_1:param_2],width=2))
abline(v=0) ## vertical line -density function for 0
abline(v=mn) ## vertical line - mean

prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob

#### Compute the empirical probability based on sample values ###
p1b1 <- betadraw1[param_1:param_2]
head(p1b1)

quantile(p1b1, probs = seq(0, 1, by= 0.1))

###################
summary(betadraw1[1,1,param_1:param_2])
exp()
##log(odds ratio)=0.7833; odds ratio = exp(0.7833)=2.18;
summary(betadraw1[1,2,param_1:param_2])

# ® With all the data can I get one model?
#   ◊ Create betameansoverall, compute the mean for all the 30 values, pull all the people together
#   ◊ 14 values, first value -.17 is the beta1, which pulls all the people together
#   ◊ .45, pool together they like it, cuz its positive
##log(odds ratio)=-0.264; odds ratio = exp(-0.264)=0.76;
betameansoverall <- apply(betadraw1[,,param_1:param_2],c(2),mean)
betameansoverall

# ◊ Put into percentiles
#   } 50th percentile -.10
#   } They don't like it if you look at 50% quantile
#   } 90% confidence band is from -1.36 to .69
#   } 90% confidence neg to positive, wide band
#   } All the apples and oranges, 424 respondents into one bucket.

perc <- apply(betadraw1[,,param_1:param_2],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc


#####################################################
#####################################################
########### Chapter 3 - Fitting a Hierarchical Bayes Multi-Nomial Logistic Model
########### with Prior Ownership as a Covariate
#####################################################
#####################################################

zownertest=matrix(scale(zowner,scale=FALSE),ncol=1)
Data2=list(p=3,lgtdata=lgtdata,Z=zownertest)
options(warn=-1)
# testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2)

dim(testrun2$Deltadraw)
apply(testrun2$Deltadraw[param_1:param_2,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
deltadraw1 <- apply(testrun2$Deltadraw[param_1:param_2,],c(2),mean)
deltadraw1
betadraw2=testrun2$betadraw
dim(betadraw2)

options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw2[50,1,]),betadraw2[50,1,])
plot(1:length(betadraw2[50,2,]),betadraw2[50,2,])
mtext("Respondent 50: Screen 7 vs 10", outer=TRUE,  cex=1, line=-2)

options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw2[50,3,]),betadraw2[50,3,])
plot(1:length(betadraw2[50,4,]),betadraw2[50,4,])
mtext("Respondent 50: Ram 16 vs 32", outer=TRUE,  cex=1, line=-2)

options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw2[50,7,]),betadraw2[50,7,])
plot(1:length(betadraw2[50,8,]),betadraw2[50,8,])
mtext("Respondent 50: Price 299 vs 399", outer=TRUE,  cex=1, line=-2)
options(repr.plot.width=6, repr.plot.height=3)
par(mfrow=c(1,2))
plot(1:length(betadraw2[50,5,]),betadraw2[50,5,])
plot(1:length(betadraw2[50,6,]),betadraw2[50,6,])
mtext("Respondent 50: Processor 2 vs 2.5", outer=TRUE,  cex=1, line=-2)

prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob

#### Compute the empirical probability based on sample values ###
p1b1 <- betadraw2[param_1:param_2]
head(p1b1)

quantile(p1b1, probs = seq(0, 1, by= 0.1))

###################
summary(betadraw2[1,1,param_1:param_2])

##log(odds ratio)=0.7833; odds ratio = exp(0.7833)=2.18;
summary(betadraw2[1,2,param_1:param_2])

# ® With all the data can I get one model?
#   ◊ Create betameansoverall, compute the mean for all the 30 values, pull all the people together
#   ◊ 14 values, first value -.17 is the beta1, which pulls all the people together
#   ◊ .45, pool together they like it, cuz its positive
##log(odds ratio)=-0.264; odds ratio = exp(-0.264)=0.76;
betameansoverall <- apply(betadraw2[,,param_1:param_2],c(2),mean)
betameansoverall

# ◊ Put into percentiles
#   } 50th percentile -.10
#   } They don't like it if you look at 50% quantile
#   } 90% confidence band is from -1.36 to .69
#   } 90% confidence neg to positive, wide band
#   } All the apples and oranges, 424 respondents into one bucket.

perc <- apply(betadraw2[,,param_1:param_2],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc

betameans <- apply(betadraw2[,,param_1:param_2],c(1,2),mean)
str(betameans)
head(betameans)
dim(betameans)
dim(t(betameans))
dim(X.matrix)
xbeta=X.matrix%*%t(betameans)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
head(pchoicemat)
dim(pchoicemat)
custchoice <- max.col(pchoicemat)
head(custchoice)
str(custchoice)
ydatavec <- as.vector(t(ydata))
str(ydatavec)
head(ydatavec)
table(custchoice,ydatavec)
require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)
auc(roctest)
par(mfrow=c(1,3))
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC)
logliketest <- testrun1$loglike
str(logliketest)
mean(logliketest)
hist(logliketest)
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
### Predicted frequencies for the 36 choice sets using individual models
apply(m2, 2, function(x){tabulate(na.omit(x))})

## This process needs to be repeated for betadraw2##


#####################################################
#####################################################
########### Chapter 5 - Predicting Extra Scenarios, Using Betas
########### From All the Pooled Respondents
#####################################################
#####################################################

ex_scen <- read.csv("extra-scenarios.csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])

### predict extra scenarios using the overall model  ###################
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
dim(betavec)
betavec
dim(Xextra.matrix)
xextrabeta=Xextra.matrix%*%(betavec)
dim(xextrabeta)
xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
dim(xbetaextra2)

expxbetaextra2=exp(xbetaextra2)
rsumvec=rowSums(expxbetaextra2)
pchoicemat=expxbetaextra2/rsumvec
head(pchoicemat)
dim(pchoicemat)

### predict extra scenarios based on individual models #############################

xextrabetaind=Xextra.matrix%*%(t(betameans))
xbetaextra2ind=matrix(xextrabetaind,ncol=3,byrow=TRUE)

#xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
#dim(xextrabetaind)
#xbetaextra2ind=rbind(matrix(xextrabetaind[1:3,],ncol=3,byrow=TRUE),
#                     matrix(xextrabetaind[4:6,],ncol=3,byrow=TRUE))
dim(xbetaextra2ind)


expxbetaextra2ind=exp(xbetaextra2ind)
rsumvecind=rowSums(expxbetaextra2ind)
pchoicematind=expxbetaextra2ind/rsumvecind
dim(pchoicematind)
head(pchoicematind)


custchoiceind <- max.col(pchoicematind)
head(custchoiceind)
str(custchoiceind)
extra1 <- custchoiceind[1:424]
extra2 <- custchoiceind[425:848]
table(extra1)
table(extra2)

################################################################################################
#accuracy based on confusion matrix for each of the 424 respondents using individual models
############ for all the 36 choice sets - actual response vs predicte response ###############
###############################################################################################
resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoice[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
}
plot(resp_accuracy, main = "Model Accuracy by Respondent")
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)
head(ydatadf)
rn <- rownames(ydatadf)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)

str(resp_all)
hist(resp_all$resp_accuracy)
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]





#####################################################
#####################################################
########### Chapter 4 - Make Customer Choice Prediction Using Invidiual
########### Respondent's Model & Goodness of Fit & Validation
#####################################################
#####################################################

betameans <- apply(betadraw1[,,param_1:param_2],c(1,2),mean)
str(betameans)
head(betameans)
dim(betameans)
dim(t(betameans))
dim(X.matrix)
xbeta=X.matrix%*%t(betameans)
dim(xbeta)
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)
expxbeta2=exp(xbeta2)
rsumvec=rowSums(expxbeta2)
pchoicemat=expxbeta2/rsumvec
head(pchoicemat)
dim(pchoicemat)
custchoice <- max.col(pchoicemat)
head(custchoice)
str(custchoice)
ydatavec <- as.vector(t(ydata))
str(ydatavec)
head(ydatavec)
table(custchoice,ydatavec)
require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)
auc(roctest)
par(mfrow=c(1,3))
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC)
logliketest <- testrun1$loglike
str(logliketest)
mean(logliketest)
hist(logliketest)
m <- matrix(custchoice, nrow =36, byrow=F)
m2 <- t(m)
### Predicted frequencies for the 36 choice sets using individual models
apply(m2, 2, function(x){tabulate(na.omit(x))})

## This process needs to be repeated for betadraw2##


#####################################################
#####################################################
########### Chapter 5 - Predicting Extra Scenarios, Using Betas
########### From All the Pooled Respondents
#####################################################
#####################################################

ex_scen <- read.csv("extra-scenarios.csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])

### predict extra scenarios using the overall model  ###################
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
dim(betavec)
betavec
dim(Xextra.matrix)
xextrabeta=Xextra.matrix%*%(betavec)
dim(xextrabeta)
xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
dim(xbetaextra2)

expxbetaextra2=exp(xbetaextra2)
rsumvec=rowSums(expxbetaextra2)
pchoicemat=expxbetaextra2/rsumvec
head(pchoicemat)
dim(pchoicemat)

### predict extra scenarios based on individual models #############################

xextrabetaind=Xextra.matrix%*%(t(betameans))
xbetaextra2ind=matrix(xextrabetaind,ncol=3,byrow=TRUE)

#xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
#dim(xextrabetaind)
#xbetaextra2ind=rbind(matrix(xextrabetaind[1:3,],ncol=3,byrow=TRUE),
#                     matrix(xextrabetaind[4:6,],ncol=3,byrow=TRUE))
dim(xbetaextra2ind)


expxbetaextra2ind=exp(xbetaextra2ind)
rsumvecind=rowSums(expxbetaextra2ind)
pchoicematind=expxbetaextra2ind/rsumvecind
dim(pchoicematind)
head(pchoicematind)


custchoiceind <- max.col(pchoicematind)
head(custchoiceind)
str(custchoiceind)
extra1 <- custchoiceind[1:424]
extra2 <- custchoiceind[425:848]
table(extra1)
table(extra2)

################################################################################################
#accuracy based on confusion matrix for each of the 424 respondents using individual models
############ for all the 36 choice sets - actual response vs predicte response ###############
###############################################################################################
resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoice[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
}
plot(resp_accuracy, main = "Model Accuracy by Respondent")
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)
head(ydatadf)
rn <- rownames(ydatadf)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)

str(resp_all)
hist(resp_all$resp_accuracy)
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]
