# get the foreign package 
install.packages("foreign")
library(foreign)


# Get the haven package 
install.packages("haven")
library(haven) 

NSdata <- haven::read_dta("/Users/NewMacBookAir/Downloads/NS.dta")



library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation
install.packages("dplyr")
library(dplyr)


z <- data.frame(NSdata$aware_count,1:2000)
z <- z%>%
filter (NSdata.aware_count>0)%>%
  mutate(ID=1:1959)

#plot histogram of data 
p1 <- ggplot(data=z, aes(x=NSdata.aware_count, y=after_stat(density))) + geom_histogram(color="grey60",fill="cornsilk",linewidth=0.2)
             
             
print(p1)

# add empirical density curve to smooth out the shape of the histogram. This does not assume any probability distribution 
p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)

#Get maximum likelihood parameters for normal 
normPars <- fitdistr(z$NSdata.aware_count,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute


# Plot normal proability density 
meanML <- normPars$estimate["mean"]

sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$NSdata.aware_count),len=length(z$NSdata.aware_count))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z), args = list(mean = meanML, sd = sdML))
p1 + stat

# plot exponential probability density 
expoPars <- fitdistr(z$NSdata.aware_count,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$NSdata.aware_count), args = list(rate=rateML))
p1 + stat + stat2


# plot uniform probability density 

stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$NSdata.aware_count), args = list(min=min(z$NSdata.aware_count), max=max(z$NSdata.aware_count)))
p1 + stat + stat2 + stat3

#Plot gamma probability density 
gammaPars <- fitdistr(z$NSdata.aware_count,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$NSdata.aware_count), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4



# Plot beta probability density 
pSpecial <- ggplot(data=z, aes(x=NSdata.aware_count/(max(NSdata.aware_count + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$NSdata.aware_count/max(z$NSdata.aware_count + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$NSdata.aware_count), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial



