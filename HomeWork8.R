install.packages("tidyverse")
library(tidyverse)

library(ggplot2)

n = 100  # number of observations (rows)

age <- runif(n) # random uniform values (independent)
aware_count <- runif(n) # a second random column (dependent)
ID <- seq_len(n) # creates a sequence from 1:n (if n > 0!)
regData <- data.frame(ID,age,aware_count)
head(regData)
str(regData)

# model
regModel <- lm(aware_count~age,data=regData)


# model output
regModel # printed output is sparse
str(regModel) # complicated, but has "coefficients"
# model output

regPlot <- ggplot(data=regData + aes(x=age,y=aware_count) + 
                    geom_point() + 
                    stat_smooth(method=lm,se=0.50)) # default se=0.95 

nGroup <- 3 # number of treatment groups
nName <- c("no_pollicy","hemp_policy", "marijuana_policy") # names of groups
nSize <- c(35,24,29) # number of observations in each group
nMean <- c(8,12,15) # mean of each group
nSD <- c(5,5,5) # standard deviation of each group

ID <- 1:(sum(nSize)) # id vector for each row
aware_count <- c(rnorm(n=nSize[1],mean=nMean[1],sd=nSD[1]),
                 rnorm(n=nSize[2],mean=nMean[2],sd=nSD[2]),
                 rnorm(n=nSize[3],mean=nMean[3],sd=nSD[3]))
TGroup <- rep(nName,nSize)
ANOdata <- data.frame(ID,TGroup,aware_count)
str(ANOdata)
print(ANOdata)

ANOmodel <- aov(aware_count~TGroup,data=ANOdata)
print(ANOmodel)
print(summary(ANOmodel))
z <- summary(ANOmodel)
str(z)


ANOPlot <- ggplot(data=ANOdata) + 
  aes(x=TGroup,y=aware_count,fill=TGroup) +
  geom_boxplot()
print(ANOPlot)
