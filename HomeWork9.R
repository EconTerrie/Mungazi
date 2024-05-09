
library(tidyverse)
library(ggplot2)

generate_ID <- function(nSize) {
  return(1:sum(nSize))
}
generate_aware_count <- function(nSize,nMean,nSD) {
  aware_count <- c(rnorm(n=nSize[1],mean=nMean[1],sd=nSD[1]),
                   rnorm(n=nSize[2],mean=nMean[2],sd=nSD[2]),
                   rnorm(n=nSize[3],mean=nMean[3],sd=nSD[3]))
  return(aware_count)
}
create_ANOdata <- function(ID,TGroup, aware_count) {
  return(ANOdata)
}
nGroup <- 3 
nName <- c("no_policy","hemp_policy","marijuana_policy")
nSize <- c(35,24,29)
nMean <- c(8,10,16)
nSD <- c(5,5,5)
ID <- generate_ID(nSize)
aware_count <- generate_aware_count(nSize,nMean,nSD)
TGroup <- rep(nName,nSize)
ANOdata <- create_ANOdata(ID,TGroup,aware_count)
str(ANOdata)
print(ANOdata)
ANOmodel <- aov(aware_count ~ TGroup,data=ANOdata)
print(ANOmodel)
print(summary(ANOmodel))
z <- summary(ANOmodel)
str(z)




