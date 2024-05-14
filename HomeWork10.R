

zero_count <- function (x) {
  counter <- 0
  for (i in x) {
    if (i==0) {
        counter <- counter + 1 
        }
  }
  return(counter)
}

zero_count_subsetting <- function(x) { 
  return(sum(x==0))
}

matrix <- function(rows,cols) {
  mat <- outer(1:rows,1:columns,"*")
  return(mat)
}

set.seed(123)
data <- data.frame( group = rep(1:3, each = 20), response = c(rnorm(20, mean = 5), rnorm(20, mean = 7), rnorm(20, mean = 9)) )


shuffle_and_mean <- function(data) { 
data$response <- sample(data$response) 
means <- tapply(data$response, data$group, mean) 
return(means) 
} 

results <- data.frame(replicate = 1:100, group1_mean = rep(NA,100),group2_mean = rep(NA,100),group3_mean=rep(NA,100))
for (i in 1:100) {
  results[i,"group1_mean"] <- shuffle_and_mean(data)[1]
  results[i,"group2_mean"] <- shuffle_and_mean(data)[2]
  results[i,"group3_mean"] <- shuffle_and_mean(data)[3]
} 

library(ggplot2) 
library(tidyr) 
results_long <- gather(results, group, mean, -replicate)  
ggplot(results_long, aes(x = mean, fill = group)) + geom_histogram(binwidth = .5, color = "black", position = "identity", alpha = .5) + facet_wrap(~group, scales = "free") + labs(title = "Distribution of Reshuffled Means by Group") 

generate_data <- function(group,n,mean) {
df <-data.frame(group=rep(group,n),response=rnorm(n,mean=mean))
}
n <-100
group1_mean <-10
group2_mean <- 20
group3_mean <- 30 

group1_data <- generate_data("Group1",n,group1_meanf)
group2_data <- generate_data("Group2",n,group2_mean)
group3_data <- generate_data("Group3",n,group3_mean)

final_data <- bind_rows(group1_data,group2_data,group3_data)
print(final_data)

custom_function <- function(data) {
  data$response <- sample(data$response)
  
  means <- tapply(final_data$response,data$group,FUN = mean)
  return(means)
}

reshuffled_means <- custom_function(df)
print(reshuffled_means)

results <- data.frame(replicate=1:100) 
for(i in 1:100) {
  results[i,c("Group1","Group2","Group3")] <- reshuffle_and_calculate_means(data)
} 

library(ggplot2)
ggplot(data = results, aes(x=Group1))+
  geom_histogram(binwidth=0.5,fill="lightblue",color="black",alpha=0.7) +
  geom_histogram(data = results, aes(x=Group2),binwidth=0.5,fill="lightblue",color="black",alpha=0.7) +
  geom_histogram(data = results, aes(x=Group3),binwidth=0.5,fill="lightblue",color="black",alpha=0.7) +
  labs(title = "Comparison of Reshuffled Means vs Original Means",x="Group Mean",y="Frequency" +
    theme_minimal()
  
  