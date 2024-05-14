
setwd("/Users/NewMacBookAir/Desktop") 
summary_stats <- data.frame( file_name = character(), abundance = numeric(), species_richness = numeric(), year = integer(), stringsAsFactors = FALSE ) 
clean_data <- function(data) {  
  return(na.omit(data)) 
  } 
extract_year <- function(file_name) { 
  return(as.integer(substr(file_name, start = 1, stop = 4))) 
  }  
calculate_abundance <- function(data) { 
  return(nrow(data)) } 
calculate_species_richness <- function(data) { 
  return(length(unique(data$species_column)))  
} 

(year in 2013:2023) { data_folder <- paste("countdata_", year,".csv") 
setwd(data_folder)
csv_files <- list.files(pattern = "countdata") 
for (file in csv_files) { 
  data <- read.csv(file)  
  cleaned_data <- clean_data(data) 
  abundance <- calculate_abundance(cleaned_data) 
  species_richness <- calculate_species_richness(cleaned_data)  
  file_year <- extract_year(file)  
  summary_stats <- rbind(summary_stats, cbind(file, abundance, species_richness, file_year)) 
 } 
} 

setwd("path_to_your_project_folder") 
write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)