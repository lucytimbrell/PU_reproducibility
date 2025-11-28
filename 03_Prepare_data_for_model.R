library(here)
library(lme4)
library(dplyr)
setwd(paste(here::here(),"/Nov_2025_workflow",sep="",collapse=""))
library(tidyverse)

# Import all datasets
training_data <- read.csv("Training_data.csv")
testing_data <- read.csv("Testing_data.csv")
individuals <- read.csv("Meta_data.csv")
true_values <- read.csv("PU_Countfunction_nov.csv")  # Third dataset with true assemblage PU values

true_values$PU_13

# Convert Observer to factor in all datasets
training_data$Observer <- as.factor(training_data$Observer)
testing_data$Observer <- as.factor(testing_data$Observer)
individuals$Observer <- factor(individuals$Observer)
true_values$X <- as.factor(true_values$X)

# Combine training and testing data
combined_data <- rbind(training_data, testing_data)
combined_data$Dataset <- factor(c(rep("Training", nrow(training_data)), rep("Testing", nrow(testing_data))))

combined_data


pu_cols <- paste0("PU", 1:33)


# Join with meta data (keep all metadata columns)
combined_data <- combined_data %>%
  left_join(individuals, by = "Observer")


combined_data

# Reshape to long format for modeling
long_data <- combined_data %>%
  pivot_longer(
    cols = all_of(pu_cols),
    names_to = "PU",
    values_to = "Presence"
  ) %>%
  select(Observer, Assemblage, Dataset, PU, Presence, 
         Lithics_experience_yrs, CO_experience_yrs, QM_experience_yrs,
         Days_between_training_testing, Career_stage, Continent,
         Continent_training, Continent_research, PU_familiarity,
         PU_experience, PU_application)

names(true_values)[1]<-"Assemblage"

names(true_values)

# Reshape true_values to long format for easier joining
true_long <- true_values %>%
  pivot_longer(
    cols = starts_with("PU"),
    names_to = "PU",
    values_to = "True_Presence"
  ) 

for(i in 1:length(long_data$Assemblage)){
  long_data$Assemblage[i]<-paste("Assemblage",long_data$Assemblage[i],sep="",collapse="")
}

long_data$Assemblage
# Join long_data with true values
long_data <- long_data %>%
  left_join(true_long, by = c("Assemblage", "PU"))

# Add a column for deviation: 1 if different, 0 if same
long_data <- long_data %>%
  mutate(Deviation = if_else(Presence != True_Presence, 1, 0))

long_data <- long_data %>%
  mutate(FP = if_else(Presence > True_Presence, 1, 0))

long_data <- long_data %>%
  mutate(FN = if_else(Presence < True_Presence, 1, 0))


long_data <- long_data %>%
  mutate(
    TP = if_else(Presence == 1 & True_Presence == 1, 1, 0),
    TN = if_else(Presence == 0 & True_Presence == 0, 1, 0)
  )



setwd(paste(here::here(),"/Nov_2025_workflow",sep="",collapse=""))

write.csv(long_data, "model_input_nov.csv")


