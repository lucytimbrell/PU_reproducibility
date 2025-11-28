# Set working directory and load packages
library(irrCAC)
library(irr)
library(dplyr)
library(tidyverse)
library(irrCAC)

# Import data
training_data <- read.csv("Training_data.csv")
testing_data <- read.csv("Testing_data.csv")
individuals <- read.csv("Meta_data.csv")

training_data[] <-lapply(training_data, as.factor)
str(training_data)

testing_data[] <-lapply(testing_data, as.factor)
str(testing_data)

individuals$Observer <- factor(individuals$Observer)

# Combine training and testing 
combined_data <- rbind(training_data, testing_data)
combined_data$Dataset <- factor(c(rep("Training", nrow(training_data)),
                                  rep("Testing", nrow(testing_data))))

# Join with meta data 
combined_data <- combined_data %>%
  left_join(individuals, by = "Observer")
head(combined_data)

# Data cleaning - accidental 2 instead of 1
combined_data[paste0("PU", 1:33)] <- lapply(combined_data[paste0("PU", 1:33)], function(x) {
  x <- as.numeric(as.character(x))  # convert from factor or character to numeric
  x[x == 2] <- 1                    # replace only 2s with 1
  return(x)
})

############## AC1 and %Unanimous for each PU across datasets ############## 

data_long <- combined_data %>%
  pivot_longer(cols = all_of(paste0("PU", 1:33)),  # explicitly select only PU1 to PU33
               names_to = "PU",
               values_to = "Value")

# Create empty lists to store results
gwet_results <- list()
agree_results <- list()

# Loop over PU1 to PU33
for (i in 1:33) {
  
  pu_name <- paste0("PU", i)
  
  # Filter data for the current PU
  data_pu <- data_long %>% filter(PU == pu_name)
  
  # Pivot to wide format for observers
  pu_wide <- data_pu %>%
    select(Observer, Assemblage, Value) %>%
    pivot_wider(names_from = Observer, values_from = Value) %>%
    select(-Assemblage)  # remove Assemblage if using icc()
  
  # Store GWET estimate
  gwet_results[[pu_name]] <- gwet.ac1.raw(pu_wide)$est
  
  # Store agreement output
  agree_results[[pu_name]] <- agree(pu_wide)$value
}


gwet_data <- do.call(rbind, gwet_results)
agree_data <- do.call(rbind, agree_results)

# Create empty lists
training_agree_results <- list()
testing_agree_results  <- list()

for (i in 1:33) {
  
  pu_name <- paste0("PU", i)
  
  # --- TRAINING DATA ---
  data_train_pu <- data_long %>%
    filter(PU == pu_name, Dataset == "Training")
  
  train_wide <- data_train_pu %>%
    select(Observer, Assemblage, Value) %>%
    pivot_wider(names_from = Observer, values_from = Value) %>%
    select(-Assemblage)
  
  # Store percentage agreement for training
  training_agree_results[[pu_name]] <- agree(train_wide)$value
  
  
  # --- TESTING DATA ---
  data_test_pu <- data_long %>%
    filter(PU == pu_name, Dataset == "Testing")
  
  test_wide <- data_test_pu %>%
    select(Observer, Assemblage, Value) %>%
    pivot_wider(names_from = Observer, values_from = Value) %>%
    select(-Assemblage)
  
  # Store percentage agreement for testing
  testing_agree_results[[pu_name]]  <- agree(test_wide)$value
}

# Convert lists into data frames
training_agree_data <- do.call(rbind, training_agree_results)
testing_agree_data  <- do.call(rbind, testing_agree_results)

final_df <- cbind(gwet_data[,2:5], agree_data, training_agree_data, testing_agree_data)
colnames(final_df) <- c("Gwet agreement", "Gwet agreement change", "Gwet AC1 Coefficient", "std error", "unanimity", "training_unanimity", "testing_unanimity")

# Add row names as PU1 to PU33
rownames(final_df) <- paste0("PU", 1:33)

# Export to CSV
write.csv(final_df, "GWET.csv", row.names = TRUE)
