## Author: L. Timbrell (05/04/2024)
## Project: The Risk Project
## Description: This script produces a function for counting procedural units from an assemblage, following the codebook by Timbrell and Paige (2024)


# The 'quantify_pu' function returns the presence/absence of procedural units for an assemblage

quantify_pu <- function(cores, flakes){ # input core and flake data
  
        results <- matrix(0, nrow = 33, ncol = 1) # results for 33 procedural units
        row.names(results) <- paste0("PU_", seq(1,33))
        colnames(results) <- "Presence/absence"
        
       results[1] <- ifelse(any( # Raw material treatment
        flakes$Raw.material.treatment == "Heat treated"), 1, 0)
       results[2] <- ifelse(any( # Faceting of core platform
         cores$Platform.preparation == "Faceted" & cores$Hierarchical.core == "Yes") | 
           any((flakes$Platform.preparation == "Faceted") & any((cores$Hierarchical.core == "Yes"))), 1, 0)
       results[3] <- ifelse(any( # Face shaping through radial removals
         cores$Dorsal.scar.pattern == "Centripetal" & 
           cores$Type == "Levallois"  | cores$Type == "Centripetal Levallois" &
           cores$Levallois.Type == "Preferential"), 1, 0)
       results[4] <- ifelse(any( # Lateral trimming
         cores$Core.management == "Lateral trimming" & cores$Hierarchical.core == "Yes"), 1, 0)
       results[5] <- ifelse(any( # Distal trimming
         cores$Core.management == "Distal trimming" & cores$Hierarchical.core == "Yes"), 1, 0)
       results[6] <- ifelse(any( # Back shaping
         cores$Core.management == "Back shaping" & cores$Hierarchical.core == "Yes" |
           cores$Type =="Naviform"), 1, 0)
       results[7] <- ifelse(any( # Cresting
         cores$Core.management == "Crests" | 
           cores$Core.management == "Lateral crests" |
           cores$Core.management == "Frontal crests"), 1, 0)
       results[8] <- ifelse(any( # Debordante
           cores$Core.management == "Debordante" & cores$Hierarchical.core == "Yes", na.rm = TRUE) |
             (any(flakes$Type == "Debordante", na.rm = TRUE) & any(cores$Hierarchical.core == "Yes", na.rm = TRUE)), 1, 0)
       results[9] <- ifelse(any( # Overshot flakes - lots of conditions to rule out those caused byknapping mistakes
           cores$Core.management == "Plunging flakes" &
             cores$Core.use %in% c("Blades", "Bladelets"), na.rm = TRUE) |
             (any(flakes$Flake.termination %in% c("Plunging", "Overshot"), na.rm = TRUE) &
                 any(cores$Core.use %in% c("Blades", "Bladelets") & cores$Hierarchical.core == "Yes", na.rm = TRUE)),1, 0)
      results[10] <- ifelse(any( #  Kombewa
        flakes$Type == "Kombewa" | flakes$Type == "Janus"), 1, 0)
      results[11] <- ifelse(any(
        (cores$Core.management == "Core Tablet" & cores$Hierarchical.core == "Yes") |
          (flakes$Type == "Core tablet" & any(cores$Hierarchical.core == "Yes"))), 1, 0)
      results[12] <- ifelse(any( #  Abrasion/grinding
       cores$Platform.preparation == "Crushed" | cores$Platform.preparation == "Abraded" |
         flakes$Platform.preparation == "Crushed" | flakes$Platform.preparation == "Abraded"), 1, 0) 
      results[13] <- ifelse(any( # Overhang removal/microchipping of area below platform
       flakes$Overhang.removal == "Yes", na.rm = TRUE) | any(cores$Overhang.removal == "Yes", na.rm = TRUE) , 1, 0)
      results[14] <- ifelse(any( # Percussion by striking with hard hammer
       flakes$Initiation.type == "Hertzian"), 1, 0)
      results[15] <- ifelse(all( # Core supported by hand
        cores$Type == "Bipolar" &  flakes$Initiation.type == "Wedging") , 0, 1)
      results[16] <- ifelse(any( # Use of anvil to support core
        cores$Type == "Bipolar") | any(flakes$Initiation.type == "Wedging"), 1, 0)
      results[17] <- ifelse(any(  #  Core rotation
        cores$Number.of.core.surfaces > 2  & cores$Type != "Single Platform"), 1, 0)
      results[18] <- ifelse(any( #  Soft hammer
        flakes$Platform.lipping == "Yes" & flakes$Initiation.type == "Bending" & flakes$Bulb == "Absent" | flakes$Bulb == "Diffuse"), 1, 0)
      results[19] <- ifelse(any( # Indirect percussion
        flakes$Initiation.type == "Punch"), 1, 0)
      results[20] <- ifelse(any( # Flaking with application of pressure
        flakes$Initiation.type == "Bending" & flakes$Flake.termination == "Feather"), 1, 0)
      results[21] <- ifelse(any( # Pecking/hammer dressing
        flakes$Active.edge.retouch.type == "Pecking dressing" |flakes$Inactive.edge.retouch.type == "Pecking dressing",  na.rm = TRUE),1 ,0)
       results[22] <- ifelse(any(
         (cores$Length.of.dominant.scar / cores$Maximum.dimension) >= 0.5 |
           (cores$Type == "Levallois" & flakes$X.Cortex < 0) |
           ((flakes$Type %in% c("Blade", "Bladelets")) & flakes$X.Cortex < 0)),1, 0)
       results[23] <- ifelse(any( #  Ochre use
         flakes$Raw.material.treatment == "Ochre"), 1, 0)
       results[24] <- ifelse(any( #  Asphalt use
         flakes$Raw.material.treatment == "Asphalt"), 1, 0)
        results[25] <- ifelse(any( # Tanging
          flakes$Inactive.edge.retouch.type == "Tanged",  na.rm = TRUE), 1, 0)
        results[26] <- ifelse(any( #  Invasive retouch
          flakes$Retouch.index >= 0.5), 1, 0)
        results[27] <- ifelse(any( #   Unifacial retouch 
          flakes$Retouched.face == "Dorsal" | flakes$Retouched.face == "Ventral",  na.rm = TRUE), 1, 0)
        results[28] <- ifelse(any( # Backing
          flakes$Inactive.edge.retouch.type == "Backing" | flakes$Angle.of.retouch == "Abrupt",  na.rm = TRUE), 1, 0)
        results[29] <- ifelse(any( # Notching
          flakes$Inactive.edge.retouch.type == "Notched" |  flakes$Active.edge.retouch.type == "Notched" | 
            flakes$Inactive.edge.retouch.type == "Denticulated" |  flakes$Active.edge.retouch.type == "Denticulated" |
            flakes$Inactive.edge.retouch.type == "Clustered" & flakes$Angle.of.retouch == "Abrupt" | 
            flakes$Active.edge.retouch.type == "Clustered" & flakes$Angle.of.retouch == "Abrupt",  na.rm = TRUE), 1, 0)
        results[30] <- ifelse(any( # Burination
          flakes$Inactive.edge.retouch.type == "Burination" |  flakes$Active.edge.retouch.type == "Burination", na.rm = TRUE), 1, 0)
        results[31] <- ifelse(any( # Tranchet removal
          flakes$Active.edge.retouch.type == "Tranchet removal" & flakes$Type == "Core tool" | 
            flakes$Type == "Core tool" & flakes$Tranchet.removal == "Present", na.rm = TRUE), 1, 0)
        results[32] <- ifelse(any( # Pressure retouch
          flakes$Inactive.edge.retouch.type == "Pressure flaking" | flakes$Active.edge.retouch.type == "Pressure flaking",  na.rm = TRUE), 1, 0)
        results[33,] <- ifelse(any( #  Bifacial retouch
          flakes$Retouched.face == "Both",  na.rm = TRUE), 1, 0)
        
    return(results)
} 

# The 'count_pu' function returns the total number of pus for an assemblage

count_pu <- function(results){ # results from quantify PU
  
   N <- sum(results[,1])
  
  return(N)
}

#################### Training #################### 
setwd("/Users/lucytimbrell/Documents/LT_documents/MPI-UoL postdoc/Replicability study/Materials for observers/Simulated data/Training")

# Create empty lists to store results
training_assemblage_results <- list()
training_assemblage_overall <- list()

# Loop through Assemblage_6 to Assemblage_20
for (i in 1:5) {
  
  # Construct file names
  core_file <- paste0("Assemblage_", i, "_Cores.csv")
  flake_file <- paste0("Assemblage_", i, "_Flakes.csv")
  
  # Read the data
  cores <- read.csv(core_file)
  flakes <- read.csv(flake_file)
  
  # Run quantify and count functions
  assemblage_name <- paste0("Assemblage_", i)
  training_assemblage_results[[assemblage_name]] <- quantify_pu(cores, flakes)
  training_assemblage_overall[[assemblage_name]] <- count_pu(training_assemblage_results[[assemblage_name]])
}

#################### Testing #################### 
setwd("/Users/lucytimbrell/Documents/LT_documents/MPI-UoL postdoc/Replicability study/Materials for observers/Simulated data/Testing")

# Create empty lists to store results
testing_assemblage_results <- list()
testing_assemblage_overall <- list()

# Loop through Assemblage_6 to Assemblage_20
for (i in 6:20) {
  
  # Construct file names
  core_file <- paste0("Assemblage_", i, "_Cores.csv")
  flake_file <- paste0("Assemblage_", i, "_Flakes.csv")
  
  # Read the data
  cores <- read.csv(core_file)
  flakes <- read.csv(flake_file)
  
  # Run quantify and count functions
  assemblage_name <- paste0("Assemblage_", i)
  testing_assemblage_results[[assemblage_name]] <- quantify_pu(cores, flakes)
  testing_assemblage_overall[[assemblage_name]] <- count_pu(testing_assemblage_results[[assemblage_name]])
}

#################### Plots #################### 
setwd("/Users/lucytimbrell/Documents/LT_documents/MPI-UoL postdoc/Replicability study/Results")

training_data <- read.csv("Training_data.csv")
testing_data <- read.csv("Testing_data.csv")

combined_data <- rbind(training_data, testing_data)
combined_data$Dataset <- factor(c(rep("Training", nrow(training_data)),
                                  rep("Testing", nrow(testing_data))))


assemblage_overall <- c(training_assemblage_overall, testing_assemblage_overall)
assemblage_results <- c(training_assemblage_results, testing_assemblage_results)

combined_PUs <- do.call(cbind, assemblage_results)
colnames(combined_PUs) <- paste0("Assemblage_", seq(1,20))
write.csv(combined_PUs, "Observer_data_revised.csv")

library(ggplot2)
library(ggplot2)
library(dplyr)


all_assem <- data.frame()

for (i in 1:20) {
  temp <- subset(combined_data, Assemblage == as.character(i))
  temp$Assemblage <- factor(paste0("Assemblage ", i), levels = paste0("Assemblage ", 1:20))
  temp$Overall <- as.numeric(assemblage_overall[[paste0("Assemblage_", i)]])
  all_assem <- rbind(all_assem, temp)
}

write.csv(all_assem, "Observer_data2.csv")

ggplot(all_assem, aes(x = as.numeric(as.character(Ass_total)), fill = Dataset)) +
  geom_bar(color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = Overall),
             color = "darkred", linetype = "dashed", linewidth = 0.8) +
  ggh4x::facet_wrap2(~ Assemblage, scales = "free_y", axes = "all") +
  scale_x_continuous(breaks = 20:30) +
  scale_fill_manual(values = c("Training" = "darkorange", "Testing" = "steelblue")) +  # customize colors here
  labs(
    title = "Total PUs for each assemblage",
    x = "Observer PU counts",
    y = "Frequency",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))