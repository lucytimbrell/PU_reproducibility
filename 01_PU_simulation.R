## Author: L. Timbrell (05/04/2024)
## Project: The Risk Project
## Description: This script randomly simulates lithic attribute data for a given number of assemblages. Probabilities are set as equal for each attribute class, 
## though a number of dependencies have been coded so that data are realistic

setwd("/Users/lucytimbrell/Documents/LT_documents/MPI-UoL postdoc/Replicability study/Random props Simulated data")

library(MASS)
library(faux)

set.seed(123456789)

n <- 20 ## Number of assemblages to produce
N <- 100 ## Number of artefacts in simulated assemblage (half will be cores and half will be flakes)

################################################ SIMULATION  ########################################################################
####################################################################################################################################

for(a in 1:n){
  
######## CORES ########

## Core attribute classes and probabilities ##

dorsal_scar_pattern <- c("Unidirectional", "UnidirectionalConvergent", "Bidirectional", "Centripetal","Orthogonal", "Multidirectional", "Indeterminate")
x <- runif(length(dorsal_scar_pattern), 0, 1)
p1 <- x / sum(x)

core_type <- c("Single Platform", "Levallois", "Discoidal", "Multiple Platform", "Centripetal Levallois", "Bipolar", "Irregular", "Indeterminate", "Bidirectional", "Blade core")
x <- runif(length(core_type), 0, 1)
p2 <- x / sum(x)

levallois_removal <- c("Preferential", "Recurrent")
x <- runif(length(levallois_removal), 0, 1)
p3 <- x / sum(x)

core_use <- c("Flakes", "Blades", "Points", "Bladelets", "Indeterminate")
x <- runif(length(core_use), 0, 1)
p4 <- x / sum(x)

management_platform_surface <- c("Levallois", "Circumferential", 'Debordante', "Core Tablet","Crests", "None", "Indeterminate", "Back shaping", 'Lateral trimming', 'Distal trimming')
x <- runif(length(management_platform_surface), 0, 1)
p5 <- x / sum(x)

core_platform_preparation <- c("Faceted", "Crushed", "Abdraded", "Indeterminate", "None")
x <- runif(length(core_platform_preparation), 0, 1)
p6 <- x / sum(x)

n_surfaces <- c(2, 3, 4, 5, 6, 7, "Indeterminate")
x <- runif(length(n_surfaces), 0, 1)
p7 <- x / sum(x)

hierarchical <- c("Yes", "No", "Indeterminate")
x <- runif(length(hierarchical), 0, 1)
p8 <- x / sum(x)


## Create lists of attributes and probabilities
core_var_list <- list(dorsal_scar_pattern, core_type, levallois_removal, core_use, management_platform_surface, core_platform_preparation, n_surfaces, hierarchical)
core_prob_list <- list(p1, p2, p3, p4, p5, p6, p7,p8)

###### Simulation ######  

# Core attribute data

core_sim_data <- matrix(0, ncol = length(core_var_list), nrow = N/2) # Empty matrix to populate

for(i in 1:length(core_var_list)){
  
  if(i == 2){ # Core type
    for(j in 1:nrow(core_sim_data)){
      core_sim_data[j,i] <- ifelse(core_sim_data[j,i-1] == "Unidirectional" | core_sim_data[j,i-1] == "UnidirectionalConvergent", # If unidirectional dorsal scar pattern, then cannot be Centripetal, discoidal or bidirectional core,
                                   sample(x = core_var_list[[i]][-c(3,4,5,9)], size = 1, prob = core_prob_list[[i]][-c(3,4,5,9)], replace = TRUE),
                                      ifelse(core_sim_data[j,i-1] == "Multidirectional", # If multidirectional scar pattern, cannot be a single platform, bidirectional or bipolar core
                                          sample(x = core_var_list[[i]][-c(1,6,9)], size = 1, prob = core_prob_list[[i]][-c(1,6, 9)], replace = TRUE),
                                          ifelse(core_sim_data[j,i-1] == "Bidirectional", # If bidirectional scar pattern, cannot be irregular, single platform, multiple platform or discoidal/centripetal
                                                 sample(x = core_var_list[[i]][-c(1,3,4, 5, 7)], size = 1, prob = core_prob_list[[i]][-c(1,3,4, 5, 7)], replace = TRUE),
                                                 ifelse(core_sim_data[j,i-1] == "Centripetal", # If centripetal dorsal scar pattern, then can only be levallois, discoid or centripetal core type
                                                    sample(x = core_var_list[[i]][c(2,3,5)], size = 1, prob = core_prob_list[[i]][c(2,3,5)], replace = TRUE), #
                                                           sample(x = core_var_list[[i]][-c(3:5)], size = 1, prob = core_prob_list[[i]][-c(3:5)], replace = TRUE)))))} # removing discoidal and centripetal which can only have centripetal DSP and multiplatform
  }
    
  else{
  if(i == 3){ # Levallois removal type
    for(j in 1:nrow(core_sim_data)){
    core_sim_data[j,i] <- ifelse(core_sim_data[j,i-1] == "Levallois" | core_sim_data[j,i-1] == "Centripetal Levallois"  , 
                                 sample(x = core_var_list[[i]], size = 1, prob = core_prob_list[[i]], replace = TRUE), 
                                 NA)} 
       } # If Levallois, then randomly sample from Levallois removal type attribtute, if not then NA
  

else{
 
  if(i == 4){  # Core use 
    for(j in 1:nrow(core_sim_data)){
      core_sim_data[j,i] <- ifelse(core_sim_data[j,i-2] == "Discoidal" | core_sim_data[j,i-2] == "Centripetal Levallois" , # if discoidal or radial core then cannot have blade/bladelet removals
                                   sample(x = core_var_list[[i]][-c(2, 4)], size = 1, prob = core_prob_list[[i]][-c(2,4)], replace = TRUE),
                                     ifelse(core_sim_data[j,i-2] == "Multiple Platform" | core_sim_data[j,i-2] == "Irregular", # if irregular or multiple platform, then flakes
                                            sample(x = core_var_list[[i]][1], size = 1, prob = core_prob_list[[i]][1], replace = TRUE),
                                                 ifelse(core_sim_data[j,i-2] == "Levallois", # if Levallois then cannot have bladelet removals
                                                        sample(x = core_var_list[[i]][-4], size = 1, prob = core_prob_list[[i]][-4], replace = TRUE),
                                                           ifelse(core_sim_data[j,i-2] == "Blade core", # if Blade core, then produced blades or bladelets
                                                               sample(x = core_var_list[[i]][c(2,4)], size = 1, prob = core_prob_list[[i]][c(2,4)], replace = TRUE),
                                                               ifelse(core_sim_data[j,i-2] == "Bipolar", 
                                                                      sample(x = core_var_list[[i]][c(1,2,5)], size = 1, prob = core_prob_list[[i]][c(1,2,5)], replace = TRUE),
                                                                   ifelse(core_sim_data[j,i-2] == "Indeterminate", # if Indeterminate core type, then indeterminate
                                                                       sample(x = core_var_list[[i]][5], size = 1, prob = core_prob_list[[i]][5], replace = TRUE),
                                                                            sample(x = core_var_list[[i]][-2], size = 1, prob = core_prob_list[[i]][-2], replace = TRUE))))))) } 
    }
      else{
        
  if(i == 5){ #Core management 
    for(j in 1:nrow(core_sim_data)){
      core_sim_data[j,i] <-  ifelse(core_sim_data[j,i-3] == "Bipolar" | core_sim_data[j,i-3] == "Single Platform", # If bipolar or single platform then none
                                       "None", 
                                    ifelse(core_sim_data[j,i-3] == "Blade core", # If blade core, can be debordante, core tablet, crests, none or indetermediate
                                           sample(x = core_var_list[[i]][c(3:7)], size = 1, prob = core_prob_list[[i]][c(3:7)], replace = TRUE), 
                                           ifelse(core_sim_data[j,i-3] == "Blade core" & core_sim_data[j,i-2] == "Bladelets" , # If bladelet core, can be debordante, core tablet, crests, none, indetermediate or back shaping 
                                                  sample(x = core_var_list[[i]][c(3:8)], size = 1, prob = core_prob_list[[i]][c(3:8)], replace = TRUE), 
                                               ifelse(core_sim_data[j,i-3] == "Bidirectional", # If bidirectional, can be debordante, none or indeterminate
                                           sample(x = core_var_list[[i]][c(3, 6, 7)], size = 1, prob = core_prob_list[[i]][c(3,6,7)], replace = TRUE), 
                                             ifelse(core_sim_data[j,i-3] == "Discoidal" | core_sim_data[j,i-3] == "Multiple Platform", # If discoidal or multiplatform then either circumfrential or crests
                                              sample(x = core_var_list[[i]][c(2,5,6,7)], size = 1, prob = core_prob_list[[i]][c(2,5,6,7)], replace = TRUE), 
                                              ifelse(core_sim_data[j,i-3] == "Centripetal Levallois", # If centripetal Levallois then remove debordant and back shaping
                                                     sample(x = core_var_list[[i]][-c(3,5, 8)], size = 1, prob = core_prob_list[[i]][-c(3,5, 8)], replace = TRUE),
                                              ifelse(core_sim_data[j,i-3] == "Levallois", # If a Levallois core, then include Levallois, lateral and distal trimming, exclude back shaping
                                                  sample(x = core_var_list[[i]][-c(5,8)], size = 1, prob = core_prob_list[[i]][-c(5,8)], replace = TRUE),
                                                        sample(x = core_var_list[[i]][-c(1,3,4,8:10)], size = 1, prob = core_prob_list[[i]][-c(1,3,4,8:10)], replace = TRUE))))))))}# Anything else, remove debordante, core tablet,  Levallois, back shaping, lateral and distal trimming 
     }  
  
  else{
   
    if(i == 6){ #Platform preparation
      for(j in 1:nrow(core_sim_data)){
        core_sim_data[j,i] <- ifelse(core_sim_data[j,i-4] == "Levallois" | core_sim_data[j,i-4] == "Centripetal Levallois", # If Levallois then must have facetted or abraded platforms
                                     sample(x = core_var_list[[i]][c(1,3)], size = 1, prob = core_prob_list[[i]][c(1,3)], replace = TRUE),
                                        ifelse(core_sim_data[j,i-4] == "Single Platform" | core_sim_data[j,i-4] == "Bipolar",  # If Single platform core, then no platform prep
                                           "None",
                                              sample(x = core_var_list[[i]], size = 1, prob = core_prob_list[[i]], replace = TRUE)))}
        
      }
   else{
     
     if(i == 7){ # Number of surfaces 
     for(j in 1:nrow(core_sim_data)){
       core_sim_data[j,i] <- ifelse(core_sim_data[j,i-5] == "Irregular", # If irregular core then between 4 and 7 core surfaces
                                    sample(x = core_var_list[[i]][-c(1:2)], size = 1, prob = core_prob_list[[i]][-c(1:2)], replace = TRUE), 
                                         ifelse(core_sim_data[j,i-5] == "Bipolar" | core_sim_data[j,i-5] == "Bidirectional" | core_sim_data[j,i-5] == "Blade core", # if bipolar, bidirectional or blade core, then only 2 or 3 core surfaces
                                           sample(x = core_var_list[[i]][c(1:2)], size = 1, prob = core_prob_list[[i]][c(1:2)], replace = TRUE), 
                                              ifelse(core_sim_data[j,i-5] == "Multiple Platform ", # if multiple platform then > 2 surfaces 
                                                    sample(x = core_var_list[[i]][-1], size = 1, prob = core_prob_list[[i]][-1], replace = TRUE), # Everything else between 2 and 4
                                                         sample(x = core_var_list[[i]][c(1:2)], size = 1, prob = core_prob_list[[i]][c(1:2)], replace = TRUE))))}
   }  
  
  else{
    if(i == 8){ # Hierarchical core
      for(j in 1:nrow(core_sim_data)){
        core_sim_data[j, i] <- "No"
        
        if(core_sim_data[j, i - 3] %in% c("Debordante", "Core tablet", "Lateral trimming", "Distal trimming", "Backing shaping")) {
          core_sim_data[j, i] <- sample(x = core_var_list[[i]][(1)], size = 1, prob = core_prob_list[[i]][(1)], replace = TRUE)
        }
        
        if(core_sim_data[j, i] == "No" &&
           (core_sim_data[j, i - 6] == "Levallois" || core_sim_data[j, i - 6] == "Centripetal Levallois" ||core_sim_data[j, i - 6] == "Blade core")) {
          core_sim_data[j, i] <- sample(x = core_var_list[[i]][1], size = 1, prob = core_prob_list[[i]][1], replace = TRUE)
        }
      }
    }                            
   else{
  core_sim_data[,i] <- sample(x = core_var_list[[i]], size = N/2, prob = core_prob_list[[i]], replace = TRUE)}
   }
 }
}
}
    }
  }
  }

##### Core metrics #####

## means of individual distributions
mu1 <- 62.7 # maxmimum dimension
mu2 <- 26.3 # thickness
mu3 <- 42.6 # length of dominant scar

## variance
sigma1 <- 300 # maxmimum dimension
sigma2 <- 135.2  # thickness
sigma3 <- 200 # length of dominant scar
  
## Correlations
X1 <- 0.6  # md + thickness
X2 <- 0.8 # md + lds
X3 <- 0.5 # thickness + lds

###### Simulation ######  
core_metric_dat <- mvrnorm(N/2, mu = c(mu1, mu2, mu3),
               Sigma = matrix(c(sigma1, X1     ,     X3,
                                X1    , sigma2,     X2,
                                X3    , X2    , sigma3),
                              ncol = 3, byrow = TRUE), empirical = TRUE)

core_metric_dat <- abs(core_metric_dat) # ensure none are negative
colnames(core_metric_dat) <- c("Maximum dimension", "Thickness", "Length of dominant scar")
core_metric_dat <- as.data.frame(core_metric_dat)

core_metric_dat$`Length of dominant scar` <- pmin(core_metric_dat$`Length of dominant scar`, 
                                                  core_metric_dat$`Maximum dimension` - 5)

core_weight <- rnorm_pre(core_metric_dat$Length, mu = 110.1, sd = 149.08, r = 0.9, empirical = TRUE) # Simulate weight based on length
core_weight <- abs(core_weight)
cortex <- sample(x = c("0%", "1-10%", "11-20%", "21-30%", "31-40%", "41-60%"), size = N/2, prob = c(0.9, 0.05, 0.02, 0.01, 0.01, 0.01),  replace = TRUE)

#### Final dataset #########  
core_table <- as.data.frame(cbind(round(core_weight, 2), cortex, round(core_metric_dat,2), core_sim_data)) 
colnames(core_table) <- c("Weight",
                          "%Cortex",
                          "Maximum dimension",
                          "Thickness",
                          "Length of dominant scar",
                          "Dorsal scar pattern", 
                          "Type", 
                          "Levallois Type", 
                          "Core use", 
                          "Core management", 
                          "Platform preparation",
                          "Number of core surfaces", 
                          "Hierarchical core")
rownames(core_table) <- paste0(seq(1:(N/2)))

write.csv(core_table, paste0("Assemblage_",a,"_Cores.csv"))

######## FLAKES ########
## We need to simulate metrics first as the type of flake depends on this 

# Flake metrics 

# These are somewhat correlated so we can include those correlations here
## means of individual distributions 
mu1 <- 55.9 # max length
mu2 <-28.6 # max width
mu3 <- 11.8 # max thickness

## variance
sigma1 <- 640.2 # max length
sigma2 <-271.2  # max width
sigma3 <- 40.6 # max thickness

## Correlations
X1 <- 0.8  # length + width
X2 <- 0.6 # length + thickness
X3 <- 0.61 # width + thickness

###### Simulation ###### 
flake_metric_dat <- mvrnorm(N/2, mu = c(mu1, mu2, mu3),
                            Sigma = matrix(c(sigma1, X1     ,     X3,
                                             X1    , sigma2,     X2,
                                             X3    , X2    , sigma3),
                                           ncol = 3, byrow = TRUE), empirical = TRUE)

colnames(flake_metric_dat) <- c("Length", "Width", "Thickness")
flake_metric_dat <- as.data.frame(flake_metric_dat)

# flake_weight <- rnorm(n = N/2, mean = 70, sd = 55)
flake_weight <- rnorm_pre(flake_metric_dat$Length, mu = 33.3, sd = 52.2, r = 0.7, empirical = TRUE) # Simulate weight based on length
flake_weight <- abs(flake_weight)

flake_cortex <- sample(x = c("0%", "1-10%", "11-20%", "21-30%", "31-40%", "41-60%"), size = N/2, prob = c(0.9, 0.05, 0.02, 0.01, 0.01, 0.01),  replace = TRUE)

## Flake attribute classes and probabilities ##

raw_material_treatment <- c("Ochre", "Heat treated", "Abrasion", "Asphalt", "None")
q1 <- c(0.01, 0.01, 0.01, 0.01, 0.96)

initation_type <- c("Hertzian", "Bending","Punch", "Wedging" ,"Indeterminate", "Indeterminate")
q2 <- c(0.75, 0.05, 0.05, 0.05, 0.05, 0.05)

lateral_edges <- c("Converging", "Elongated", "Parallel", "Ovoid", "Indeterminate")
x <- runif(length(lateral_edges), 0, 1)
q3 <- x / sum(x)

flake_type <- c("Flake", "Kombewa","Retouched flake","Levallois point") # Blades removed as depends on flake metrics, core tools not included as only typed when NA initiation
x <- runif(length(flake_type), 0, 1)
q4 <- x / sum(x)

flake_distal_terminus <- c("Feather", "Step", "Hinge", "Overshot", "Blunt", "Crushed", "Indeterminate") # need to include depending on previous column (i.e. retouched are likely Indeterminate)
x <- runif(length(flake_distal_terminus), 0, 1)
q5 <- x / sum(x)

platform_lipping <- c('Yes', "No", "Indeterminate")
x <- runif(length(platform_lipping), 0, 1)
q6 <- x / sum(x)

flake_platform_preparation <- c("Faceted", "None", "Crushed", "Abraded" ,"Indeterminate")
x <- runif(length(flake_platform_preparation), 0, 1)
q7 <- x / sum(x)

active_edge_retouch <- c("Regular", "Denticulated", "Notched", "Burination", "Pressure flaking", "Pecking dressing")
x <- runif(length(active_edge_retouch ), 0, 1)
q8 <- x / sum(x)

inactive_edge_retouch <- c("Regular", "Tanged", "Basal thinning", "Truncation", "Shouldered", "Backing")
x <- runif(length(inactive_edge_retouch), 0, 1)
q9 <- x / sum(x)

retouched_face <- c("Dorsal", "Ventral", "Both", "Indeterminate")
x <- runif(length(retouched_face), 0, 1)
q10 <- x / sum(x)

tranchet_removal <- c("Present", "Absent", "Indeterminate")
x <- runif(length(tranchet_removal), 0, 1)
q11 <- x / sum(x)

bulb <- c("Present",  "Indeterminate", "Absent")
x <- runif(length(bulb), 0, 1)
q12 <- x / sum(x)

angle_retouch <-  c("Flat",  "Abrupt", "Semi-abrupt")
x <- runif(length(angle_retouch ), 0, 1)
q13 <- x / sum(x)

overhang_removal <- c("Yes", "No", "Indeterminate") 
x <- runif(length(overhang_removal ), 0, 1)
q14 <- x / sum(x)

# Create lists of attributes and probabilities 

flake_var_list <- list(raw_material_treatment, 
                       initation_type,
                       lateral_edges,
                       flake_type, 
                       flake_distal_terminus, 
                       platform_lipping, 
                       flake_platform_preparation,
                       active_edge_retouch, 
                       inactive_edge_retouch, 
                       retouched_face,
                       tranchet_removal,
                       bulb,
                       angle_retouch,
                       overhang_removal)
flake_prob_list <- list(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14)

###### Simulation ###### 
## Unlike the core dataset, there are continuous variables that depend on the presence of a certain attribute class. We therefore need to include simulation of continuous data within this loop

# Flake attributes

flake_sim_data <- matrix(0, ncol = length(flake_var_list)+4, nrow = N/2) # Add 4 extra columns for additional continuous variables we will simulate

for(i in 1:length(flake_var_list)){
  
  if(i == 2){ # Initiation type
    for(j in 1:nrow(flake_sim_data)){
      flake_sim_data[j,i] <- ifelse(any(core_table$`Core Type` == "Bipolar"),
                                    sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                    sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE))} # If bipolar present, then flakes with wedging initiation
  } 
    else{
      if(i == 3){ # Lateral edges
        for(j in 1:nrow(flake_metric_dat)){
          flake_sim_data[j,i] <- ifelse(flake_sim_data[j,i-1] == "Wedging",
                                               "Parallel",
                                               sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE))} # If not retouched, then code next column as NA,  If retouched, then randomly sample from active edge retouch type attribtute
      } 
     
       else{
  if(i == 4){ # Flake type
    for(j in 1:nrow(flake_metric_dat)){
      flake_sim_data[j,i] <- ifelse(flake_metric_dat$Length[j] >= flake_metric_dat$Width[j]*2 & flake_sim_data[j,i-2] == "Hertzian" , sample(x = c("Blade", "Retouched blade", "Debordante"), size = 1, prob = c(0.5, 0.3, 0.2), replace = TRUE), 
                                    ifelse(flake_metric_dat$Length[j] >= flake_metric_dat$Width[j]*2 & flake_sim_data[j,i-2] == "Punch" , sample(x = c("Blade", "Retouched blade"), size = 1, prob = c(0.5, 0.5), replace = TRUE), 
                                           ifelse(flake_sim_data[j,i-2] == "Punch", "Flake",
                                    ifelse(flake_metric_dat$Length[j] >= flake_metric_dat$Width[j]*2 & flake_metric_dat$Length[j] < 10, "Bladelet",
                                        ifelse(flake_sim_data[j,i-1] == "Converging", sample(x = flake_var_list[[i]][c(1,3)], size = 1, prob = flake_prob_list[[i]][c(1,3)], replace = TRUE),
                                                ifelse(flake_sim_data[j,i-2] == "Wedging", sample(x = "Flake", size = 1, prob = 1, replace = TRUE),
                                                       ifelse(flake_sim_data[j,i-2] == "Bending", sample(x = flake_var_list[[i]][-c(4,6)], size = 1, prob = flake_prob_list[[i]][-c(4,6)], replace = TRUE),
                                                          ifelse(flake_sim_data[j,i-2] == "NA", "Core tool",                                                                                                                    
                                                             sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE)))))))))} # If not retouched, then code next column as NA,  If retouched, then randomly sample from active edge retouch type attribtute
    } 
  else{
  if(i == 5){ # Termination
    for(j in 1:nrow(flake_metric_dat)){
      flake_sim_data[j,i] <- ifelse(flake_sim_data[j,i-3] == "Wedging", "Step", 
                                           sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE))} # If not retouched, then code next column as NA,  If retouched, then randomly sample from active edge retouch type attribtute
  }
  else{
    if(i == 6){ # Platform lipping
      for(j in 1:nrow(flake_sim_data)){
        flake_sim_data[j,i] <- ifelse(flake_sim_data[j,i-4] == "Bending",
                                      sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                      "No")} # If flake produced by bending intitiation then platform lipping, if not then not present
    } 
  
  else{
  if(i == 8){ # Active edge retouch type
    for(j in 1:nrow(flake_sim_data)){
      flake_sim_data[j,i] <- ifelse(flake_sim_data[j,i-4] == "Core tool",
                                    sample(x = c("Regular", "Pressure", NA), size = 1, prob = c(0.33,0.33, 0.33), replace = TRUE), 
                                       ifelse(flake_sim_data[j,i-4] == "Retouched flake"|flake_sim_data[j,i-4] =="Retouched blade",
                                            sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                                  NA))} # If not retouched, then code next column as NA,  If retouched, then randomly sample from active edge retouch type attribtute
  } 
  
  else{
    if(i == 9){ # Inactive edge retouch type 
      for(j in 1:nrow(flake_sim_data)){
        flake_sim_data[j,i] <- ifelse(flake_sim_data[j,i-5] == "Retouched flake"|flake_sim_data[j,i-5] =="Retouched blade", 
                                      sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                      NA) # If not retouched, then code next column as NA,  If retouched, then randomly sample from active edge retouch type attribtute
        ifelse(is.na(flake_sim_data[j,i]), b <- c(NA, NA, NA),
        ifelse(flake_sim_data[j,i] == "Tanged", 
               b <- round(cbind(rnorm(n = 1, mean = 12, sd = 4),rnorm(n = 1, mean = 12, sd = 5), rnorm(n = 1, mean = 15, sd = 5)),2), 
               b <- c(NA, NA, NA)))  # Need to set up if statement differently to allow for vector output
            flake_sim_data[j,c(11:13)] <- b } # And if Inactive edge is tanged, simulate tang metrics
    }
    
    else{
      if(i == 10){ # Retouched face
        
        for(j in 1:nrow(flake_sim_data)){
          flake_sim_data[j,i] <-ifelse(flake_sim_data[j,i-6] == "Retouched flake"|flake_sim_data[j,i-6] =="Retouched blade", 
                                       sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                       NA)
          
          ifelse(is.na(flake_sim_data[j,i-1]), flake_sim_data[j,14] <- NA,
             ifelse(flake_sim_data[j,i-1] == "Pressure flaking", flake_sim_data[j,14] <- 1,
                    flake_sim_data[j,14] <- round(runif(1, min=0.1, max=1), 2) ))}
      }
      
      else{
        if(i == 11){ # Tranchet removal
          for(j in 1:nrow(flake_sim_data)){
            flake_sim_data[j,15] <-ifelse(flake_sim_data[j,i-7] == "Core tool",
                                         sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                         "Absent")}
        }
        
        else{
          if(i == 12){ # Bulb
            for(j in 1:nrow(flake_sim_data)){
              flake_sim_data[j,16] <-ifelse(flake_sim_data[j,i-10] == "Hertzian" & is.na(flake_sim_data[j,i-10]),
                                            sample(x = flake_var_list[[i]][-1], size = 1, prob = flake_prob_list[[i]][-1], replace = TRUE),
                                                 ifelse(flake_sim_data[j,i-10] == "Wedging", "Absent", 
                                                        sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE)))}
                                  
          }
          
          else{
            if(i == 13){ # Angle of retouch
              for(j in 1:nrow(flake_sim_data)){
                flake_sim_data[j,17] <-ifelse(flake_sim_data[j,i-9] == "Retouched flake"|flake_sim_data[j,i-9] =="Retouched blade",
                                              sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE), 
                                              NA)}
            }
            
            else{
              if(i == 14){ # Overhang removal
                for(j in 1:nrow(flake_sim_data)){
                  flake_sim_data[j,18] <- ifelse(flake_sim_data[j,i-12] == "Wedging"|flake_sim_data[j,i-12] =="Punch" | flake_sim_data[j,i-12] =="Bending",
                                                sample(x = flake_var_list[[i]][2], size = 1, prob = flake_prob_list[[i]][2], replace = TRUE),
                                                            sample(x = flake_var_list[[i]], size = 1, prob = flake_prob_list[[i]], replace = TRUE))}
              }     
            
      else{
        flake_sim_data[,i] <- sample(x = flake_var_list[[i]], size = N/2, prob = flake_prob_list[[i]], replace = TRUE)}
      }
    }
  }
      }
    }
}
}
       }
    }
}
}
}
#### Final dataset #########  
flake_table <- as.data.frame(cbind(round(flake_weight, 2), round(flake_metric_dat, 2), flake_cortex, flake_sim_data))
colnames(flake_table) <- c("Weight", 
                           "Length",
                           "Width",
                           "Thickness",
                           "%Cortex",
                          "Raw material treatment", 
                          "Initiation type",
                          "Lateral edge shape",
                          "Type", 
                          "Flake termination", 
                          "Platform lipping", 
                          "Platform preparation",
                          "Active edge retouch type", 
                          "Inactive edge retouch type",
                          "Retouched face",
                          "Max tang width",
                          "Max tang thickness",
                          "Max tang length", 
                          "Retouch index",
                          "Tranchet removal",
                          "Bulb",
                          "Angle of retouch",
                          "Overhang removal")
rownames(flake_table) <- paste0(seq(1:(N/2)))

write.csv(flake_table, paste0("Assemblage_",a,"_Flakes.csv"))

print(paste0("Simulation ", a, " complete"))
}


