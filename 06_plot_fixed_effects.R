library(here)
setwd(paste(here::here(),"/Nov_2025_workflow",sep="",collapse=""))
library(lme4)
library(dplyr)
library(tidyverse)
library(tidybayes)
library(posterior)

long_data<-read.csv("model_input_nov.csv")
m <- readRDS("deviation_model_bernoulli_dec.RDS")


setwd(paste(here::here(),"/Nov_2025_workflow/Figures",sep="",collapse=""))



# Get population-level intercept
pop_int <- as_draws_df(m) %>% 
  select(b_Intercept) %>% 
  mutate(Intercept = exp(b_Intercept))  # Convert to count scale

pop_int
# Plot
ggplot(pop_int, aes(x = Intercept)) +
  geom_density(alpha = 0.7) +
  labs(title = "Overall Population-Level Estimate",
       x = "Expected Deviation probability",
       y = "Posterior Density") +
  theme_minimal()



### What is the effect of training on deviation across all procedural units, assemblages, and observers?
### 

# Create reference data frame
ref_data <- data.frame(
  Lithics_experience_yrs = 15,
  Dataset = c("Training","Testing"),  # Most frequent level
  Assemblage = NA,  # Will be marginalized
  Observer = NA     # Will be marginalized
)

# Add PU levels to reference data
ref_data <- expand_grid(
  ref_data,
  PU = unique(long_data$PU)
)


hist(long_data$Lithics_experience_yrs)
ref_data# Plot PU-specific posteriors

# Generate predictions for each PU
pu_predictions <- m %>%
  add_epred_draws(
    newdata = ref_data,
    re_formula = ~(1 + Dataset | PU),  # Include only PU random effects
  )

pu_predictions
pp<-ggplot(pu_predictions, aes(x = .epred, fill = Dataset)) +
  geom_density(alpha=.5) +
  labs(x = "Expected Deviation Count",
       y = "Posterior Density") +
  theme_minimal()
pp
###Plot population 

ggsave("pop_effect_training_on_deviation_ppt_nov.tiff",plot=pp,
       width=24,
       height=16, 
       units="cm")

ggsave("pop_effect_training_on_deviation_nov.tiff",plot=pp,
       width=30,
       height=20, 
       units="cm")


ggsave("pop_effect_training_on_deviation_ppt_nov.png",plot=pp,
       width=24,
       height=16, 
       units="cm")

ggsave("pop_effect_training_on_deviation_nov.png",plot=pp,
       width=30,
       height=20, 
       units="cm")




####
#### What is effect of training on each PU code? 


library(ggridges)


pu_order <- pu_predictions %>%
  group_by(PU) %>%
  summarize(median_prob = median(.epred)) %>%
  arrange(median_prob) %>%
  pull(PU)

pu_predictions <- pu_predictions %>%
  mutate(PU = factor(PU, levels = pu_order))

library(ggridges)
library(viridis)

# Create ordered factor for PUs based on median deviation probability
pu_order <- pu_predictions %>%
  group_by(PU) %>%
  summarize(median_prob = median(.epred)) %>%
  arrange(median_prob) %>%
  pull(PU)

pu_predictions <- pu_predictions %>%
  mutate(PU = factor(PU, levels = pu_order))

# Create ridgeline plot
pr<-ggplot(pu_predictions, 
           aes(x = .epred, y = PU, 
               fill = Dataset, height = after_stat(density))) +
  geom_density_ridges(
    alpha = 0.7, 
    scale = 1,
    rel_min_height = 0.001,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  scale_fill_viridis_d(
    option = "magma", 
    begin = 0.3, 
    end = 0.7
  ) +
  labs(
    title = "PU-Specific Deviation Probabilities",
    subtitle = "Holding Lithics experience at 15 years",
    x = "Expected Deviation Probability",
    y = "Procedural Unit (PU)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

pr
### Print the ridgeline plot. p2. 

ggsave("Effect_of_training_on_pu_error_ppt_dev_nov.tiff",plot=pr,
       width=24,
       height=16, 
       units="cm")

ggsave("Effect_of_training_on_pu_error_nov.tiff",plot=pr,
       width=30,
       height=20, 
       units="cm")


ggsave("Effect_of_training_on_pu_error_ppt_nov.png",plot=pr,
       width=24,
       height=16, 
       units="cm")

ggsave("Effect_of_training_on_pu_error_nov.png",plot=pr,
       width=30,
       height=20, 
       units="cm")



### Explore effects of training on overall error in coding PU as present. 

# Create reference data with both conditions
ref_exp_cond <- expand_grid(
  Lithics_experience_yrs = seq(
    min(long_data$Lithics_experience_yrs),
    max(long_data$Lithics_experience_yrs),
    length.out = 50
  ),
  Dataset = c("Training", "Testing"),
  Assemblage = NA,
  Observer = NA,
  PU = NA
)

# Get predictions
exp_cond_predictions <- m %>%
  add_epred_draws(
    newdata = ref_exp_cond,
    re_formula = NA
  ) %>%
  group_by(Lithics_experience_yrs, Dataset) %>%
  summarize(
    median = median(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975)
  )

# Plot
pe<-ggplot(exp_cond_predictions, aes(x = Lithics_experience_yrs, color = Dataset, fill = Dataset)) +
  # Posterior median lines
  geom_line(aes(y = median), size = 1.2) +
  # Credible intervals
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  # Raw data points
  # geom_jitter(
  #    data = long_data,
  #    aes(y = Deviation, shape = Dataset),
  #    width = 0.3, height = 0.02, alpha = 0.1
  #  ) +
  labs(
    title = "Experience Effect by Condition",
    x = "Years of Lithics Experience",
    y = "Deviation Probability"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

pe
ggsave("Experience_effect_by_condition_ppt_nov.tiff",plot=pe,
       width=12,
       height=8, 
       units="cm")

ggsave("Experience_effect_by_condition_nov.tiff",plot=pe,
       width=24,
       height=16, 
       units="cm")


ggsave("Experience_effect_by_condition_ppt_nov.png",plot=pe,
       width=12,
       height=8, 
       units="cm")

ggsave("Experience_effect_by_condition_nov.png",plot=pe,
       width=24,
       height=16, 
       units="cm")
