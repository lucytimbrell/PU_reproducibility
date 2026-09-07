library(here)
setwd(paste(here::here(),"/June_2026_workflow",sep="",collapse=""))

library(cmdstanr)
library(brms)


# Import all datasets
long_data<-read.csv("model_input_nov.csv")
length(long_data$X)

#We want to ignore cases where there is no procedural unit present, and it was not judged as present by the coder.
#Note the november model has not had hte TNS removed, double check if thats a massive issue. 
#For AIC/BIC contrast the current version with the TNS against the old version with the TNS. 
#Later run both without the TNS to make sure we are kosher 
#long_data<-long_data[-which(long_data$TN==1),]
length(long_data$X)

pu_model <- brm(
  formula = Deviation ~ Dataset + Lithics_experience_yrs + Days_between_training_testing + Continent_training+
    (1 + Dataset | PU) +  # Random intercepts and slopes for Dataset
    (1 | Assemblage) + (1 | Observer),
  data = long_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),         # Regularizing fixed effects
    prior(exponential(1), class = "sd")       # Regularizing random effects
  ),
  cores = 4,
  chains = 4,
  seed = 123,
  iter=2000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  backend = "cmdstanr"
)

summary(pu_model)
saveRDS(pu_model, "deviation_model_bernoulli_Jun_2026.RDS")

prior_model <- brm(
  formula = Deviation ~ Dataset + Lithics_experience_yrs + Days_between_training_testing + Continent_training+
    (1 + Dataset | PU) +  # Random intercepts and slopes for Dataset
    (1 | Assemblage) + (1 | Observer),
  data = long_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1), class = "b"),         # Regularizing fixed effects
    prior(exponential(1), class = "sd")       # Regularizing random effects
  ),
  sample_prior = "only",  # Key for prior predictive checks
  cores = 4,              # Reduce to 1 core for simplicity
  chains = 4,             
  iter = 2000,            
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  backend = "cmdstanr"
)

saveRDS(prior_model, "priors_deviation_model_bernoulli_Jun_2026.RDS")

