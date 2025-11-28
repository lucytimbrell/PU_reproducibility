library(here)
setwd(paste(here::here(),"/Nov_2025_workflow",sep="",collapse=""))

library(lme4)
library(dplyr)
library(tidyverse)


# Import all datasets
long_data<-read.csv("model_input.csv")


# Assuming long_data already has Presence, True_Presence, FP, FN, Deviation
setwd(paste(here::here(),"/Nov_2025_workflow/Tables",sep="",collapse=""))


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)  


# 2. Pivot longer on the four judgment types
summary_df <- long_data %>%
  select(Dataset, TP, FP, FN, TN) %>%
  pivot_longer(TP:TN, names_to = "Judgment", values_to = "Count") %>%
  group_by(Dataset, Judgment) %>%
  summarise(
    n = sum(Count),
    total = n_distinct(row_number()[Dataset == first(Dataset)]),  # total observations per dataset
    .groups = "drop"
  ) %>%
  mutate(
    Percent = n / sum(n) * 100
  )

# Inspect the table
print(summary_df)
write.csv(summary_df,"judgement_values_nov.csv")


# Create summary table with percentages that sum to 100% within each Dataset
summary_df <- long_data %>%
  select(Dataset, TP, FP, FN, TN) %>%
  pivot_longer(cols = c(TP, FP, FN, TN), names_to = "Judgment", values_to = "Count") %>%
  group_by(Dataset, Judgment) %>%
  summarise(n = sum(Count), .groups = "drop_last") %>%
  mutate(
    total = sum(n),  # total within each Dataset
    Percent = (n / total) * 100
  ) %>%
  ungroup()

summary_df
# 3. Barplot of percentage by judgment type, faceted by dataset
ggplot(summary_df, aes(x = Judgment, y = Percent, fill=Dataset)) +
  geom_col(position="dodge") +
  #facet_wrap(~ Dataset) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Observer Judgment Outcomes by Dataset",
    x = "Judgment Type",
    y = "Percentage of Judgments"
  ) +
  theme_minimal(base_size = 14)


head(long_data)

#training effect by PU familiarity
# Create summary table with percentages that sum to 100% within each Dataset
summary_df <- long_data %>%
  select(PU_familiarity, TP, FP, FN, TN) %>%
  pivot_longer(cols = c(TP, FP, FN, TN), names_to = "Judgment", values_to = "Count") %>%
  group_by(PU_familiarity, Judgment) %>%
  summarise(n = sum(Count), .groups = "drop_last") %>%
  mutate(
    total = sum(n),  # total within each Dataset
    Percent = (n / total) * 100
  ) %>%
  ungroup()

# 3. Barplot of percentage by judgment type, faceted by dataset
ggplot(summary_df, aes(x = Judgment, y = Percent, fill=PU_familiarity)) +
  geom_col(position="dodge") +
  #facet_wrap(~ Dataset) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Observer Judgment Outcomes by Dataset",
    x = "Judgment Type",
    y = "Percentage of Judgments"
  ) +
  theme_minimal(base_size = 14)








#training effect by PU familiarity
# Create summary table with percentages that sum to 100% within each Dataset
summary_df <- long_data %>%
  select(PU_familiarity, TP, FP, FN, TN) %>%
  pivot_longer(cols = c(TP, FP, FN, TN), names_to = "Judgment", values_to = "Count") %>%
  group_by(PU_familiarity, Judgment) %>%
  summarise(n = sum(Count), .groups = "drop_last") %>%
  mutate(
    total = sum(n),  # total within each Dataset
    Percent = (n / total) * 100
  ) %>%
  ungroup()

# 3. Barplot of percentage by judgment type, faceted by dataset
ggplot(summary_df, aes(x = Judgment, y = Percent, fill=PU_familiarity)) +
  geom_col(position="dodge") +
  #facet_wrap(~ Dataset) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "Observer Judgment Outcomes by Dataset",
    x = "Judgment Type",
    y = "Percentage of Judgments"
  ) +
  theme_minimal(base_size = 14)

