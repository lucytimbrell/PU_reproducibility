library(tidyverse)
df<-read.csv("GWET.csv")
names(df)[7:8]<-c("training", "testing")
names(df)[4]<-"Gwet_AC1"
names(df)[3]<-"Chance_agreement"
df$PU <- factor(df$PU, levels = df$PU)

# ---- LONG FORMAT FOR TRAIN/TEST PLOT --------------------------------
names(df)
df_long <- df %>%
  select(PU, training, testing) %>%
  pivot_longer(cols = c(training, testing),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(Metric = recode(Metric,
                         train_unan = "Training unanimity",
                         test_unan = "Testing unanimity"))


# ---- PLOTS -----------------------------------------------------------

library(ggplot2)
library(patchwork)

p1 <- ggplot(df, aes(PU, Gwet_AC1)) +
  geom_point(size = 2.5, color = "black") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Gwet's AC1",
       title = "Gwet’s AC1 by Procedural Unit")

p1

cb_colors <- c("Training" = "#0072B2",   # Blue
               "Testing"  = "#E69F00")   # Orange

p2 <- ggplot(df_long, aes(PU, Value, color = Metric, group = Metric)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Procedural Unit",
       y = "Unanimity",
       color = "",
       title = "Training vs Testing Unanimity")

p2
# Combine stacked plots
t<-p1 / p2
t

ggsave("unanimity_plot.png",t,dpi=400)
