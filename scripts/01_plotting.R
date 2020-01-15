####
# project: "Replication Tomlinson et al. 2013"
# title: Plotting of pilot data"
# author: "Mathias Stoeber & Timo Roettger"
# date: "13/01/2020"
###


## Prepare

# load packages

if (!require(data.table)) {install.packages('data.table')}
if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(readxl)) {install.packages('readxl')}
if (!require(mousetrap)) {install.packages('ggpubr')}

library(data.table)
library(tidyverse)
library(readxl)
library(ggpubr)

# load data
df <- read_csv("derived_data/derivedDF.csv")
df_red <- df %>%
  filter(sentence_type == "Some critical")

df_red$cluster2 <- as.factor(as.character(df_red$cluster2))
df_red$cluster4 <- as.factor(as.character(df_red$cluster4))

levels(df_red$cluster2) <- c("cluster1", "cluster2")
df_red$cluster2 <- factor(df_red$cluster2, levels = c("cluster1", "cluster2"))

levels(df_red$cluster4) <- c("cluster1", "cluster2", "cluster3", "cluster4")
df_red$cluster4 <- factor(df_red$cluster4, levels = c("cluster1", "cluster2", "cluster3", "cluster4"))


# aggregate data overall (ignore clusters)
df_agg <- df_red %>%
  group_by(Condition, sentence_type, subject_nr, steps) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T)
            ) %>%
  group_by(Condition, sentence_type, steps) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T)
            )

# plot sentence type x correct response
agg_plot <-
ggplot(df_agg) +
  geom_segment(x = 0, xend = 0, y = -Inf, yend = +Inf, lty = "dashed", color = "grey") +
  geom_point(aes(-xpos, ypos, color = Condition), size = 3, alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), limits = c(-100, 1450)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1350, 1450)) +
  labs(title = "(a) aggregated trajectories",
       subtitle = "flipped and time-normalized\n",
       x = "\nhorizontal position",
       y = "vertical position\n"
  ) +
  scale_colour_manual(values = c("#d01c8b",  "#4dac26"), ) +
  scale_fill_manual(values = c("#d01c8b",  "#4dac26")) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "grey"),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))

# aggregate data over clusters2
df_agg_clusters2 <- df_red %>%
  group_by(Condition, subject_nr, steps, cluster2) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T)) %>%
  group_by(Condition, steps, cluster2) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T))

# aggregate data over clusters3
df_agg_clusters3 <- df_red %>%
  group_by(Condition, subject_nr, steps, cluster3) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T)) %>%
  group_by(Condition, steps, cluster3) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T))

# aggregate data over clusters4
df_agg_clusters4 <- df_red %>%
  group_by(Condition, subject_nr, steps, cluster4) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T)) %>%
  group_by(Condition, steps, cluster4) %>%
  summarize(xpos = mean(xpos, na.rm = T),
            ypos = mean(ypos, na.rm = T),
            timestamps = mean(timestamps, na.rm = T))

cluster_percentages2 <- df_red %>%
  group_by(
    Condition, sentence_type,
    cluster2) %>%
  summarise(n = n()) %>%
  mutate(Percent = paste(round(100 * n / sum(n)), "%", sep = ""))

cluster_percentages3 <- df_red %>%
  group_by(
    Condition, sentence_type,
    cluster3) %>%
  summarise(n = n()) %>%
  mutate(Percent = paste(round(100 * n / sum(n)), "%", sep = ""))

cluster_percentages4 <- df_red %>%
  group_by(
    Condition, sentence_type,
    cluster4) %>%
  summarise(n = n()) %>%
  mutate(Percent = paste(round(100 * n / sum(n)), "%", sep = ""))


# plot cluster2
agg_plot_cluster2 <-
  ggplot(df_agg_clusters2) +
  geom_path(data = df_red, aes(-xpos, ypos, color = Condition, group = mt_id), size = 0.5, alpha = 0.05) +
  geom_rect(xmin = -1.4, xmax = -0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5, lty = "dashed") +
  geom_rect(xmin = 1.4, xmax = 0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5) +
  geom_point(aes(-xpos, ypos, color = Condition), size = 2, alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), limits = c(-200, 1650)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1850, 1850)) +
  geom_text(data = cluster_percentages2[cluster_percentages2$Condition == "Logical",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 1000, size = 6) +
  geom_text(data = cluster_percentages2[cluster_percentages2$Condition == "Pragmatic",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 800, size = 6) +
  labs(title = "(b) cluster-specific trajectories",
       subtitle = "% indicate amount of clusters per group\n",
       x = "\nhorizontal position",
       y = "vertical position\n",
       color = "Condition"
  ) +
  scale_colour_manual(values = c("#d01c8b",  "#4dac26"), ) +
  scale_fill_manual(values = c("#d01c8b",  "#4dac26")) +
  facet_grid(~ cluster2) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))

# comment TR: Does not capture the variance at all in cluster 2
# Let's try three clusters

# plot cluster3
agg_plot_cluster3 <-
  ggplot(df_agg_clusters3) +
  geom_path(data = df_red, aes(-xpos, ypos, color = Condition, group = mt_id), size = 0.5, alpha = 0.05) +
  geom_rect(xmin = -1.4, xmax = -0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5, lty = "dashed") +
  geom_rect(xmin = 1.4, xmax = 0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5) +
  geom_point(aes(-xpos, ypos, color = Condition), size = 2, alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), limits = c(-200, 1650)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1850, 1850)) +
  geom_text(data = cluster_percentages3[cluster_percentages3$Condition == "Logical",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 1000, size = 6) +
  geom_text(data = cluster_percentages3[cluster_percentages3$Condition == "Pragmatic",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 800, size = 6) +
  labs(title = "(b) cluster-specific trajectories",
       subtitle = "% indicate amount of clusters per group\n",
       x = "\nhorizontal position",
       y = "vertical position\n",
       color = "Condition"
  ) +
  scale_colour_manual(values = c("#d01c8b",  "#4dac26"), ) +
  scale_fill_manual(values = c("#d01c8b",  "#4dac26")) +
  facet_grid(~ cluster3) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))

# Still a little weird, let's try 4

# plot cluster4
agg_plot_cluster4 <-
  ggplot(df_agg_clusters4) +
  geom_path(data = df_red, aes(-xpos, ypos, color = Condition, group = mt_id), size = 0.5, alpha = 0.05) +
  geom_rect(xmin = -1.4, xmax = -0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5, lty = "dashed") +
  geom_rect(xmin = 1.4, xmax = 0.8, ymin = 1.25, ymax = 1.85,
            color = "grey", fill = NA, alpha = 0.5) +
  geom_point(aes(-xpos, ypos, color = Condition), size = 2, alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), limits = c(-200, 1650)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1850, 1850)) +
  geom_text(data = cluster_percentages4[cluster_percentages4$Condition == "Logical",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 1000, size = 6) +
  geom_text(data = cluster_percentages4[cluster_percentages4$Condition == "Pragmatic",],
            aes(color = Condition, label = Percent),
            x = -1300, y = 800, size = 6) +
  labs(title = "(b) cluster-specific trajectories",
       subtitle = "% indicate amount of clusters per group\n",
       x = "\nhorizontal position",
       y = "vertical position\n",
       color = "Condition"
  ) +
  scale_colour_manual(values = c("#d01c8b",  "#4dac26"), ) +
  scale_fill_manual(values = c("#d01c8b",  "#4dac26")) +
  facet_grid(~ cluster4) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16, face = "bold", color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))


# store plots
ggsave(filename = "plots/agg_plot.png",
       plot = agg_plot,
       device = "png",
       width = 240,
       height = 170,
       units = "mm",
       dpi = 300)

ggsave(filename = "plots/agg_plot_cluster2.png",
       plot = agg_plot_cluster2,
       device = "png",
       width = 240,
       height = 170,
       units = "mm",
       dpi = 300)

ggsave(filename = "plots/agg_plot_cluster3.png",
       plot = agg_plot_cluster3,
       device = "png",
       width = 295,
       height = 170,
       units = "mm",
       dpi = 300)

ggsave(filename = "plots/agg_plot_cluster4.png",
       plot = agg_plot_cluster4,
       device = "png",
       width = 350,
       height = 170,
       units = "mm",
       dpi = 300)
