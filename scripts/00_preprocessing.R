####
# project: "Replication Tomlinson et al. 2013"
# title: Preprocessing of OpenSesame data"
# author: "Mathias Stoeber & Timo Roettger"
# date: "13/01/2020"
###


# Initialise --------------------------------------------------------------

if (!require(readbulk)) {install.packages('readbulk')}
if (!require(mousetrap)) {install.packages('mousetrap')}
if (!require(tidyverse)) {install.packages('tidyverse')}

# Load packages
library(readbulk)
library(mousetrap)
library(tidyverse)

# Read multiple data files (from raw OpenSesame output)
raw_data <-
  readbulk::read_opensesame(directory = "raw_data/Pilots 1-3/pilot_03/", extension = ".csv")

# Preprocessing on the raw data -------------------------------------------

# Exclude all trials from the training block
raw_data <- raw_data %>%
  filter(!is.na(count_trials_test))

# convert the relevant `initiation_time`` column to type `numeric`
raw_data$initiation_time_tracking2_test <- as.numeric(raw_data$initiation_time_tracking2_test)

# Insert column into raw_data indicating group membership
raw_data$Condition[
  raw_data$experiment_file == "false_trueleft.osexp" |
    raw_data$experiment_file == "false_trueright.osexp"] <- "Pragmatic"

raw_data$Condition[
  raw_data$experiment_file == "true_trueleft.osexp" |
    raw_data$experiment_file == "true_trueright.osexp"] <- "Logical"

# Record some metadata
n_before <- nrow(raw_data)
n_wrong_responses <- nrow(filter(raw_data, correct == 0))
n_correct_responses <- nrow(filter(raw_data, correct == 1))
n_slow_responses <-
  nrow(filter(raw_data, initiation_time_tracking2_test > 200))
n_swift_responses <-
  nrow(filter(raw_data, initiation_time_tracking2_test < 200))


# Trial exclusion ---------------------------------------------------------

stats_errors <- raw_data %>%
  filter(correct == 0) %>%
  group_by(
    #subject_nr,
    Condition) %>%
  summarise(err_count = n(),
            err_percent = round((n() / n_before) * 100, digits = 2))

stats_slow <- raw_data %>%
  filter(initiation_time_tracking2_test > 200) %>%
  group_by(
    #subject_nr,
    Condition) %>%
  summarise(slow_count = n(),
            slow_percent = round((n() / n_before) * 100, digits = 2))

# Exclude wrong responses
raw_data <- raw_data %>% filter(correct == 1)

# Exclude slow responses
raw_data <-
  raw_data %>% filter(initiation_time_tracking2_test <= 200)



# Create mousetrap object -------------------------------------------------

mtdata <- mt_import_mousetrap(
  raw_data,
  xpos_label = c("xpos_tracking1_test",
                 "xpos_tracking2_test"),
  ypos_label = c("ypos_tracking1_test",
                 "ypos_tracking2_test"),
  timestamps_label = c("timestamps_tracking1_test",
                       "timestamps_tracking2_test")
  )


# Removal of 'early movers' -----------------------------------------------

# If the first sample of a trajectory has already left the start button (plus some additional area), exclude the trial.

# The start button: 64*64
# x: -32 to 32
# ytop: 640
#
# The exclusion box: 96*96
# x: -48 to 48
# ytop: 624

# Export & exclude early movers
temp <- mt_export_long(mtdata, use2_variables = colnames(mtdata$data)) %>%
  group_by(mt_id) %>%
  filter(!any(mt_seq == 1 && (ypos < 624 | xpos < -48 | xpos > 48)))

# Re-import into a mousetrap object
mtdata <- mt_import_long(temp)


# Preprocessing on the mousetrap ------------------------------------------

mtdata <- mt_remap_symmetric(mtdata)
mtdata <- mt_align_start(mtdata)
mtdata <- mt_derivatives(mtdata)
mtdata <- mt_measures(mtdata)
mtdata <- mt_time_normalize(mtdata)
mtdata <- mt_spatialize(mtdata)
mtdata <- mt_derivatives(mtdata, use = "tn_trajectories", save_as = "tn_trajectories")

# Sampling resolution check -----------------------------------------------

res_check <- mt_check_resolution(mtdata, desired = 10)
print(res_check$summary)
print(res_check$relative_frequencies_desired)


# Custom prototype matching -----------------------------------------------

prototypes <- read_csv("derived_data/prototypes.csv")
prototypes <- mt_import_long(prototypes)
prototypes <- prototypes$trajectories
# mt_plot(prototypes, color = "mt_id")
mtdata <- mt_map(mtdata, use = "sp_trajectories", save_as = "prototyping", prototypes = prototypes)
# mtdata$prototyping

# Cluster -----------------------------------------------

# estimtate clusters with hclust (takes 30 seconds on my machine)
k_hclust <- mt_cluster_k(
  mtdata,
  use = "tn_trajectories",
  # range of k to consider
  kseq = 2:15,
  # which of the 4 measures to compute
  compute = c("stability", "gap", "jump", "slope"),
  # use hierarchical clustering ("hclust") or k-means clustering ("kmeans")
  method = "hclust"
)

# output results of clustering
bind_rows(unlist(k_hclust$kopt)) %>%
  mutate(method = "hclust") %>%
  select(method, everything())

# gap and jump are useless (again)
# stab (2) and slope (3) yield different results

# we proceed with 2 to 4 clusters

# clusters "space normalized"
mtdata <- mt_cluster(mtdata,
                     use = "sp_trajectories",
                     n_cluster = 2, method = "hclust", save_as = "clustering2")
mtdata <- mt_cluster(mtdata,
                     use = "sp_trajectories",
                     n_cluster = 3, method = "hclust", save_as = "clustering3")
mtdata <- mt_cluster(mtdata,
                     use = "sp_trajectories",
                     n_cluster = 4, method = "hclust", save_as = "clustering4")

mtdata$clustering2$cluster2 <- mtdata$clustering2$cluster
mtdata$clustering2$cluster3 <- mtdata$clustering3$cluster
mtdata$clustering4$cluster4 <- mtdata$clustering4$cluster

# add everything into one data frame
df <- mtdata$data %>%
  full_join(mtdata$measures, by = "mt_id") %>%
  full_join(mtdata$clustering2, by = "mt_id") %>%
  full_join(mtdata$clustering3, by = "mt_id") %>%
  full_join(mtdata$prototyping, by = "mt_id")

# convert array tn_trajectories to data_frame
df_tn <- as.data.frame.table(mtdata$tn_trajectories) %>%
  spread(key = Var3, value = Freq) %>%
  select(-Var2) %>%
  rename("mt_id" = Var1)

df_sp <- as.data.frame.table(mtdata$sp_trajectories) %>%
  spread(key = Var3, value = Freq) %>%
  select(-Var2) %>%
  rename("xpos_sp" = xpos,
         "ypos_sp" = ypos) %>%
  rename("mt_id" = Var1)

df_all_untrimmed <- full_join(df, df_tn, by = "mt_id") %>%
  full_join(df_sp)

# restrict to relevant columns
df_all <- df_all_untrimmed %>%
  select(sentence_type, Condition, subject_nr, stimulus,
         cluster2, cluster3, cluster4, mt_id, prototype,
         timestamps, steps, AUC, xpos, ypos, xpos_sp, ypos_sp
         )

# write into derived_data
write_csv(df_all, "derived_data/derivedDF.csv")

