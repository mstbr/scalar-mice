####
# project: "Replication Tomlinson et al. 2013"
# title: Modelling of pilot data"
# author: "Mathias Stoeber & Timo Roettger"
# date: "13/01/2020"
###

##############
## Prepare ###
##############

# install packages if required
if (!require(data.table)) {install.packages('data.table')}
if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(readxl)) {install.packages('readxl')}
if (!require(brms)) {install.packages('brms')}
if (!require(ggbeeswarm)) {install.packages('ggbeeswarm')}

# load packages
library(data.table)
library(tidyverse)
library(readxl)
library(brms)
library(ggbeeswarm)


# load data
df <- read_csv("derived_data/derivedDF.csv")

df$cluster2 <- as.factor(as.character(df$cluster2))
df$cluster3 <- as.factor(as.character(df$cluster3))
df$prototype <- as.factor(as.character(df$prototype))

df$prototype <- ifelse(df$prototype == "1", 0, 1)

# reduce to what we need (only some critical)
df_red <- df %>%
  filter(timestamps == 0,
         sentence_type == "Some critical")

# scale dependent variable
df_red$AUC_c <- scale(df_red$AUC, scale = TRUE)

# contrast code condition
df_red$Condition_c <- ifelse(df_red$Condition == "Logical", 0.5, -0.5)


############
## Model ###
############

## model AUC to reproduce Spivey et al. (2005) results
# define weakly informative priors
priors_xmdl <- c(
  set_prior("normal(0,2)", class = "b"),
  set_prior("normal(0,2)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)

# model AUC (ignoring clusters)
xmdl_AUC <- brm(AUC_c ~ Condition_c
                + (1  | subject_nr)
                + (1 | stimulus),
                family = "gaussian",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.9999, max_treedepth = 12),
                seed = 999
)


# check model fit
pp_check(xmdl_AUC)
## looks trash because of the weird bumps on the right

# store model objects
save(xmdl_AUC, file = "models/AUC_models.RData")


## model different cluster types with multinomial model

# define weakly informative priors
priors_xmdl <- c(
  set_prior("normal(0,2)", class = "b"),
  set_prior("normal(0,2)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)


# model prototypes
xmdl_clusters <- brm(prototype ~ Condition_c
                     + (1  | subject_nr)
                     + (1 | stimulus),
                family = "bernoulli",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.9999, max_treedepth = 12),
                seed = 999
)

# check model fit
pp_check(xmdl_clusters)

# store model objects
save(xmdl_clusters,  file = "models/clusters_models.RData")

##################################
## Extract posteriors and plot ###
##################################

load(file = "models/AUC_models.RData")
load(file = "models/clusters_models.RData")


# extract posteriors for condition - only model
posteriors_AUC <- posterior_samples(xmdl_AUC) %>%
  mutate(
    delta = b_Condition_c,
    Logical = delta * 0.5,
    Pragmatic = delta * -0.5
  )

parameters = c("delta",
               "Logical",
               "Pragmatic")

# create vectors to store in
lci = uci = mean = name = prob_post = prob_prior = ropeBF <- c()

# specify prior as above
prior = rnorm(100000, mean = 0, sd = 2)

# specify ROPE for BF calculation
ROPE = c(-0.05, 0.05)

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_AUC[[parameters[i]]]),2))
  name <- c(name, parameters[i])

  # loop through parameters and extract amount of values within ROPE for both posterior and prior
  prob_post <- c(prob_post, sum(between(posteriors_AUC[[parameters[i]]], ROPE[1], ROPE[2])) /
               sum(!between(posteriors_AUC[[parameters[i]]], ROPE[1], ROPE[2])))
  prob_prior <- c(prob_prior, sum(between(prior, ROPE[1], ROPE[2])) /
                sum(!between(prior, ROPE[1], ROPE[2])))

}

# wrangle and calculate BF
posteriors_output_cond = data.frame(mean, lci, uci, name, prob_post, prob_prior)
colnames(posteriors_output_cond) <- c("AUC", "lci", "uci", "Condition", "posterior odds", "prior odds")
posteriors_output_cond <- posteriors_output_cond %>%
  mutate(BF = `prior odds` / `posterior odds`)

# aggregate AUC scores for each subject
df_agg <- df_red %>%
  group_by(Condition, subject_nr) %>%
  summarize(AUC = mean(AUC_c, na.rm = TRUE))

# plot
Cond_plot <-
  ggplot(df_agg, aes(x = Condition, y = AUC, color = Condition, fill = Condition)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", color = "grey") +
  geom_quasirandom(data = df_agg, size = 2, alpha = 0.2, width = 0.1) +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 2)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(0.8, 1.2)) +
  geom_errorbar(data = posteriors_output_cond %>% filter(Condition != "delta"),
                aes(x = Condition, ymin = lci, ymax = uci), color = "black", width = .05, size = 1, inherit.aes = FALSE) +
  geom_point(data = posteriors_output_cond %>% filter(Condition != "delta"),
             size = 5, pch = 21, color = "black", inherit.aes = TRUE) +
  labs(title = "Model ignoring clusters",
       subtitle = "semitransparent points are listener averages\n",
       y = "scaled AUC values\n"
  ) +
  scale_colour_manual(values = c("#d01c8b",  "#4dac26"), ) +
  scale_fill_manual(values = c("#d01c8b",  "#4dac26")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))


# extract posteriors for cluster models
# probabilities!
posteriors_clusters_proportion <- posterior_samples(xmdl_clusters) %>%
    mutate(
      Intercept = plogis(b_Intercept),
      Pragmatic = plogis(b_Intercept + (0.5 * b_Condition_c)),
      Logical = plogis(b_Intercept + (-0.5 * b_Condition_c)),
      delta = Logical - Pragmatic
    )

# logodds!
posteriors_clusters_logit <- posterior_samples(xmdl_clusters) %>%
  mutate(
    Intercept = b_Intercept,
    Pragmatic = b_Intercept + (0.5 * b_Condition_c),
    Logical = b_Intercept + (-0.5 * b_Condition_c),
    delta = Logical - Pragmatic
  )


parameters = c("delta",
               "Logical",
               "Pragmatic")

# create vectors to store in
lci = uci = mean = name = prob_post = prob_prior = ropeBF <- c()


# loop through parameters and extract post mean, and CIs in probability space
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_clusters_proportion[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_clusters_proportion[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_clusters_proportion[[parameters[i]]]),2))
  name <- c(name, parameters[i])
}

# wrangle and calculate BF
posteriors_cluster_probs = data.frame(mean, lci, uci, name)
colnames(posteriors_output_cluster) <- c("Proportion", "lci", "uci", "Condition")

# create vectors to store in
lci = uci = mean = name = prob_post = prob_prior = ropeBF <- c()

# specify prior as above
prior = rnorm(100000, mean = 0, sd = 2)

# specify ROPE for BF calculation
ROPE = c(-0.1, 0.1)

# loop through parameters and extract post mean, CI, and BF in logodd space
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_clusters_logit[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_clusters_logit[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_clusters_logit[[parameters[i]]]),2))
  name <- c(name, parameters[i])

  # loop through parameters and extract amount of values within ROPE for both posterior and prior
  prob_post <- c(prob_post, sum(between(posteriors_clusters_logit[[parameters[i]]], ROPE[1], ROPE[2])) /
                   sum(!between(posteriors_clusters_logit[[parameters[i]]], ROPE[1], ROPE[2])))
  prob_prior <- c(prob_prior, sum(between(prior, ROPE[1], ROPE[2])) /
                    sum(!between(prior, ROPE[1], ROPE[2])))

}

# wrangle and calculate BF
posteriors_cluster_logodds = data.frame(mean, lci, uci, name, prob_post, prob_prior)
colnames(posteriors_cluster_logodds) <- c("Proportion", "lci", "uci", "Condition", "posterior odds", "prior odds")
posteriors_cluster_logodds <- posteriors_cluster_logodds %>%
  mutate(BF = `prior odds` / `posterior odds`)

# store posteriors
write.csv(posteriors_output_cond, file = "derived_data/posteriors_output_cond.csv")
write.csv(posteriors_cluster_probs, file = "derived_data/posteriors_cluster_probs.csv")
write.csv(posteriors_cluster_logodds, file = "derived_data/posteriors_cluster_logodds.csv")


