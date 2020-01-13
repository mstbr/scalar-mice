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
if (!require(mousetrap)) {install.packages('brms')}
if (!require(mousetrap)) {install.packages('ggbeeswarm')}
if (!require(mousetrap)) {install.packages('ggpubr')}

# load packages
library(data.table)
library(tidyverse)
library(readxl)
library(brms)
library(ggbeeswarm)
library(ggpubr)


# load data
df <- read_csv("derived_data/derivedDF.csv")

df$cluster2 <- as.factor(as.character(df$cluster2))
df$cluster4 <- as.factor(as.character(df$cluster4))

# reduce to what we need
df_red <- df[df$timestamps == 0,]

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
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)

# model AUC (ignoring clusters)
xmdl_AUC <- brm(AUC_c ~ 1 + Condition_c * sentence_type
                + (Condition_c * sentence_type  | subject_nr)
                + (Condition_c * sentence_type  | item1)
                + (1 | subject_nr:item1),
                family = "gaussian",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.99),
                seed = 999
)


# check model fit
pp_check(xmdl_AUC)
## looks okay but has a significant dent to the left of the mode

# store model objects
save(xmdl_AUC, file = "SGK_ReAnalysis/models/AUC_models.RData")


## model different cluster types with multinomial model

# define weakly informative priors
priors_xmdl <- c(
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)

# check how clusters are distributed across items
xtabs(~cluster + item1, df_red)
# cells are nicely filled

# check how clusters are distributed across subjects
xtabs(~cluster + subject_nr, df_red)
# many empty cells

# model clusters
xmdl_clusters <- brm(cluster ~ 1 + condition_c
                + (1 | subject_nr)
                + (condition_c | item1),
                family = "categorical",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.99),
                seed = 999
)

# store model objects
save(xmdl_clusters,  file = "SGK_ReAnalysis/models/clusters_models.RData")


## model AUC as a function of cluster x condition
# define weakly informative priors
priors_xmdl <- c(
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)


# model AUC x clusters
xmdl_AUC_cluster <- brm(AUC_c ~ 1 + condition_c * cluster
                + (condition_c | subject_nr)
                + (condition_c * cluster | item1)
                + (1 | subject_nr:item1),
                family = "gaussian",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.99),
                seed = 999
)


# check model fit
pp_check(xmdl_AUC_cluster)
## has the remaining dent which might be due to unexplained variance due to subject-specific movement behavior

# store model objects
save(xmdl_AUC_cluster, file = "SGK_ReAnalysis/models/AUC_clusters_models.RData")


##################################
## Extract posteriors and plot ###
##################################

#load(file = "SGK_ReAnalysis/models/AUC_clusters_models.RData")
load(file = "SGK_ReAnalysis/models/AUC_models.RData")
load(file = "SGK_ReAnalysis/models/AUC_clusters_models.RData")
load(file = "SGK_ReAnalysis/models/clusters_models.RData")


# extract posteriors for condition - only model
posteriors_AUC <- posterior_samples(xmdl_AUC) %>%
  mutate(
    delta = b_condition_c,
  )

parameters = c("delta")

# create vectors to store in
name <- c()
lci <- c()
uci <- c()
mean <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_AUC[[parameters[i]]]),2))
  name <- c(name, parameters[i])
}

posteriors_output_cond = data.frame(mean, lci, uci, name)
colnames(posteriors_output_cond) <- c("AUC", "lci", "uci", "name")

# aggregate AUC differences for each subject
df_agg <- df_red %>%
  group_by(condition, subject_nr) %>%
  summarize(AUC = mean(AUC_c, na.rm = TRUE)) %>%
  spread(condition, AUC) %>%
  mutate(AUC = cohort - control)

# plot
Cond_plot <-
  ggplot(df_agg, aes(x = 1, y = AUC)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", color = "black") +
  geom_quasirandom(size = 2, alpha = 0.5, width = 0.1, color = "grey") +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 2.5)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.8, 1.2)) +
  geom_errorbar(data = posteriors_output_cond, aes(ymin = lci, ymax = uci), color = "black", width = .05) +
  geom_point(data = posteriors_output_cond, size = 5, pch = 21, color = "black", fill = "black") +
  geom_text(x = 2.5, y = -1.5, color = "grey", label = "control > cohort",size = 6) +
  geom_text(x = 2.5, y = 2, color = "grey", label = "cohort > control",size = 6) +
  labs(title = "(a) Overall",
       y = "scaled AUC difference\n between cohort and control\n"
  ) +
  scale_colour_manual(values = c("#a1dab4", "#41b6c4", "#225ea8", "#253494")) +
  scale_fill_manual(values = c("#a1dab4", "#41b6c4", "#225ea8", "#253494")) +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))


# extract posteriors for cluster models
posteriors_clusters <- posterior_samples(xmdl_clusters) %>%
  mutate(
    # probabilities!
    curved_base = 1 / (1 + exp(b_mustraight_Intercept) + exp(b_mudcom_Intercept)),
    curved_control = 1 / (1 + exp(b_mustraight_Intercept + b_mustraight_Intercept * -.5) +
                              exp(b_mudcom_Intercept + b_mudcom_condition_c * -.5)),
    curved_cohort = 1 / (1 + exp(b_mustraight_Intercept + b_mustraight_condition_c * .5) +
                              exp(b_mudcom_Intercept + b_mudcom_condition_c * .5)),

    straight_base = curved_base * (exp(b_mustraight_Intercept)),
    straight_control = curved_control * (exp(b_mustraight_Intercept + b_mustraight_Intercept * -.5)),
    straight_cohort = curved_cohort * (exp(b_mustraight_Intercept + b_mustraight_Intercept * .5)),

    dcom_base = curved_base * (exp(b_mudcom_Intercept)),
    dcom_control = curved_base * (exp(b_mudcom_Intercept + b_mudcom_condition_c * -.5)),
    dcom_cohort = curved_base * (exp(b_mudcom_Intercept + b_mudcom_condition_c * .5)),

    delta_curved = curved_control - curved_cohort,
    delta_straight = straight_control - straight_cohort,
    delta_dcom = dcom_control - dcom_cohort

  )

parameters = c("curved_control", "curved_cohort",
               "straight_control", "straight_cohort",
               "dcom_control", "dcom_cohort",
               "delta_curved", "delta_straight", "delta_dcom")

# create vectors to store in
name <- c()
lci <- c()
uci <- c()
mean <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_clusters[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_clusters[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_clusters[[parameters[i]]]),2))
  name <- c(name, parameters[i])
}

posteriors_output_clusters = data.frame(mean, lci, uci, name)
colnames(posteriors_output_clusters) <- c("proportion", "lci", "uci", "name")
posteriors_output_clusters$type = c(rep("estimate", 6), rep("delta", 3))
posteriors_output_clusters$condition = c(rep(c("control", "cohort"), 3), rep(NA, 3))
posteriors_output_clusters$cluster = c(rep(c("curved", "straight", "dcom"), each = 2),
                                       c("curved", "straight", "dcom"))



# extract posteriors for condition x cluster model
posteriors_AUC_clusters <- posterior_samples(xmdl_AUC_cluster) %>%
  mutate(
    curved = b_Intercept,
    straight = b_Intercept + b_clusterstraight,
    dcom = b_Intercept + b_clusterdcom,

    control_curved = curved + (b_condition_c * -0.5),
    cohort_curved = curved + (b_condition_c * 0.5),

    control_straight = straight + (b_condition_c * -0.5) + (`b_condition_c:clusterstraight` * -0.5),
    cohort_straight  = straight + (b_condition_c * 0.5) + (`b_condition_c:clusterstraight` * 0.5),

    control_dcom = dcom + (b_condition_c * -0.5) + (`b_condition_c:clusterdcom` * -0.5),
    cohort_dcom = dcom + (b_condition_c * 0.5) + (`b_condition_c:clusterdcom` * 0.5),

    delta_straight = b_condition_c,
    delta_curved = b_condition_c +  `b_condition_c:clusterstraight`,
    delta_dcom = b_condition_c + `b_condition_c:clusterdcom`
  )


##########

# BF attempt
library(bayestestR)
library(logspline)
posteriors_AUC_clusters

bayesfactor_parameters(posteriors_AUC$delta,
                       prior = distribution_normal(length(posteriors_AUC$delta), 0, .1),
                       direction = "two-sided", null = 0, verbose = TRUE)

library(simr)
library(lme4)
xmdl = lmer(AUC_c ~ 1 + condition_c * cluster
            + (condition_c | subject_nr)
            + (condition_c * cluster | item1), data = df_red)

powerSim(xmdl, nsim = 20, test = fixed("condition_c"))

##########

parameters = c("delta_straight", "delta_curved", "delta_dcom")

# create vectors to store in
name <- c()
lci <- c()
uci <- c()
mean <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_AUC_clusters[[parameters[i]]]))[1],3))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_AUC_clusters[[parameters[i]]]))[2],3))
  mean <- c(mean, round(mean(posteriors_AUC_clusters[[parameters[i]]]),3))
  name <- c(name, parameters[i])
}

posteriors_output_condclusters = data.frame(mean, lci, uci, name)
colnames(posteriors_output_condclusters) <- c("AUC", "lci", "uci", "name")
posteriors_output_condclusters$cluster = c("curved", "straight", "dcom")


# plot results

# aggregate AUC differences for each subject
df_agg_cluster <- df_red %>%
  group_by(condition, subject_nr, cluster) %>%
  summarize(AUC = mean(AUC_c, na.rm = TRUE)) %>%
  spread(condition, AUC) %>%
  mutate(AUC = cohort - control)


# plot
CondClusters_plot <-
ggplot(df_agg_cluster, aes(x = cluster, y = AUC)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", color = "black") +
  geom_quasirandom(size = 2, alpha = 0.5, , width = 0.1, color = "grey") +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 2.5)) +
  geom_errorbar(data = posteriors_output_condclusters, aes(ymin = lci, ymax = uci), color = "black", width = .1) +
  geom_point(data = posteriors_output_condclusters, size = 5, pch = 21, color = "black", fill = "black") +
  annotate("text", x = 2, y = -1.5, color = "grey", label = "smaller AUC in cohort condition") +
  annotate("text", x = 2, y = 2, color = "grey", label = "greater AUC in cohort condition") +
  labs(title = "(b) Cluster-dependent effects",
       subtitle = "semitransparent points are listener averages\n",
       x = "\ntrajectory cluster",
       y = ""
  ) +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))


## generate combined plot
uber_estimates <-
  #quartz()
  ggarrange(Cond_plot, CondClusters_plot,
            heights = c(1,1),
            widths = c(0.4,1),
            font.label = list(size = 28),
            align = "h",
            ncol = 2, nrow = 1,
            common.legend = F)

# store plots
ggsave(filename = "SGK_ReAnalysis/plots/uber_estimates.pdf",
       plot = uber_estimates,
       device = "pdf",
       width = 235,
       height = 150,
       units = "mm",
       dpi = 300)


# store posteriors
write.csv(posteriors_output_cond, file = "SGK_ReAnalysis/derived_data/posteriors_output_cond.csv")
write.csv(posteriors_output_clusters, file = "SGK_ReAnalysis/derived_data/posteriors_output_cluster.csv")
write.csv(posteriors_output_condclusters, file = "SGK_ReAnalysis/derived_data/posteriors_output_condclusters.csv")

