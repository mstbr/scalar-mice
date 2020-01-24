####
# project: "Replication Tomlinson et al. 2013"
# title: Modelling of pilot data"
# author: "Mathias Stoeber & Timo Roettger"
# date: "13/01/2020"
###

##############
## Prepare ###
##############

### TR: NOT YET translated (original from Spivey Replication)

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
df$cluster3 <- as.factor(as.character(df$cluster3))
df$prototype <- as.factor(as.character(df$prototype))

# reduce to what we need (only critical)
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
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)

# model AUC (ignoring clusters)
xmdl_AUC <- brm(AUC_c ~ 1 + Condition_c
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
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)

# check how clusters are distributed across subjects
xtabs(~cluster2 + subject_nr, df_red)
xtabs(~cluster3 + subject_nr, df_red)
# many empty cells, so no slopes

# model clusters
xmdl_clusters <- brm(cluster2 ~ 1 + Condition_c
                     + (1  | subject_nr)
                     + (1 | stimulus),
                family = "bernoulli",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.9999, max_treedepth = 12),
                seed = 999
)

# store model objects
save(xmdl_clusters,  file = "models/clusters_models.RData")


## model AUC as a function of cluster x condition
# define weakly informative priors
priors_xmdl <- c(
  set_prior("normal(0,1)", class = "b"),
  set_prior("normal(0,1)", class = "Intercept"),
  set_prior("lkj(2)", class = "cor"),
  set_prior("student-t(3,0,1)", class = "sd")
)


# model AUC x clusters
xmdl_AUC_cluster <- brm(AUC_c ~ 1 + Condition_c * cluster3
                + (1 | subject_nr)
                + (1 + Condition_c | stimulus),
                family = "gaussian",
                data = df_red, save_all_pars = TRUE,
                chains = 4, cores = 4,
                iter = 4000, warmup = 2000,
                control = list(adapt_delta = 0.9999, max_treedepth = 12),
                seed = 999
)


# check model fit
pp_check(xmdl_AUC_cluster)
## has the remaining dent which might be due to unexplained variance due to subject-specific movement behavior

# store model objects
save(xmdl_AUC_cluster, file = "models/AUC_clusters_models.RData")


##################################
## Extract posteriors and plot ###
##################################

load(file = "models/AUC_models.RData")
load(file = "models/AUC_clusters_models.RData")
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
name <- c()
lci <- c()
uci <- c()
mean <- c()
prob <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_AUC[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_AUC[[parameters[i]]]),2))
  name <- c(name, parameters[i])
  prob <- c(prob, round(length(which(posteriors_AUC[[parameters[i]]] > 0)) / length(posteriors_AUC[[parameters[i]]]), 2))
}

posteriors_output_cond = data.frame(mean, lci, uci, name, prob)
colnames(posteriors_output_cond) <- c("AUC", "lci", "uci", "Condition", "probability of beta > 0")

# aggregate AUC scores for each subject
df_agg <- df_red %>%
  group_by(Condition, subject_nr) %>%
  summarize(AUC = mean(AUC_c, na.rm = TRUE))

# plot
Cond_plot <-
  ggplot(df_agg, aes(x = Condition, y = AUC, color = Condition, fill = Condition)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", color = "grey") +
  geom_quasirandom(data = df_agg, size = 2, alpha = 0.5, width = 0.1) +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 2)) +
  #scale_x_continuous(expand = c(0, 0), limits = c(0.8, 1.2)) +
  geom_errorbar(data = posteriors_output_cond %>% filter(Condition != "delta"),
                aes(x = Condition, ymin = lci, ymax = uci), color = "black", width = .05, size = 1, inherit.aes = FALSE) +
  geom_point(data = posteriors_output_cond %>% filter(Condition != "delta"),
             size = 5, pch = 21, color = "black", inherit.aes = TRUE) +
  labs(title = "(a) Model without clusters",
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
posteriors_clusters <- posterior_samples(xmdl_clusters) %>%
  mutate(
    # probabilities!
    cluster1_base = 1 / (1 + exp(b_mu2_Intercept) + exp(b_mu3_Intercept) + exp(b_mu4_Intercept)),
    cluster1_logical = 1 / (1 + exp(b_mu2_Intercept + b_mu2_Condition_c * .5) +
                              exp(b_mu3_Intercept + b_mu3_Condition_c * .5) +
                              exp(b_mu4_Intercept + b_mu4_Condition_c * .5)),
    cluster1_pragmatic = 1 / (1 + exp(b_mu2_Intercept + b_mu2_Condition_c * -.5) +
                                exp(b_mu3_Intercept + b_mu3_Condition_c * -.5) +
                                exp(b_mu4_Intercept + b_mu4_Condition_c * -.5)),

    cluster2_base = cluster1_base * (exp(b_mu2_Intercept)),
    cluster2_logical = cluster1_logical * (exp(b_mu2_Intercept + b_mu2_Condition_c * .5)),
    cluster2_pragmatic = cluster1_pragmatic * (exp(b_mu2_Intercept + b_mu2_Condition_c * -.5)),

    cluster3_base = cluster1_base * (exp(b_mu3_Intercept)),
    cluster3_logical = cluster1_logical * (exp(b_mu3_Intercept + b_mu3_Condition_c * .5)),
    cluster3_pragmatic = cluster1_pragmatic * (exp(b_mu3_Intercept + b_mu3_Condition_c * -.5)),

    cluster4_base = cluster1_base * (exp(b_mu4_Intercept)),
    cluster4_logical = cluster1_logical * (exp(b_mu4_Intercept + b_mu4_Condition_c * .5)),
    cluster4_pragmatic = cluster1_pragmatic * (exp(b_mu4_Intercept + b_mu4_Condition_c * -.5)),

    delta_cluster1 = cluster1_logical - cluster1_pragmatic,
    delta_cluster2 = cluster2_logical - cluster2_pragmatic,
    delta_cluster3 = cluster3_logical - cluster3_pragmatic,
    delta_cluster4 = cluster4_logical - cluster4_pragmatic,

  )

parameters = c("cluster1_logical", "cluster1_pragmatic",
               "cluster2_logical", "cluster2_pragmatic",
               "cluster3_logical", "cluster3_pragmatic",
               "cluster4_logical", "cluster4_pragmatic",
               "delta_cluster1", "delta_cluster2",
               "delta_cluster3", "delta_cluster4")

# create vectors to store in
name <- c()
lci <- c()
uci <- c()
mean <- c()
prob <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_clusters[[parameters[i]]]))[1],2))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_clusters[[parameters[i]]]))[2],2))
  mean <- c(mean, round(mean(posteriors_clusters[[parameters[i]]]),2))
  name <- c(name, parameters[i])
  prob <- c(prob, round(length(which(posteriors_clusters[[parameters[i]]] > 0)) / length(posteriors_clusters[[parameters[i]]]), 2))
}

posteriors_output_clusters = data.frame(mean, lci, uci, name, prob)
colnames(posteriors_output_clusters) <- c("proportion", "lci", "uci", "name", "probability of beta > 0")
posteriors_output_clusters$type = c(rep("estimate", 8), rep("delta", 4))
posteriors_output_clusters$Condition = c(rep(c("logical", "pragmatic"), 4), rep(NA, 4))
posteriors_output_clusters$cluster = c(rep(c("cluster1", "cluster2", "cluster3", "cluster4"), each = 2),
                                       c("cluster1", "cluster2", "cluster3", "cluster4"))


# extract posteriors for condition x cluster model
posteriors_AUC_clusters <- posterior_samples(xmdl_AUC_cluster) %>%
  mutate(
    cluster1 = b_Intercept,
    cluster2 = b_Intercept + b_cluster42,
    cluster3 = b_Intercept + b_cluster43,
    cluster4 =  b_Intercept + b_cluster44,

    logical_cluster1 = cluster1 + (b_Condition_c * 0.5),
    pragmatic_cluster1 = cluster1 + (b_Condition_c * -0.5),

    logical_cluster2 = cluster2 + (b_Condition_c * 0.5) + (`b_Condition_c:cluster42` * 0.5),
    pragmatic_cluster2  = cluster2 + (b_Condition_c * -0.5) + (`b_Condition_c:cluster42` * -0.5),

    logical_cluster3 = cluster3 + (b_Condition_c * 0.5) + (`b_Condition_c:cluster43` * 0.5),
    pragmatic_cluster3 = cluster3 + (b_Condition_c * -0.5) + (`b_Condition_c:cluster43` * -0.5),

    logical_cluster4 = cluster4 + (b_Condition_c * 0.5) + (`b_Condition_c:cluster44` * 0.5),
    pragmatic_scluster4 = cluster4 + (b_Condition_c * -0.5) + (`b_Condition_c:cluster44` * -0.5),

    delta_cluster1 = b_Condition_c,
    delta_cluster2 = b_Condition_c +  `b_Condition_c:cluster42`,
    delta_cluster3 = b_Condition_c + `b_Condition_c:cluster43`,
    delta_cluster4 = b_Condition_c + `b_Condition_c:cluster44`
  )



parameters = c("delta_cluster1", "delta_cluster2", "delta_cluster3", "delta_cluster4")

# create vectors to store in
name <- c()
lci <- c()
uci <- c()
mean <- c()
prob <- c()

# loop through parameters and extract post mean, CI, and probs
for (i in 1:length(parameters)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(posteriors_AUC_clusters[[parameters[i]]]))[1],3))
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(posteriors_AUC_clusters[[parameters[i]]]))[2],3))
  mean <- c(mean, round(mean(posteriors_AUC_clusters[[parameters[i]]]),3))
  name <- c(name, parameters[i])
  prob <- c(prob, round(length(which(posteriors_AUC_clusters[[parameters[i]]] > 0)) / length(posteriors_AUC_clusters[[parameters[i]]]), 2))

}

posteriors_output_condclusters = data.frame(mean, lci, uci, name, prob)
colnames(posteriors_output_condclusters) <-  c("AUC", "lci", "uci", "Condition", "probability of beta > 0")
posteriors_output_condclusters$cluster = c("cluster1", "cluster2", "cluster3", "cluster4")


# plot results


# plot
CondClusters_plot <-
ggplot(posteriors_output_condclusters, aes(x = cluster, y = AUC)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 2)) +
  geom_errorbar(data = posteriors_output_condclusters, aes(ymin = lci, ymax = uci), color = "black", width = .1) +
  geom_point(data = posteriors_output_condclusters, size = 5, pch = 21, color = "black", fill = "black") +
  annotate("text", x = 2.5, y = -1.5, color = "grey", label = "pragmatic > logical") +
  annotate("text", x = 2.5, y = 1.5, color = "grey", label = "logical > pragmatic") +
  labs(title = "(b) Model including interaction with clusters",
       subtitle = " \n",
       x = "\ntrajectory cluster",
       y = "scaled AUC differences\n"
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
            widths = c(0.8,1),
            font.label = list(size = 28),
            align = "h",
            ncol = 2, nrow = 1,
            common.legend = F)

# store plots
ggsave(filename = "plots/uber_estimates.png",
       plot = uber_estimates,
       device = "png",
       width = 255,
       height = 150,
       units = "mm",
       dpi = 300)


# store posteriors
write.csv(posteriors_output_cond, file = "derived_data/posteriors_output_cond.csv")
write.csv(posteriors_output_clusters, file = "derived_data/posteriors_output_cluster.csv")
write.csv(posteriors_output_condclusters, file = "derived_data/posteriors_output_condclusters.csv")

