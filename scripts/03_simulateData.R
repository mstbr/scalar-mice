####
# project: "Replication Tomlinson et al. 2013"
# title: "Power analysis"
# author: "Timo Roettger"
# date: "2020-01-22"
###

##############
## Prepare ###
##############

# install packages if required
if (!require(faux)) {install.packages('faux')}
if (!require(tidyverse)) {install.packages('tidyverse')}
if (!require(lme4)) {install.packages('lme4')}
if (!require(broom.mixed)) {install.packages('broom.mixed')}
if (!require(ggbeeswarm)) {install.packages('ggbeeswarm')}

library(tidyverse)
library(faux)
library(lme4)
library(ggbeeswarm)
library(broom.mixed)


## simulate data

# establishing the data-generating parameters

sim_lmer <- function(

# set all data-generating parameters
b0     = 0,        # intercept; i.e., the grand mean. Because we have standardized AUC values, this is 0
b1     = 0.55,    # slope; i.e, effect of cohort vs. competitor. Our Pilot indicates a AUC difference of  1.10
                      # let's be super cautious and half this effect 0.55

# variance components: We take the values from the pilot data
S_intercept_sd = 0.47,     # by-subject random intercept sd (pilot = 0.47)
I_intercept_sd = 0.33,     # by-item random intercept sd (pilot = 0.33)
residual_sd = 0.75,        # residual (error) sd (pilot = 0.75)

# set number of subjects and items
nsubj  = 30,  # number of subjects
nitem  = 30  # number of items
)
{

# Simulate the sampling of subject random effects

sub <- tibble(
    sub_id = 1:nsubj,
    sub_i  = rnorm(nsubj, 0, S_intercept_sd),
    condition = rep(c("logical", "pragmatic"), each = nsubj / 2)
  )

# Simulate the sampling of item random effects
item <- tibble(
  item_id = 1:nitem,
  item_i  = rnorm(nitem, 0, I_intercept_sd)
)

# put them together
trials <- crossing(
  sub_id = sub$sub_id, # get subject IDs from the sub data table
  item_id = item$item_id,
# get stimulus IDs from the stim data table
) %>%
  left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
  left_join(item, by = "item_id")   # includes the intercept for each stimulus


# Now we generate the DV by adding all of this together
dat <- trials %>%
  mutate(
    # effect-code subject condition and stimulus version
    condition.e = recode(condition, "logical" = -0.5, "pragmatic" = +0.5),
    # calculate trial-specific effects by adding overall effects and slopes
    cond_eff = b1,
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, residual_sd),
    # calculate DV from intercepts, effects, and error
    dv = b0 + sub_i + item_i + err +
      (condition.e * cond_eff)
  )


# run lmer and capture any warnings
ww <- ""
suppressMessages(suppressWarnings(
    mod <- withCallingHandlers({
      lmer(dv ~ condition.e +
             (1 | sub_id) +
             (1 | item_id),
           dat, REML = FALSE)},
      warning = function(w) { ww <<- w$message }
    )
  ))

# wrangle
summary <- broom.mixed::tidy(mod) %>%
  mutate(warnings = ww) %>%
  filter(effect == "fixed",
         term == "condition.e")

return(summary)

}


## run many simulations
sim_no = 1000

# create empty vector to store results in
mod_conc = c()

# specify what subject numbers to simulate
subj_n <- c(8,16,24,32)

# loop through different subj_numbers
for (j in subj_n) {

# run loop through sim_no
for (i in 1:sim_no) {
  mod <- sim_lmer(nsubj = j)
  mod <- mod %>% mutate(subj_no = j)
  # store effect and SE

  mod_conc <- rbind(mod_conc, mod)

  # counter
  if (i%%50 == 0) print(paste("Almost there, counter at: ", i, "for subj_n of ", j))

  } # end sim_no
} # end subj_n

# save data.frame
write.csv(mod_conc, file = "derived_data/fakeData.csv")

# agg for plot
df_agg <- mod_conc %>%
  filter(!grepl("failed", warnings)) %>%
  group_by(subj_no) %>%
  summarize(all = n(),
            power = sum(statistic > 2) / all,
            mean_effect = mean(estimate, na.rm = T),
            mean_SE = mean(std.error, na.rm = T),
            mean_t = mean(statistic, na.rm = T),
            )

# plot effect magnitude
ggplot(mod_conc, aes(x = subj_no, y = estimate, color = subj_no, fill = subj_no)) +
  geom_quasirandom(alpha = 0.3) +
  geom_point(data = df_agg, aes(x = subj_no, y = mean_effect, color = subj_no, fill = subj_no),
             size = 3) +
  geom_line(data = df_agg, aes(x = subj_no, y = mean_effect, group = 1), color = "black")

# plot power
power_curve <-
ggplot(df_agg, aes(x = subj_no, y = power)) +
  geom_point(size = 3) +
  geom_path(aes(group = 1), color = "black") +
  geom_segment(y = 0.9, yend = 0.9, x = -Inf, xend = Inf,
               lty = "dashed", color = "grey") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(8,16,24,32),  limits = c(6, 34)) +
  labs(title = "Power curve",
       subtitle = "based on half of the original effect size\n",
       y = "\npower",
       x = "subject number\n"
  ) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.15),
        legend.key.height = unit(2,"line"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        axis.line = element_blank(),
        axis.text = element_text(size = 16, face = "bold", color = "grey"),
        axis.title = element_text(size = 16, face = "bold", color = "grey"),
        plot.title = element_text(size = 16, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        plot.margin = unit(c(1,1,1,1),"cm"))


# save power curve plot
ggsave(filename = "plots/power_curve.pdf",
       plot = power_curve,
       device = "pdf",
       width = 160,
       height = 160,
       units = "mm",
       dpi = 300)
