####
# project: "Replication Tomlinson et al. 2013"
# title: "Power analysis"
# author: "Timo Roettger"
# date: "2020-01-22"
###


library(tidyverse)
library(faux)
library(lme4)
library(ggbeeswarm)
library(broom.mixed)


# simulate data from lmer object

### Establishing the data-generating parameters

sim_lmer <- function(

# set all data-generating parameters
b0     = 0,        # intercept; i.e., the grand mean. Because we have standardized AUC values, this is 0
b1     = 0.555,    # slope; i.e, effect of cohort vs. competitor. Our Pilot indicates a AUC difference of  1.11
                      # let's be super cautious and half this effect 0.555

# variance components: We take the pilot data and mulitply the variance by a conservative number:
x = 2,

S_intercept_sd = 0.12 * x,     # by-subject random intercept sd (pilot = 0.77)
I_intercept_sd = 0.17 * x,     # by-item random intercept sd (pilot = 0.04)
#S_slope_sd = 0.2 * x,          # by-subject random slope sd (pilot = 0.2)
#I_slope_sd = 0.11 * x,         # by-item random slope sd (pilot = 0.11)
#S_cor   = -0.37 * x,           # by-subject correlation between intercept and slope (pilot = -0.37)
#I_cor   =  0.3 * x,            # by-item correlation between intercept and slope (pilot = 0.28)
residual_sd = 0.83 * x,        # residual (error) sd (pilot = 0.73)

# set number of subjects and items
nsubj  = 30,  # number of subjects
nitem  = 30  # number of items
)
{

### Simulating the sampling process



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

# run model
# mod <- lmer(dv ~ condition.e +
#               (1 + condition.e | sub_id) +
#               (1 + condition.e | item_id),
#             data = dat,
#             control = lmerControl(calc.derivs = FALSE))


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

summary <- broom.mixed::tidy(mod) %>%
  mutate(warnings = ww) %>%
  filter(effect == "fixed",
         term == "condition.e")

return(summary)

}

# run many simulations
sim_no = 250
mod_conc = c()

subj_n <- c(2,4,6,8,16,24,32)

# loop through different subj_numbers
for (j in subj_n) {
# run loop x sim_no
for (i in 1:sim_no) {
  mod <- sim_lmer(nsubj = j)
  mod <- mod %>% mutate(subj_no = j)
  # store effect and SE

  mod_conc <- rbind(mod_conc, mod)

  # counter
  if (i%%50 == 0) print(paste("Almost there, counter at: ", i, "for subj_n of ", j))

  } # end sim_no
} # end subj_n

# make data.frame
write.csv(mod_conc, file = "derived_data/fakeData.csv")

# agg for plot
df_agg <- mod_conc %>%
  filter(!grepl("failed", warnings)) %>%
  group_by(subj_no) %>%
  summarize(all = n(),
            power = sum(statistic > 2 | statistic < -2) / all,
            mean_effect = mean(estimate, na.rm = T),
            mean_SE = mean(std.error, na.rm = T),
            mean_t = mean(statistic, na.rm = T),
            )

# plot effect / SE
ggplot(mod_conc, aes(x = subj_no, y = estimate, color = subj_no, fill = subj_no)) +
  geom_quasirandom(alpha = 0.3) +
  geom_point(data = df_agg, aes(x = subj_no, y = mean_effect, color = subj_no, fill = subj_no),
             size = 3) +
  geom_line(data = df_agg, aes(x = subj_no, y = mean_effect, group = 1), color = "black")

# plot power
power_curve <-
ggplot(df_agg, aes(x = subj_no, y = power)) +
  geom_point() +
  geom_path(aes(group = 1), color = "black") +
  geom_segment(y = 0.9, yend = 0.9, x = -Inf, xend = Inf,
               lty = "dashed", color = "grey") +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 32)) +
  labs(title = "Power curve",
       subtitle = "based on half of the original effect size\n",
       x = "\npower",
       y = "subject number\n"
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
