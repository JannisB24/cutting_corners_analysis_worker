# check whether empirical beliefs is a moderator to treatment effect
mediator_model <- lm(
  slider_beliefs.1.player.empirical_belief_compliance ~ els_slider.1.player.treatment +
    ability_sliders_50_z + 
    slider_50_difficulty_z,
  data = data
)
summary(mediator_model)

outcome_model <- lm(
  moved_ratio_50_all ~ els_slider.1.player.treatment +
    slider_beliefs.1.player.empirical_belief_compliance +
    ability_sliders_50_z + 
    slider_50_difficulty_z,
  data = data
)
summary(outcome_model)

med_results <- mediate(
  mediator_model,
  outcome_model,
  treat = "els_slider.1.player.treatment",
  mediator = "slider_beliefs.1.player.empirical_belief_compliance",
  boot = TRUE,  # Use bootstrapping for inference
  sims = 1000   # Number of bootstrap simulations
)
summary(med_results)
plot(med_results)

# install.packages("ggplot2") # if needed
library(ggplot2)

# Build a tidy data frame from `med_results`
effects_list <- list(
  # ACME
  if (!is.null(med_results$d0)) data.frame(effect = "ACME", condition = "Control",
                                           estimate = med_results$d0,
                                           ci_low = med_results$d0.ci[1],
                                           ci_high = med_results$d0.ci[2],
                                           p_value = med_results$d0.p) else NULL,
  if (!is.null(med_results$d1)) data.frame(effect = "ACME", condition = "Treated",
                                           estimate = med_results$d1,
                                           ci_low = med_results$d1.ci[1],
                                           ci_high = med_results$d1.ci[2],
                                           p_value = med_results$d1.p) else NULL,
  if (!is.null(med_results$d.avg)) data.frame(effect = "ACME", condition = "Average",
                                              estimate = med_results$d.avg,
                                              ci_low = med_results$d.avg.ci[1],
                                              ci_high = med_results$d.avg.ci[2],
                                              p_value = med_results$d.avg.p) else NULL,
  
  # ADE
  if (!is.null(med_results$z0)) data.frame(effect = "ADE", condition = "Control",
                                           estimate = med_results$z0,
                                           ci_low = med_results$z0.ci[1],
                                           ci_high = med_results$z0.ci[2],
                                           p_value = med_results$z0.p) else NULL,
  if (!is.null(med_results$z1)) data.frame(effect = "ADE", condition = "Treated",
                                           estimate = med_results$z1,
                                           ci_low = med_results$z1.ci[1],
                                           ci_high = med_results$z1.ci[2],
                                           p_value = med_results$z1.p) else NULL,
  if (!is.null(med_results$z.avg)) data.frame(effect = "ADE", condition = "Average",
                                              estimate = med_results$z.avg,
                                              ci_low = med_results$z.avg.ci[1],
                                              ci_high = med_results$z.avg.ci[2],
                                              p_value = med_results$z.avg.p) else NULL,
  
  # Total effect
  if (!is.null(med_results$tau.coef)) data.frame(effect = "Total Effect", condition = "Average",
                                                 estimate = med_results$tau.coef,
                                                 ci_low = med_results$tau.ci[1],
                                                 ci_high = med_results$tau.ci[2],
                                                 p_value = med_results$tau.p) else NULL,
  
  # Proportion mediated
  if (!is.null(med_results$n0)) data.frame(effect = "Prop. Mediated", condition = "Control",
                                           estimate = med_results$n0,
                                           ci_low = med_results$n0.ci[1],
                                           ci_high = med_results$n0.ci[2],
                                           p_value = med_results$n0.p) else NULL,
  if (!is.null(med_results$n1)) data.frame(effect = "Prop. Mediated", condition = "Treated",
                                           estimate = med_results$n1,
                                           ci_low = med_results$n1.ci[1],
                                           ci_high = med_results$n1.ci[2],
                                           p_value = med_results$n1.p) else NULL,
  if (!is.null(med_results$n.avg)) data.frame(effect = "Prop. Mediated", condition = "Average",
                                              estimate = med_results$n.avg,
                                              ci_low = med_results$n.avg.ci[1],
                                              ci_high = med_results$n.avg.ci[2],
                                              p_value = med_results$n.avg.p) else NULL
)

effects_df <- do.call(rbind, effects_list)

# Order factors for cleaner display
effects_df$effect <- factor(
  effects_df$effect,
  levels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")
)
effects_df$condition <- factor(
  effects_df$condition,
  levels = c("Control", "Treated", "Average")
)

# Create a label that mirrors the base plot grouping
effects_df$y_label <- paste(effects_df$effect, effects_df$condition, sep = " - ")

# Build the ggplot
gg_med <- ggplot(effects_df, aes(x = estimate, y = y_label, color = effect)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high), size = 0.7) +
  scale_color_brewer(palette = "Dark2", guide = guide_legend(title = "Effect")) +
  labs(
    title = "Mediation Analysis Effects",
    subtitle = "Points = estimates, lines = 95% CIs",
    x = "Estimate",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

gg_med

# check whether personal beliefs are a moderator to treatment effect
mediator_model2 <- lm(
  slider_beliefs.1.player.normative_belief_personal ~ els_slider.1.player.treatment +
    ability_sliders_50_z + 
    slider_50_difficulty_z +
    slider_beliefs.1.player.empirical_belief_compliance +
    group_compliance_binary +
    group_noncompliance_binary,
  data = data
)
summary(mediator_model2)

outcome_model2 <- lm(
  ratio_50_inside ~ els_slider.1.player.treatment +
    slider_beliefs.1.player.normative_belief_personal +
    ability_sliders_50_z + 
    slider_50_difficulty_z +
    slider_beliefs.1.player.empirical_belief_compliance +
    group_compliance_binary +
    group_noncompliance_binary,
  data = data
)
summary(outcome_model2)

med_results2 <- mediate(
  mediator_model2,
  outcome_model2,
  treat = "els_slider.1.player.treatment",
  mediator = "slider_beliefs.1.player.normative_belief_personal",
  boot = TRUE,  # Use bootstrapping for inference
  sims = 1000   # Number of bootstrap simulations
)
summary(med_results2)
plot(med_results2)
