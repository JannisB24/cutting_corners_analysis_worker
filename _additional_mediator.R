# check whether empirical beliefs is a moderator to treatment effect
mediator_model <- lm(
  slider_beliefs.1.player.empirical_belief_compliance ~ els_slider.1.player.treatment +
    ability_sliders_50_z + 
    slider_50_difficulty_z +
    slider_beliefs.1.player.normative_belief_personal +
    group_compliance_binary +
    group_noncompliance_binary,
  data = data
)
summary(mediator_model)

outcome_model <- lm(
  ratio_50_inside ~ els_slider.1.player.treatment +
    slider_beliefs.1.player.empirical_belief_compliance +
    ability_sliders_50_z + 
    slider_50_difficulty_z +
    slider_beliefs.1.player.normative_belief_personal +
    group_compliance_binary +
    group_noncompliance_binary,
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
