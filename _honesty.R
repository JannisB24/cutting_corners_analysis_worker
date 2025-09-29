##########################################################################################
# Honesty
##########################################################################################

mother_birth_year_0 <- sum(data$honesty.1.player.mother_birth_year == 0, na.rm = TRUE)
mother_birth_year_1 <- sum(data$honesty.1.player.mother_birth_year == 1, na.rm = TRUE)
mother_birth_year_total <- mother_birth_year_0 + mother_birth_year_1
mother_birth_year_perc_0 <- mother_birth_year_0 / mother_birth_year_total
print(mother_birth_year_perc_0)
mother_birth_year_perc_1 <- mother_birth_year_1 / mother_birth_year_total
print(mother_birth_year_perc_1)

mother_birth_year_perc_1_low <- sum(data_low$honesty.1.player.mother_birth_year == 1, na.rm = TRUE) / 
  (sum(data_low$honesty.1.player.mother_birth_year == 0, na.rm = TRUE) + 
     sum(data_low$honesty.1.player.mother_birth_year == 1, na.rm = TRUE))
print(mother_birth_year_perc_1_low)
mother_birth_year_perc_1_high <- sum(data_high$honesty.1.player.mother_birth_year == 1, na.rm = TRUE) /
  (sum(data_high$honesty.1.player.mother_birth_year == 0, na.rm = TRUE) + 
     sum(data_high$honesty.1.player.mother_birth_year == 1, na.rm = TRUE))
print(mother_birth_year_perc_1_high)

# binomial test: whether lying exists
honesty_binom_test_all <- binom.test(
  sum(data$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  sum(data$honesty.1.player.mother_birth_year == 0, na.rm = TRUE) + sum(data$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  p = 0.5,
  alternative = "two.sided"
)
print(honesty_binom_test_all)
honesty_binom_test_low <- binom.test(
  sum(data_low$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  sum(data_low$honesty.1.player.mother_birth_year == 0, na.rm = TRUE) + sum(data_low$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  p = 0.5,
  alternative = "two.sided"
)
print(honesty_binom_test_low)
honesty_binom_test_high <- binom.test(
  sum(data_high$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  sum(data_high$honesty.1.player.mother_birth_year == 0, na.rm = TRUE) + sum(data_high$honesty.1.player.mother_birth_year == 1, na.rm = TRUE),
  p = 0.5,
  alternative = "two.sided"
)
print(honesty_binom_test_high)

# Correlation tests between rule-following and honesty
cor.test(data$moved_ratio_50_all, data$honesty.1.player.mother_birth_year, method = "pearson")
cor.test(data$sliders_at_50_count, data$honesty.1.player.mother_birth_year, method = "pearson")

# Chi-square test: mother_birth_year across treatments
honesty_treatment_chisq <- chisq.test(
  table(data$honesty.1.player.mother_birth_year, 
        data$els_slider.1.player.treatment)
)
print(honesty_treatment_chisq)

# Fisher's exact test: mother_birth_year across treatments
honesty_treatment_fisher <- fisher.test(
  table(data$honesty.1.player.mother_birth_year, 
        data$els_slider.1.player.treatment)
)
print(honesty_treatment_fisher)

# regression of mother_birth_year on worker variables
mother_birth_year_worker_vars_regression <- lm(honesty.1.player.mother_birth_year ~ 
                                                 els_slider.1.player.work_experience_worker + 
                                                 els_slider.1.player.work_type_apprenticeship +
                                                 els_slider.1.player.work_type_internship +
                                                 els_slider.1.player.work_type_working_student +
                                                 els_slider.1.player.work_type_part_time_job +
                                                 els_slider.1.player.work_type_full_time_job +
                                                 els_slider.1.player.work_type_volunteer_work +
                                                 els_slider.1.player.work_experience_duration,
                                               data = data)
summary(mother_birth_year_worker_vars_regression)

# regression of mother_birth_year on treatment and slider task, controlling for ability, and difficulty
mother_birth_year_treatment_regression <- lm(honesty.1.player.mother_birth_year ~ 
                                               els_slider.1.player.treatment + 
                                               moved_ratio_50_all +
                                               ability_sliders_50_z +
                                               slider_50_difficulty_z, 
                                             data = data)
summary(mother_birth_year_treatment_regression)

# regression of mother_birth_year on beliefs, controlling for ability, and difficulty
mother_birth_year_beliefs_regression <- lm(honesty.1.player.mother_birth_year ~ 
                                             els_slider.1.player.treatment *
                                             (slider_beliefs.1.player.empirical_belief_compliance +
                                                slider_beliefs.1.player.normative_belief_personal +
                                                slider_beliefs.1.player.normative_belief_group_compliance +
                                                slider_beliefs.1.player.normative_belief_group_noncompliance) +
                                             ability_score +
                                             els_slider.1.player.slider_50_difficulty,
                                           data = data)
summary(mother_birth_year_beliefs_regression)

# regression of mother_birth_year on ELS, MF20, Rule Orientation
mother_birth_year_other_scales_regression <- lm(honesty.1.player.mother_birth_year ~ 
                                                  els_slider.1.player.treatment +
                                                  moved_ratio_50_all +
                                                  els_slider.1.player.els_score + 
                                                  moral_foundations_20.1.player.moral_foundations_score + 
                                                  rule_orientation.1.player.rule_orientation_score +
                                                  ability_sliders_50_z +
                                                  slider_50_difficulty_z,,
                                                data = data)
summary(mother_birth_year_other_scales_regression)

honesty_regression(mother_birth_year_other_scales_regression, 
                   title = "Regression of Mother Birth Year on ELS, MF20, and Rule Orientation",
                   covariate.labels = c("Treatment (Low)", 
                                        "Moved Ratio (50)", 
                                        "ELS Score", 
                                        "MF20 Score", 
                                        "Rule Orientation Score", 
                                        "Ability (z)", 
                                        "Difficulty (z)"))

# regression of mother_birth_year on demographics
mother_birth_year_demographics_regression <- lm(honesty.1.player.mother_birth_year ~ 
                                                  demographics.1.player.age + 
                                                  gender_male +
                                                  bachelor_or_higher +
                                                  political_left,
                                                data = data)
summary(mother_birth_year_demographics_regression)