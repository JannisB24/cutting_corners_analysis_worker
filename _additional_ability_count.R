# regressions with worker vars
moved_ratio_50_all_work_exp_regression <- lm(moved_ratio_50_all ~ 
                                               els_slider.1.player.work_experience_worker + 
                                               els_slider.1.player.work_type_apprenticeship +
                                               els_slider.1.player.work_type_internship +
                                               els_slider.1.player.work_type_working_student +
                                               els_slider.1.player.work_type_part_time_job +
                                               els_slider.1.player.work_type_full_time_job +
                                               els_slider.1.player.work_type_volunteer_work +
                                               els_slider.1.player.work_experience_duration +
                                               ability_sliders_50 +
                                               els_slider.1.player.slider_50_difficulty, 
                                             data = data)
summary(moved_ratio_50_all_work_exp_regression)

sliders_at_50_count_work_exp_regression <- lm(sliders_at_50_count ~ 
                                                els_slider.1.player.work_experience_worker + 
                                                els_slider.1.player.work_type_apprenticeship +
                                                els_slider.1.player.work_type_internship +
                                                els_slider.1.player.work_type_working_student +
                                                els_slider.1.player.work_type_part_time_job +
                                                els_slider.1.player.work_type_full_time_job +
                                                els_slider.1.player.work_type_volunteer_work +
                                                els_slider.1.player.work_experience_duration +
                                                ability_sliders_50 +
                                                els_slider.1.player.slider_50_difficulty, 
                                              data = data)
summary(sliders_at_50_count_work_exp_regression)

# regressions with treatment, controlling for difficulty, skill, and sociodemographics
moved_ratio_50_all_treatment_regression <- lm(moved_ratio_50_all ~ els_slider.1.player.treatment + 
                                                ability_sliders_50 +
                                                els_slider.1.player.slider_50_difficulty + 
                                                demographics.1.player.age + 
                                                gender_male +
                                                bachelor_or_higher +
                                                political_left,
                                              data = data)
summary(moved_ratio_50_all_treatment_regression)

sliders_at_50_count_treatment_regression <- lm(sliders_at_50_count ~ els_slider.1.player.treatment + 
                                                 ability_sliders_50 +
                                                 els_slider.1.player.slider_50_difficulty + 
                                                 demographics.1.player.age + 
                                                 gender_male +
                                                 bachelor_or_higher +
                                                 political_left,
                                               data = data)
summary(sliders_at_50_count_treatment_regression)

# regressions with treatment, beliefs, controlling for difficulty and skill
moved_ratio_50_all_belief_regression <- lm(moved_ratio_50_all ~ els_slider.1.player.treatment +
                                             personal_belief_z +
                                             group_compliance_z +
                                             group_noncompliance_z +
                                             empirical_belief_z +
                                             els_slider.1.player.slider_50_difficulty + 
                                             ability_sliders_50,
                                           data = data)
summary(moved_ratio_50_all_belief_regression)

sliders_at_50_count_belief_regression <- lm(sliders_at_50_count ~ els_slider.1.player.treatment *
                                              personal_belief_z +
                                              group_compliance_z +
                                              group_noncompliance_z +
                                              empirical_belief_z +
                                              els_slider.1.player.slider_50_difficulty + 
                                              ability_sliders_50,
                                            data = data)
summary(sliders_at_50_count_belief_regression)

# additionally control for interaction effects between treatment and beliefs
moved_ratio_50_all_interaction_belief_regression <- lm(moved_ratio_50_all ~
                                                         els_slider.1.player.treatment *
                                                         (personal_belief_z +
                                                            group_compliance_z +
                                                            group_noncompliance_z +
                                                            empirical_belief_z) +
                                                         els_slider.1.player.slider_50_difficulty + 
                                                         ability_sliders_50,
                                                       data = data)
summary(moved_ratio_50_all_interaction_belief_regression)

sliders_at_50_count_interaction_belief_regression <- lm(sliders_at_50_count ~
                                                          els_slider.1.player.treatment *
                                                          (personal_belief_z +
                                                             group_compliance_z +
                                                             group_noncompliance_z +
                                                             empirical_belief_z) +
                                                          els_slider.1.player.slider_50_difficulty + 
                                                          ability_sliders_50,
                                                        data = data)
summary(sliders_at_50_count_interaction_belief_regression)

# single belief regressions
moved_ratio_50_all_personal_belief_regression <- lm(moved_ratio_50_all ~ 
                                                      els_slider.1.player.treatment +
                                                      personal_belief_z +
                                                      els_slider.1.player.slider_50_difficulty + 
                                                      ability_sliders_50,
                                                    data = data)
summary(moved_ratio_50_all_personal_belief_regression)

moved_ratio_50_all_group_compliance_regression <- lm(moved_ratio_50_all ~ 
                                                       els_slider.1.player.treatment +
                                                       group_compliance_z +
                                                       els_slider.1.player.slider_50_difficulty + 
                                                       ability_sliders_50,
                                                     data = data)
summary(moved_ratio_50_all_group_compliance_regression)

moved_ratio_50_all_group_noncompliance_regression <- lm(moved_ratio_50_all ~ 
                                                          els_slider.1.player.treatment +
                                                          group_noncompliance_z +
                                                          els_slider.1.player.slider_50_difficulty + 
                                                          ability_sliders_50,
                                                        data = data)
summary(moved_ratio_50_all_group_noncompliance_regression)

moved_ratio_50_all_empirical_belief_regression <- lm(moved_ratio_50_all ~ 
                                                       els_slider.1.player.treatment +
                                                       empirical_belief_z +
                                                       els_slider.1.player.slider_50_difficulty + 
                                                       ability_sliders_50,
                                                     data = data)
summary(moved_ratio_50_all_empirical_belief_regression)

# regression of ratio_50_all on els_score, moral_foundations_score, rule_orientation_score, controlling for treatment, difficulty, and skill
moved_ratio_50_all_mf20_rule_regression <- lm(moved_ratio_50_all ~ 
                                                els_slider.1.player.treatment +
                                                els_slider.1.player.els_score + 
                                                moral_foundations_20.1.player.moral_foundations_score + 
                                                rule_orientation.1.player.rule_orientation_score + 
                                                els_slider.1.player.slider_50_difficulty + 
                                                ability_sliders_50,
                                              data = data)
summary(moved_ratio_50_all_mf20_rule_regression)

sliders_at_50_count_mf20_rule_regression <- lm(sliders_at_50_count ~ 
                                                 els_slider.1.player.treatment +
                                                 els_slider.1.player.els_score + 
                                                 moral_foundations_20.1.player.moral_foundations_score + 
                                                 rule_orientation.1.player.rule_orientation_score + 
                                                 els_slider.1.player.slider_50_difficulty + 
                                                 ability_sliders_50,
                                               data = data)
summary(sliders_at_50_count_mf20_rule_regression)

# regression of ratio_50_all on mother_birth_year, controlling for difficulty and skill
moved_ratio_50_all_mother_birth_year_regression <- lm(moved_ratio_50_all ~ 
                                                        els_slider.1.player.treatment +          
                                                        honesty.1.player.mother_birth_year + 
                                                        els_slider.1.player.slider_50_difficulty + 
                                                        ability_sliders_50,
                                                      data = data)
summary(moved_ratio_50_all_mother_birth_year_regression)

sliders_at_50_count_mother_birth_year_regression <- lm(sliders_at_50_count ~ 
                                                         els_slider.1.player.treatment +          
                                                         honesty.1.player.mother_birth_year + 
                                                         els_slider.1.player.slider_50_difficulty + 
                                                         ability_sliders_50,
                                                       data = data)
summary(sliders_at_50_count_mother_birth_year_regression)