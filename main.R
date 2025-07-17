library(readr)
library(ggplot2)
library(psych)
library(dplyr)

# read csv
data <- read_csv("prep_data.csv")

# functions
source("_exclude_otree_standard_vars.R")
source("_exclude_manager_vars.R")
source("_data_integrity_checks.R")
source("_time_boxplots.R")
source("_likert_barchart_15.R")
source("_likert_barchart_05.R")
source("_likert_barchart_06.R")
source("_els_single_item_table.R")
source("_slider_hist.R")
source("_slider_by_treatment.R")
source("_slider_ratios.R")
source("_slider_ratios_moved.R")
source("_slider_beliefs_hist.R")
source("_slider_beliefs_emp_hist.R")
source("_mf20_total_score_barchart.R")
source("_rule_orientation_total_score_barchart.R")
source("_summary_statistics.R")

##########################################################################################
# Data Cleaning and Integrity Checks
##########################################################################################

# exclusions
data <- data[data$els_slider.1.player.q1_mistakes == 0, ]
data <- data[data$els_slider.1.player.q2_mistakes == 0, ]
data <- data[data$moral_foundations_20.1.player.catch_math <= 2, ]
data <- data[data$moral_foundations_20.1.player.catch_good >= 3, ]
data <- exclude_otree_standard_vars(data)
data <- exclude_manager_vars(data)

# exclude variables for which every observation is NA (seemingly irrelevant variables)
all_variables <- names(data)
data <- data[, colSums(is.na(data)) < nrow(data)]
excluded_variables <- setdiff(all_variables, names(data))

# testing: bootstrap to 200 observations
data <- data[sample(nrow(data), 200, replace = TRUE), ]

# run data integrity checks
#data_integrity_checks(data)

treat_high <- data[data$els_slider.1.player.treatment == "high", ]
treat_low <- data[data$els_slider.1.player.treatment == "low", ]

##########################################################################################
# ELS Score
##########################################################################################

# relative table for work_experience_worker
work_exp_levels <- unique(data$els_slider.1.player.work_experience_worker)
work_exp_levels <- work_exp_levels[!is.na(work_exp_levels)]
work_exp_table <- data.frame(
  Low = round(sapply(work_exp_levels, function(x) mean(treat_low$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4),
  High = round(sapply(work_exp_levels, function(x) mean(treat_high$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4),
  Total = round(sapply(work_exp_levels, function(x) mean(data$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4)
)
print(work_exp_table)

# relative table for work_type
work_type_vars <- c("els_slider.1.player.work_type_apprenticeship",
                    "els_slider.1.player.work_type_internship", 
                    "els_slider.1.player.work_type_working_student",
                    "els_slider.1.player.work_type_part_time_job",
                    "els_slider.1.player.work_type_full_time_job",
                    "els_slider.1.player.work_type_volunteer_work")
work_type_table <- data.frame(
  Low = round(sapply(work_type_vars, function(x) mean(treat_low[[x]], na.rm = TRUE)), 4),
  High = round(sapply(work_type_vars, function(x) mean(treat_high[[x]], na.rm = TRUE)), 4),
  Total = round(sapply(work_type_vars, function(x) mean(data[[x]], na.rm = TRUE)), 4)
)
print(work_type_table)

# relative table for work_experience_duration
work_exp_duration_levels <- unique(data$els_slider.1.player.work_experience_duration)
work_exp_duration_levels <- work_exp_duration_levels[!is.na(work_exp_duration_levels)]
work_exp_duration_table <- data.frame(
  Low = round(sapply(work_exp_duration_levels, function(x) mean(treat_low$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4),
  High = round(sapply(work_exp_duration_levels, function(x) mean(treat_high$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4),
  Total = round(sapply(work_exp_duration_levels, function(x) mean(data$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4)
)
print(work_exp_duration_table)

# relative distribution of the ELS score from 10 to 50
ggplot(data, aes(x = els_slider.1.player.els_score)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "#4A90E2", color = "white", alpha = 0.8) + 
  scale_x_continuous(breaks = seq(10, 50, by = 5), limits = c(10, 50), 
                     expand = expansion(mult = 0.02)) +
  labs(title = "Distribution of ELS Scores", x = "ELS Score", y = "Density") +
  geom_vline(aes(xintercept = median(els_slider.1.player.els_score, na.rm = TRUE)), 
             color = "black", linetype = "dashed", linewidth = 0.8) +
  theme_classic()

summary(data$els_slider.1.player.els_score)
quantile(data$els_slider.1.player.els_score, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
sd(data$els_slider.1.player.els_score, na.rm = TRUE)

likert_barchart_15(data, "els_slider.1.player.worker_personal_life", "führt sein Privatleben auf ethische Weise.")
likert_barchart_15(data, "els_slider.1.player.worker_results_obtained", "definiert Erfolg nicht nur an den Ergebnissen, sondern auch daran, wie diese erzielt werden.")
likert_barchart_15(data, "els_slider.1.player.worker_listen_employees", "hört auf das, was Mitarbeiter zu sagen haben.")
likert_barchart_15(data, "els_slider.1.player.worker_discipline_employees", "zieht Mitarbeiter, die gegen ethische Standards verstoßen, zur Rechenschaft.")
likert_barchart_15(data, "els_slider.1.player.worker_fair_decisions", "trifft faire und ausgewogene Entscheidungen.")
likert_barchart_15(data, "els_slider.1.player.worker_trust", "ist vertrauenswürdig.")
likert_barchart_15(data, "els_slider.1.player.worker_attention_check", "Kreuzen Sie bitte 'Stimme zu' an, um zu zeigen, dass Sie diese Frage gelesen haben.")
likert_barchart_15(data, "els_slider.1.player.worker_discuss_employees", "diskutiert Ethik oder Werte im Unternehmenskontext mit Mitarbeitern.")
likert_barchart_15(data, "els_slider.1.player.worker_set_example", "setzt ein Beispiel dafür, wie Dinge auf ethisch richtige Weise getan werden.")
likert_barchart_15(data, "els_slider.1.player.worker_right_thing", "fragt sich bei Entscheidungen: 'Was ist die richtige Entscheidung?'")
likert_barchart_15(data, "els_slider.1.player.worker_best_interest", "hat das Wohl der Mitarbeiter im Auge.")

els_likert_items <- list(
  "els_slider.1.player.worker_personal_life" = "führt sein Privatleben auf ethische Weise.",
  "els_slider.1.player.worker_results_obtained" = "definiert Erfolg nicht nur an den Ergebnissen, sondern auch daran, wie diese erzielt werden.",
  "els_slider.1.player.worker_listen_employees" = "hört auf das, was Mitarbeiter zu sagen haben.",
  "els_slider.1.player.worker_discipline_employees" = "zieht Mitarbeiter, die gegen ethische Standards verstoßen, zur Rechenschaft.",
  "els_slider.1.player.worker_fair_decisions" = "trifft faire und ausgewogene Entscheidungen.",
  "els_slider.1.player.worker_trust" = "ist vertrauenswürdig.",
  "els_slider.1.player.worker_attention_check" = "Kreuzen Sie bitte 'Stimme zu' an, um zu zeigen, dass Sie diese Frage gelesen haben.",
  "els_slider.1.player.worker_discuss_employees" = "diskutiert Ethik oder Werte im Unternehmenskontext mit Mitarbeitern.",
  "els_slider.1.player.worker_set_example" = "setzt ein Beispiel dafür, wie Dinge auf ethisch richtige Weise getan werden.",
  "els_slider.1.player.worker_right_thing" = "fragt sich bei Entscheidungen: 'Was ist die richtige Entscheidung?'",
  "els_slider.1.player.worker_best_interest" = "hat das Wohl der Mitarbeiter im Auge."
)
els_single_item_table(data, els_likert_items)

# Cronbach's alpha
els_items <- data %>% 
  select(
    els_slider.1.player.worker_personal_life,
    els_slider.1.player.worker_results_obtained,
    els_slider.1.player.worker_listen_employees,
    els_slider.1.player.worker_discipline_employees,
    els_slider.1.player.worker_fair_decisions,
    els_slider.1.player.worker_trust,
    els_slider.1.player.worker_discuss_employees,
    els_slider.1.player.worker_set_example,
    els_slider.1.player.worker_right_thing,
    els_slider.1.player.worker_best_interest
  )
els_alpha_result <- psych::alpha(els_items)
print(els_alpha_result$total$raw_alpha)

# time boxplots
time_boxplots(data, "els_slider.1.player.worker_pre_time", "Worker Pre Time")
time_boxplots(data, "els_slider.1.player.worker_time", "Worker Time")
time_boxplots(data, "els_slider.1.player.worker_explanation_time", "Worker Explanation Time")
time_boxplots(data, "els_slider.1.player.worker_results_time", "Worker Results Time")
time_boxplots(data, "els_slider.1.player.slider_instructions_time", "Slider Instructions Time")
time_boxplots(data, "els_slider.1.player.comparison_time", "Comparison Time")
time_boxplots(data, "els_slider.1.player.display_rule_time", "Display Rule Time")
time_boxplots(data, "els_slider.1.player.slider_task_time", "Slider Task Time")

# regression of els_score on time
els_score_time_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.worker_pre_time +
  els_slider.1.player.worker_time +
  els_slider.1.player.worker_explanation_time +
  els_slider.1.player.worker_results_time +
  els_slider.1.player.slider_instructions_time +
  els_slider.1.player.comparison_time +
  els_slider.1.player.display_rule_time +
  els_slider.1.player.slider_task_time,
  data = data
)
summary(els_score_time_regression)

# regression of els_score on work_experience_worker
els_score_work_experience_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.work_experience_worker,
  data = data
)
summary(els_score_work_experience_regression)

# regression of els_score on work_type
els_score_work_type_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.work_type_apprenticeship +
  els_slider.1.player.work_type_internship +
  els_slider.1.player.work_type_working_student +
  els_slider.1.player.work_type_part_time_job +
  els_slider.1.player.work_type_full_time_job +
  els_slider.1.player.work_type_volunteer_work,
  data = data
)
summary(els_score_work_type_regression)

# regression of els_score on work_experience_duration
els_score_work_experience_duration_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.work_experience_duration,
  data = data
)
summary(els_score_work_experience_duration_regression)

# regression of els_score on treatment
els_score_treatment_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.treatment,
  data = data
)
summary(els_score_treatment_regression)

# Chi-square test: Work exp across treatments
work_exp_treatment_chisq <- chisq.test(
  table(data$els_slider.1.player.work_experience_worker, 
        data$els_slider.1.player.treatment)
)

# Chi-square tests: Each work type across treatments
work_type_vars <- c("els_slider.1.player.work_type_apprenticeship",
                    "els_slider.1.player.work_type_internship", 
                    "els_slider.1.player.work_type_working_student",
                    "els_slider.1.player.work_type_part_time_job",
                    "els_slider.1.player.work_type_full_time_job",
                    "els_slider.1.player.work_type_volunteer_work")
work_type_treatment_chisq <- list()
for(i in 1:length(work_type_vars)) {
  work_type_treatment_chisq[[work_type_vars[i]]] <- chisq.test(
    table(data[[work_type_vars[i]]], 
          data$els_slider.1.player.treatment)
  )
}

# Chi-square test: Work experience duration across treatments
work_exp_duration_treatment_chisq <- chisq.test(
  table(data$els_slider.1.player.work_experience_duration, 
        data$els_slider.1.player.treatment)
)

# Mann-Whitney U test for ELS scores across treatments
els_score_treatment_mann_whitney <- wilcox.test(
  els_slider.1.player.els_score ~ els_slider.1.player.treatment,
  data = data,
  exact = FALSE
)


##########################################################################################
# Slider Task
##########################################################################################

data_low <- data[data$els_slider.1.player.treatment == "low", ]
data_high <- data[data$els_slider.1.player.treatment == "high", ]

# Counts of sliders at 50 or range 40-60 by treatment
data <- slider_by_treatment(data)
data_low <- slider_by_treatment(data_low)
data_high <- slider_by_treatment(data_high)

# Ratios of sliders at 50 and in range 40-60 by treatment
data <- slider_ratios(data)
data_low <- slider_ratios(data_low)
data_high <- slider_ratios(data_high)

# Rule-following ratios among moved sliders only
data <- slider_ratios_moved(data)
data_low <- slider_ratios_moved(data_low)
data_high <- slider_ratios_moved(data_high)

# Histograms of sliders at 50 and in range 40-60 by treatment
slider_hist(data, "sliders_at_50_count", "Sliders at 50 - All", "Count of Sliders at 50")
slider_hist(data_low, "sliders_at_50_count", "Sliders at 50 - Low", "Count of Sliders at 50")
slider_hist(data_high, "sliders_at_50_count", "Sliders at 50 - High", "Count of Sliders at 50")
slider_hist(data, "sliders_in_range_count", "Sliders in Range 40-60 - All", "Count of Sliders in Range 40-60")
slider_hist(data_low, "sliders_in_range_count", "Sliders in Range 40-60 - Low", "Count of Sliders in Range 40-60")
slider_hist(data_high, "sliders_in_range_count", "Sliders in Range 40-60 - High", "Count of Sliders in Range 40-60")

# Summary Stats
summary_tables_slider <- create_all_summary_tables(data)

# treatment variation tests
wilcox.test(data_low$sliders_at_50_count, data_high$sliders_at_50_count)
wilcox.test(data_low$sliders_in_range_count, data_high$sliders_in_range_count)
wilcox.test(data_low$ratio_50_all, data_high$ratio_50_all)
wilcox.test(data_low$ratio_50_outside, data_high$ratio_50_outside)
wilcox.test(data_low$ratio_50_inside, data_high$ratio_50_inside)
wilcox.test(data_low$ratio_4060_all, data_high$ratio_4060_all)
wilcox.test(data_low$ratio_4060_outside, data_high$ratio_4060_outside)
wilcox.test(data_low$ratio_4060_inside, data_high$ratio_4060_inside)
wilcox.test(data_low$moved_ratio_50_all, data_high$moved_ratio_50_all)
wilcox.test(data_low$moved_ratio_50_outside, data_high$moved_ratio_50_outside)
wilcox.test(data_low$moved_ratio_50_inside, data_high$moved_ratio_50_inside)

# regressions with worker vars
ratio_50_all_work_exp_regression <- lm(ratio_50_all ~ work_exp + work_type + work_exp_duration, data = regression_data)
summary(ratio_50_all_work_exp_regression)

ratio_50_arbitrary_work_exp_regression <- lm(ratio_50_arbitrary_compliance ~ work_exp + work_type + work_exp_duration, data = regression_data)
summary(ratio_50_arbitrary_work_exp_regression)

ratio_50_active_work_exp_regression <- lm(ratio_50_active_following ~ work_exp + work_type + work_exp_duration, data = regression_data)
summary(ratio_50_active_work_exp_regression)

ratio_50_effort_work_exp_regression <- lm(ratio_50_effort_required ~ work_exp + work_type + work_exp_duration, data = regression_data)
summary(ratio_50_effort_work_exp_regression)

##########################################################################################
# Slider Beliefs
##########################################################################################
# Normative Belief: Personal Normative Belief
norm_pers_all <- slider_beliefs_hist(data, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - All")
norm_pers_low <- slider_beliefs_hist(data_low, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - Low")
norm_pers_high <- slider_beliefs_hist(data_high, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - High")

# Normative Belief: Group Compliance
norm_group_compl_all <- slider_beliefs_hist(data, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - All")
norm_group_compl_low <- slider_beliefs_hist(data_low, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - Low")
norm_group_compl_high <- slider_beliefs_hist(data_high, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - High")

# Normative Belief: Group Non-compliance
norm_group_noncompl_all <- slider_beliefs_hist(data, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - All")
norm_group_noncompl_low <- slider_beliefs_hist(data_low, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - Low")
norm_group_noncompl_high <- slider_beliefs_hist(data_high, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - High")

# Empirical Belief: Compliance
emp_all <- slider_beliefs_emp_hist(data, "Empirical Belief Compliance - All")
emp_low <- slider_beliefs_emp_hist(data_low, "Empirical Belief Compliance - Low")
emp_high <- slider_beliefs_emp_hist(data_high, "Empirical Belief Compliance - High")

# Correlations
belief_vars <- c("slider_beliefs.1.player.normative_belief_personal",
                 "slider_beliefs.1.player.normative_belief_group_compliance", 
                 "slider_beliefs.1.player.normative_belief_group_noncompliance",
                 "slider_beliefs.1.player.empirical_belief_compliance")
cor(data[, belief_vars], use = "complete.obs")

cor.test(data$slider_beliefs.1.player.normative_belief_personal, 
         data$slider_beliefs.1.player.normative_belief_group_compliance)
cor.test(data$slider_beliefs.1.player.normative_belief_personal, 
         data$slider_beliefs.1.player.empirical_belief_compliance)
cor.test(data$slider_beliefs.1.player.normative_belief_group_compliance, 
         data$slider_beliefs.1.player.normative_belief_group_noncompliance)
cor.test(data$slider_beliefs.1.player.empirical_belief_compliance, 
         data$slider_beliefs.1.player.normative_belief_group_compliance)

plot(data$slider_beliefs.1.player.normative_belief_personal, 
     data$slider_beliefs.1.player.normative_belief_group_compliance,
     xlab = "Personal Belief", ylab = "Group Compliance Belief")
plot(data$slider_beliefs.1.player.normative_belief_personal, 
     data$slider_beliefs.1.player.empirical_belief_compliance,
     xlab = "Personal Belief", ylab = "Empirical Belief")
plot(data$slider_beliefs.1.player.normative_belief_group_compliance, 
     data$slider_beliefs.1.player.normative_belief_group_noncompliance,
     xlab = "Group Compliance Belief", ylab = "Group Non-compliance Belief")
plot(data$slider_beliefs.1.player.empirical_belief_compliance, 
     data$slider_beliefs.1.player.normative_belief_group_compliance,
     xlab = "Empirical Belief", ylab = "Group Compliance Belief")

# treatment variation tests
chisq.test(table(data$treatment, data$slider_beliefs.1.player.normative_belief_personal))
chisq.test(table(data$treatment, data$slider_beliefs.1.player.normative_belief_group_compliance))
chisq.test(table(data$treatment, data$slider_beliefs.1.player.normative_belief_group_noncompliance))
wilcox.test(data_low$slider_beliefs.1.player.empirical_belief_compliance, data_high$slider_beliefs.1.player.empirical_belief_compliance)

##########################################################################################
# Moral Foundations 20
##########################################################################################

# Create a bar chart for each of the 20 moral foundations questions (without harm/fairness - just "binding foundations")
likert_barchart_05(data, "moral_foundations_20.1.player.ingroup_lovecountry", "Ob Handlungen aus Liebe zum eigenen Land geschehen")
likert_barchart_05(data, "moral_foundations_20.1.player.ingroup_betray", "Ob jemand etwas getan hat, um seine oder ihre Gruppe zu hintergehen und sie zu betrügen")
likert_barchart_05(data, "moral_foundations_20.1.player.ingroup_history", "Ich bin stolz auf die Geschichte meines Landes.")
likert_barchart_05(data, "moral_foundations_20.1.player.ingroup_family", "Menschen sollten ihren Familienmitgliedern gegenüber loyal sein, auch wenn sie etwas Falsches getan haben.")

likert_barchart_05(data, "moral_foundations_20.1.player.authority_respect", "Ob jemand einen Mangel an Respekt vor Authoritäten gezeigt hat")
likert_barchart_05(data, "moral_foundations_20.1.player.authority_traditions", "Ob jemand sich an die Traditionen der Gesellschaft gehalten hat")
likert_barchart_05(data, "moral_foundations_20.1.player.authority_kidrespect", "Alle Kinder sollten Respekt gegenüber Autoritäten lernen.")
likert_barchart_05(data, "moral_foundations_20.1.player.authority_sexroles", "Männer und Frauen nehmen in der Gesellschaft verschiedene Rollen ein.")

likert_barchart_05(data, "moral_foundations_20.1.player.purity_decency", "Ob jemand gegen Anstand und Rechtschaffenheit verstossen hat")
likert_barchart_05(data, "moral_foundations_20.1.player.purity_disgusting", "Ob jemand etwas Abstoßendes getan hat")
likert_barchart_05(data, "moral_foundations_20.1.player.purity_harmless", "Menschen sollten keine Dinge tun, die abstoßend sind, auch wenn keiner dabei gestört oder verletzt wird.")
likert_barchart_05(data, "moral_foundations_20.1.player.purity_unnatural", "Ich würde bestimmte Taten falsch finden, wenn sie unnatürlich sind.")

likert_barchart_05(data, "moral_foundations_20.1.player.catch_math", "Ob jemand in Mathematik gute Leistungen zeigt")
likert_barchart_05(data, "moral_foundations_20.1.player.catch_good", "Es ist besser gute Dinge zu tun, als schlechte.")

mf20_total_score_barchart(data, "moral_foundations_20.1.player.moral_foundations_score", "Moral Foundations Scores Distribution")

# Cronbach's alpha
mf20_ingroup_items <- data %>% 
  select(
    moral_foundations_20.1.player.ingroup_lovecountry,
    moral_foundations_20.1.player.ingroup_betray,
    moral_foundations_20.1.player.ingroup_history,
    moral_foundations_20.1.player.ingroup_family
  )
mf20_ingroup_alpha <- psych::alpha(mf20_ingroup_items)
print(mf20_ingroup_alpha$total$raw_alpha)

mf20_authority_items <- data %>% 
  select(
    moral_foundations_20.1.player.authority_respect,
    moral_foundations_20.1.player.authority_traditions,
    moral_foundations_20.1.player.authority_kidrespect,
    moral_foundations_20.1.player.authority_sexroles
  )
mf20_authority_alpha <- psych::alpha(mf20_authority_items)
print(mf20_authority_alpha$total$raw_alpha)

mf20_purity_items <- data %>% 
  select(
    moral_foundations_20.1.player.purity_decency,
    moral_foundations_20.1.player.purity_disgusting,
    moral_foundations_20.1.player.purity_harmless,
    moral_foundations_20.1.player.purity_unnatural
  )
mf20_purity_alpha <- psych::alpha(mf20_purity_items)
print(mf20_purity_alpha$total$raw_alpha)

mf20_binding_items <- data %>% 
  select(
    moral_foundations_20.1.player.ingroup_lovecountry,
    moral_foundations_20.1.player.ingroup_betray,
    moral_foundations_20.1.player.ingroup_history,
    moral_foundations_20.1.player.ingroup_family,
    moral_foundations_20.1.player.authority_respect,
    moral_foundations_20.1.player.authority_traditions,
    moral_foundations_20.1.player.authority_kidrespect,
    moral_foundations_20.1.player.authority_sexroles,
    moral_foundations_20.1.player.purity_decency,
    moral_foundations_20.1.player.purity_disgusting,
    moral_foundations_20.1.player.purity_harmless,
    moral_foundations_20.1.player.purity_unnatural
  )
mf20_binding_alpha <- psych::alpha(mf20_binding_items)
print(mf20_binding_alpha$total$raw_alpha)

# time boxplots
time_boxplots(data, "moral_foundations_20.1.player.part_1_time", "Part 1 Time")
time_boxplots(data, "moral_foundations_20.1.player.part_2_time", "Part 2 Time")

# regressions for moral foundations scores
mf20_score_regression <- lm(
  moral_foundations_20.1.player.moral_foundations_score ~
    moral_foundations_20.1.player.part_1_time +
    moral_foundations_20.1.player.part_2_time,
  data = data
)
summary(mf20_score_regression)

# regression of moral foundations score on rule orientation score
mf20_rule_orientation_regression <- lm(
  moral_foundations_20.1.player.moral_foundations_score ~
    rule_orientation.1.player.rule_orientation_score,
  data = data
)
summary(mf20_rule_orientation_regression)

# regression of moral foundations score on demographics
mf20_demographics_regression <- lm(
  moral_foundations_20.1.player.moral_foundations_score ~ 
    demographics.1.player.age + 
    demographics.1.player.gender +
    education_numeric + 
    #    education_other +
    #    education_missing +
    has_management_education +
    demographics.1.player.work_experience_manager +
    demographics.1.player.management_experience +
    demographics.1.player.people_lead,
  data = data_demographics_regression
)
summary(mf20_demographics_regression)

# regression of authority subscale on els and rule orientation and setting both rules
mf20_authority_els_rule_regression <- lm(
  moral_foundations_20.1.player.authority_respect +
    moral_foundations_20.1.player.authority_traditions +
    moral_foundations_20.1.player.authority_kidrespect +
    moral_foundations_20.1.player.authority_sexroles ~
    els_slider.1.player.els_score +
    rule_orientation.1.player.rule_orientation_score +
  data = data
)
summary(mf20_authority_els_rule_regression)

# regression of authority subscale on demographics
mf20_authority_demographics_regression <- lm(
  moral_foundations_20.1.player.authority_respect +
    moral_foundations_20.1.player.authority_traditions +
    moral_foundations_20.1.player.authority_kidrespect +
    moral_foundations_20.1.player.authority_sexroles ~
    demographics.1.player.age + 
    demographics.1.player.gender +
    education_numeric + 
    #    education_other +
    #    education_missing +
  data = data_demographics_regression
)
summary(mf20_authority_demographics_regression)

##########################################################################################
# Rule Orientation
##########################################################################################

likert_barchart_06(data, "rule_orientation.1.player.moral_principles", "die gesetzliche Vorschrift eindeutig gegen meine moralischen Prinzipien verstößt.")
likert_barchart_06(data, "rule_orientation.1.player.unreasonable", "die gesetzliche Vorschrift unangemessene Anforderungen an mich stellt.")
likert_barchart_06(data, "rule_orientation.1.player.expensive", "die Befolgung der gesetzlichen Vorschrift für mich sehr teuer ist.")
likert_barchart_06(data, "rule_orientation.1.player.not_enforced", "die gesetzliche Vorschrift nicht durchgesetzt wird.")
likert_barchart_06(data, "rule_orientation.1.player.others_break", "die meisten meiner direkten Kollegen und/oder Freunde die gesetzliche Vorschrift ebenfalls brechen.")
likert_barchart_06(data, "rule_orientation.1.player.unable", "ich auf irgendeine Weise nicht in der Lage bin, das zu tun, was die gesetzliche Vorschrift verlangt.")
likert_barchart_06(data, "rule_orientation.1.player.others_justified", "die meisten meiner direkten Kollegen und/oder Freunde das Brechen der gesetzlichen Vorschrift für gerechtfertigt halten.")
likert_barchart_06(data, "rule_orientation.1.player.not_know", "ich die gesetzliche Vorschrift nicht kenne.")
likert_barchart_06(data, "rule_orientation.1.player.not_understand", "ich die gesetzliche Vorschrift nicht verstehe.")
likert_barchart_06(data, "rule_orientation.1.player.not_published", "die gesetzliche Vorschrift nicht veröffentlicht wurde.")
likert_barchart_06(data, "rule_orientation.1.player.no_harm", "das Brechen der gesetzlichen Vorschrift niemandem schadet.")
likert_barchart_06(data, "rule_orientation.1.player.benefit", "das Brechen der gesetzlichen Vorschrift mir oder jemand anderem nützt.")

rule_orientation_total_score_barchart(data, "rule_orientation.1.player.rule_orientation_score", "Rule Orientation Scores Distribution")

# cronbach's alpha
rule_orientation_items <- data %>% 
  select(
    rule_orientation.1.player.moral_principles,
    rule_orientation.1.player.unreasonable,
    rule_orientation.1.player.expensive,
    rule_orientation.1.player.not_enforced,
    rule_orientation.1.player.others_break,
    rule_orientation.1.player.unable,
    rule_orientation.1.player.others_justified,
    rule_orientation.1.player.not_know,
    rule_orientation.1.player.not_understand,
    rule_orientation.1.player.not_published,
    rule_orientation.1.player.no_harm,
    rule_orientation.1.player.benefit
  )
rule_orientation_alpha <- psych::alpha(rule_orientation_items)
print(rule_orientation_alpha$total$raw_alpha)

# time boxplots
time_boxplots(data, "rule_orientation.1.player.part_1_time", "Rule Orientation Instructions Time")

# regressions for rule orientation scores
rule_orientation_regression <- lm(
  rule_orientation.1.player.rule_orientation_score ~
    rule_orientation.1.player.part_1_time,
  data = data
)
summary(rule_orientation_regression)

# regression of rule orientation score on demographics
rule_orientation_demographics_regression <- lm(
  rule_orientation.1.player.rule_orientation_score ~ 
    demographics.1.player.age + 
    demographics.1.player.gender +
    education_numeric + 
    #    education_other +
    #    education_missing +
    has_management_education +
    demographics.1.player.work_experience_manager +
    demographics.1.player.management_experience +
    demographics.1.player.people_lead,
  data = data_demographics_regression
)
summary(rule_orientation_demographics_regression)

##########################################################################################
# Demographics
##########################################################################################

age <- summary_statistics_age(data)
gender <- summary_statistics_gender(data)
education <- summary_statistics_education(data)
management_education <- summary_statistics_management_education(data)
work_experience_manager <- summary_statistics_work_experience_manager(data)
management_experience <- summary_statistics_management_experience(data)
people_led <- summary_statistics_people_led(data)
political_orientation <- summary_statistics_political_orientation(data)

# time boxplots
time_boxplots(data, "demographics.1.player.demographics_time", "Demographics Time")