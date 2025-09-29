library(readr)      # Data import (no conflicts)
library(dplyr)      # Data wrangling (loads 'select', 'filter', etc.)
library(ggplot2)    # Plotting (depends on dplyr, no major conflicts)
library(psych)      # Psychometrics (may mask 'describe', but not 'select')
library(stargazer)  # Reporting (no conflicts)
library(mediation)  # Mediation analysis (masks 'mediate' from psych)
library(car)
library(stringr)
library(quantreg)


# read csv
session_1 <- read_csv("raw/all_apps_wide-2025-07-29.csv")
session_2 <- read_csv("raw/all_apps_wide-2025-08-04.csv")
session_3 <- read_csv("raw/all_apps_wide-2025-08-04-2.csv")
session_4 <- read_csv("raw/all_apps_wide-2025-08-04-3.csv")
session_5 <- read_csv("raw/all_apps_wide-2025-08-05.csv")
session_6 <- read_csv("raw/all_apps_wide-2025-08-18.csv")
session_7 <- read_csv("raw/all_apps_wide-2025-08-20.csv")
session_8 <- read_csv("raw/all_apps_wide-2025-08-22.csv")
session_9 <- read_csv("raw/all_apps_wide-2025-08-22-2.csv")
session_10 <- read_csv("raw/all_apps_wide-2025-09-08.csv")
session_11 <- read_csv("raw/all_apps_wide-2025-09-08-2.csv")
session_12 <- read_csv("raw/all_apps_wide-2025-09-15.csv")

data <- dplyr::bind_rows(session_1, session_2, session_3, session_4, session_5, session_6, session_7, session_8, session_9, session_10, session_11, session_12)

# functions
source("_exclude_otree_standard_vars.R")
source("_exclude_manager_vars.R")
source("_data_integrity_checks.R")
source("_stargazer_functions.R")
source("_time_boxplots.R")
source("_likert_barchart_15.R")
source("_likert_barchart_05.R")
source("_likert_barchart_06.R")
source("_els_single_item_table.R")
source("_slider_hist.R")
source("_slider_by_treatment.R")
source("_slider_ratios.R")
source("_slider_ratios_moved.R")
source("_quantile_regression_tables.R")
source("_ability_measures.R")
source("_slider_beliefs_hist.R")
source("_mf20_total_score_barchart.R")
source("_rule_orientation_total_score_barchart.R")
source("_summary_statistics.R")

##########################################################################################
# Data Cleaning and Integrity Checks
##########################################################################################

# exclusions
data <- data[data$els_slider.1.player.worker_attention_check == 4, ]
#data <- data[data$els_slider.1.player.q1_mistakes == 0, ]
#data <- data[data$els_slider.1.player.q2_mistakes == 0, ]
#data <- data[data$els_slider.1.player.slider_comprehension_1_mistakes == 0, ]
#data <- data[data$els_slider.1.player.slider_comprehension_2_mistakes == 0, ]
#data <- data[data$moral_foundations_20.1.player.catch_math <= 2, ]
#data <- data[data$moral_foundations_20.1.player.catch_good >= 3, ]
data <- exclude_otree_standard_vars(data)
data <- exclude_manager_vars(data)

# exclude variables for which every observation is NA (seemingly irrelevant variables)
all_variables <- names(data)
data <- data[, colSums(is.na(data)) < nrow(data)]
excluded_variables <- setdiff(all_variables, names(data))

# remove the person who's PC froze
data <- data[!is.na(data$els_slider.1.player.slider_final_1), ]

# run data integrity checks
#data_integrity_checks(data)

treat_high <- data[data$els_slider.1.player.treatment == "high", ]
treat_low <- data[data$els_slider.1.player.treatment == "low", ]

##########################################################################################
# Format Demographics & Beliefs
##########################################################################################

data$work_experience_binary <- ifelse(
  data$els_slider.1.player.work_experience_worker %in% c("yes_currently", "yes_previous"), 1L,
  ifelse(data$els_slider.1.player.work_experience_worker == "no", 0L, NA_integer_)
)
data$work_duration_more_2_years <- ifelse(
  data$els_slider.1.player.work_experience_duration == "more_2_years", 1L,
  ifelse(is.na(data$els_slider.1.player.work_experience_duration) | data$els_slider.1.player.work_experience_duration == "prefer_not_to_say", NA_integer_, 0L)
)
data$distance_assigned_manager <- abs(data$els_slider.1.player.manager_els - data$els_slider.1.player.els_score)
data$gender_male <- ifelse(data$demographics.1.player.gender == "male", 1, ifelse(data$demographics.1.player.gender == "female", 0, NA))
data$bachelor_or_higher <- ifelse(
  data$demographics.1.player.education %in% c("bachelor_degree", "master_degree"), 1,
  ifelse(data$demographics.1.player.education %in% c("high_school", "middle_school"), 0, NA)
)
data$political_left <- ifelse(
  data$demographics.1.player.political_orientation == "linke", 1,
  ifelse(data$demographics.1.player.political_orientation %in% c(
    "afd", "cdu", "fdp", "gruene", "spd"
  ), 0, NA)
)
data$personal_belief_z <- scale(data$slider_beliefs.1.player.normative_belief_personal)
data$group_compliance_z <- scale(data$slider_beliefs.1.player.normative_belief_group_compliance)
data$group_noncompliance_z <- scale(data$slider_beliefs.1.player.normative_belief_group_noncompliance)
data$empirical_belief_z <- scale(data$slider_beliefs.1.player.empirical_belief_compliance)

# binary versions of beliefs (whether appropriate or not)
data$group_compliance_binary <- ifelse(data$slider_beliefs.1.player.normative_belief_group_compliance > 0, 1,
                                         ifelse(data$slider_beliefs.1.player.normative_belief_group_compliance < 0, 0, NA))
data$group_noncompliance_binary <- ifelse(data$slider_beliefs.1.player.normative_belief_group_noncompliance > 0, 1,
                                            ifelse(data$slider_beliefs.1.player.normative_belief_group_noncompliance < 0, 0, NA))

##########################################################################################
# ELS Score
##########################################################################################

# relative table for work_experience_worker
work_exp_levels <- c("yes_currently", "yes_previous", "no")
work_exp_levels <- work_exp_levels[!is.na(work_exp_levels)]
work_exp_table <- data.frame(
  Low = round(sapply(work_exp_levels, function(x) mean(treat_low$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4),
  High = round(sapply(work_exp_levels, function(x) mean(treat_high$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4),
  All = round(sapply(work_exp_levels, function(x) mean(data$els_slider.1.player.work_experience_worker == x, na.rm = TRUE)), 4)
)
rownames(work_exp_table) <- c("Current exp", "Previous exp", "No exp")
print(work_exp_table)
work_experience_summary(work_exp_table, "work_experience_table.tex", title = "Work Experience by Treatment", label = "tab:work_experience", 
                        row_labels = row_labels, col_labels = col_labels,
                        notes = "Proportions of work experience categories by treatment.")

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
  All = round(sapply(work_type_vars, function(x) mean(data[[x]], na.rm = TRUE)), 4)
)
rownames(work_type_table) <- c("Apprenticeship", "Internship", "Working Student", "Part-time Job", "Full-time Job", "Volunteer Work")
print(work_type_table)
work_experience_summary(work_type_table, "work_type_table.tex", title = "Work Types by Treatment", label = "tab:work_type", 
                        row_labels = row_labels, col_labels = col_labels,
                        notes = '\\parbox{0.7\\textwidth}{Proportions of work types by treatment given they did not select "No exp" in the work experience question. Note that participants could select multiple work types.}')

# relative table for work_experience_duration
work_exp_duration_levels <- c("more_2_years", "1_2_years", "6_12_months", "3_6_months", "less_3_months", "prefer_not_to_say")
work_exp_duration_levels <- work_exp_duration_levels[!is.na(work_exp_duration_levels)]
work_exp_duration_table <- data.frame(
  Low = round(sapply(work_exp_duration_levels, function(x) mean(treat_low$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4),
  High = round(sapply(work_exp_duration_levels, function(x) mean(treat_high$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4),
  All = round(sapply(work_exp_duration_levels, function(x) mean(data$els_slider.1.player.work_experience_duration == x, na.rm = TRUE)), 4)
)
rownames(work_exp_duration_table) <- c("More than 2 years", "1-2 years", "6-12 months", "3-6 months", "Less than 3 months", "Prefer not to say")
print(work_exp_duration_table)
work_experience_summary(work_exp_duration_table, "work_experience_duration_table.tex", title = "Work Experience Duration by Treatment", label = "tab:work_experience_duration", 
                        row_labels = row_labels, col_labels = col_labels,
                        notes = '\\parbox{0.7\\textwidth}{Proportions of work experience duration categories by treatment given they did not select "No exp" in the work experience question.}')

# relative distribution of the ELS score from 10 to 50
ggplot(data, aes(x = els_slider.1.player.els_score)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "#4A90E2", color = "white", alpha = 0.8) + 
  scale_x_continuous(breaks = seq(10, 50, by = 5), limits = c(10, 51), 
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 0.18, by = 0.02), limits = c(0, 0.18),
                     expand = expansion(mult = 0.02)) +
  labs(x = "ELS Score", y = "Density") +
  geom_vline(aes(xintercept = median(els_slider.1.player.els_score, na.rm = TRUE)), 
             color = "black", linetype = "dashed", linewidth = 0.8) +
  theme_classic()
ggsave("../Masterthesis-Overleaf/Images/ELS_distribution_full_sample.png", width = 6, height = 4, dpi = 400)

summary(data$els_slider.1.player.els_score)
quantile(data$els_slider.1.player.els_score, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
sd(data$els_slider.1.player.els_score, na.rm = TRUE)

mean(treat_low$els_slider.1.player.els_score, na.rm = TRUE)
mean(treat_high$els_slider.1.player.els_score, na.rm = TRUE)

mean(data[data$els_slider.1.player.work_experience_worker == "yes_currently", ]$els_slider.1.player.els_score, na.rm = TRUE)
mean(data[data$els_slider.1.player.work_experience_worker == "yes_previous", ]$els_slider.1.player.els_score, na.rm = TRUE)
mean(data[data$els_slider.1.player.work_experience_worker == "no", ]$els_slider.1.player.els_score, na.rm = TRUE)

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
  dplyr::select(
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
  els_slider.1.player.els_score ~ work_experience_binary,
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
  els_slider.1.player.els_score ~ work_duration_more_2_years,
  data = data
)
summary(els_score_work_experience_duration_regression)

# regression of els_score on treatment
els_score_treatment_regression <- lm(
  els_slider.1.player.els_score ~ els_slider.1.player.treatment,
  data = data
)
summary(els_score_treatment_regression)

# regression of els_score on normative_belief_personal, normative_belief_group_compliance, normative_belief_group_noncompliance, and empirical_belief_compliance
els_score_beliefs_regression <- lm(
  els_slider.1.player.els_score ~ 
    personal_belief_z +
    group_compliance_z +
    group_noncompliance_z +
    empirical_belief_z ,
  data = data
)
summary(els_score_beliefs_regression)
# Note: Workers of unethical (real) managers expect others to comply to the rule more likely (p = 0.0583) - not anymore (p = 0.167)

# Scatter plot of els_score and empirical_belief_compliance with regression line
ggplot(data, aes(x = slider_beliefs.1.player.empirical_belief_compliance, 
                 y = els_slider.1.player.els_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "ELS Score vs Empirical Belief Compliance",
       x = "Empirical Belief Compliance",
       y = "ELS Score") +
  theme_minimal()

# regression of els_score on moral_foundations_score, rule_orientation_score
els_score_mf20_rule_regression <- lm(
  els_slider.1.player.els_score ~ 
    moral_foundations_20.1.player.moral_foundations_score +
    rule_orientation.1.player.rule_orientation_score,
  data = data
)
summary(els_score_mf20_rule_regression)
# Note: Workers of more ethical managers have lower rule orientation score (p = 0.0903)

# regression of els_score on mother_birth_year
els_score_mother_birth_year_regression <- lm(
  els_slider.1.player.els_score ~ 
    honesty.1.player.mother_birth_year,
  data = data
)
summary(els_score_mother_birth_year_regression)

# regression of els_score on demographics
els_score_demographics_regression <- lm(
  els_slider.1.player.els_score ~ 
    demographics.1.player.age +
    gender_male +
    bachelor_or_higher +
    political_left,
  data = data
)
summary(els_score_demographics_regression)

# Chi-square test: Work exp across treatments
work_exp_treatment_chisq <- chisq.test(
  table(data$els_slider.1.player.work_experience_worker, 
        data$els_slider.1.player.treatment)
)
print(work_exp_treatment_chisq)

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
print(work_type_treatment_chisq)

# Chi-square test: Work experience duration across treatments
work_exp_duration_treatment_chisq <- chisq.test(
  table(data$work_duration_more_2_years, 
        data$els_slider.1.player.treatment)
)
print(work_exp_duration_treatment_chisq)

# Mann-Whitney U test for ELS scores across treatments
els_score_treatment_mann_whitney <- wilcox.test(
  els_slider.1.player.els_score ~ els_slider.1.player.treatment,
  data = data,
  exact = FALSE
)
print(els_score_treatment_mann_whitney)

# Kruskal-Wallis test for ELS scores across work_experience
els_score_work_experience_kruskal <- kruskal.test(
  els_slider.1.player.els_score ~ els_slider.1.player.work_experience_worker,
  data = data
)
print(els_score_work_experience_kruskal)

# Mann-Whitney U test for distance_assigned_manager across treatments
wilcox.test(
  distance_assigned_manager ~ els_slider.1.player.treatment,
  data = data,
  exact = FALSE
)

##########################################################################################
# Slider Task
##########################################################################################

data_low <- data[data$els_slider.1.player.treatment == "low", ]
data_high <- data[data$els_slider.1.player.treatment == "high", ]

# ability measures
data <- ability_measures(data)
data$ability_sliders_50_z <- scale(data$ability_sliders_50)
data$slider_50_difficulty_z <- scale(data$els_slider.1.player.slider_50_difficulty)

data_low <- ability_measures(data_low)
data_high <- ability_measures(data_high)

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
ggplot(data, aes(x = sliders_at_50_count)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "#4A90E2", color = "white", alpha = 0.8) + 
  scale_x_continuous(breaks = seq(0, 48, by = 6), limits = c(-1, 49), 
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 0.1, by = 0.02), limits = c(0, 0.1),
                     expand = expansion(mult = 0.02)) +
  labs(x = "Count of sliders at 50", y = "Density") +
  geom_vline(aes(xintercept = median(sliders_at_50_count, na.rm = TRUE)), 
             color = "black", linetype = "dashed", linewidth = 0.8) +
  theme_classic()
ggsave("../Masterthesis-Overleaf/Images/sliders_at_50_full_sample.png", width = 6, height = 4, dpi = 400)

# Histograms of moved sliders at 50 for both treatments

plot_data <- data %>%
  mutate(bin = cut(moved_ratio_50_all, 
                   breaks = seq(0, 1, by = 0.1), 
                   include.lowest = TRUE, 
                   right = TRUE)) %>%
  count(els_slider.1.player.treatment, bin, name = "n") %>%
  group_by(els_slider.1.player.treatment) %>%
  mutate(
    N_treat = sum(n),
    prop = n / N_treat,
    se = sqrt(prop * (1 - prop) / N_treat),
    ci_low = pmax(prop - 1.96 * se, 0),
    ci_high = pmin(prop + 1.96 * se, 1)
  ) %>%
  mutate(els_slider.1.player.treatment = factor(els_slider.1.player.treatment,
                                                levels = c("low", "high"))) %>%
  ungroup()

ggplot(plot_data, aes(x = bin, y = prop, fill = els_slider.1.player.treatment)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8, color = "white", alpha = 0.9) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high, group = els_slider.1.player.treatment),
    position = position_dodge(width = 0.9),
    width = 0.2,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = 0.02),
                     breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  scale_fill_manual(values = c("low" = "#4A90E2", "high" = "#E94A2B")) +
  labs(x = "Ratio of sliders moved to the value 50 (binned, %)", y = "Share within treatment", fill = "Treatment") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.9),
        legend.justification = c(0, 1))

ggsave("../Masterthesis-Overleaf/Images/moved_sliders_50_all_treatment_bins.png", width = 6, height = 4, dpi = 400)


slider_hist(data_low, "sliders_at_50_count", "Sliders at 50 - Low", "Count of Sliders at 50")
slider_hist(data_high, "sliders_at_50_count", "Sliders at 50 - High", "Count of Sliders at 50")
slider_hist(data, "sliders_in_range_count", "Sliders in Range 40-60 - All", "Count of Sliders in Range 40-60")
slider_hist(data_low, "sliders_in_range_count", "Sliders in Range 40-60 - Low", "Count of Sliders in Range 40-60")
slider_hist(data_high, "sliders_in_range_count", "Sliders in Range 40-60 - High", "Count of Sliders in Range 40-60")

# Summary Stats
summary_tables_slider <- create_all_summary_tables(data)

# treatment variation tests (as in the PAP)
#i)
wilcox.test(data_low$moved_ratio_50_all, data_high$moved_ratio_50_all)
ks.test(data_low$moved_ratio_50_all, data_high$moved_ratio_50_all)
fligner.test(moved_ratio_50_all ~ els_slider.1.player.treatment, data = data)
#ii)
wilcox.test(data_low$sliders_at_50_count, data_high$sliders_at_50_count)
ks.test(data_low$sliders_at_50_count, data_high$sliders_at_50_count)
fligner.test(sliders_at_50_count ~ els_slider.1.player.treatment, data = data)

# quantile regression
taus <- c(0.1, 0.3, 0.5, 0.6, 0.7)
quantile_regression_models <- lapply(taus, function(tau) {
  rq(moved_ratio_50_all ~ els_slider.1.player.treatment + 
       ability_sliders_50_z +
       slider_50_difficulty_z, tau = tau, data = data)
})
rank_results <- lapply(seq_along(quantile_regression_models), function(i) {
  model <- quantile_regression_models[[i]]
  tau <- taus[i]
  rank_summary <- summary(model, se = "rank")
  list(
    tau = tau,
    summary = rank_summary
  )
})
quantile_regression_tables(rank_results, taus)

# more treatment variation tests
wilcox.test(data_low$sliders_in_range_count, data_high$sliders_in_range_count)
wilcox.test(data_low$sliders_at_50_given_4060, data_high$sliders_at_50_given_4060)
wilcox.test(data_low$ratio_50_all, data_high$ratio_50_all)
wilcox.test(data_low$ratio_50_outside, data_high$ratio_50_outside)
wilcox.test(data_low$ratio_50_inside, data_high$ratio_50_inside)
wilcox.test(data_low$ratio_4060_all, data_high$ratio_4060_all)
wilcox.test(data_low$ratio_4060_outside, data_high$ratio_4060_outside)
wilcox.test(data_low$ratio_4060_inside, data_high$ratio_4060_inside)
wilcox.test(data_low$moved_ratio_50_outside, data_high$moved_ratio_50_outside)
wilcox.test(data_low$moved_ratio_50_inside, data_high$moved_ratio_50_inside)

# regression with work experience
moved_ratio_50_work_exp_regression <- lm(moved_ratio_50_all ~ 
                                           work_experience_binary +
                                           els_slider.1.player.els_score +
                                           ability_sliders_50_z +
                                           slider_50_difficulty_z,
                                           data = data)
summary(moved_ratio_50_work_exp_regression)

sliders_at_50_count_work_exp_regression <- lm(sliders_at_50_count ~ 
                                                 work_experience_binary +
                                                 els_slider.1.player.els_score +
                                                 ability_sliders_50_z +
                                                 slider_50_difficulty_z,
                                               data = data)
summary(sliders_at_50_count_work_exp_regression)

moved_ratio_50_work_exp(moved_ratio_50_work_exp_regression, sliders_at_50_count_work_exp_regression,
                        title = "Regression of dependent variables on work experience and ELS, controlling for ability and difficulty",
                        covariate.labels = c("Work Experience", "ELS Score", "Ability (z)", "Difficulty (z)"))



# regressions with worker vars
moved_ratio_50_all_work_regression <- lm(moved_ratio_50_all ~ 
                                         work_duration_more_2_years +
                                         els_slider.1.player.work_type_apprenticeship +
                                         els_slider.1.player.work_type_internship +
                                         els_slider.1.player.work_type_working_student +
                                         els_slider.1.player.work_type_part_time_job +
                                         els_slider.1.player.work_type_full_time_job +
                                         els_slider.1.player.work_type_volunteer_work +
                                         ability_sliders_50_z +
                                         slider_50_difficulty_z, 
                                       data = data)
summary(moved_ratio_50_all_work_regression)

sliders_at_50_count_all_work_regression <- lm(sliders_at_50_count ~ 
                                            work_duration_more_2_years +
                                            els_slider.1.player.work_type_apprenticeship +
                                            els_slider.1.player.work_type_internship +
                                            els_slider.1.player.work_type_working_student +
                                            els_slider.1.player.work_type_part_time_job +
                                            els_slider.1.player.work_type_full_time_job +
                                            els_slider.1.player.work_type_volunteer_work +
                                            ability_sliders_50_z +
                                            slider_50_difficulty_z, 
                                          data = data)
summary(sliders_at_50_count_all_work_regression)

moved_ratio_50_all_work(moved_ratio_50_all_work_regression, sliders_at_50_count_all_work_regression,
                        title = "Regression of dependent variables on work types and duration, controlling for ability and difficulty",
                        covariate.labels = c("Work Duration > 2 years", "Apprenticeship", "Internship", "Working Student", "Part-time Job", "Full-time Job", "Volunteer Work", "Ability (z)", "Difficulty (z)"))

# regressions with treatment, controlling for difficulty, and skill
moved_ratio_50_all_treatment_basic_regression <- lm(moved_ratio_50_all ~ els_slider.1.player.treatment + 
                                                       ability_sliders_50_z + 
                                                       slider_50_difficulty_z,
                                                data = data)
summary(moved_ratio_50_all_treatment_basic_regression)

sliders_at_50_count_treatment_basic_regression <- lm(sliders_at_50_count ~ els_slider.1.player.treatment + 
                                                        ability_sliders_50_z +
                                                        slider_50_difficulty_z,
                                                   data = data)
summary(sliders_at_50_count_treatment_basic_regression)
treatment_basic_regression(moved_ratio_50_all_treatment_basic_regression, sliders_at_50_count_treatment_basic_regression,
                                title = "Regression of dependent variables on treatment, controlling for ability and difficulty",
                                covariate.labels = c("Treatment (Low)", "Ability (z)", "Difficulty (z)"))

# regressions with treatment, controlling for difficulty, skill, and sociodemographics
moved_ratio_50_treatment_demographics_regression <- lm(moved_ratio_50_all ~ els_slider.1.player.treatment + 
                                          demographics.1.player.age + 
                                          gender_male +
                                          bachelor_or_higher +
                                          political_left +
                                          ability_sliders_50_z +
                                          slider_50_difficulty_z,
                                        data = data)
summary(moved_ratio_50_all_treatment_regression)

sliders_at_50_count_treatment_demographics_regression <- lm(sliders_at_50_count ~ els_slider.1.player.treatment + 
                                           demographics.1.player.age + 
                                           gender_male +
                                           bachelor_or_higher +
                                           political_left +
                                           ability_sliders_50_z +
                                           slider_50_difficulty_z,
                                         data = data)
summary(sliders_at_50_count_treatment_regression)

treatment_demographics(moved_ratio_50_treatment_demographics_regression, sliders_at_50_count_treatment_demographics_regression,
                                title = "Regression of dependent variables on treatment and demographics, controlling for ability and difficulty",
                                covariate.labels = c("Treatment (Low)", "Age", "Male", "Bachelor or Higher", "Political Left", "Ability (z)", "Difficulty (z)"))

# regressions with treatment, beliefs, controlling for difficulty and skill
moved_ratio_50_all_belief_regression <- lm(moved_ratio_50_all ~ els_slider.1.player.treatment +
                                        personal_belief_z +
                                        group_compliance_z +
                                        group_noncompliance_z +
                                        empirical_belief_z +
                                        ability_sliders_50_z +
                                        slider_50_difficulty_z,
                                      data = data)
summary(moved_ratio_50_all_belief_regression)

sliders_at_50_count_belief_regression <- lm(sliders_at_50_count ~ els_slider.1.player.treatment +
                                          personal_belief_z +
                                          group_compliance_z +
                                          group_noncompliance_z +
                                          empirical_belief_z +
                                          ability_sliders_50_z +
                                          slider_50_difficulty_z,
                                        data = data)
summary(sliders_at_50_count_belief_regression)

all_beliefs_without_interaction(moved_ratio_50_all_belief_regression, sliders_at_50_count_belief_regression,
                        title = "Regression of dependent variables on treatment and beliefs, controlling for ability and difficulty",
                        covariate.labels = c(
                          "Treatment (Low)", 
                          "Personal Belief (z)",
                          "Group Compliance (z)",
                          "Group Noncompliance (z)",
                          "Empirical Belief (z)",
                          "Ability (z)", 
                          "Difficulty (z)"
                        ))

# additionally control for interaction effects between treatment and beliefs
moved_ratio_50_all_interaction_belief_regression <- lm(moved_ratio_50_all ~
                                        els_slider.1.player.treatment *
                                        (personal_belief_z +
                                        group_compliance_z +
                                        group_noncompliance_z +
                                        empirical_belief_z) +
                                        ability_sliders_50_z + 
                                        slider_50_difficulty_z,
                                      data = data)
summary(moved_ratio_50_all_interaction_belief_regression)

sliders_at_50_count_interaction_belief_regression <- lm(sliders_at_50_count ~
                                               els_slider.1.player.treatment *
                                               (personal_belief_z +
                                                group_compliance_z +
                                                group_noncompliance_z +
                                                empirical_belief_z) +
                                                ability_sliders_50_z + 
                                                slider_50_difficulty_z,
                                             data = data)
summary(sliders_at_50_count_interaction_belief_regression)

stargazer(moved_ratio_50_all_interaction_belief_regression, 
          sliders_at_50_count_interaction_belief_regression,
          type = "latex", 
          title = "Regression Results: Rule-following on interaction with beliefs",
          covariate.labels = c(
            "Treatment", 
            "Personal Belief (z)",
            "Group Compliance (z)",
            "Group Noncompliance (z)",
            "Empirical Belief (z)",
            "Treatment x Personal Belief (z)",
            "Treatment x Group Compliance (z)",
            "Treatment x Group Noncompliance (z)",
            "Treatment x Empirical Belief (z)",
            "Difficulty (z)",
            "Ability (z)"
          ),
          label = "tab:interaction_belief_regression",
          out = "../Masterthesis-Overleaf/4_Results/interaction_belief_regression.tex")

# single belief regressions
moved_ratio_50_all_personal_belief_regression <- lm(moved_ratio_50_all ~ 
                                                 els_slider.1.player.treatment +
                                                 slider_beliefs.1.player.normative_belief_personal +
                                                 ability_sliders_50_z + 
                                                 slider_50_difficulty_z,
                                               data = data)
summary(moved_ratio_50_all_personal_belief_regression)

sliders_at_50_count_personal_belief_regression <- lm(sliders_at_50_count ~ 
                                               els_slider.1.player.treatment +
                                               slider_beliefs.1.player.normative_belief_personal +
                                               ability_sliders_50_z + 
                                               slider_50_difficulty_z,
                                             data = data)
summary(sliders_at_50_count_personal_belief_regression)
personal_belief_regression(moved_ratio_50_all_personal_belief_regression, sliders_at_50_count_personal_belief_regression,
                          title = "Personal Belief Regression",
                          covariate.labels = c("Treatment (Low)", "Personal Belief (z)", "Ability (z)", "Difficulty (z)"))

moved_ratio_50_all_group_compliance_regression <- lm(moved_ratio_50_all ~ 
                                                     els_slider.1.player.treatment +
                                                     group_compliance_binary +
                                                     ability_sliders_50_z + 
                                                     slider_50_difficulty_z,
                                                   data = data)
summary(moved_ratio_50_all_group_compliance_regression)

sliders_at_50_count_group_compliance_regression <- lm(sliders_at_50_count ~ 
                                                     els_slider.1.player.treatment +
                                                     group_compliance_binary +
                                                     ability_sliders_50_z + 
                                                     slider_50_difficulty_z,
                                                   data = data)
summary(sliders_at_50_count_group_compliance_regression)
group_compliance_regression(moved_ratio_50_all_group_compliance_regression, sliders_at_50_count_group_compliance_regression,
                          title = "Group Compliance Regression",
                          covariate.labels = c("Treatment (Low)", "Group Compliance", "Ability (z)", "Difficulty (z)"))

moved_ratio_50_all_group_noncompliance_regression <- lm(moved_ratio_50_all ~ 
                                                     els_slider.1.player.treatment +
                                                     group_noncompliance_binary +
                                                     ability_sliders_50_z + 
                                                     slider_50_difficulty_z,
                                                   data = data)
summary(moved_ratio_50_all_group_noncompliance_regression)

sliders_at_50_count_group_noncompliance_regression <- lm(sliders_at_50_count ~ 
                                                     els_slider.1.player.treatment +
                                                     group_noncompliance_binary +
                                                     ability_sliders_50_z + 
                                                     slider_50_difficulty_z,
                                                   data = data)
summary(sliders_at_50_count_group_noncompliance_regression)
group_noncompliance_regression(moved_ratio_50_all_group_noncompliance_regression, sliders_at_50_count_group_noncompliance_regression,
                          title = "Group Non-compliance Regression",
                          covariate.labels = c("Treatment (Low)", "Group Non-compliance", "Ability (z)", "Difficulty (z)"))

moved_ratio_50_all_empirical_belief_regression <- lm(moved_ratio_50_all ~ 
                                                       els_slider.1.player.treatment +
                                                       slider_beliefs.1.player.empirical_belief_compliance +
                                                       ability_sliders_50_z + 
                                                       slider_50_difficulty_z,
                                                     data = data)
summary(moved_ratio_50_all_empirical_belief_regression)

sliders_at_50_count_empirical_belief_regression <- lm(sliders_at_50_count ~ 
                                                       els_slider.1.player.treatment +
                                                       slider_beliefs.1.player.empirical_belief_compliance +
                                                       ability_sliders_50_z + 
                                                       slider_50_difficulty_z,
                                                     data = data)
summary(sliders_at_50_count_empirical_belief_regression)
empirical_belief_regression(moved_ratio_50_all_empirical_belief_regression, sliders_at_50_count_empirical_belief_regression,
                          title = "Descriptive Belief Regression",
                          covariate.labels = c("Treatment (Low)", "Descriptive Belief (z)", "Ability (z)", "Difficulty (z)"))

# regression of ratio_50_all on els_score, moral_foundations_score, rule_orientation_score, controlling for treatment, difficulty, and skill
moved_ratio_50_all_mf20_rule_regression <- lm(moved_ratio_50_all ~ 
                                            els_slider.1.player.treatment *
                                            (els_slider.1.player.els_score + 
                                            moral_foundations_20.1.player.moral_foundations_score + 
                                            rule_orientation.1.player.rule_orientation_score) + 
                                            ability_sliders_50_z + 
                                            slider_50_difficulty_z,
                                          data = data)
summary(moved_ratio_50_all_mf20_rule_regression)

sliders_at_50_count_mf20_rule_regression <- lm(sliders_at_50_count ~ 
                                               els_slider.1.player.treatment *
                                               (els_slider.1.player.els_score + 
                                               moral_foundations_20.1.player.moral_foundations_score + 
                                               rule_orientation.1.player.rule_orientation_score) + 
                                               ability_sliders_50_z + 
                                               slider_50_difficulty_z,
                                             data = data)
summary(sliders_at_50_count_mf20_rule_regression)

mf_ros_regression(moved_ratio_50_all_mf20_rule_regression, sliders_at_50_count_mf20_rule_regression,
                        title = "Regression of dependent variables on ELS, MF20, and ROS, controlling for treatment, ability, and difficulty",
                        covariate.labels = c(
                          "Treatment (Low)", 
                          "ELS Score",
                          "Moral Foundations Score",
                          "Rule Orientation Score",
                          "Treatment x ELS Score",
                          "Treatment x Moral Foundations Score",
                          "Treatment x Rule Orientation Score",
                          "Ability (z)", 
                          "Difficulty (z)"
                        ))

# regression of ratio_50_all on mother_birth_year, controlling for difficulty and skill
moved_ratio_50_all_mother_birth_year_regression <- lm(moved_ratio_50_all ~ 
                                                   els_slider.1.player.treatment *          
                                                   honesty.1.player.mother_birth_year + 
                                                   ability_sliders_50_z + 
                                                   slider_50_difficulty_z,
                                                 data = data)
summary(moved_ratio_50_all_mother_birth_year_regression)

sliders_at_50_count_mother_birth_year_regression <- lm(sliders_at_50_count ~ 
                                                      els_slider.1.player.treatment *          
                                                      honesty.1.player.mother_birth_year + 
                                                      ability_sliders_50_z + 
                                                      slider_50_difficulty_z,
                                                      data = data)
summary(sliders_at_50_count_mother_birth_year_regression)

##########################################################################################
# Slider Beliefs
##########################################################################################
# Normative Belief: Personal Normative Belief
norm_pers_all <- slider_beliefs_pers_hist_ggplot(data, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - All", c("Never follow rule", "Rarely follow rule", "Often follow rule", "Always follow rule"))
ggsave("../Masterthesis-Overleaf/Images/normative_belief_personal_full_sample.png", width = 6, height = 4, dpi = 400)
norm_pers_low <- slider_beliefs_pers_hist_ggplot(data_low, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - Low", c("Never follow rule", "Rarely follow rule", "Often follow rule", "Always follow rule"))
norm_pers_high <- slider_beliefs_pers_hist_ggplot(data_high, "slider_beliefs.1.player.normative_belief_personal", "Personal Normative Belief - High", c("Never follow rule", "Rarely follow rule", "Often follow rule", "Always follow rule"))

# Normative Belief: Group Compliance
norm_group_compl_all <- slider_beliefs_group_hist_ggplot(data, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - All", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))
ggsave("../Masterthesis-Overleaf/Images/normative_belief_group_compliance_full_sample.png", width = 6, height = 4, dpi = 400)
norm_group_compl_low <- slider_beliefs_group_hist_ggplot(data_low, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - Low", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))
norm_group_compl_high <- slider_beliefs_group_hist_ggplot(data_high, "slider_beliefs.1.player.normative_belief_group_compliance", "Group Evaluation of Compliance - High", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))

# Normative Belief: Group Non-compliance
norm_group_noncompl_all <- slider_beliefs_group_hist_ggplot(data, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - All", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))
ggsave("../Masterthesis-Overleaf/Images/normative_belief_group_noncompliance_full_sample.png", width = 6, height = 4, dpi = 400)
norm_group_noncompl_low <- slider_beliefs_group_hist_ggplot(data_low, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - Low", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))
norm_group_noncompl_high <- slider_beliefs_group_hist_ggplot(data_high, "slider_beliefs.1.player.normative_belief_group_noncompliance", "Group Evaluation of Non-compliance - High", c("Very socially\ninappropriate", "Somewhat socially\ninappropriate", "Somewhat socially\nappropriate", "Very socially\nappropriate"))

# Empirical Belief: Compliance
emp_all <- slider_beliefs_emp_hist_ggplot(data, "Empirical Belief Compliance - All")
ggsave("../Masterthesis-Overleaf/Images/empirical_belief_compliance_full_sample.png", width = 6, height = 4, dpi = 400)
emp_low <- slider_beliefs_emp_hist_ggplot(data_low, "Empirical Belief Compliance - Low")
emp_high <- slider_beliefs_emp_hist_ggplot(data_high, "Empirical Belief Compliance - High")

# Plotting approval/disapproval ratings on ratio_50_inside
bin_edges <- list(c(0,20), c(21,40), c(41,60), c(61,80), c(81,100))
midpoints <- c(10, 30, 50, 70, 90)
range_labels <- c("0–20", "21–40", "41–60", "61–80", "81–100")

means_low <- sapply(bin_edges, function(b)
  mean(data_low$moved_ratio_50_all[
    data_low$slider_beliefs.1.player.empirical_belief_compliance >= b[1] &
      data_low$slider_beliefs.1.player.empirical_belief_compliance <= b[2]
  ], na.rm = TRUE)
)
means_high <- sapply(bin_edges, function(b)
  mean(data_high$moved_ratio_50_all[
    data_high$slider_beliefs.1.player.empirical_belief_compliance >= b[1] &
      data_high$slider_beliefs.1.player.empirical_belief_compliance <= b[2]
  ], na.rm = TRUE)
)

df_plot <- data.frame(
  Empirical_Beliefs = rep(midpoints, 2),
  Range = factor(rep(range_labels, 2), levels = range_labels),
  Moved_ratio_50_all = c(means_low, means_high),
  Treatment = rep(c("Low", "High"), each = 5)
)

ggplot(df_plot, aes(x = Empirical_Beliefs, y = Moved_ratio_50_all, color = Treatment, group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = midpoints,
    labels = range_labels,
    name = "Empirical Beliefs"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  ylab("Ratio 50 Inside") +
  theme_classic()

# Correlations
belief_vars <- c("slider_beliefs.1.player.normative_belief_personal",
                 "slider_beliefs.1.player.normative_belief_group_compliance", 
                 "slider_beliefs.1.player.normative_belief_group_noncompliance",
                 "slider_beliefs.1.player.empirical_belief_compliance",
                 "moved_ratio_50_all")
cor(data[, belief_vars], use = "complete.obs")

# regression of empirical belief on treatment, els_score, moral_foundations_score, rule_orientation_score, and mother_birth_year controlling for difficulty and skill
personal_belief_questionnaires_regression <- lm(slider_beliefs.1.player.normative_belief_personal ~ 
                                    els_slider.1.player.treatment +
                                    moved_ratio_50_all +
                                    els_slider.1.player.els_score +
                                    moral_foundations_20.1.player.moral_foundations_score +
                                    rule_orientation.1.player.rule_orientation_score +
                                    honesty.1.player.mother_birth_year +
                                    ability_sliders_50_z + 
                                    slider_50_difficulty_z,
                                  data = data)
summary(personal_belief_questionnaires_regression)

group_compliance_questionnaires_regression <- lm(slider_beliefs.1.player.normative_belief_group_compliance ~ 
                                    els_slider.1.player.treatment +
                                    moved_ratio_50_all +
                                    els_slider.1.player.els_score +
                                    moral_foundations_20.1.player.moral_foundations_score +
                                    rule_orientation.1.player.rule_orientation_score +
                                    honesty.1.player.mother_birth_year +
                                    ability_sliders_50_z + 
                                    slider_50_difficulty_z,
                                  data = data)
summary(group_compliance_questionnaires_regression)

group_noncompliance_questionnaires_regression <- lm(slider_beliefs.1.player.normative_belief_group_noncompliance ~ 
                                    els_slider.1.player.treatment +
                                    moved_ratio_50_all +
                                    els_slider.1.player.els_score +
                                    moral_foundations_20.1.player.moral_foundations_score +
                                    rule_orientation.1.player.rule_orientation_score +
                                    honesty.1.player.mother_birth_year +
                                    ability_sliders_50_z + 
                                    slider_50_difficulty_z,
                                  data = data)
summary(group_noncompliance_questionnaires_regression)

empirical_belief_questionnaires_regression <- lm(slider_beliefs.1.player.empirical_belief_compliance ~ 
                                    els_slider.1.player.treatment +
                                    moved_ratio_50_all +
                                    els_slider.1.player.els_score +
                                    moral_foundations_20.1.player.moral_foundations_score +
                                    rule_orientation.1.player.rule_orientation_score +
                                    honesty.1.player.mother_birth_year +
                                    ability_sliders_50_z + 
                                    slider_50_difficulty_z,
                                  data = data)
summary(empirical_belief_questionnaires_regression)

all_variables_on_beliefs(personal_belief_questionnaires_regression,
                                 group_compliance_questionnaires_regression,
                                 group_noncompliance_questionnaires_regression,
                                 empirical_belief_questionnaires_regression,
                        title = "Regression of beliefs on treatment, ELS, MF20, ROS, and Mother's Birth Year, controlling for ability and difficulty",
                        covariate.labels = c(
                          "Treatment (Low)", 
                          "Moved Sliders (50)",
                          "ELS Score",
                          "Moral Foundations Score",
                          "Rule Orientation Score",
                          "Mother's Birth Year",
                          "Ability (z)", 
                          "Difficulty (z)"
                        ))

# regression of empirical belief on demographics controlling for difficulty and skill
empirical_belief_demographics_regression <- lm(slider_beliefs.1.player.empirical_belief_compliance ~ 
                                                 demographics.1.player.age + 
                                                 gender_male +
                                                 bachelor_or_higher +
                                                 political_left,
                                               data = data)
summary(empirical_belief_demographics_regression)
                                               

# sample split regressions
moved_ratio_50_all_personal_belief_regression_low <- lm(moved_ratio_50_all ~ 
                                                     slider_beliefs.1.player.normative_belief_personal +
                                                     els_slider.1.player.slider_50_difficulty + 
                                                     ability_score,
                                                   data = data_low)
summary(moved_ratio_50_all_personal_belief_regression_low)
moved_ratio_50_all_personal_belief_regression_high <- lm(moved_ratio_50_all ~ 
                                                      slider_beliefs.1.player.normative_belief_personal +
                                                      els_slider.1.player.slider_50_difficulty + 
                                                      ability_score,
                                                    data = data_high)
summary(moved_ratio_50_all_personal_belief_regression_high)
moved_ratio_50_all_group_compliance_regression_low <- lm(moved_ratio_50_all ~ 
                                                       group_compliance_binary +
                                                       els_slider.1.player.slider_50_difficulty + 
                                                       ability_score,
                                                     data = data_low)
summary(moved_ratio_50_all_group_compliance_regression_low)
moved_ratio_50_all_group_compliance_regression_high <- lm(moved_ratio_50_all ~ 
                                                        group_compliance_binary +
                                                        els_slider.1.player.slider_50_difficulty + 
                                                        ability_score,
                                                      data = data_high)
summary(moved_ratio_50_all_group_compliance_regression_high)
moved_ratio_50_all_group_noncompliance_regression_low <- lm(moved_ratio_50_all ~ 
                                                          group_noncompliance_binary +
                                                          els_slider.1.player.slider_50_difficulty + 
                                                          ability_score,
                                                        data = data_low)
summary(moved_ratio_50_all_group_noncompliance_regression_low)
moved_ratio_50_all_group_noncompliance_regression_high <- lm(moved_ratio_50_all ~ 
                                                           group_noncompliance_binary +
                                                           els_slider.1.player.slider_50_difficulty + 
                                                           ability_score,
                                                         data = data_high)
summary(moved_ratio_50_all_group_noncompliance_regression_high)
moved_ratio_50_all_empirical_belief_regression_low <- lm(moved_ratio_50_all ~ 
                                                       slider_beliefs.1.player.empirical_belief_compliance +
                                                       els_slider.1.player.slider_50_difficulty + 
                                                       ability_score,
                                                     data = data_low)
summary(moved_ratio_50_all_empirical_belief_regression_low)
moved_ratio_50_all_empirical_belief_regression_high <- lm(moved_ratio_50_all ~ 
                                                        slider_beliefs.1.player.empirical_belief_compliance +
                                                        els_slider.1.player.slider_50_difficulty + 
                                                        ability_score,
                                                      data = data_high)
summary(moved_ratio_50_all_empirical_belief_regression_high)

# Pearson correlation tests
cor.test(data$slider_beliefs.1.player.normative_belief_personal, data$moved_ratio_50_all)
cor.test(data$slider_beliefs.1.player.normative_belief_group_compliance, data$moved_ratio_50_all)
cor.test(data$slider_beliefs.1.player.normative_belief_group_noncompliance, data$moved_ratio_50_all)
cor.test(data$slider_beliefs.1.player.empirical_belief_compliance, data$moved_ratio_50_all)

# treatment variation tests
chisq.test(table(data$els_slider.1.player.treatment, data$slider_beliefs.1.player.normative_belief_personal))
chisq.test(table(data$els_slider.1.player.treatment, data$slider_beliefs.1.player.normative_belief_group_compliance))
chisq.test(table(data$els_slider.1.player.treatment, data$slider_beliefs.1.player.normative_belief_group_noncompliance))
wilcox.test(data_low$slider_beliefs.1.player.empirical_belief_compliance, data_high$slider_beliefs.1.player.empirical_belief_compliance)

# treatment correlations with beliefs
cor.test(data$els_slider.1.player.manager_els, data$slider_beliefs.1.player.normative_belief_personal)
cor.test(data$els_slider.1.player.manager_els, data$slider_beliefs.1.player.normative_belief_group_compliance)
cor.test(data$els_slider.1.player.manager_els, data$slider_beliefs.1.player.normative_belief_group_noncompliance)
cor.test(data$els_slider.1.player.manager_els, data$slider_beliefs.1.player.empirical_belief_compliance)

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
  dplyr::select(
    moral_foundations_20.1.player.ingroup_lovecountry,
    moral_foundations_20.1.player.ingroup_betray,
    moral_foundations_20.1.player.ingroup_history,
    moral_foundations_20.1.player.ingroup_family
  )
mf20_ingroup_alpha <- psych::alpha(mf20_ingroup_items)
print(mf20_ingroup_alpha$total$raw_alpha)

mf20_authority_items <- data %>% 
  dplyr::select(
    moral_foundations_20.1.player.authority_respect,
    moral_foundations_20.1.player.authority_traditions,
    moral_foundations_20.1.player.authority_kidrespect,
    moral_foundations_20.1.player.authority_sexroles
  )
mf20_authority_alpha <- psych::alpha(mf20_authority_items)
print(mf20_authority_alpha$total$raw_alpha)

mf20_purity_items <- data %>% 
  dplyr::select(
    moral_foundations_20.1.player.purity_decency,
    moral_foundations_20.1.player.purity_disgusting,
    moral_foundations_20.1.player.purity_harmless,
    moral_foundations_20.1.player.purity_unnatural
  )
mf20_purity_alpha <- psych::alpha(mf20_purity_items)
print(mf20_purity_alpha$total$raw_alpha)

mf20_binding_items <- data %>% 
  dplyr::select(
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

# regression of moral foundations score on work variables
mf20_work_vars_regression <- lm(
  moral_foundations_20.1.player.moral_foundations_score ~ 
    els_slider.1.player.work_experience_worker + 
    els_slider.1.player.work_type_apprenticeship +
    els_slider.1.player.work_type_internship +
    els_slider.1.player.work_type_working_student +
    els_slider.1.player.work_type_part_time_job +
    els_slider.1.player.work_type_full_time_job +
    els_slider.1.player.work_type_volunteer_work +
    els_slider.1.player.work_experience_duration,
  data = data
)
summary(mf20_work_vars_regression)

# regression of moral foundations score on demographics
mf20_demographics_regression <- lm(
  moral_foundations_20.1.player.moral_foundations_score ~ 
    els_slider.1.player.treatment +
    demographics.1.player.age +
    gender_male +
    bachelor_or_higher +
    political_left,
  data = data
)
summary(mf20_demographics_regression)

# regression of authority subscale on els and rule orientation and setting both rules
mf20_authority_els_rule_regression <- lm(
  moral_foundations_20.1.player.authority_respect +
  moral_foundations_20.1.player.authority_traditions +
  moral_foundations_20.1.player.authority_kidrespect +
  moral_foundations_20.1.player.authority_sexroles ~
  els_slider.1.player.treatment +
  els_slider.1.player.els_score +
  rule_orientation.1.player.rule_orientation_score,
  data = data
)
summary(mf20_authority_els_rule_regression)

# regression of authority subscale on demographics
mf20_authority_demographics_regression <- lm(
  moral_foundations_20.1.player.authority_respect +
    moral_foundations_20.1.player.authority_traditions +
    moral_foundations_20.1.player.authority_kidrespect +
    moral_foundations_20.1.player.authority_sexroles ~
    els_slider.1.player.treatment +
    demographics.1.player.age +
    gender_male +
    bachelor_or_higher +
    political_left,
  data = data
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
  dplyr::select(
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

# regression of rule orientation score on worker variables
rule_orientation_work_vars_regression <- lm(
  rule_orientation.1.player.rule_orientation_score ~ 
    els_slider.1.player.treatment +
    els_slider.1.player.work_experience_worker + 
    els_slider.1.player.work_type_apprenticeship +
    els_slider.1.player.work_type_internship +
    els_slider.1.player.work_type_working_student +
    els_slider.1.player.work_type_part_time_job +
    els_slider.1.player.work_type_full_time_job +
    els_slider.1.player.work_type_volunteer_work +
    els_slider.1.player.work_experience_duration,
  data = data
)
summary(rule_orientation_work_vars_regression)

# regression of rule orientation score on demographics
rule_orientation_demographics_regression <- lm(
  rule_orientation.1.player.rule_orientation_score ~ 
    els_slider.1.player.treatment +
    demographics.1.player.age +
    gender_male +
    bachelor_or_higher +
    political_left,
  data = data
)
summary(rule_orientation_demographics_regression)

##########################################################################################
# Demographics
##########################################################################################

age <- summary_statistics_age(data)
gender <- summary_statistics_gender(data)
education <- summary_statistics_education(data)
political_orientation <- summary_statistics_political_orientation(data)

# time boxplots
time_boxplots(data, "demographics.1.player.demographics_time", "Demographics Time")

##########################################################################################
# Overleaf Sync
##########################################################################################

system("git -C ../Masterthesis-Overleaf pull origin main")
system("git -C ../Masterthesis-Overleaf add .")
if (system("git -C ../Masterthesis-Overleaf diff --cached --quiet") != 0) {
  system("git -C ../Masterthesis-Overleaf commit -m 'Auto update from R script'")
  system("git -C ../Masterthesis-Overleaf push origin main")
}