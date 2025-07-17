# Function to exclude worker variables
exclude_manager_vars <- function(data) {
  worker_vars <- c(
    "participant.no_captcha",
    "participant.no_keyboard",
    "participant.no_consent",
    "participant.inactivity",
    "participant.no_manager_experience",
    "intro.1.player.keyboard_question",
    "intro.1.player.is_dropout",
    "intro.1.player.captcha_num",
    "intro.1.player.captcha",
    "intro.1.player.captcha_2",
    "intro.1.player.captcha_wrong",
    "intro.1.player.captcha_2_wrong",
    "intro.1.player.management_experience",
    "intro.1.player.captcha_start_time",
    "intro.1.player.captcha_end_time",
    "intro.1.player.captcha_time",
    "intro.1.player.captcha_2_start_time",
    "intro.1.player.captcha_2_end_time",
    "intro.1.player.captcha_2_time",
    "intro.1.player.prolific_id_start_time",
    "intro.1.player.prolific_id_end_time",
    "intro.1.player.prolific_id_time",
    "intro.1.player.manager_screener_start_time",
    "intro.1.player.manager_screener_end_time",
    "intro.1.player.manager_screener_time",
    "intro.1.player.consent_start_time",
    "intro.1.player.consent_end_time",
    "intro.1.player.consent_time",
    "els_slider.1.player.manager_personal_life",
    "els_slider.1.player.manager_results_obtained",
    "els_slider.1.player.manager_listen_employees",
    "els_slider.1.player.manager_discipline_employees",
    "els_slider.1.player.manager_fair_decisions",
    "els_slider.1.player.manager_trust",
    "els_slider.1.player.manager_attention_check",
    "els_slider.1.player.manager_discuss_employees",
    "els_slider.1.player.manager_set_example",
    "els_slider.1.player.manager_right_thing",
    "els_slider.1.player.manager_best_interest",
    "els_slider.1.player.manager_start_time",
    "els_slider.1.player.manager_end_time",
    "els_slider.1.player.manager_time",
    "els_slider.1.player.manager_results_start_time",
    "els_slider.1.player.manager_results_end_time",
    "els_slider.1.player.manager_results_time",
    "demographics.1.player.management_education",
    "demographics.1.player.work_experience_manager",
    "demographics.1.player.management_experience",
    "demographics.1.player.people_lead",
    "payment.1.player.set_rule_why"
  )
  
  # Identify which variables exist in the dataset
  vars_to_exclude <- intersect(worker_vars, names(data))
  
  if (length(vars_to_exclude) > 0) {
    # Drop the variables
    data <- data[, !names(data) %in% vars_to_exclude]
  }
  
  return(data)
}