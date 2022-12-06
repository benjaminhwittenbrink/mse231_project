####################################################################################
#### Authors: Benjamin Wittenbrink, Eva, John, Michelle, Thomas
#### Class: MS&E 231
#### Title: REPLILCATION: Exposure to Opposing Views can Increase Political Polarization: 
#### Evidence from a Large-scale Field Experiment on Social Media
####################################################################################
# NOTE: posted data does not match results reported in the paper! 
# in particular, we have a differing number of treated/bot followers/partially compliant
# than are reported in the paper...

library(AER)
library(dplyr)
library(lmtest)
library(sandwich)
library(pander)
library(mice)

RANDOM_SEED <- 352

df <- readRDS("data/bail_etal_data.rds") 

## COMPLIANCE 
# construct continuous compliance measure 
df <- df %>% 
  mutate(
    complier_scale = rowSums(
      select(., starts_with("substantive_question_correct"), starts_with("animal_correct")), 
      na.rm = TRUE), 
    half_complier = ifelse(complier_scale > 0 & complier_scale < 6, 1, 0),
    perfect_complier = ifelse(complier_scale == 6, 1, 0),
  )
# NOTE: posted data does not match results reported in the paper! 
# in particular, we have a differing number of treated/bot followers/partially compliant
# than are reported in the paper...

## OUTCOMES 
# outcome measures 
# we want to recode liberal measures on the same scale as conservative, so we will 
# recode them by flipping their values 
liberal_values_qs <- c(
  "government_should_regulate_businesses_wave_",
  "racial_discrimination_hurts_black_people_wave_",
  "immigrants_strengthen_country_wave_",
  "corporations_make_too_much_profit_wave_",
  "homosexuality_should_be_accepted_wave_"
)
liberal_values_qs_w1 <- paste0(liberal_values_qs, "1")
liberal_values_qs_w5 <- paste0(liberal_values_qs, "5")
liberal_values_qs_waves <- c(liberal_values_qs_w1, liberal_values_qs_w5)
df <- df %>% 
  mutate(across(all_of(liberal_values_qs_waves), ~ 8 - .x))

# conservatives value questions 
conservative_values_qs <- c(
  "government_wasteful_inefficient_wave_",
  "poor_people_have_it_easy_wave_",
  "government_cannot_afford_to_help_needy_wave_",
  "best_way_peace_military_strength_wave_",
  "stricter_environmental_laws_damaging_wave_"
)
conservative_values_qs_w1 <- paste0(conservative_values_qs, "1")
conservative_values_qs_w5 <- paste0(conservative_values_qs, "5")
conservative_values_qs_waves <- c(conservative_values_qs_w1, conservative_values_qs_w5)
all_values <- c(
  liberal_values_qs,
  conservative_values_qs
)

# create average score 
df <- df %>% 
  mutate(
    substantive_ideology_scale_wave_1 = rowMeans(select(., paste0(all_values, "1")), na.rm = TRUE), 
    substantive_ideology_scale_wave_5 = rowMeans(select(., paste0(all_values, "5")), na.rm = TRUE)
  )

## CONTROLS 
# specify the controls here 
control_vars <- c(
  "caseid",
  "birth_year",
  "family_income",
  "education",
  "gender",
  "ideo_homogeneity_offline",
  "northeast",
  "north_central",
  "south",
  "west",
  "percent_co_party",
  "friends_count_wave_1",
  "strong_partisan",
  "political_wave_1",
  "freq_twitter_wave_1",
  "bin_maker"
)
# strong partisan isn't included in the data so add it
df <- df %>% 
  mutate(
    strong_partisan = ifelse(
      party_id_weak_strong_wave_1 == 1 || party_id_weak_strong_wave_1 == 7, 
      1, 0)
  )


run_ITT <- function(df, caption) {
  mod <- lm(
    outcome_wave_5 ~ 
      treat + outcome_wave_1 + percent_co_party + ideo_homogeneity_offline +
      friends_count_wave_1 + birth_year + family_income + education + gender + northeast +
      north_central + south + as.factor(bin_maker),
    data=df
  )
  return(mod = mod)
}


run_CACE <- function(df, complier_var) { 
  # filter out rows with missing values 
  df <- df[complete.cases(df), ]
  # estimate
  mod <- ivreg(
    outcome_wave_5 ~
      get(complier_var) + outcome_wave_1 + percent_co_party + 
      friends_count_wave_1 + birth_year + family_income + education + gender + 
      ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker) |
      treat + outcome_wave_1 + percent_co_party + 
      friends_count_wave_1 + birth_year + family_income + education + gender + 
      ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker),
    data = df
  )
  res <- as.data.frame(coeftest(mod, vcov = vcovHC, type = "HC1")[,])
  res <- res %>% mutate(complier_sample = complier_var)
  return(res)
}

fig3_process_models <- function(df, ests) { 
  
  n_perf_complier <- df %>% filter(perfect_complier == 1) %>% nrow()
  n_half_complier <- df %>% filter(half_complier == 1) %>% nrow()
  n_bot_followers <- df %>% filter(bot_followers == 1) %>% nrow()
  n_itt <- df %>% filter(treat == 1) %>% nrow()
  
  sample_factor_labels <- paste0(c(
    "Fully Compliant \nRespondents (n=", 
    "Partially Compliant \nRespondents (n=", 
    "Minimally Compliant \nRespondents (n=", 
    "Respondents Assigned to \nTreatment (n="), 
    c(n_perf_complier, n_half_complier, n_bot_followers, n_itt), ")")
  
  ests <- ests %>% mutate(
    complier_sample = factor(
      complier_sample, levels = c("perfect_complier", "half_complier", 
                                  "bot_followers", "itt" ),
      labels = sample_factor_labels
    )
  )
  colnames(ests) <- c("estimate", "se", "t", "p", "complier_sample")
  row.names(ests) <- NULL
  ests <- ests %>% select(complier_sample, everything())
  return(ests)
}

library(ggplot2)
library(xtable)

fig3_plot <- function(ests, party){ 
  col <- ifelse(party == "D", "blue", "red")
  tit <- ifelse(party == "D", "Democrats", "Republicans")
  interval1 <- -qnorm((1-0.9)/2) # 90% multiplier 
  interval2 <- -qnorm((1-0.95)/2) # 95% multiplier
  po <- ggplot(ests) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
    geom_point(
      aes(x = complier_sample, y = estimate),
      position = position_dodge(width = 1 /2), size = 2, colour = col
    ) +
    geom_linerange(
      aes(
        x = complier_sample,
        ymin = estimate - se * interval1,
        ymax = estimate + se * interval1
      ), linewidth = 1, position = position_dodge(width = 1 / 2), colour = col
    ) +
    geom_linerange(
      aes(
        x = complier_sample, y = estimate,
        ymin = estimate - se * interval2,
        ymax = estimate + se * interval2
      ), linewidth = .5, position = position_dodge(width = 1 / 2), colour = col
    ) + theme(
      axis.text = element_text(size = 12, face = "bold", colour = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 12, colour = "black"),
      legend.position = "none",
      legend.key = element_blank(),
      legend.title = element_blank()
    ) +
    ylim(c(-1, 1)) + labs(x = "", y = "") + coord_flip() + ggtitle(tit)
  return(po)
}


run_all_anlaysis <- function(var, df) {
  df <- df %>%
    mutate(
      outcome_wave_1 = get(paste0(var, "1")),
      outcome_wave_5 = get(paste0(var, "5"))
    )
  # impute
  imputation_vars <- c("outcome_wave_1", control_vars)
  
  imputed_data <- df %>% 
    select(all_of(imputation_vars)) %>%
    mutate(
      caseid = as.character(caseid),
      percent_co_party = log(percent_co_party + 1),
      friends_count_wave_1 = log(friends_count_wave_1 + 1)
    ) %>%
    mice(m = 15, seed = RANDOM_SEED, exclude = c("caseid","bin_maker")) %>%
    complete(action = 15)
  # construct final data as these cols + imputed data (as in paper)
  final_df <- bind_cols(
    df %>% select(all_of(c("treat", "perfect_complier",
                           "half_complier",
                           "bot_followers",
                           "party_id_wave_1",
                           "party_id_weak_strong_wave_1",
                           "outcome_wave_5",
                           "endtime_wave_5"))),
    imputed_data
  )
  ## ITT (5.1)
  # subset by party 
  final_df_r <- final_df %>% filter(party_id_wave_1 == 2)
  final_df_d <- final_df %>% filter(party_id_wave_1 == 1)
  
  r_ITT <- run_ITT(final_df_r[complete.cases(final_df_r), ], "Intent-to-Treat Model (Republicans)")
  d_ITT <- run_ITT(final_df_d[complete.cases(final_df_d), ], "Intent-to-Treat Model (Democrats)")
  
  r_ITT_tab <- r_ITT %>% summary(cluster="bin_maker") %>% .$coefficients %>% as.data.frame()
  d_ITT_tab <- d_ITT %>% summary(cluster="bin_maker") %>% .$coefficients %>% as.data.frame()
  
  ## CACE (5.2)
  dfs <- list(d = final_df_d, r = final_df_r)
  full_compliance_models <- lapply(dfs, run_CACE, complier_var = "perfect_complier") 
  half_compliance_models <- lapply(dfs, run_CACE, complier_var = "half_complier")
  bot_follower_models <- lapply(dfs, run_CACE, complier_var = "bot_followers")
  
  ## recreate figure 3 
  d_ests <- bind_rows(
    full_compliance_models$d[2, ],
    half_compliance_models$d[2, ],
    bot_follower_models$d[2, ],
    d_ITT_tab[2,] %>% mutate(complier_sample = "itt")
  )
  fig3_d_ests <- fig3_process_models(final_df_d, d_ests) %>% 
    mutate(outcome = var)
  
  r_ests <- bind_rows(
    full_compliance_models$r[2, ],
    half_compliance_models$r[2, ],
    bot_follower_models$r[2, ],
    r_ITT_tab[2,] %>% mutate(complier_sample = "itt")
  )
  fig3_r_ests <- fig3_process_models(final_df_r, r_ests) %>% 
    mutate(outcome = var)
  
  return(list(d=fig3_d_ests, r=fig3_r_ests))
}

# res <- lapply(c(all_values, "substantive_ideology_scale_wave_"), run_all_anlaysis, df = df)
res <- lapply(all_values, run_all_anlaysis, df = df)

get_d <- function(tmp) return(tmp$d)
get_r <- function(tmp) return(tmp$r)

outcome_labels <- c(
  "Government should \nregulate businesses (rev)",
  "Racial discrimination \nhurts black people (rev)",
  "Immigrants strength \ncountry (rev)",
  "Corporations make too \nmuch profit (rev)",
  "Homosexuality should \nbe accepted (rev)",
  "Government is wasteful \nand inefficient",
  "Poor people have \nit easy",
  "Government can't afford \nto help needy",
  "Best way for peace \nis military strength",
  "Stricter environmental \nlaws are damaging"
)

process_ests <- function(res) {
  interval1 <- -qnorm((1-0.9)/2) # 90% multiplier 
  interval2 <- -qnorm((1-0.95)/2) # 95% multiplier
  res <- res %>% bind_rows() %>% 
    mutate(
      outcome = factor(outcome, levels=all_values, labels=outcome_labels), 
      ymin1 = estimate - se * interval1,
      ymin2 = estimate - se * interval2, 
      ymax1 = estimate + se * interval1,
      ymax2 = estimate + se * interval2,
      ymin1 = ifelse(ymin1 < -2, -2, ymin1),
      ymin2 = ifelse(ymin2 < -2, -2, ymin2),
      ymax1 = ifelse(ymax1 > 2, 2, ymax1),
      ymax2 = ifelse(ymax2 > 2, 2, ymax2)
    )
  
  # agg_res <- res %>% filter(is.na(outcome)) %>% mutate(outcome = "Ideology scale (mean)")
  # res <-res %>% filter(!is.na(outcome))
  # return(list(res, agg_res))
  return(res)
}

plot_outcomes <- function(res, party) {
  tit <- ifelse(party == "D", "Democrats", "Republicans")
  po <- ggplot(res) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 3) + 
    geom_point(
      aes(x = complier_sample, y = estimate, color = outcome),
      position = position_dodge(width = 1 /2), size = 4
    ) +
    geom_linerange(
      aes(
        x = complier_sample, ymin = ymin1, ymax = ymax1, color = outcome
      ), linewidth = 2, position = position_dodge(width = 1 / 2)
    ) +
    geom_linerange(
      aes(
        x = complier_sample, ymin = ymin2, ymax = ymax2, color = outcome
      ), linewidth = 1, position = position_dodge(width = 1 / 2)
    ) + theme(
      axis.text = element_text(size = 14, face = "bold", colour = "black"),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "bottom", 
      legend.title = element_text(size = 14, colour = "black"),
      legend.text=element_text(size = 14, colour = "black")
    ) +
    labs(colour = "Outcome")+
    ylim(c(-2, 2)) + labs(x = "", y = "") + coord_flip() + ggtitle(tit)
  po <- po + scale_color_brewer(palette="Set3")
  return(po)
}

d_res <- lapply(res, get_d) %>% bind_rows() %>% process_ests()
r_res <- lapply(res, get_r) %>% bind_rows %>% process_ests()

d_plot <- plot_outcomes(d_res, "D")
r_plot <- plot_outcomes(r_res, "R")

ggsave("output/bail_etal_D_all_outcomes.png", plot = d_plot, width = 16, height = 9)
ggsave("output/bail_etal_R_all_outcomes.png", plot = r_plot, width = 16, height = 9)
