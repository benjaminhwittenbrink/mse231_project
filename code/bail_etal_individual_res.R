####################################################################################
#### Authors: Benjamin Wittenbrink, Eva, John, Michelle, Thomas
#### Class: MS&E 231
#### Title: REPLILCATION: Exposure to Opposing Views can Increase Political Polarization: 
#### Evidence from a Large-scale Field Experiment on Social Media
####################################################################################
# NOTE: posted data does not match results reported in the paper! 
# in particular, we have a differing number of treated/bot followers/partially compliant
# than are reported in the paper...

library(dplyr)

RANDOM_SEED <- 352

setwd('/Users/evabatelaan/Documents/11 2022 Autumn/MS&E 231/mse231_project')
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

for (i in 1:length(all_values)) {
  # Select current outcome var 
  df <- df %>%
    mutate(
      substantive_ideology_scale_wave_1 = paste0(all_values[i], "1"),
      substantive_ideology_scale_wave_5 = paste0(all_values[i], "5")
    )
  ## MISSING DATA 
  # we will impute the missing data here using MICE 
  library(mice)
  imputation_vars <- c("substantive_ideology_scale_wave_1", control_vars)
  
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
                           "substantive_ideology_scale_wave_5",
                           "endtime_wave_5"))),
    imputed_data
  )
  
  ### ANALYSIS
  library(lmtest)
  library(sandwich)
  library(pander)
  
  ## ITT (5.1)
  # subset by party 
  final_df_r <- final_df %>% filter(party_id_wave_1 == 2)
  final_df_d <- final_df %>% filter(party_id_wave_1 == 1)
  
  run_ITT <- function(df, caption) {
    mod <- lm(
      substantive_ideology_scale_wave_5 ~ 
        treat + substantive_ideology_scale_wave_1 + percent_co_party + ideo_homogeneity_offline +
        friends_count_wave_1 + birth_year + family_income + education + gender + northeast +
        north_central + south + as.factor(bin_maker),
      data=df
    )
    coefficients<-as.data.frame(
      coeftest(mod, vcov = vcovHC(mod, type="HC1"))[2:13, 1:4]
    )
    panderOptions('digits',3) 
    panderOptions('table.split.table', 300) 
    set.caption(caption) 
    pander(coefficients)
    return(list(mod = mod))
  }
  
  r_ITT <- run_ITT(final_df_r[complete.cases(final_df_r), ], "Intent-to-Treat Model (Republicans)")
  d_ITT <- run_ITT(final_df_d[complete.cases(final_df_d), ], "Intent-to-Treat Model (Democrats)")
  
  # NOTE: results are not the same!
  
  ## CACE (5.2)
  library(AER)
  # note the authors use library(ivpack) but this is deprecated 
  
  dfs <- list(final_df_d, final_df_r)
  
  run_CACE <- function(df, complier_var) { 
    # filter out rows with missing values 
    df <- df[complete.cases(df), ]
    # estimate
    mod <- ivreg(
      substantive_ideology_scale_wave_5 ~
        get(complier_var) + substantive_ideology_scale_wave_1 + percent_co_party + 
        friends_count_wave_1 + birth_year + family_income + education + gender + 
        ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker) |
        treat + + substantive_ideology_scale_wave_1 + percent_co_party + 
        friends_count_wave_1 + birth_year + family_income + education + gender + 
        ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker),
      data = df
    )
    res <- as.data.frame(coeftest(mod, vcov = vcovHC, type = "HC1")[,])
    res <- res %>%mutate(complier_sample = complier_var)
    return(res)
  }
  
  full_compliance_models <- lapply(dfs, run_CACE, complier_var = "perfect_complier") 
  half_compliance_models <- lapply(dfs, run_CACE, complier_var = "half_complier")
  bot_follower_models <- lapply(dfs, run_CACE, complier_var = "bot_followers")
  
  # combine samples:
  r_models <- bind_rows(
    full_compliance_models[[2]], half_compliance_models[[2]], bot_follower_models[[2]]
  )
  d_models <- bind_rows(
    full_compliance_models[[2]], half_compliance_models[[2]], bot_follower_models[[2]]
  )
  
  # make plots 
  
  ## Fisher's exact (ITT w/ no covars) (5.3)
  calc_fishers_exact <- function(outcome, treat, group, B = 1000) { 
    # filter out nas
    nas <- which(is.na(outcome) | is.na(treat) | is.na(group))
    outcome <- outcome[-nas]
    treat <- treat[-nas]
    group <- group[-nas] %>% as.character()
    
    weights <- as.numeric(table(group))
    itts <- vector(length = B + 1)
    for (b in 1:(B+1)) { 
      treated_mean <- c() 
      control_mean <- c()
      treat_tmp <- rep(0, length(treat))
      for (g in unique(group)) { 
        if (b == 1) {
          treat_tmp <- treat
        } else {
          treat_tmp[group == g] <- sample(treat[group == g])
        }
        treated_mean <- c(treated_mean, mean(outcome[group == g | treat_tmp == 1]))
        control_mean <- c(control_mean, mean(outcome[group == g | treat_tmp == 0]))
      }
      itts[b] <- weighted.mean(treated_mean, weights) - weighted.mean(control_mean, weights)
    }
    return(list(null = itts[2:length(itts)], obs = itts[1]))
  }
  
  final_df_r <- final_df_r %>% mutate(
    substantive_ideology_change = substantive_ideology_scale_wave_5 - substantive_ideology_scale_wave_1
  )
  final_df_d <- final_df_d %>% mutate(
    substantive_ideology_change = substantive_ideology_scale_wave_5 - substantive_ideology_scale_wave_1
  )
  
  # repubs
  fisher_exact_r <- calc_fishers_exact(
    final_df_r$substantive_ideology_change,
    final_df_r$treat, final_df_r$bin_maker, B = 1000
  )
  
  r_itt<-fisher_exact_r$obs
  r_itt_p_value<-mean(fisher_exact_r$null>r_itt)
  
  # democrats
  fisher_exact_d <- calc_fishers_exact(
    final_df_d$substantive_ideology_change,
    final_df_d$treat, final_df_d$bin_maker, B = 1000
  )
  
  d_itt<-fisher_exact_d$obs
  d_itt_p_value<-mean(fisher_exact_d$null>d_itt)
}
