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
  return(mod)
}

r_ITT <- run_ITT(final_df_r[complete.cases(final_df_r), ], "Intent-to-Treat Model (Republicans)")
d_ITT <- run_ITT(final_df_d[complete.cases(final_df_d), ], "Intent-to-Treat Model (Democrats)")

r_ITT_tab <- r_ITT %>% summary(cluster="bin_maker") %>% .$coefficients %>% as.data.frame()
d_ITT_tab <- d_ITT %>% summary(cluster="bin_maker") %>% .$coefficients %>% as.data.frame()

# NOTE: results are not the same!

## CACE (5.2)
library(AER)
# note the authors use library(ivpack) but this is deprecated 

run_CACE <- function(df, complier_var) { 
  # filter out rows with missing values 
  df <- df[complete.cases(df), ]
  # estimate
  mod <- ivreg(
    substantive_ideology_scale_wave_5 ~
      get(complier_var) + substantive_ideology_scale_wave_1 + percent_co_party + 
      friends_count_wave_1 + birth_year + family_income + education + gender + 
      ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker) |
      treat + substantive_ideology_scale_wave_1 + percent_co_party + 
      friends_count_wave_1 + birth_year + family_income + education + gender + 
      ideo_homogeneity_offline + northeast + north_central + south  + as.factor(bin_maker),
    data = df
  )
  res <- as.data.frame(coeftest(mod, vcov = vcovHC, type = "HC1")[,])
  res <- res %>%mutate(complier_sample = complier_var)
  return(res)
}

dfs <- list(d = final_df_d, r = final_df_r)
full_compliance_models <- lapply(dfs, run_CACE, complier_var = "perfect_complier") 
half_compliance_models <- lapply(dfs, run_CACE, complier_var = "half_complier")
bot_follower_models <- lapply(dfs, run_CACE, complier_var = "bot_followers")

# combine samples:
d_models <- bind_rows(
  full_compliance_models$d, half_compliance_models$d, bot_follower_models$d
)
r_models <- bind_rows(
  full_compliance_models$r, half_compliance_models$r, bot_follower_models$r
)

fig3_process_models <- function(df, ests) { 
  
  n_perf_complier <- df %>% filter(perfect_complier == 1) %>% nrow()
  n_half_complier <- df %>% filter(half_complier == 1) %>% nrow()
  n_bot_followers <- df %>% filter(bot_followers == 1) %>% nrow()
  n_itt <- df %>% filter(treat == 1) %>% nrow()
  
  sample_factor_labels <- paste0(c(
    "Fully Compliant Respondents (n=", 
    "Partially Compliant Respondents (n=", 
    "Minimally Compliant Respondents (n=", 
    "Respondents Assigned to Treatment (n="), 
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


## recreate figure 3 
d_ests <- bind_rows(
  full_compliance_models$d[2, ],
  half_compliance_models$d[2, ],
  bot_follower_models$d[2, ],
  d_ITT_tab[2,] %>% mutate(complier_sample = "itt")
)
fig3_d_ests <- fig3_process_models(final_df_d, d_ests)
fig3_d <- fig3_plot(fig3_d_ests, "D")
ggsave("output/bail_etal_fig3_D.png", plot = fig3_d, width = 10, height = 6)
fig3_d_ests_tab <- xtable(fig3_d_ests)
print(fig3_d_ests_tab, file = "output/bail_etal_fig3_D_tab.tex")

r_ests <- bind_rows(
  full_compliance_models$r[2, ],
  half_compliance_models$r[2, ],
  bot_follower_models$r[2, ],
  r_ITT_tab[2,] %>% mutate(complier_sample = "itt")
)
fig3_r_ests <- fig3_process_models(final_df_r, r_ests)
fig3_r <- fig3_plot(fig3_r_ests, "D")
ggsave("output/bail_etal_fig3_R.png", plot = fig3_r, width = 10, height = 6)
fig3_r_ests_tab <- xtable(fig3_r_ests)
print(fig3_r_ests_tab, file = "output/bail_etal_fig3_R_tab.tex")

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
