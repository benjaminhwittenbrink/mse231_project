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
library(xtable)

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


## gen characteristics table
tab <- df %>% 
  mutate(age = 2016 - birth_year, 
         female = as.numeric(gender==2)
  )
sum_vars <- c("age", "female", "northeast", "north_central", "south", "west", "treat", "bot_followers", "half_complier", "perfect_complier")
sum_stats <- tibble(
  all_sample = tab %>% 
    select(sum_vars) %>% 
    summarise_all(~ mean(.x, na.rm=TRUE)) %>% unlist(),
  R_sample = tab %>% filter(party_id_wave_1 == 2) %>% 
    select(sum_vars) %>% 
    summarise_all(~ mean(.x, na.rm=TRUE)) %>% unlist(),
  D_sample = tab %>% filter(party_id_wave_1 == 1) %>% 
    select(sum_vars) %>% 
    summarise_all(~ mean(.x, na.rm=TRUE)) %>% unlist()
)

# ds = 691, rs = 529
# ds 416, 271, 211, 66
# rs 316, 181, 121, 53

paper_sum_stats <- tibble(
  all_study = c(50.49, 0.52, 0.18, 0.2, 0.39, 0.23, 0.600, 0.370, 0.272, 0.098),
  R_study = c(50.72, 0.48, 0.16, 0.18, 0.44, 0.21, 0.597, 0.342, 0.229, 0.100),
  D_study = c(50.31, 0.55, 0.21, 0.21, 0.34, 0.24, 0.602, 0.392, 0.305, 0.096)
)

sum_stats <- bind_cols(sum_stats, paper_sum_stats)
sum_stats <- sum_stats %>% select(sort(colnames(sum_stats)))
sum_stats <- sum_stats %>% mutate(Covariate = sum_vars) %>% select(Covariate, everything())
addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c(
  "& \\multicolumn{2}{c}{All} & \\multicolumn{2}{c}{Democrats} & \\multicolumn{2}{c}{Republicans} \\\\", 
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} & Replication & Paper  & Replication & Paper  & Replication & Paper \\\\"
)
sum_stats_tab <- xtable(sum_stats, digits = 3)
print(
  sum_stats_tab,
  include.colnames=FALSE, include.rownames=FALSE, 
  add.to.row = addtorow,
  file = "output/bail_etal_sumstats.tex"
)


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
  col <- ifelse(party == "D", "#0000FF", "#E50000")
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
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title = element_text(size = 14, colour = "black"),
      legend.position = "none",
      legend.key = element_blank(),
      legend.title = element_blank()
    ) +
   scale_y_continuous(breaks=c(-1, -0.5, 0, 0.5, 1), limits=c(-1, 1)) + labs(x = "", y = "") + 
    coord_flip() + ggtitle(tit)
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
ggsave("output/bail_etal_fig3_D.png", plot = fig3_d, width = 8, height = 5)
fig3_d_ests_tab <- xtable(fig3_d_ests)
print(fig3_d_ests_tab, file = "output/bail_etal_fig3_D_tab.tex")

r_ests <- bind_rows(
  full_compliance_models$r[2, ],
  half_compliance_models$r[2, ],
  bot_follower_models$r[2, ],
  r_ITT_tab[2,] %>% mutate(complier_sample = "itt")
)
fig3_r_ests <- fig3_process_models(final_df_r, r_ests)
fig3_r <- fig3_plot(fig3_r_ests, "R")
ggsave("output/bail_etal_fig3_R.png", plot = fig3_r, width = 8, height = 5)
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

## Attrition checks (6.1+)
final_df_d <- final_df_d %>% mutate(wave_5_missing = as.numeric(is.na(endtime_wave_5)))
final_df_r <- final_df_r %>% mutate(wave_5_missing = as.numeric(is.na(endtime_wave_5)))

run_attrit_model <- function(df){
  # fit reduced model 
  reduced_model<-lm(
    substantive_ideology_scale_wave_1 ~ 
      wave_5_missing + percent_co_party + political_wave_1 + freq_twitter_wave_1 + 
      friends_count_wave_1 + strong_partisan + birth_year + family_income + education +
      gender + ideo_homogeneity_offline + northeast + north_central + south,
   data=df)
  
  # include interactions
  full_model<-lm(
    substantive_ideology_scale_wave_1 ~ 
      wave_5_missing*(percent_co_party + political_wave_1 + freq_twitter_wave_1 + 
                        friends_count_wave_1 + strong_partisan + birth_year + family_income + education +
                        gender + ideo_homogeneity_offline + northeast + north_central + south),
    data=df)
  
  return(list(reduced=reduced_model, full=full_model))
}

attrit_models_d <- run_attrit_model(final_df_d)
anova(attrit_models_d$reduced, attrit_models_d$full)

attrit_models_r <- run_attrit_model(final_df_r)
anova(attrit_models_r$reduced, attrit_models_r$full)

## Outliers (6.3)

