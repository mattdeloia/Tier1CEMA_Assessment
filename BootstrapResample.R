library(moderndive)
library(skimr)
library(infer)

#Problem Solving resample
Tier1_resample <- df_scored %>% ungroup() %>% 
  filter (Miss_P <.10) %>% 
  filter(Work_Role=="Tier_1") %>%
  specify(response=Problem_Solving) %>% 
  generate(reps=1000, type="bootstrap") %>% 
  calculate(stat="mean")
  summarise(stat = mean(Problem_Solving))
  

Tier1_percentile_ci <- Tier1_resample %>% get_confidence_interval()

TierOther_resample <- df_scored %>% ungroup() %>% 
  filter (Miss_P <.10) %>% 
  filter(Work_Role=="Tier_Other") %>%
  specify(response=Problem_Solving) %>% 
  generate(reps=1000, type="bootstrap") %>% 
  calculate(stat="mean")
TierOther_percentile_ci <- TierOther_resample %>% get_confidence_interval()

visualise(Tier1_resample) + shade_ci (endpoints = Tier1_percentile_ci) +  shade_ci(endpoints = TierOther_percentile_ci, color="red", fill="red", alpha=.2)


#Hypothesis testing
  
  null_distribution <- df_scored %>% ungroup() %>% 
    mutate(Work_Role = as.factor(Work_Role)) %>% 
    filter(Miss_P < .20, infreq < .75) %>% 
    specify(formula = Problem_Solving ~ Work_Role) %>% 
    hypothesize(null = "independence") %>% 
    generate(reps = 1000, type = "permute") %>% 
    calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other"))

obs_diff_prop <- df_scored %>% ungroup() %>% 
  mutate(Work_Role = as.factor(Work_Role)) %>% 
  filter(Miss_P < .20, infreq < .75) %>% 
  specify(formula = Problem_Solving ~ Work_Role) %>% 
  calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other"))

visualise(null_distribution) + shade_p_value(obs_stat = obs_diff_prop, direction = if_else(obs_diff_prop>0, "right", "left") )


p_value <- function(personalitytrait, missingness) {
  df_scored %>%  
    filter(Miss_P < missingness) %>%
    gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
    filter(Trait==personalitytrait) %>% 
    specify(formula = Score ~ Work_Role) %>% 
    hypothesize(null = "independence") %>% 
    generate(reps = 1000, type = "permute") %>% 
    calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) %>% 
    get_p_value(obs_stat = df_scored %>%  
                  filter(Miss_P < missingness) %>%
                  gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
                  filter(Trait==personalitytrait) %>% 
                  specify(formula = Score ~ Work_Role) %>%  
                  calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")), 
                direction = if_else(obs_diff_prop>0, "right", "left"))
}

p_value("Problem_Solving", .20)$p_value

hypothesis_test <- function (personalitytrait, missingness) {
  df_scored %>%
    # mutate(Work_Role = as.factor(Work_Role)) %>% 
    filter(Miss_P < missingness) %>%
    gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
    filter(Trait==personalitytrait) %>% 
    specify(formula = Score ~ Work_Role) %>%
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other"))
}


percentile_ci <- function(personalitytrait, missingness, confidence) {
  df_scored %>%
    # mutate(Work_Role = as.factor(Work_Role)) %>% 
    filter(Miss_P < missingness) %>%
    gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
    filter(Trait==personalitytrait) %>% 
    specify(formula = Score ~ Work_Role) %>%
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) %>% 
    get_confidence_interval(level = confidence, type = "percentile")
}


hypothesisplot <- function (personalitytrait, missingness, confidence) {
  visualise(df_scored %>%
              filter(Miss_P < missingness) %>%
              gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
              filter(Trait==personalitytrait) %>% 
              specify(formula = Score ~ Work_Role) %>%
              generate(reps = 1000, type = "bootstrap") %>% 
              calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) ) +
    shade_confidence_interval(endpoints = df_scored %>%
                                filter(Miss_P < missingness) %>%
                                gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
                                filter(Trait==personalitytrait) %>% 
                                specify(formula = Score ~ Work_Role) %>%
                                generate(reps = 1000, type = "bootstrap") %>% 
                                calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) %>% 
                                get_confidence_interval(level = confidence, type = "percentile")) +
    geom_vline(xintercept = 0, linetype="dashed", color= "red", size = 1.5)
}

hypothesisplot("Problem_Solving", .20, .99)

