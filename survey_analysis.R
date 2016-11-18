library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

survey_results <- read.csv("survey_real_results.csv")
pmus_df <- read.csv("pop_dist.tsv", sep = "\t")

# Data Munging
survey_results <- filter(survey_results, Answer.Gender != "")

## Relabel variables
survey_results$Answer.Age <- mapvalues(survey_results$Answer.Age, from = c("Over65"), to = c("65+"))
survey_results$Answer.Race <- mapvalues(survey_results$Answer.Race, from = c("Black","Hispanic","Other","White"), to = c("black","hispanic","other","white"))
survey_results$Answer.Gender <- mapvalues(survey_results$Answer.Gender, from = c("Female","Male"), to = c("female","male"))
survey_results$Answer.Education <- mapvalues(survey_results$Answer.Education, from = c("HS","College","Grad"), to = c("high school graduate","college degree","postgraduate degree"))

## Collapse data
survey_results_agg <- group_by(survey_results, Answer.Age, Answer.Gender, Answer.Education, Answer.Race) %>%
                        summarize(count=n())

## Calculate proportions of survey and population
survey_results_agg$proportion_of_pop <- survey_results_agg$count/sum(survey_results_agg$count)
pmus_df$pop_prop <- pmus_df$N/sum(pmus_df$N)

## Join with PMUS
names(survey_results_agg) <- c("age","sex","education","race","survey_count","survey_prop")
joint_df <- merge(pmus_df, survey_results_agg, all = TRUE)
joint_df$survey_prop <- ifelse(is.na(joint_df$survey_prop), 0, joint_df$survey_prop)

# Compute and Plot Demographic Distributions
## Table
table(survey_results$Answer.Age)
table(survey_results$Answer.Race)
table(survey_results$Answer.Gender)
table(survey_results$Answer.Education)

## Plots
gender_df <- group_by(joint_df, sex) %>% summarize(pop_prop = sum(pop_prop), survey_prop = sum(survey_prop))
gender_df <- gather(gender_df, source, proportion, pop_prop:survey_prop, factor_key=TRUE)
ggplot(gender_df, aes(x = sex, y = proportion, fill = source, group = source)) +
  geom_bar(stat = "Identity", position="dodge") +
  labs(x = "Sex", y = "Proportion") +
  