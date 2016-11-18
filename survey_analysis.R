library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

survey_results <- read.csv("survey_real_results.csv")
pmus_df <- read.csv("pop_dist.tsv", sep = "\t")

########################
# Demographic Analysis #
########################
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
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
theme_set(theme_bw())
colors_for_plot <- gg_color_hue(4)

# Age
age_df <- group_by(joint_df, age) %>% summarize(pop_prop = sum(pop_prop), survey_prop = sum(survey_prop))
age_df <- gather(age_df, source, proportion, pop_prop:survey_prop, factor_key=TRUE)
ggplot(age_df, aes(x = age, y = proportion, fill = source, group = source)) +
  geom_bar(stat = "Identity", position="dodge") +
  labs(x = "Age", y = "Proportion", fill = "") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,.6, by = .1)) +
  scale_fill_manual(values = colors_for_plot[3:4],
                    breaks = c("pop_prop","survey_prop"), 
                    labels = c("Population", "Survey"))
ggsave("Age_Dist.pdf", width = 6, height = 5)

# Gender
gender_df <- group_by(joint_df, sex) %>% summarize(pop_prop = sum(pop_prop), survey_prop = sum(survey_prop))
gender_df <- gather(gender_df, source, proportion, pop_prop:survey_prop, factor_key=TRUE)
ggplot(gender_df, aes(x = sex, y = proportion, fill = source, group = source)) +
  geom_bar(stat = "Identity", position="dodge") +
  labs(x = "Sex", y = "Proportion", fill = "") +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,.6, by = .1)) +
  scale_x_discrete(breaks = c("male","female"),
                  labels = c("Male","Female")) +
  scale_fill_manual(values = colors_for_plot[3:4],
                    breaks = c("pop_prop","survey_prop"), 
                    labels = c("Population", "Survey"))
ggsave("Gender_Dist.pdf", width = 6, height = 5)

# Race
race_df <- group_by(joint_df, race) %>% summarize(pop_prop = sum(pop_prop), survey_prop = sum(survey_prop))
race_df <- gather(race_df, source, proportion, pop_prop:survey_prop, factor_key=TRUE)
ggplot(race_df, aes(x = race, y = proportion, fill = source, group = source)) +
  geom_bar(stat = "Identity", position="dodge") +
  labs(x = "Race", y = "Proportion", fill = "") +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0,.8, by = .1)) +
  scale_x_discrete(breaks = c("black","hispanic","other","white"),
                   labels = c("Black","Hispanic","Other","White")) +
  scale_fill_manual(values = colors_for_plot[3:4],
                    breaks = c("pop_prop","survey_prop"), 
                    labels = c("Population", "Survey"))
ggsave("Race_Dist.pdf", width = 6, height = 5)

# Education
education_df <- group_by(joint_df, education) %>% summarize(pop_prop = sum(pop_prop), survey_prop = sum(survey_prop))
education_df <- gather(education_df, source, proportion, pop_prop:survey_prop, factor_key=TRUE)
education_df$education <- factor(education_df$education, 
                                 levels = c("no high school diploma","high school graduate","college degree","postgraduate degree"), 
                                 labels = c("No HS Diploma","HS Graduate","College Grad","Post-Grad"), ordered=TRUE)
ggplot(education_df, aes(x = education, y = proportion, fill = source, group = source)) +
  geom_bar(stat = "Identity", position="dodge") +
  labs(x = "Education", y = "Proportion", fill = "") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0,.6, by = .1)) +
  scale_fill_manual(values = colors_for_plot[3:4],
                    breaks = c("pop_prop","survey_prop"), 
                    labels = c("Population", "Survey"))
ggsave("Education_Dist.pdf", width = 6, height = 5)

###############################
# Unadjusted Answers Analysis #
###############################
# Number of responses in category
table(survey_results$Answer.Political)
table(survey_results$Answer.Automation)
table(survey_results$Answer.Universal)

prop.table(table(survey_results$Answer.Education, survey_results$Answer.Political),1)

# Automation answer by demo
prop.table(table(survey_results$Answer.Age, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Race, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Gender, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Education, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Political, survey_results$Answer.Automation),1)

# UBI by demo
prop.table(table(survey_results$Answer.Age, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Race, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Gender, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Education, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Political, survey_results$Answer.Universal),1)