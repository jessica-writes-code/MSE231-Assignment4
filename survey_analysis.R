library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(nnet)

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

# Automation answer by demo
prop.table(table(survey_results$Answer.Age, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Race, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Gender, survey_results$Answer.Automation),1)
prop.table(table(survey_results$Answer.Education, survey_results$Answer.Automation),1)
edu_auto_df <- data.frame(prop.table(table(survey_results$Answer.Education, survey_results$Answer.Automation),1))
prop.table(table(survey_results$Answer.Political, survey_results$Answer.Automation),1)
pol_auto_df <- data.frame(prop.table(table(survey_results$Answer.Political, survey_results$Answer.Automation),1))

## Education-Automation Plot
names(edu_auto_df) <- c("demographic","answer","proportion")
edu_auto_df$demographic <- factor(edu_auto_df$demographic, 
                                 levels = c("no high school diploma","high school graduate","college degree","postgraduate degree"), 
                                 labels = c("No HS Diploma","HS Graduate","College Grad","Post-Grad"), ordered=TRUE)
ggplot(edu_auto_df, aes(x = answer, y = proportion, fill = demographic)) +
  geom_bar(stat = "Identity", position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Do you think automation/AI will eliminate your job?", y = "Proportion", fill = "")
ggsave("Edu_Auto_Dist.pdf", width = 6, heigh = 5)

## Political-Automation Plot
names(pol_auto_df) <- c("demographic","answer","proportion")
pol_auto_df$demographic <- factor(pol_auto_df$demographic, 
                                  levels = c("Conservative","Moderate","Liberal"), ordered=TRUE)
ggplot(pol_auto_df, aes(x = answer, y = proportion, fill = demographic)) +
  geom_bar(stat = "Identity", position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Do you think automation/AI will eliminate your job?", y = "Proportion", fill = "")
ggsave("Pol_Auto_Dist.pdf", width = 6, heigh = 5)

# UBI by demo
prop.table(table(survey_results$Answer.Age, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Race, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Gender, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Education, survey_results$Answer.Universal),1)
prop.table(table(survey_results$Answer.Political, survey_results$Answer.Universal),1)
pol_univ_df <- data.frame(prop.table(table(survey_results$Answer.Political, survey_results$Answer.Universal),1))

## Political-Universal Plot
names(pol_univ_df) <- c("demographic","answer","proportion")
pol_univ_df$demographic <- factor(pol_univ_df$demographic, 
                                  levels = c("Conservative","Moderate","Liberal"), ordered=TRUE)
ggplot(pol_univ_df, aes(x = answer, y = proportion, fill = demographic)) +
  geom_bar(stat = "Identity", position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Would you support a policy of Universal Basic Income?", y = "Proportion", fill = "")
ggsave("Pol_Univ_Dist.pdf", width = 6, heigh = 5)

# UBI vs. Auto
prop.table(table(survey_results$Answer.Automation, survey_results$Answer.Universal),1)
auto_univ_df <- data.frame(prop.table(table(survey_results$Answer.Automation, survey_results$Answer.Universal),1))

## Automation-Universal Plot
names(auto_univ_df) <- c("auto","ubi","proportion")
auto_univ_df$auto <- factor(auto_univ_df$auto, 
                            levels = c("Yes","Unsure","No"), ordered=TRUE)
auto_univ_df$ubi <- factor(auto_univ_df$ubi, 
                            levels = c("Yes","Unsure","No"), ordered=TRUE)
ggplot(auto_univ_df, aes(x = ubi, y = proportion, fill = auto)) +
  geom_bar(stat = "Identity", position = "dodge") +
  theme(legend.position = "bottom") +
  labs(x = "Would you support a policy of Universal Basic Income?", y = "Proportion", fill = "Automation/AI Response")
ggsave("Auto_Univ_Dist.pdf", width = 6, heigh = 5)

###########################
# Statistical Adjustments #
###########################
## Adjust survey data & renormalize
pmus_df <- filter(pmus_df, education != "no high school diploma")
pmus_df$pop_prop <- pmus_df$N/sum(pmus_df$N)

## Pairwise Combinations of Factors
ages <- unique(survey_results$Answer.Age)
genders <- unique(survey_results$Answer.Gender)
races <- unique(survey_results$Answer.Race)
educations <- unique(survey_results$Answer.Education)

combos_df <- expand.grid(ages, genders, races, educations)
names(combos_df) <- c("age","sex","race","education")
names(survey_results)[28:34] <- c("age","automation","education","sex","political","race","universal")

## Automation
automation_model <- multinom(automation ~ sex + age + race + education, data=survey_results)
automation_predictions <- predict(automation_model, newdata = combos_df, type="probs")
automation_df <- cbind(combos_df, automation_predictions)

### Weight by survey data
automation_weights_df <- merge(pmus_df, automation_df)
automation_weights_df$weight_no <- automation_weights_df$No * automation_weights_df$pop_prop
automation_weights_df$weight_unsure <- automation_weights_df$Unsure * automation_weights_df$pop_prop
automation_weights_df$weight_yes <- automation_weights_df$Yes * automation_weights_df$pop_prop

automation_adj <- c(sum(automation_weights_df$weight_yes), sum(automation_weights_df$weight_unsure), sum(automation_weights_df$weight_no))
names(automation_adj) <- c("Yes","Unsure","No")
automation_adj

## UBI
ubi_model <- multinom(universal ~ sex + age + race + education, data=survey_results)
ubi_predictions <- predict(ubi_model, newdata = combos_df, type="probs")
ubi_df <- cbind(combos_df, ubi_predictions)

### Weight by survey data
ubi_weights_df <- merge(pmus_df, ubi_df)
ubi_weights_df$weight_no <- ubi_weights_df$No * ubi_weights_df$pop_prop
ubi_weights_df$weight_unsure <- ubi_weights_df$Unsure * ubi_weights_df$pop_prop
ubi_weights_df$weight_yes <- ubi_weights_df$Yes * ubi_weights_df$pop_prop

ubi_adj <- c(sum(ubi_weights_df$weight_yes), sum(ubi_weights_df$weight_unsure), sum(ubi_weights_df$weight_no))
names(ubi_adj) <- c("Yes","Unsure","No")
ubi_adj