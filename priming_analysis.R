# loading necessary packages
library(dplyr)
library(magrittr)
library(tidyr) # for drop_na function #SPECIAL
library(janitor)
library(ggplot2)
library(brms)
library(readr) #for locale argument (Turkish characters in data). #SPECIAL
library(bayesplot) # for  mcmc_intervals_data function (to get the median estimate, min and max credible intervals for plot). #SPECIAL

# import the priming data 
df_priming <- read_csv(file = "data_priming.csv", locale = locale(date_names = "tr", encoding = "UTF-8")) %>% drop_na()
View(df_priming)

# making sure that columns have clean names
colnames(df_priming) %>% make_clean_names() 

# encode vector types
df_priming$subject_id %<>% as.factor()
df_priming$condition %<>% as.factor()
df_priming$item %<>% as.factor()
df_priming$word %<>% as.character()
df_priming$RT %<>% as.integer()
df_priming$response_yes %<>% as.factor()

# add another column named "correctness" with an ifelse function. Ben buna baktým senin araþtýrma sorularýna göre accuracy bakmayabilirsin.
df_priming$correctness <- ifelse(df_priming$response_yes == 1 & df_priming$condition != "filler" | 
                                   df_priming$response_yes == 0 & df_priming$condition == "filler", "Correct", "Incorrect")
# select the related columns
rel_df <- df_priming %>% select(-item, -word)

# create another df without fillers
rel_df_no_filler <- rel_df %>% subset(condition != "filler")

# create another df without fillers and with all correct responses
rel_df_no_filler_all_correct <- rel_df_no_filler %>% subset(correctness == "Correct")

# summarize the data
rel_df_no_filler %>% 
  group_by(condition) %>% 
  dplyr::summarise(correctness = mean(correctness == "Correct")
  )

rel_df_no_filler_all_correct %>% 
  group_by(condition) %>% 
  dplyr::summarise(m = mean(RT)
                   )

# create a box plot only with the correct responses. Plotlarý ben böyle yaptým ama sen tema, text size vs çýkartabilirsin.
p1 <- rel_df_no_filler_all_correct %>% ggplot(aes(condition, RT)) + geom_boxplot() +
  scale_x_discrete(labels = c("Related", "Unrelated"))+
  scale_y_continuous(limits = c(150, 2500)) +
  xlab("Condition") +
  theme_classic() +
  theme(text=element_text(size=15))

ggsave("Fig-1.png", plot = p1, width = 6, height = 6)

#create a bar plot for accuracy.Bu da special bi plot
p2 <- rel_df_no_filler %>%   ggplot(aes(x= correctness, group = condition)) + 
  geom_bar(aes(y = ..prop.., fill = condition), stat="count", position = "dodge") +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5, position = position_dodge(width= 0.9), size = 3) +
  labs(y = "Percent", fill="Condition") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Correctness") + 
  theme_classic() + scale_fill_brewer(labels = c("Related", "Unrelated"), palette = "Greys") +
  theme(text=element_text(size=20))

ggsave("Fig-2.png", plot = p2, width = 6, height = 6)

# contrast coding for model 1 (using sum contrasts)
rel_df_no_filler_all_correct$c_condition <- rel_df_no_filler_all_correct$condition %>% dplyr::recode("condition_unrelated"=-.5, "condition_related"=.5)

# contrast coding for  model 2 (using sum contrasts)
rel_df_no_filler$c_condition <- rel_df_no_filler$condition %>% dplyr::recode("condition_unrelated"=-.5, "condition_related"=.5)
rel_df_no_filler$c_correctness <- rel_df_no_filler$correctness %>% dplyr::recode("Incorrect" = -.5, "Correct" = .5)

# model for RT by Condition
priming_m1 <- brm(RT ~ c_condition, data = rel_df_no_filler_all_correct,
                                  chains = 4, cores = 4)

# creating a df for the model results with the mcmc_intervals_data function. I included only the predictor estimate. SPECIAL
df_m1 <- mcmc_intervals_data(priming_m1, pars = vars(starts_with("b_"))) %>% subset(parameter != "b_Intercept")

# recoding the predictor estimate. SPECIAL
df_m1$parameter %<>% dplyr::recode(b_c_condition = "Related")

# plotting the model plot with credible intervals, and a vertical line at the intercept (0). SPECIAL. Modele plot yapmak zorunda deðilsin
p3 <- df_m1 %>% ggplot(aes(m, parameter)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_classic() +
  xlab("Estimate (ms)") + ylab("Coefficient") +
  theme(text=element_text(size=20))

ggsave("Fig-3.png", plot = p3, width = 6, height = 6)

# model for Accuracy by Condition
priming_m2 <- brm(c_correctness ~ c_condition, data = rel_df_no_filler,
                  family = bernoulli(link = "logit"),
                  chains = 4, cores = 4)  

# creating a df for the model results with the mcmc_intervals_data function. I included only the predictor estimate. SPECIAL
df_m2 <- mcmc_intervals_data(priming_m2, pars = vars(starts_with("b_"))) %>% subset(parameter != "b_Intercept")

# recoding the predictor estimate. SPECIAL
df_m2$parameter %<>% dplyr::recode(b_c_condition = "Related")

# plotting the model plot with credible intervals, and a dashed vertical line at the intercept (0). SPECIAL. Modele plot yapmak zorundna deðilsin
p4 <- df_m2 %>% ggplot(aes(m, parameter)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = l, xmax = h), alpha = 1, height = 0, size = 2) +
  geom_errorbarh(aes(xmin = ll, xmax = hh, height = 0)) + 
  geom_vline(xintercept = 0, linetype = "dashed", size = 1, colour = "black") +
  theme_classic() +
  scale_x_continuous(limits = c(-.5, 2)) +
  xlab("Estimate (log)") + ylab("Coefficient") +
  theme(text=element_text(size=20))

ggsave("Fig-4.png", plot = p4, width = 6, height = 6)
