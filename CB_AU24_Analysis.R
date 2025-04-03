## College Bowl AU2024 Analyses
## Stephanie Pedron (pedron.2@osu.edu)

rm(list=ls())

## setup
library(foreign)
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)
library(pwr)
library(ggplot2)
library(ggpubr)

## Data
pre <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/pre.xlsx")
post <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/post.xlsx")
long <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/long.xlsx")

length(unique(long$id))
length(unique(pre$id))
length(unique(post$id))

## removing missing in longitudinal
long <- long[!is.na(long$id),]

## keeping only unique IDs and first instance of unique IDs
pre <- pre[!duplicated(pre$id), ]
post <- post[!duplicated(post$id), ]
long <- long[!duplicated(long$id), ]


##### Tables: Differences in groups across partisanship and voting record #####

## PRE
table1a <- table(pre$political_spectrum, pre$shared_goals)
table1b <- table(pre$political_spectrum, pre$future_of_democracy)
table1c <- table(pre$political_spectrum, pre$share_point_of_view)
table1d <- table(pre$political_spectrum, pre$share_point_of_view_campus)
table1e <- table(pre$political_spectrum, pre$impact_of_conversation)

table1f <- table(pre$voting_record, pre$shared_goals)
table1g <- table(pre$voting_record, pre$future_of_democracy)
table1h <- table(pre$voting_record, pre$share_point_of_view)
table1i <- table(pre$voting_record, pre$share_point_of_view_campus)
table1j <- table(pre$voting_record, pre$impact_of_conversation)

## POST
table2a <- table(post$political_spectrum, post$FDBK01_shared_goals)
table2b <- table(post$political_spectrum, post$FDBK01_future_of_democracy)
table2c <- table(post$political_spectrum, post$FDBK01_share_point_of_view)
table2d <- table(post$political_spectrum, post$FDBK01_share_point_of_view_campus)
table2e <- table(post$political_spectrum, post$FDBK01_impact_of_conversation)

table2f <- table(post$voting_record, post$shared_goals)
table2g <- table(post$voting_record, post$FDBK01_future_of_democracy)
table2h <- table(post$voting_record, post$FDBK01_share_point_of_view)
table2i <- table(post$voting_record, post$FDBK01_share_point_of_view_campus)
table2j <- table(post$voting_record, post$FDBK01_impact_of_conversation)

table_list1 <- list(
  "table1a" = table1a,
  "table1b" = table1b,
  "table1c" = table1c,
  "table1d" = table1d,
  "table1e" = table1e,
  "table1f" = table1f,
  "table1g" = table1g,
  "table1h" = table1h,
  "table1i" = table1i,
  "table1j" = table1j)

## POST
table_list2 <- list(
  "table2a" = table2a,
  "table2b" = table2b,
  "table2c" = table2c,
  "table2d" = table2d,
  "table2e" = table2e,
  "table2f" = table2f,
  "table2g" = table2g,
  "table2h" = table2h,
  "table2i" = table2i,
  "table2j" = table2j)

table_list1 <- lapply(table_list1, as.data.frame)
table_list2 <- lapply(table_list2, as.data.frame)

# library(writexl)
# filename <- "tables_PRE.xlsx"
# write_xlsx(table_list1, path = filename)
# 
# filename2 <- "tables_POST.xlsx"
# write_xlsx(table_list2, path = filename2)

#

##### Tables: Demographic Counts #####

## PRE
table(pre$gender)
table(pre$political_spectrum)
table(pre$race)
table(pre$urban_rural)
table(pre$voting_record)

## post
table(post$gender)
table(post$political_spectrum)
table(post$race)
table(post$urban_rural)
table(post$voting_record)
table(post$FDBK01_registered_vote)

## long
table(long$`True or false: Across party lines, Americans share many of the same goals for the country.`)


long_demo <- merge(long, post[, c("id", "political_spectrum", "gender", "race", "urban_rural", "voting_record",
                                 "FDBK01_registered_vote")], 
               by = "id", all.x = TRUE)

table(long_demo$gender)
table(long_demo$political_spectrum)
table(long_demo$race)
table(long_demo$urban_rural)
table(long_demo$voting_record)
table(long_demo$FDBK01_registered_vote)

## Long: Raw Counts of other variables
table(long$share_point_of_view)
table(long$share_point_of_view_campus)
table(long$future_of_democracy)
table(long$impact_of_conversation)
table(long$prevalence_check)

#
###### Cleaning variables #####
pre$future_of_democracy %<>%
  plyr::mapvalues(
    c("hopeful", "it's not something I think about", "neither hopeful nor unhopeful", "not hopeful",
      "somehwat hopeful", "somewhat unhopeful", "somewhere in between", "very hopeful", "very unhopeful"),
    c("5", "3", "3", "1", "4", "2", "3", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() # higher means more hopeful

pre$shared_goals %<>%
  plyr::mapvalues(
    c("FALSE", "TRUE"), # true means we're united when it comes to goals
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

pre$share_point_of_view_campus %<>%
  plyr::mapvalues(
    c("Neither comfortable nor uncomfortable", "NULL", "Somewhat comfortable", "Somewhat uncomfortable",
      "Very comfortable", "Very uncomfortable"),
    c("3", NA, "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() # higher is more comfy

pre$impact_of_conversation %<>%
  plyr::mapvalues(
    c("A big impact", "moderate", "none", "NULL", "slight", "strong", "very strong"),
    c(NA, "3", "1", NA, "2", "4", "5")
  ) %>% 
  as.vector() %>% 
  as.numeric()

pre$share_point_of_view %<>%
  plyr::mapvalues(
    c("I feel fine", "I don't feel one way or the other", "I feel uncomfortable", "Neither comfortable nor uncomfortable",
      "Somewhat comfortable", "Somewhat uncomfortable", "Very comfortable", "Very uncomfortable"),
    c("3", "3", "2", "3", "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

post$FDBK01_future_of_democracy %<>%
  plyr::mapvalues(
    c("Neither hopeful nor unhopeful", "Somewhat hopeful", "Somewhat unhopeful", "Very hopeful", "Very unhopeful"),
    c("3", "4", "2", "5", "1"),
  ) %>% 
  as.vector() %>% 
  as.numeric()

post$FDBK01_shared_goals %<>%
  plyr::mapvalues(
    c("False. We are mostly divided when it comes to goals.", "True. We are mostly united when it comes to goals."), # true means we're united when it comes to goals
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post$FDBK01_share_point_of_view %<>%
  plyr::mapvalues(
    c("Neither comfortable nor uncomfortable", "Somewhat comfortable", "Somewhat uncomfortable",
      "Very comfortable", "Very uncomfortable"),
    c("3", "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

post$FDBK01_share_point_of_view_campus %<>%
  plyr::mapvalues(
    c("Neither comfortable nor uncomfortable", "Somewhat comfortable", "Somewhat uncomfortable",
      "Very comfortable", "Very uncomfortable"),
    c("3", "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() # higher is more comfy

post$FDBK01_impact_of_conversation %<>%
  plyr::mapvalues(
    c("Moderate", "None", "Slight", "Strong", "Very strong"),
    c("3", "1", "2", "4", "5"),
  ) %>% 
  as.vector() %>% 
  as.numeric()

long$future_of_democracy %<>%
  plyr::mapvalues(
    c("Neither hopeful nor unhopeful", "Somewhat hopeful", "Somewhat unhopeful", "Very hopeful", "Very unhopeful"),
    c("3", "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric()

long$share_point_of_view_campus %<>%
  plyr::mapvalues(
    c("Neither comfortable nor uncomfortable", "NULL", "Somewhat comfortable", "Somewhat uncomfortable",
      "Very comfortable", "Very uncomfortable"),
    c("3", NA, "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() 

long$impact_of_conversation %<>%
  plyr::mapvalues(
    c("Moderate", "None", "Slight", "Strong", "Very strong"),
    c("3", "1", "2", "4", "5"),
  ) %>% 
  as.vector() %>% 
  as.numeric()

long$share_point_of_view %<>%
  plyr::mapvalues(
    c("Neither comfortable nor uncomfortable", "NULL", "Somewhat comfortable", "Somewhat uncomfortable",
      "Very comfortable", "Very uncomfortable"),
    c("3", NA, "4", "2", "5", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() 

long$shared_goals <- long$`True or false: Across party lines, Americans share many of the same goals for the country.` %>% 
  plyr::mapvalues(
    c("False. We are mostly divided when it comes to goals.",
      "True. We are mostly united when it comes to goals."),
    c("0", "1")
  ) %>% 
  as.vector() %>% 
  as.numeric() 

#

##### Group Analysis #####

# pre and post
pre_filtered <- pre %>% filter(id %in% post$id) # only want those IDs also in post; 6110 observations
post_filtered <- post %>% filter(id %in% pre_filtered$id) # uneven so need to do again

t.test(pre_filtered$future_of_democracy, post_filtered$FDBK01_future_of_democracy, paired = T, alternative = "two.sided")
t.test(pre_filtered$shared_goals, post_filtered$FDBK01_shared_goals, paired = T, alternative = "two.sided")
t.test(pre_filtered$share_point_of_view, post_filtered$FDBK01_share_point_of_view, paired = T, alternative = "two.sided")
t.test(pre_filtered$share_point_of_view_campus, post_filtered$FDBK01_share_point_of_view_campus, paired = T, alternative = "two.sided")
t.test(pre_filtered$impact_of_conversation, post_filtered$FDBK01_impact_of_conversation, paired = T, alternative = "two.sided")


# post and long
post_filter2 <- post %>%  filter(id %in% long$id)
long_filter <- long %>%  filter(id %in% post_filter2$id) # 625 observations
t.test(post_filter2$FDBK01_future_of_democracy, long_filter$future_of_democracy, paired = T, alternative = "two.sided")
t.test(post_filter2$FDBK01_share_point_of_view_campus, long_filter$share_point_of_view_campus, paired = T, alternative = "two.sided")
t.test(post_filter2$FDBK01_share_point_of_view, long_filter$share_point_of_view, paired = T, alternative = "two.sided")
t.test(post_filter2$FDBK01_impact_of_conversation, long_filter$impact_of_conversation, paired = T, alternative = "two.sided")
t.test(post_filter2$FDBK01_shared_goals, long_filter$shared_goals, paired = T, alternative = "two.sided")

# pre and long
pre_filteredx <- pre %>%  filter(id %in% long_filter$id)
long_filterx <- long_filter %>%  filter(id %in% pre_filteredx$id) # 621 observations

t.test(pre_filteredx$future_of_democracy, long_filterx$future_of_democracy, paired = T, alternative = "two.sided")
t.test(pre_filteredx$shared_goals, long_filterx$shared_goals, paired = T, alternative = "two.sided")
t.test(pre_filteredx$share_point_of_view, long_filterx$share_point_of_view, paired = T, alternative = "two.sided")
t.test(pre_filteredx$share_point_of_view_campus, long_filterx$share_point_of_view_campus, paired = T, alternative = "two.sided")
t.test(pre_filteredx$impact_of_conversation, long_filterx$impact_of_conversation, paired = T, alternative = "two.sided")

## testing post and long by political spectrum
## CONCLUSION: ALL NON SIG, REMOVING FROM ANALYSIS
# long_filter <- merge(long_filter, post_filter2[, c("id", "political_spectrum")], 
#                    by = "id", all.x = TRUE)
# post_filter2_gop <- post_filter2 %>% 
#   filter(political_spectrum == "conservative" | political_spectrum == "somewhat conservative")
# long_filter_gop <- long_filter %>% 
#   filter(political_spectrum == "conservative" | political_spectrum == "somewhat conservative")
# t.test(post_filter2_gop$FDBK01_share_point_of_view, long_filter_gop$share_point_of_view, paired = T, alternative = "two.sided")
# 
# post_filter2_lib <- post_filter2 %>% 
#   filter(political_spectrum == "liberal" | political_spectrum == "somewhat liberal")
# long_filter_lib <- long_filter %>% 
#   filter(political_spectrum == "liberal" | political_spectrum == "somewhat liberal")
# t.test(post_filter2_lib$FDBK01_share_point_of_view, long_filter_lib$share_point_of_view, paired = T, alternative = "two.sided")
# 
# post_filter2_mod <- post_filter2 %>% 
#   filter(political_spectrum == "moderate or centrist")
# long_filter_mod <- long_filter %>% 
#   filter(political_spectrum == "moderate or centrist")
# t.test(post_filter2_mod$FDBK01_share_point_of_view, long_filter_mod$share_point_of_view, paired = T, alternative = "two.sided")


## friedman test
## CONCLUSION: Shows that there have been significant differences in attitudes over time, but doesn't give new info
## Just refer to t.test results

## Prepping data
pre_fried <- pre_filtered %>%  filter (id %in% long_filter$id)
pre_fried$time <- 1
long_filter <- long_filter %>%  filter (id %in% pre_fried$id)
long_filter$time <- 3
post_filter2 <- post_filter2 %>%  filter (id %in% pre_fried$id) # 623 common in all three
post_filter2$time <- 2


## Future of Democracy
combined <- merge(pre_fried,long_filter, by = "id", suffixes = c("_pre", "_long"))
combined <- merge(combined, post_filter2, by = "id")

combined <- combined %>% 
  mutate(t1 = future_of_democracy_pre,
         t2 = FDBK01_future_of_democracy,
         t3 = future_of_democracy_long)
combined <- combined %>% 
  gather(key = "time", value = "democracy", t1, t2, t3) # making the data long
combined$time <- as.factor(combined$time)

friedman_data <- combined %>% 
  select(democracy, time)
friedman_result <- friedman.test(as.matrix(friedman_data))
friedman_result

## P-valuess adjusted based on bonferroni correction
library(rstatix)
pwc2 <- combined %>% 
  wilcox_test(democracy ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pwc2

## Share Point of View
combined <- merge(pre_fried,long_filter, by = "id", suffixes = c("_pre", "_long"))
combined <- merge(combined, post_filter2, by = "id")

combined <- combined %>% 
  mutate(t1 = share_point_of_view_pre,
         t2 = FDBK01_share_point_of_view,
         t3 = share_point_of_view_long)
combined <- combined %>% 
  gather(key = "time", value = "share_pov", t1, t2, t3) # making the data long
combined$time <- as.factor(combined$time)

friedman_data <- combined %>% 
  select(share_pov, time)
friedman_result <- friedman.test(as.matrix(friedman_data))
friedman_result

## P-valuess adjusted based on bonferroni correction
combined$share_pov <- as.numeric(combined$share_pov)
pwc2 <- combined %>% 
  wilcox_test(share_pov ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pwc2

## Share Point of View Campus
combined <- merge(pre_fried,long_filter, by = "id", suffixes = c("_pre", "_long"))
combined <- merge(combined, post_filter2, by = "id")

combined <- combined %>% 
  mutate(t1 = share_point_of_view_campus_pre,
         t2 = FDBK01_share_point_of_view_campus,
         t3 = share_point_of_view_campus_long)
combined <- combined %>% 
  gather(key = "time", value = "share_pov_campus", t1, t2, t3) # making the data long
combined$time <- as.factor(combined$time)

friedman_data <- combined %>% 
  select(share_pov_campus, time)
friedman_result <- friedman.test(as.matrix(friedman_data))
friedman_result

## P-valuess adjusted based on bonferroni correction
pwc2 <- combined %>% 
  wilcox_test(share_pov_campus ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pwc2

## Impact of Conversation
combined <- merge(pre_fried,long_filter, by = "id", suffixes = c("_pre", "_long"))
combined <- merge(combined, post_filter2, by = "id")

combined <- combined %>% 
  mutate(t1 = impact_of_conversation_pre,
         t2 = FDBK01_impact_of_conversation,
         t3 = impact_of_conversation_long)
combined <- combined %>% 
  gather(key = "time", value = "impact_of_conversation", t1, t2, t3) # making the data long
combined$time <- as.factor(combined$time)

friedman_data <- combined %>% 
  select(impact_of_conversation, time)
friedman_result <- friedman.test(as.matrix(friedman_data))
friedman_result

## P-values adjusted based on bonferroni correction
pwc2 <- combined %>% 
  wilcox_test(impact_of_conversation ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pwc2

#

##### Effect Sizes #####

library(effsize)
cohen.d(pre_filtered$share_point_of_view, post_filtered$FDBK01_share_point_of_view, paired = T, na.rm = T)
cohen.d(pre_filtered$share_point_of_view_campus, post_filtered$FDBK01_share_point_of_view_campus, paired = T, na.rm = T)
cohen.d(pre_filtered$future_of_democracy, post_filtered$FDBK01_future_of_democracy, paired = T, na.rm = T)
cohen.d(pre_filtered$impact_of_conversation, post_filtered$FDBK01_impact_of_conversation, paired = T, na.rm = T)
cohen.d(pre_filtered$shared_goals, post_filtered$FDBK01_shared_goals, paired = T, na.rm = T)

cohen.d(post_filter2$FDBK01_share_point_of_view, long_filter$share_point_of_view, paired = T, na.rm = T)
cohen.d(post_filter2$FDBK01_share_point_of_view_campus, long_filter$share_point_of_view_campus, paired = T, na.rm = T)
cohen.d(post_filter2$FDBK01_future_of_democracy, long_filter$future_of_democracy, paired = T, na.rm = T)
cohen.d(post_filter2$FDBK01_impact_of_conversation, long_filter$impact_of_conversation, paired = T, na.rm = T)
cohen.d(post_filter2$FDBK01_shared_goals, long_filter$shared_goals, paired = T, na.rm = T)

pre_filteredx <- pre %>%  filter(id %in% long_filter$id)
long_filterx <- long_filter %>%  filter(id %in% pre_filteredx$id)

cohen.d(pre_filteredx$share_point_of_view, long_filterx$share_point_of_view, paired = T, na.rm = T)
cohen.d(pre_filteredx$share_point_of_view_campus, long_filterx$share_point_of_view_campus, paired = T, na.rm = T)
cohen.d(pre_filteredx$future_of_democracy, long_filterx$future_of_democracy, paired = T, na.rm = T)
cohen.d(pre_filteredx$impact_of_conversation, long_filterx$impact_of_conversation, paired = T, na.rm = T)
cohen.d(pre_filteredx$shared_goals, long_filterx$shared_goals, paired = T, na.rm = T)

#
##### All 3 waves DV counts #####

# use post_filter2
pre_filter2 <- pre %>%  filter(id %in% long$id)
pre_filter2 <- pre_filter2 %>%  filter(id %in% post_filter2$id)

table(pre_filter2$share_point_of_view)
table(pre_filter2$share_point_of_view_campus)
table(pre_filter2$future_of_democracy)
table(pre_filter2$impact_of_conversation)
table(pre_filter2$shared_goals)

table(post_filter2$FDBK01_share_point_of_view)
table(post_filter2$FDBK01_share_point_of_view_campus)
table(post_filter2$FDBK01_future_of_democracy)
table(post_filter2$FDBK01_impact_of_conversation)
table(post_filter2$FDBK01_shared_goals)

table(long_filter$share_point_of_view)
table(long_filter$share_point_of_view_campus)
table(long_filter$future_of_democracy)
table(long_filter$impact_of_conversation)
table(long_filter$shared_goals)
table(long_filter$prevalence_check)

#
##### Averages (Differences across political spectrum and voter reg) #####

## Political Spectrum
long2 <- merge(long, post[, c("id", "political_spectrum")], 
                                         by = "id", all.x = TRUE)

means_by_polid_pre <- pre %>%
  group_by(political_spectrum) %>%
  summarise(
    across(c(future_of_democracy, shared_goals, share_point_of_view,
             share_point_of_view_campus, impact_of_conversation), mean, na.rm = TRUE))

means_by_polid_post <- post %>%
  group_by(political_spectrum) %>%
  summarise(
    across(c(FDBK01_future_of_democracy, FDBK01_shared_goals, FDBK01_share_point_of_view,
             FDBK01_share_point_of_view_campus, FDBK01_impact_of_conversation), mean, na.rm = TRUE))

means_by_polid_long <- long2 %>%
  group_by(political_spectrum) %>%
  summarise(
    across(c(future_of_democracy, share_point_of_view,
             share_point_of_view_campus, impact_of_conversation), mean, na.rm = TRUE))


## Voting Record
long3 <- merge(long, post[, c("id", "voting_record")], 
               by = "id", all.x = TRUE)

means_by_vr_pre <- pre %>%
  group_by(voting_record) %>%
  summarise(
    across(c(future_of_democracy, shared_goals, share_point_of_view,
             share_point_of_view_campus, impact_of_conversation), mean, na.rm = TRUE))


means_by_vr_post <- post %>%
  group_by(voting_record) %>%
  summarise(
    across(c(FDBK01_future_of_democracy, FDBK01_shared_goals, FDBK01_share_point_of_view,
             FDBK01_share_point_of_view_campus, FDBK01_impact_of_conversation), mean, na.rm = TRUE))

means_by_vr_long <- long3 %>%
  group_by(voting_record) %>%
  summarise(
    across(c(future_of_democracy, share_point_of_view,
             share_point_of_view_campus, impact_of_conversation), mean, na.rm = TRUE))

## Exporting
# library(openxlsx)
# 
# wb <- createWorkbook()
# addWorksheet(wb, "Sheet1")
# writeData(wb, "Sheet1", means_by_polid_pre)
# 
# addWorksheet(wb, "Sheet2")
# writeData(wb, "Sheet2", means_by_polid_post)
# 
# addWorksheet(wb, "Sheet3")
# writeData(wb, "Sheet3", means_by_polid_long)
# 
# addWorksheet(wb, "Sheet4")
# writeData(wb, "Sheet4", means_by_vr_pre)
# 
# addWorksheet(wb, "Sheet5")
# writeData(wb, "Sheet5", means_by_vr_post)
# 
# addWorksheet(wb, "Sheet6")
# writeData(wb, "Sheet6", means_by_vr_long)
# 
# saveWorkbook(wb, "average_opinion_differences.xlsx", overwrite = TRUE)

#

##### Topic Model #####

library(stm)
long <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/long.xlsx")

## Topic Model 1 - TRUE TO SHARED GOALS QUESTION
process <- textProcessor(long$TM_TRUE, metadata = long)
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 50) # taking words included in at least 50 responses

docs <-output$documents
vocab <- output$vocab
meta <- output$meta # final number is 563

## Searching for right number of topics
topicnumbers <- c(2, 4, 6, 8, 10, 12, 14, 16) 
topicresult <- searchK(docs,vocab, topicnumbers, data = meta) # 5 topics
plot(topicresult)

topic_model <- stm(output$documents, output$vocab, K = 5, data = output$meta, init.type = "Spectral",
                   seed = 991236)

## Validating
## 1: checking word prevalence
labelTopics(topic_model, n = 5)
plot.STM(topic_model, type = "summary", labeltype="prob") # prevalence of topics

## 2: Reading examples (alter n as needed)
t1 <- findThoughts(topic_model, texts = substr(long$TM_TRUE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model, texts = substr(long$TM_TRUE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model, texts = substr(long$TM_TRUE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 3)$docs[[1]]
t4 <- findThoughts(topic_model, texts = substr(long$TM_TRUE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model, texts = substr(long$TM_TRUE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 5)$docs[[1]] 


## Topic Model 2 - FALSE TO SHARED GOALS QUESTION
process <- textProcessor(long$TM_FALSE, metadata = long)
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 20) # taking words included in at least 50 responses

docs <-output$documents
vocab <- output$vocab
meta <- output$meta # final number is 214

## Searching for right number of topics
topicnumbers <- c(2, 4, 6, 8, 10, 12, 14, 16) 
topicresult <- searchK(docs,vocab, topicnumbers, data = meta) # 10 topics
plot(topicresult)

topic_model <- stm(output$documents, output$vocab, K = 5, data = output$meta, init.type = "Spectral",
                   seed = 6548)

## Validating
## 1: checking word prevalence
labelTopics(topic_model, n = 5)
plot.STM(topic_model, type = "summary", labeltype="prob") # prevalence of topics

## 2: Reading examples (alter n as needed)
t1 <- findThoughts(topic_model, texts = substr(long$TM_FALSE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model, texts = substr(long$TM_FALSE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model, texts = substr(long$TM_FALSE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 3)$docs[[1]]
t4 <- findThoughts(topic_model, texts = substr(long$TM_FALSE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model, texts = substr(long$TM_FALSE, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 5)$docs[[1]] 


## Topic Model 3 - Confidence
process <- textProcessor(long$TM2, metadata = long)
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 30) # taking words included in at least 50 responses


docs <-output$documents
vocab <- output$vocab
meta <- output$meta # final number is 593

## Searching for right number of topics
topicnumbers <- c(2, 4, 6, 8, 10, 12, 14, 16) 
topicresult <- searchK(docs,vocab, topicnumbers, prevalence =~ prevalence_check, data = meta) 
plot(topicresult)

topic_model <- stm(output$documents, output$vocab, K = 5, data = output$meta, init.type = "Spectral",
                   seed = 12348)


## Validating
## 1: checking word prevalence
labelTopics(topic_model, n = 5)
plot.STM(topic_model, type = "summary", labeltype="prob") # prevalence of topics

## 2: Reading examples (alter n as needed)
t1 <- findThoughts(topic_model, texts = substr(long$TM2, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model, texts = substr(long$TM2, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model, texts = substr(long$TM2, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 3)$docs[[1]]
t4 <- findThoughts(topic_model, texts = substr(long$TM2, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model, texts = substr(long$TM2, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 5)$docs[[1]] 

## Checking out prevalence based on confidence level
topic_labels <- c("T1", "T2", "T3", "T4", "T5")

## Effects Estimate 
## CONCLUSION: no significant difference between different confidence levels
estimate1 <- estimateEffect(1:5 ~ prevalence_check, topic_model, metadata = meta, uncertainty = "Global")
summary(estimate1)

#

