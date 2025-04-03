## College Bowl Spring 2024 Analyses
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
pre <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/Pre.xlsx")
post <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/Post.xlsx")
longitudinal <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/longitudinal.xlsx")
transcripts <- read.csv("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/VideoTranscriptions.csv")
# names_nm <- read.csv("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/name_nomatch.csv")
prex <- pre

############## DATA CHECKING AND GROUP MATCHING   ##################
## Looking at unique values in each time dataset based on email address
pre_unique <- unique(pre$email) # all unique!
post_unique <- unique(post$email) # 4833 unique out of 4845
long_unique <- unique(longitudinal$Email) # 813 unique out of 847

## Dropping all duplicates from post and longitudinal
post <- post %>% 
  distinct(email, .keep_all = TRUE)
longitudinal <- longitudinal %>% 
  distinct(Email, .keep_all = TRUE)

# test <- pre[pre$email %in% longitudinal$Email, ]
# 
# pre$first_name <- tolower(pre$first_name)
# pre$last_name <- tolower(pre$last_name)
# pre$names <- paste(pre$first_name, pre$last_name, sep = "_")
# 
# names_nm$email <- names_nm$Email
# test <- pre[pre$email %in% names_nm$Email, ]
# test2 <- pre[pre$names %in% names_nm$names, ]
# test3 <- rbind(test, test2)
# test3 <- test3 %>% 
#   distinct(names, .keep_all = T)
# test4 <- merge(names_nm, test3, by = "names")
# test5 <- merge(names_nm, test3, by = "email")
# new_list <- names_nm[!(names_nm$email %in% test5$email),]
# 
# write.csv(test5, file = "newmatches.csv")
# write.csv(new_list, file = "newnomatch.csv")


## Matching observations in pre data to align with post data so can run Paired T-Test
pre_filtered <- pre[pre$email %in% post$email, ] # it doesn't match! Pre is now 4801 but it should be 4833
post_testing <- post[!(post$email %in% pre$email),] # these 33 obs in post data are not in pre data
post <- post[post$email %in% pre_filtered$email,] # removing the 33 from post data

rm(pre, post_testing, pre_unique, post_unique, long_unique) # removing unnecessary things

## Matching observations in pre and post to align with longitudinal data so can run ANOVA
pre2 <- pre_filtered[pre_filtered$email %in% longitudinal$Email, ] # only 531?
long_testing <- longitudinal[!(longitudinal$Email %in% pre_filtered$email),] # same respondents but they used different emails!

# write.csv(pre2, file = "email_match.csv")
# write.csv(long_testing, file = "longitudinal_email_nomatch.csv") # those who didn't match based on email

## Converting all names to lowercase and matching based on names instead
pre_filtered$first_name <- tolower(pre_filtered$first_name)
post$first_name <- tolower(post$first_name)
longitudinal$first_name <- tolower(longitudinal$`First name`)
pre_filtered$last_name <- tolower(pre_filtered$last_name)
post$last_name <- tolower(post$last_name)
longitudinal$last_name <- tolower(longitudinal$`Last name`)
pre_filtered$names <- paste(pre_filtered$first_name, pre_filtered$last_name, sep = "_")
post$names <- paste(post$first_name, post$last_name, sep = "_")
longitudinal$names <- paste(longitudinal$first_name, longitudinal$last_name, sep = "_")

pre2 <- pre_filtered[pre_filtered$names %in% longitudinal$names, ] # 673 in pre, so better!
post2 <- post[post$names %in% longitudinal$names, ] # 673 in post too
long_testing <- longitudinal[!(longitudinal$names %in% pre_filtered$names),] # list of 140 who aren't in post
# write.csv(long_testing, file = "longitudinal_name_nomatch.csv") # those who didn't match based on name
## NOTE: Original Pre data has these people but because they didn't take the post I can't include them for longitudinal analysis
longitudinal <- longitudinal[longitudinal$names %in% pre2$names,]
rm(long_testing)

# pre2_sort <- pre2[order(pre2$names), na.last = F]
# long_sort <- longitudinal[order(longitudinal$names), na.last = F]
# long_demographics <- cbind(pre2_sort, long_sort)
# write.csv(long_demographics, file = "longitudinal_name_match.csv")

## Demographics of longitudinal
# long_demographics <- read.csv("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/long_FINAL.csv")
# table(long_demographics$gender)
# table(long_demographics$gender_description)
# table(long_demographics$political_spectrum)
# table(long_demographics$voting_record)
# table(long_demographics$race)
# table(long_demographics$urban_rural)


## Transcription Data
transcripts_unique <- unique(transcripts$Name) # we have dupes
transcripts <- transcripts %>% 
  distinct(Name, .keep_all = TRUE)

## Basic transcription cleaning - getting rid of unnecessary phrases
transcripts <- transcripts %>% 
  filter(transcription != " Unavailable transcription") ## Dropping
transcripts$transcription <- gsub("i think", "", tolower(transcripts$transcription))
transcripts$transcription <- gsub("unified college bowl", "", tolower(transcripts$transcription))
transcripts$transcription <- gsub("college bowl", "", tolower(transcripts$transcription))
transcripts$transcription <- gsub("unified challenge", "", tolower(transcripts$transcription))
transcripts$transcription <- gsub("unified bowl", "", tolower(transcripts$transcription))

## Matching demographics from original pre data to transcripts file
## NOTE: removed duplicates, anonymous responses, unavailable transcriptions
## Email match code commented out below because more were retained using names
# prex <- prex %>% 
#   select(email, gender, political_spectrum, race, gender_description, urban_rural, voting_record)
# transcripts$email <- transcripts$Email
# transcripts_merged <- merge(transcripts, prex, by = "email") # only 215 retained
# transcripts <- transcripts_merged
# rm(transripts_merged, prex)

## Let's try matching by names to see if we can keep more
transcripts$names <- gsub(" ", "_", tolower(transcripts$Name))
transcripts$names <- tolower(transcripts$names)
prex$first_name <- tolower(prex$first_name)
prex$last_name <- tolower(prex$last_name)
prex$names <- paste(prex$first_name, prex$last_name, sep = "_")
prex <- prex %>% 
  select(names, gender, political_spectrum, race, gender_description, urban_rural, voting_record)
transcripts_merged <- merge(transcripts, prex, by = "names") # 236 retained

## now check for email dupes and remove
transcripts_unique <- unique(transcripts_merged$Email) 
transcripts <- transcripts_merged %>% 
  distinct(Email, .keep_all = TRUE) # final count is 223 of original 328 transcripts

rm(transcripts_merged, prex, transcripts_unique) # removing unnecessary things


############## PRE AND POST DATA ANALYSES   ##################
## Cleaning Variables - PRE DATA
pre_filtered$shared_goals %<>%
  plyr::mapvalues(
    c("FALSE", "TRUE"), # true means we're united when it comes to goals
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

pre_filtered$share_point_of_view %<>%
  plyr::mapvalues(
    c("I feel fine", "I don't feel one way or the other", "I feel uncomfortable"),
    c("3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

pre_filtered$future_of_democracy %<>%
  plyr::mapvalues(
    c("hopeful", "somewhere in between", "not hopeful", "it's not something I think about"),
    c("3", "2", "1", NA)
  ) %>%
  as.vector() %>% # the last is effectively an IDK option that doesn't fit into the scale so I'm coding it as NA
  as.numeric()

## Cleaning Variables - POST DATA
post$FDBK01_shared_goals %<>%
  plyr::mapvalues(
    c("This is mostly false. We are divided when it comes to goals.", "This is mostly true. We are united when it comes to goals."),
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post$FDBK01_share_point_of_view %<>%
  plyr::mapvalues(
    c("I feel fine", "I don't feel one way or the other", "I feel uncomfortable"),
    c("3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post$FDBK01_future_of_country %<>%
  plyr::mapvalues(
    c("Hopeful", "Somewhere in between", "Not hopeful", "It's not something I think about"),
    c("3", "2", "1", NA)
  ) %>%
  as.vector() %>% # the last is effectively an IDK option that doesn't fit into the scale so I'm coding it as NA
  as.numeric()

post$FDBK01_practice_respectful_conversations %<>%
  plyr::mapvalues(
    c("Yes, very much", "Yes, a little", "I'm not sure", "Not very much", "Not at all"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post$FDBK01_perspectives %<>%
  plyr::mapvalues(
    c("Yes, very much", "Yes, a little", "I'm not sure", "Not very much", "Not at all"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

## Quick power analysis for one of the variables
cohens_d <- (mean(pre_filtered$share_point_of_view) - mean(post$FDBK01_share_point_of_view)
             / sd(pre_filtered$share_point_of_view - post$FDBK01_share_point_of_view))
pwr.t.test(n = 4801, d = cohens_d, sig.level = 0.05, power = NULL, type = "paired", alternative = "two.sided") # highly powered with medium effect size

## Paired Sample T-Tests (PRE and POST data)
t.test(pre_filtered$shared_goals, post$FDBK01_shared_goals, paired = TRUE, alternative = "two.sided")
t.test(pre_filtered$share_point_of_view, post$FDBK01_share_point_of_view, paired = TRUE, alternative = "two.sided")
t.test(pre_filtered$future_of_democracy, post$FDBK01_future_of_country, paired = TRUE, alternative = "two.sided")

############## PRE, POST, and LONGITUDINAL ANALYSES   ##################
## Cleaning Variables - PRE DATA 2
pre2$shared_goals %<>%
  plyr::mapvalues(
    c("FALSE", "TRUE"), 
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

pre2$share_point_of_view %<>%
  plyr::mapvalues(
    c("I feel fine", "I don't feel one way or the other", "I feel uncomfortable"),
    c("3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

pre2$future_of_democracy %<>%
  plyr::mapvalues(
    c("hopeful", "somewhere in between", "not hopeful", "it's not something I think about"),
    c("3", "2", "1", NA)
  ) %>%
  as.vector() %>% 
  as.numeric()

## Cleaning Variables - POST DATA 2
post2$FDBK01_shared_goals %<>%
  plyr::mapvalues(
    c("This is mostly false. We are divided when it comes to goals.", "This is mostly true. We are united when it comes to goals."),
    c("0", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post2$FDBK01_share_point_of_view %<>%
  plyr::mapvalues(
    c("I feel fine", "I don't feel one way or the other", "I feel uncomfortable"),
    c("3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post2$FDBK01_future_of_country %<>%
  plyr::mapvalues(
    c("Hopeful", "Somewhere in between", "Not hopeful", "It's not something I think about"),
    c("3", "2", "1", NA)
  ) %>%
  as.vector() %>% # the last is effectively an IDK option that doesn't fit into the scale so I'm coding it as NA
  as.numeric()

post2$FDBK01_practice_respectful_conversations %<>%
  plyr::mapvalues(
    c("Yes, very much", "Yes, a little", "I'm not sure", "Not very much", "Not at all"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

post2$FDBK01_perspectives %<>%
  plyr::mapvalues(
    c("Yes, very much", "Yes, a little", "I'm not sure", "Not very much", "Not at all"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

## Cleaning Variables - LONGITUDINAL DATA (Share point of view variable is too different from pre and post to include)
## only matches post
longitudinal$perspectives <- longitudinal$`Since your Unify Challenge College Bowl experience, do you feel more or less open to considering new perspectives that you hadn‚Äôt considered before?` %>% 
  plyr::mapvalues(
    c("Much more open", "A little more open", "About the same", "A little less open", "Much less open"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

## only matches post
longitudinal$respectful_conversations <- longitudinal$`Since your Unify Challenge experience, do you feel more or less confident in your ability to have respectful conversations with people whose views are different from yours?` %>% 
  plyr::mapvalues(
    c("Much more confident", "A little more confident", "About the same", "A little less confident", "Much less confident"),
    c("5", "4", "3", "2", "1")
  ) %>%
  as.vector() %>% 
  as.numeric()

## has a match with pre and post
longitudinal$future_democracy <- longitudinal$`After taking the Unify Challenge, how do you feel about the future of our democracy?` %>% 
  plyr::mapvalues(
    c("A lot more hopeful", "A bit more hopeful", "About the same", "A bit less hopeful", "A lot less hopeful"),
    c("3", "3", "2", "1", "1")
  ) %>%
  as.vector() %>% # This is being coded into 3 the way it's coded for PRE and POST
  as.numeric()

## Power analysis
cohens_d <- (mean(post2$FDBK01_perspectives) - mean(longitudinal$perspectives)
             / sd(post2$FDBK01_perspectives - longitudinal$perspectives))
pwr.t.test(n = 673, d = cohens_d, sig.level = 0.05, power = NULL, type = "paired", alternative = "two.sided") # highly powered with large effect size so greater change over time
cohens_d <- (mean(post2$FDBK01_practice_respectful_conversations) - mean(longitudinal$respectful_conversations)
             / sd(post2$FDBK01_practice_respectful_conversations - longitudinal$respectful_conversations))
pwr.t.test(n = 673, d = cohens_d, sig.level = 0.05, power = NULL, type = "paired", alternative = "two.sided") # highly powered with medium effect size

## Paired Sample T-Tests (POST and LONGITUDINAL data)
t.test(post2$FDBK01_perspectives, longitudinal$perspectives, paired = TRUE, alternative = "two.sided")
t.test(post2$FDBK01_practice_respectful_conversations, longitudinal$respectful_conversations, paired = TRUE, alternative = "two.sided")
t.test(post2$FDBK01_future_of_country, longitudinal$future_democracy, paired = TRUE, alternative = "two.sided")

## One-way repeated measures ANOVA
## Data preparation
prex <- pre2 %>% 
  select(email, future_of_democracy)
postx <- post2 %>% 
  select(email, FDBK01_future_of_country)
longitudinalx <- longitudinal %>% 
  mutate(email = Email) %>% 
  select(email, future_democracy)
test <- merge(prex, postx, by = "email", all = TRUE)

# because people use different emails in longitudinal, I just use left join instead of merge
# I need it to only take values from longitudinal that match observations in the merged pre and post data
merged_data <- left_join(test, longitudinalx, by = "email") 

merged_data <- merged_data %>% 
  mutate(t1 = future_of_democracy,
         t2 = FDBK01_future_of_country,
         t3 = future_democracy)
merged_data <- merged_data %>% 
  gather(key = "time", value = "democracy", t1, t2, t3) # making the data long
merged_data$time <- as.factor(merged_data$time)
merged_data$id <- 1:673
merged_data$id <- as.factor(merged_data$id)

## boxplot
ggplot(merged_data, aes(x = time, y = democracy)) +
  geom_boxplot() +
  labs(x = "Time", y = "Democracy") + theme_minimal()

## checking normality
ggqqplot(merged_data, "democracy", facet.by = "time") # not hugging the line so problematic

## One-way Repeated Measures ANOVA Computation
library(rstatix)
res.aov <- anova_test(merged_data, dv = democracy, wid = id, within = time)
get_anova_table(res.aov) # significantly different at the different time points

## Post-hoc tests. P-values adjusted based on the Bonferroni correction
pairwise_comparisons <- merged_data %>% 
  pairwise_t_test(democracy ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pairwise_comparisons  

## Ok, but because we didn't meet the normality assumption, let's try a non-parametric approach
## Friedman Test
friedman_data <- merged_data %>% 
  select(democracy, time)
friedman_result <- friedman.test(as.matrix(friedman_data))
friedman_result

## Now, let's look at those pairwise comparisons
pwc2 <- merged_data %>% 
  wilcox_test(democracy ~ time, paired = TRUE, alternative = "two.sided", p.adjust.method = "bonferroni")
pwc2

############## TOPIC MODEL   ##################

## Question is: How did the college bowl experience stay with me?
library(stm)
set.seed(91891236)

## Processing data
process <- textProcessor(transcripts$transcription, metadata = transcripts)

## Document output
output <- prepDocuments(process$documents, process$vocab, process$meta, 
                        lower.thresh = 22) 
# Let's take words that are included in at least 22 (~10% of) responses as they may be more meaningful
# this helps us narrow down things by getting rid of uncommon words

docs <-output$documents
vocab <- output$vocab
meta <- output$meta

## Searching for right number of topics
topicnumbers <- c(2, 4, 6, 8, 10, 12, 14, 16) # trying out a lot of topics
topicresult <- searchK(docs,vocab, topicnumbers, 
                        prevalence =~ gender + political_spectrum + urban_rural + voting_record, 
                          # Let's check out prevalence based on some respondent demographics
                       data = meta)

plot(topicresult)

## Topic model
topic_model <- stm(output$documents, output$vocab, K = 10, data = output$meta, init.type = "Spectral",
                   seed = 91891236) # setting the seed in the model as an extra precaution even though I set the seed above

## Validating
## 1: checking word prevalence
labelTopics(topic_model, n = 10)
plot.STM(topic_model, type = "summary", labeltype="prob") # prevalence of topics

## 2: Reading examples (alter n as needed)
t1 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 1)$docs[[1]]
t2 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 2)$docs[[1]]
t3 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 3)$docs[[1]] ## Irrelevant topic
t4 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 4)$docs[[1]]
t5 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 5)$docs[[1]] 
t6 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 6)$docs[[1]]
t7 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 7)$docs[[1]]
t8 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 8)$docs[[1]]
t9 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 400)[as.numeric(names(output$documents))], 
                   n = 15, topics = 9)$docs[[1]]
t10 <- findThoughts(topic_model, texts = substr(transcripts$transcription, 1, 450)[as.numeric(names(output$documents))], 
                   n = 35, topics = 10)$docs[[1]]

# plots of the examples
plotQuote(t1, width = 30, text.cex = 1, main = "Topic 1") 
plotQuote(t2, width = 30, text.cex = 0.55, main = "Topic 2")
plotQuote(t3, width = 30, text.cex = 0.6, main = "Topic 3") 
plotQuote(t4, width = 30, text.cex = 1, main = "Topic 4") 
plotQuote(t5, width = 30, text.cex = 0.65, main = "Topic 5") 
plotQuote(t6, width = 30, text.cex = 0.5, main = "Topic 6") 
plotQuote(t7, width = 30, text.cex = 1, main = "Topic 7")  
plotQuote(t8, width = 30, text.cex = 0.6, main = "Topic 8") 
plotQuote(t9, width = 30, text.cex = 0.7, main = "Topic 9") 
plotQuote(t10, width = 30, text.cex = 0.6, main = "Topic 10") 

## NOTES: I did estimate effects of demographics on topic prevalence. All were null.

############## CONTINGENCY TABLES   ##################

## Let's reset everything
## setup
rm(list=ls())
library(foreign)
library(readxl)
library(dplyr)
library(tidyverse)
library(magrittr)

pre <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/Pre.xlsx")
post <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/Post.xlsx")
longitudinal <- read_xlsx("C:/Users/steph/Documents/Important Docs/Job Docs/OH/Unify America/College Bowl/longitudinal.xlsx")

## Cleaning data
post <- post %>% 
  distinct(email, .keep_all = TRUE)
longitudinal <- longitudinal %>% 
  distinct(Email, .keep_all = TRUE)
longitudinal$perspectives <- longitudinal$`Since your Unify Challenge College Bowl experience, do you feel more or less open to considering new perspectives that you hadn‚Äôt considered before?`
longitudinal$respectful_conversations <- longitudinal$`Since your Unify Challenge experience, do you feel more or less confident in your ability to have respectful conversations with people whose views are different from yours?`
longitudinal$future_of_democracy <- longitudinal$`After taking the Unify Challenge, how do you feel about the future of our democracy?`
longitudinal$share_point_of_view <- longitudinal$`Since your Unify Challenge experience, do you feel more or less open to sharing your opinion in class and on campus?`

## Copying code from above to cleanup longitudinal data
## More people in original pre data than in post who answered longitudinal so I'm using that
pre_filtered <- pre
pre_filtered$first_name <- tolower(pre_filtered$first_name)
longitudinal$first_name <- tolower(longitudinal$`First name`)
pre_filtered$last_name <- tolower(pre_filtered$last_name)
longitudinal$last_name <- tolower(longitudinal$`Last name`)
pre_filtered$names <- paste(pre_filtered$first_name, pre_filtered$last_name, sep = "_")
longitudinal$names <- paste(longitudinal$first_name, longitudinal$last_name, sep = "_")
pre2 <- pre_filtered[pre_filtered$names %in% longitudinal$names, ] 
long_testing <- longitudinal[!(longitudinal$names %in% pre_filtered$names),] 
longitudinal <- longitudinal[longitudinal$names %in% pre2$names,]

## Merging demographics with longitudinal data
pre_filtered <- pre_filtered %>% 
  select(names, email, gender, political_spectrum, race, gender_description, urban_rural, voting_record)
long_merged <- merge(longitudinal, pre_filtered, by = "names")
rm(long_testing, pre_filtered, longitudinal, pre2)


## Writing function for contingency tables
cont_table <- function(x, y){
  table(x, y) %>% 
    prop.table(1) %>% # row sums; change to 2 if you want col sums
    multiply_by(100) %>% 
    round()
}

## Contingency Tables
## PRE DATA - 7525 obs
table1a <- cont_table(pre$gender, pre$shared_goals)
table1b <- cont_table(pre$political_spectrum, pre$shared_goals)
table1c <- cont_table(pre$urban_rural, pre$shared_goals)
table1d <- cont_table(pre$voting_record, pre$shared_goals)

table1e <- cont_table(pre$gender, pre$share_point_of_view)
table1f <- cont_table(pre$political_spectrum, pre$share_point_of_view)
table1g <- cont_table(pre$urban_rural, pre$share_point_of_view)
table1h <- cont_table(pre$voting_record, pre$share_point_of_view)

table1i <- cont_table(pre$gender, pre$future_of_democracy)
table1j <- cont_table(pre$political_spectrum, pre$future_of_democracy)
table1k <- cont_table(pre$urban_rural, pre$future_of_democracy)
table1l <- cont_table(pre$voting_record, pre$future_of_democracy)

## POST DATA - 4833 obs
table2a <- cont_table(post$gender, post$FDBK01_shared_goals)
table2b <- cont_table(post$political_spectrum, post$FDBK01_shared_goals)
table2c <- cont_table(post$urban_rural, post$FDBK01_shared_goals)
table2d <- cont_table(post$voting_record, post$FDBK01_shared_goals)

table2e <- cont_table(post$gender, post$FDBK01_share_point_of_view)
table2f <- cont_table(post$political_spectrum, post$FDBK01_share_point_of_view)
table2g <- cont_table(post$urban_rural, post$FDBK01_share_point_of_view)
table2h <- cont_table(post$voting_record, post$FDBK01_share_point_of_view)

table2i <- cont_table(post$gender, post$FDBK01_future_of_country)
table2j <- cont_table(post$political_spectrum, post$FDBK01_future_of_country)
table2k <- cont_table(post$urban_rural, post$FDBK01_future_of_country)
table2l <- cont_table(post$voting_record, post$FDBK01_future_of_country)

table2m <- cont_table(post$gender, post$FDBK01_vote_midterms)
table2n <- cont_table(post$political_spectrum, post$FDBK01_vote_midterms)
table2o <- cont_table(post$urban_rural, post$FDBK01_vote_midterms)
table2p <- cont_table(post$voting_record, post$FDBK01_vote_midterms)

table2q <- cont_table(post$gender, post$FDBK01_opportunities)
table2r <- cont_table(post$political_spectrum, post$FDBK01_opportunities)
table2s <- cont_table(post$urban_rural, post$FDBK01_opportunities)
table2t <- cont_table(post$voting_record, post$FDBK01_opportunities)

table2u <- cont_table(post$gender, post$FDBK01_perspectives)
table2v <- cont_table(post$political_spectrum, post$FDBK01_perspectives)
table2w <- cont_table(post$urban_rural, post$FDBK01_perspectives)
table2x <- cont_table(post$voting_record, post$FDBK01_perspectives)

table2y <- cont_table(post$gender, post$FDBK01_examine_point_of_view)
table2z <- cont_table(post$political_spectrum, post$FDBK01_examine_point_of_view)
table2aa <- cont_table(post$urban_rural, post$FDBK01_examine_point_of_view)
table2bb <- cont_table(post$voting_record, post$FDBK01_examine_point_of_view)

table2cc <- cont_table(post$gender, post$FDBK01_practice_respectful_conversations)
table2dd <- cont_table(post$political_spectrum, post$FDBK01_practice_respectful_conversations)
table2ee <- cont_table(post$urban_rural, post$FDBK01_practice_respectful_conversations)
table2ff <- cont_table(post$voting_record, post$FDBK01_practice_respectful_conversations)

## LONGITUDINAL DATA - 812 obs
table3a <- cont_table(long_merged$gender, long_merged$share_point_of_view)
table3b <- cont_table(long_merged$political_spectrum, long_merged$share_point_of_view)
table3c <- cont_table(long_merged$urban_rural, long_merged$share_point_of_view)
table3d <- cont_table(long_merged$voting_record, long_merged$share_point_of_view)

table3e <- cont_table(long_merged$gender, long_merged$future_of_democracy)
table3f <- cont_table(long_merged$political_spectrum, long_merged$future_of_democracy)
table3g <- cont_table(long_merged$urban_rural, long_merged$future_of_democracy)
table3h <- cont_table(long_merged$voting_record, long_merged$future_of_democracy)

table3i <- cont_table(long_merged$gender, long_merged$perspectives)
table3j <- cont_table(long_merged$political_spectrum, long_merged$perspectives)
table3k <- cont_table(long_merged$urban_rural, long_merged$perspectives)
table3l <- cont_table(long_merged$voting_record, long_merged$perspectives)

table3m <- cont_table(long_merged$gender, long_merged$respectful_conversations)
table3n <- cont_table(long_merged$political_spectrum, long_merged$respectful_conversations)
table3o <- cont_table(long_merged$urban_rural, long_merged$respectful_conversations)
table3p <- cont_table(long_merged$voting_record, long_merged$respectful_conversations)

## Store tables in lists
## PRE TABLES
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
  "table1j" = table1j,
  "table1k" = table1k,
  "table1l" = table1l)

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
  "table2j" = table2j,
  "table2k" = table2k,
  "table2l" = table2l,
  "table2m" = table2m,
  "table2n" = table2n,
  "table2o" = table2o,
  "table2p" = table2p,
  "table2q" = table2q,
  "table2r" = table2r,
  "table2s" = table2s,
  "table2t" = table2t,
  "table2u" = table2u,
  "table2v" = table2v,
  "table2w" = table2w,
  "table2x" = table2x,
  "table2y" = table2y,
  "table2z" = table2z,
  "table2aa" = table2aa,
  "table2bb" = table2bb,
  "table2cc" = table2cc,
  "table2dd" = table2dd,
  "table2ee" = table2ee,
  "table2ff" = table2ff)

## LONGITUDINAL
table_list3 <- list(
  "table3a" = table3a,
  "table3b" = table3b,
  "table3c" = table3c,
  "table3d" = table3d,
  "table3e" = table3e,
  "table3f" = table3f,
  "table3g" = table3g,
  "table3h" = table3h,
  "table3i" = table3i,
  "table3j" = table3j,
  "table3k" = table3k,
  "table3l" = table3l,
  "table3m" = table3m,
  "table3n" = table3n,
  "table3o" = table3o,
  "table3p" = table3p)

table_list1 <- lapply(table_list1, as.data.frame)
table_list2 <- lapply(table_list2, as.data.frame)
table_list3 <- lapply(table_list3, as.data.frame)

## Export into Excel Sheets
# library(writexl)
# filename <- "contingency_tables_PRE.xlsx"
# write_xlsx(table_list1, path = filename)
# 
# filename2 <- "contingency_tables_POST.xlsx"
# write_xlsx(table_list2, path = filename2)
# 
# filename3 <- "contingency_tables_LONG.xlsx"
# write_xlsx(table_list3, path = filename3)

## Table Counts
table_counts <- function(x, y){
  table(x, y)
}

## PRE DATA - 7525 obs
table1a <- table_counts(pre$gender, pre$shared_goals)
table1b <- table_counts(pre$political_spectrum, pre$shared_goals)
table1c <- table_counts(pre$urban_rural, pre$shared_goals)
table1d <- table_counts(pre$voting_record, pre$shared_goals)

table1e <- table_counts(pre$gender, pre$share_point_of_view)
table1f <- table_counts(pre$political_spectrum, pre$share_point_of_view)
table1g <- table_counts(pre$urban_rural, pre$share_point_of_view)
table1h <- table_counts(pre$voting_record, pre$share_point_of_view)

table1i <- table_counts(pre$gender, pre$future_of_democracy)
table1j <- table_counts(pre$political_spectrum, pre$future_of_democracy)
table1k <- table_counts(pre$urban_rural, pre$future_of_democracy)
table1l <- table_counts(pre$voting_record, pre$future_of_democracy)

## POST DATA - 4833 obs
table2a <- table_counts(post$gender, post$FDBK01_shared_goals)
table2b <- table_counts(post$political_spectrum, post$FDBK01_shared_goals)
table2c <- table_counts(post$urban_rural, post$FDBK01_shared_goals)
table2d <- table_counts(post$voting_record, post$FDBK01_shared_goals)

table2e <- table_counts(post$gender, post$FDBK01_share_point_of_view)
table2f <- table_counts(post$political_spectrum, post$FDBK01_share_point_of_view)
table2g <- table_counts(post$urban_rural, post$FDBK01_share_point_of_view)
table2h <- table_counts(post$voting_record, post$FDBK01_share_point_of_view)

table2i <- table_counts(post$gender, post$FDBK01_future_of_country)
table2j <- table_counts(post$political_spectrum, post$FDBK01_future_of_country)
table2k <- table_counts(post$urban_rural, post$FDBK01_future_of_country)
table2l <- table_counts(post$voting_record, post$FDBK01_future_of_country)

table2m <- table_counts(post$gender, post$FDBK01_vote_midterms)
table2n <- table_counts(post$political_spectrum, post$FDBK01_vote_midterms)
table2o <- table_counts(post$urban_rural, post$FDBK01_vote_midterms)
table2p <- table_counts(post$voting_record, post$FDBK01_vote_midterms)

table2q <- table_counts(post$gender, post$FDBK01_opportunities)
table2r <- table_counts(post$political_spectrum, post$FDBK01_opportunities)
table2s <- table_counts(post$urban_rural, post$FDBK01_opportunities)
table2t <- table_counts(post$voting_record, post$FDBK01_opportunities)

table2u <- table_counts(post$gender, post$FDBK01_perspectives)
table2v <- table_counts(post$political_spectrum, post$FDBK01_perspectives)
table2w <- table_counts(post$urban_rural, post$FDBK01_perspectives)
table2x <- table_counts(post$voting_record, post$FDBK01_perspectives)

table2y <- table_counts(post$gender, post$FDBK01_examine_point_of_view)
table2z <- table_counts(post$political_spectrum, post$FDBK01_examine_point_of_view)
table2aa <- table_counts(post$urban_rural, post$FDBK01_examine_point_of_view)
table2bb <- table_counts(post$voting_record, post$FDBK01_examine_point_of_view)

table2cc <- table_counts(post$gender, post$FDBK01_practice_respectful_conversations)
table2dd <- table_counts(post$political_spectrum, post$FDBK01_practice_respectful_conversations)
table2ee <- table_counts(post$urban_rural, post$FDBK01_practice_respectful_conversations)
table2ff <- table_counts(post$voting_record, post$FDBK01_practice_respectful_conversations)

## LONGITUDINAL DATA - 812 obs
table3a <- table_counts(long_merged$gender, long_merged$share_point_of_view)
table3b <- table_counts(long_merged$political_spectrum, long_merged$share_point_of_view)
table3c <- table_counts(long_merged$urban_rural, long_merged$share_point_of_view)
table3d <- table_counts(long_merged$voting_record, long_merged$share_point_of_view)

table3e <- table_counts(long_merged$gender, long_merged$future_of_democracy)
table3f <- table_counts(long_merged$political_spectrum, long_merged$future_of_democracy)
table3g <- table_counts(long_merged$urban_rural, long_merged$future_of_democracy)
table3h <- table_counts(long_merged$voting_record, long_merged$future_of_democracy)

table3i <- table_counts(long_merged$gender, long_merged$perspectives)
table3j <- table_counts(long_merged$political_spectrum, long_merged$perspectives)
table3k <- table_counts(long_merged$urban_rural, long_merged$perspectives)
table3l <- table_counts(long_merged$voting_record, long_merged$perspectives)

table3m <- table_counts(long_merged$gender, long_merged$respectful_conversations)
table3n <- table_counts(long_merged$political_spectrum, long_merged$respectful_conversations)
table3o <- table_counts(long_merged$urban_rural, long_merged$respectful_conversations)
table3p <- table_counts(long_merged$voting_record, long_merged$respectful_conversations)

## Store tables in lists
## PRE TABLES
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
  "table1j" = table1j,
  "table1k" = table1k,
  "table1l" = table1l)

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
  "table2j" = table2j,
  "table2k" = table2k,
  "table2l" = table2l,
  "table2m" = table2m,
  "table2n" = table2n,
  "table2o" = table2o,
  "table2p" = table2p,
  "table2q" = table2q,
  "table2r" = table2r,
  "table2s" = table2s,
  "table2t" = table2t,
  "table2u" = table2u,
  "table2v" = table2v,
  "table2w" = table2w,
  "table2x" = table2x,
  "table2y" = table2y,
  "table2z" = table2z,
  "table2aa" = table2aa,
  "table2bb" = table2bb,
  "table2cc" = table2cc,
  "table2dd" = table2dd,
  "table2ee" = table2ee,
  "table2ff" = table2ff)

## LONGITUDINAL
table_list3 <- list(
  "table3a" = table3a,
  "table3b" = table3b,
  "table3c" = table3c,
  "table3d" = table3d,
  "table3e" = table3e,
  "table3f" = table3f,
  "table3g" = table3g,
  "table3h" = table3h,
  "table3i" = table3i,
  "table3j" = table3j,
  "table3k" = table3k,
  "table3l" = table3l,
  "table3m" = table3m,
  "table3n" = table3n,
  "table3o" = table3o,
  "table3p" = table3p)

table_list1 <- lapply(table_list1, as.data.frame)
table_list2 <- lapply(table_list2, as.data.frame)
table_list3 <- lapply(table_list3, as.data.frame)

## Export into Excel Sheets
# library(writexl)
# filename <- "count_tables_PRE.xlsx"
# write_xlsx(table_list1, path = filename)
# 
# filename2 <- "count_tables_POST.xlsx"
# write_xlsx(table_list2, path = filename2)
# 
# filename3 <- "count_tables_LONG.xlsx"
# write_xlsx(table_list3, path = filename3)

## Chi-square
chisq.test(table1a)
chisq.test(table1b)
chisq.test(table1c)
chisq.test(table1d) # shared goals
chisq.test(table1e)
chisq.test(table1f)
chisq.test(table1g)
chisq.test(table1h)# shared point of view
chisq.test(table1i, simulate.p.value = TRUE)
chisq.test(table1j)
chisq.test(table1k)
chisq.test(table1l) # future of democracy

chisq.test(table2a, simulate.p.value = TRUE)
chisq.test(table2b)
chisq.test(table2c)
chisq.test(table2d, simulate.p.value = TRUE) # shared goals
chisq.test(table2e, simulate.p.value = TRUE)
chisq.test(table2f)
chisq.test(table2g)
chisq.test(table2h, simulate.p.value = TRUE) # shared point of view
chisq.test(table2i, simulate.p.value = TRUE)
chisq.test(table2j)
chisq.test(table2k)
chisq.test(table2l, simulate.p.value = TRUE) # future of democracy
chisq.test(table2m, simulate.p.value = TRUE)
chisq.test(table2n, simulate.p.value = TRUE)
chisq.test(table2o)
chisq.test(table2p, simulate.p.value = TRUE) # vote midterms
chisq.test(table2q, simulate.p.value = TRUE)
chisq.test(table2r)
chisq.test(table2s)
chisq.test(table2t, simulate.p.value = TRUE) # opportunities
chisq.test(table2u, simulate.p.value = TRUE)
chisq.test(table2v, simulate.p.value = TRUE)
chisq.test(table2w)
chisq.test(table2x, simulate.p.value = TRUE) #perspectives
chisq.test(table2y, simulate.p.value = TRUE)
chisq.test(table2z, simulate.p.value = TRUE)
chisq.test(table2aa)
chisq.test(table2bb, simulate.p.value = TRUE) # examine point of view
chisq.test(table2cc, simulate.p.value = TRUE)
chisq.test(table2dd, simulate.p.value = TRUE)
chisq.test(table2ee)
chisq.test(table2ff, simulate.p.value = TRUE) # respectful conversations

chisq.test(table3a, simulate.p.value = TRUE)
chisq.test(table3b, simulate.p.value = TRUE)
chisq.test(table3c, simulate.p.value = TRUE)
chisq.test(table3d, simulate.p.value = TRUE) # share point of view
chisq.test(table3e, simulate.p.value = TRUE)
chisq.test(table3f, simulate.p.value = TRUE)
chisq.test(table3g, simulate.p.value = TRUE)
chisq.test(table3h, simulate.p.value = TRUE) # future of democracy
chisq.test(table3i, simulate.p.value = TRUE)
chisq.test(table3j, simulate.p.value = TRUE)
chisq.test(table3k, simulate.p.value = TRUE)
chisq.test(table3l, simulate.p.value = TRUE) # perspectives
chisq.test(table3m, simulate.p.value = TRUE)
chisq.test(table3n, simulate.p.value = TRUE)
chisq.test(table3o, simulate.p.value = TRUE)
chisq.test(table3p, simulate.p.value = TRUE) # respectful conversations

