################################################################################
#                              pre looks at the data                          #
################################################################################
rm(list = ls())
gc()
# library(text)
# textrpp_initialize(save_profile = TRUE)
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
pacman::p_load(sentimentr, dplyr, magrittr)
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
data.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/data/Reddit/reddit_mental_health_posts"
adhd <- read_csv(paste0(data.dir, "/adhd.csv")) %>%
  mutate(body = gsub("[^\x01-\x7F]", "", tolower(body))) %>% # to remove emoticons
  mutate(body = gsub("\n", " ", body)) %>%
  mutate(body = gsub("/", " ", body)) %>%
  mutate(methylphenidate = ifelse(grepl("methylphenidate", tolower(body)) | 
                                    grepl("methylphenidate", tolower(title)), 1,0))  %>%
  mutate(concerta = ifelse(grepl("concerta", tolower(body)) | 
                                    grepl("concerta", tolower(title)), 1,0)) 
################################################################################
# textClassify(x = adhd$body[13], device = 'cpu', model = 'distilbert-base-uncased-finetuned-sst-2-english') #it has some issues
adhd.e <- cbind(adhd, sentiment_by(get_sentences(adhd$body)))
sub <- adhd.e %>%
  summarise(n(), .by = author) %>%
  filter(`n()`>1) %>%
  filter(author != "[deleted]")
adhd.e %>%
  drop_na(sd) %>%
  filter(author %in% sub$author)  %>%
  group_by(author) %>%
  filter(n_distinct(methylphenidate)==2) %>%
  ungroup() %>%
  group_by(author,methylphenidate) %>%
  mutate_at(.vars = vars(word_count, ave_sentiment), .funs = function(x) mean(x)) %>%
  ungroup() %>%
  distinct(author, methylphenidate, .keep_all = T) %>%
  mutate(methylphenidate = as.factor(methylphenidate)) %>%
  select(author, ave_sentiment, methylphenidate) %>%
  pivot_wider(names_from = methylphenidate, values_from = ave_sentiment, id_cols = c(author)) %>%
  rename(on_MPH = 2, off_MPH = 3) %>%
  ggpaired(cond1 = "off_MPH", cond2 = "on_MPH", 
           line.color = "gray", line.size = 0.1, palette = "jco", 
           ylab = "", xlab = "", point.size = 0.3) +
  theme_minimal() +
  stat_compare_means(paired = T, size = 3, method = "t.test")
  
