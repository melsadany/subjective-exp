################################################################################
#                   identify bp users from mh subreddits data                  #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
# args <- commandArgs(trailingOnly = T)
# args <- 40
index <- 12
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# list subreddits with extracted data
files <- data.frame(f = list.files(paste0(project.dir, 
                                          # "/data/derivatives/subreddits/filtered"), 
                                          "/data/derivatives/subreddits2/filtered"), 
                                   full.names = T, pattern = ".rds")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/filtered/", "",f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))
subreddit <- files$subreddit[index]
subreddit.c <- read_rds(files$f[index])
tmp <- subreddit.c %>% 
  select(author, body, timestamp_created) %>%
  mutate(bp = grepl("bipolar", body, ignore.case = T),
         manic = grepl("manic", body, ignore.case = T) | grepl("mania", body, ignore.case = T),
         depression = grepl("depression", body, ignore.case = T) | grepl("depressed", body, ignore.case = T),
         hypomanic = grepl("hypomanic", body, ignore.case = T) | grepl("hypo manic", body, ignore.case = T),
         psychosis = grepl("psychosis", body, ignore.case = T),
         anxiety = grepl("anxiety", body, ignore.case = T) | grepl("anxious", body, ignore.case = T),
         weight = grepl("pounds", body, ignore.case = T) | grepl("weight", body, ignore.case = T)) %>%
  # mutate(diagnose = grepl("diagnos", body)) %>%
  # filter(diagnose == T) %>%
  mutate(iam_diagnosed = grepl("I am diagnosed", body, ignore.case = T) | 
           grepl("I'm diagnosed", body, ignore.case = T) | 
           grepl("I was diagnosed", body, ignore.case = T) | 
           grepl("I have been diagnosed", body, ignore.case = T) | 
           grepl("I've been diagnosed", body, ignore.case = T) |
           grepl("being diagnosed", body) | 
           grepl("diagnosed me", body) 
         ) %>%
  mutate(iam_bp = grepl("I am bipolar", body, ignore.case = T) | 
           grepl("I'm bipolar", body) |
           grepl("I am diagnosed bipolar", body, ignore.case = T) | 
           grepl("I am diagnosed with bipolar", body, ignore.case = T) | 
           grepl("I'm diagnosed bipolar", body, ignore.case = T) | 
           grepl("I'm diagnosed with bipolar", body, ignore.case = T) | 
           grepl("I was diagnosed bipolar", body, ignore.case = T) | 
           grepl("I was diagnosed with bipolar", body, ignore.case = T) | 
           grepl("I have been diagnosed bipolar", body, ignore.case = T) | 
           grepl("I have been diagnosed with bipolar", body, ignore.case = T) | 
           grepl("I've been diagnosed bipolar", body, ignore.case = T) |
           grepl("I've been diagnosed with bipolar", body, ignore.case = T) |
           grepl("being diagnosed bipolar", body) | 
           grepl("being diagnosed with bipolar", body) | 
           grepl("diagnosed me bipolar", body) |
           grepl("diagnosed me with bipolar", body))
  # mutate(embeding_text = paste0("the comment was written in ", timestamp_created, ", and says: ",
  #                               body, " when was the person diagnosed in a date format?"))



diagnosed_users <- tmp %>% 
  mutate(overall_diagnosis = iam_diagnosed | iam_bp) %>%
  filter(overall_diagnosis==T) %>%
  # filter(bp == T) %>%
  select(author, body, overall_diagnosis, bp,iam_diagnosed,iam_bp,manic,depression,hypomanic,psychosis,anxiety,weight)  %>%
  distinct(author, .keep_all = T)

write_lines(diagnosed_users$author, file = paste0("data/derivatives/diagnosed-bp-users_ids_s-", subreddit))
write_rds(diagnosed_users, file = paste0("data/derivatives/diagnosed-bp-users_meta_s-", subreddit, ".rds"))
