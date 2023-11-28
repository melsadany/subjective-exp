################################################################################
#                   identify bp users from mh subreddits data                  #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
# args <- commandArgs(trailingOnly = T)
# args <- 40
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# list subreddits with extracted data
files <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits/filtered"), 
                                   full.names = T, pattern = ".rds")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/filtered/", "",f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))

subreddit.c <- read_rds(files$f[12])
tmp <- subreddit.c %>% 
  select(author, body, timestamp_created) %>%
  mutate(diagnose = grepl("diagnos", body)) %>%
  filter(diagnose == T) %>%
  mutate(iam_diagnosed = grepl("I am diagnosed", body, ignore.case = T) | 
           grepl("I'm diagnosed", body, ignore.case = T) | 
           grepl("I was diagnosed", body, ignore.case = T) | 
           grepl("I have been diagnosed", body, ignore.case = T) | 
           grepl("I've been diagnosed", body, ignore.case = T) |
           grepl("I am bipolar", body, ignore.case = T) | 
           grepl("I'm bipolar", body) |
           grepl("being diagnosed", body) | 
           grepl("diagnosed me", body) 
         )
  # mutate(embeding_text = paste0("the comment was written in ", timestamp_created, ", and says: ",
  #                               body, " when was the person diagnosed in a date format?"))



diagnosed_users <- tmp %>% 
  filter(iam_diagnosed==T) %>%
  distinct(author)
write_lines(diagnosed_users$author, file = "data/derivatives/diagnosed-bp-users")
