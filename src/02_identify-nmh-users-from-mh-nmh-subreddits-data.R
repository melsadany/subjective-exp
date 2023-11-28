################################################################################
#                identify nmh users from mh nmh subreddits data                #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# identify mental-health users as the ones commenting in any of the mental health subreddits
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/authors-summary"

files <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))

registerDoMC(cores = 4)
mh.users.df <- foreach(i= 1: nrow(files), .combine = rbind) %dopar% {
  t <- read_rds(files$f[i]) %>%
    mutate(subreddit = files$subreddit[i])
  return(t)
}
write_rds(mh.users.df, "data/derivatives/subreddits/authors-summary/all-users-all-subreddits-summary-long.rds")
mh.users <- mh.users.df %>%
  distinct(author)
writeLines(mh.users$author, "data/derivatives/66-subreddits-mental-health-users")
####
# non mental-health subreddits
# list files of authors in subreddits
files <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/non-mental-health-subreddits/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/non-mental-health-subreddits/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))

registerDoMC(cores = 4)
nmh.users.df <- foreach(i= 1: nrow(files), .combine = rbind) %dopar% {
  t <- read_rds(files$f[i]) %>%
    mutate(subreddit = files$subreddit[i])
  return(t)
}
write_rds(nmh.users.df, "data/derivatives/non-mental-health-subreddits/authors-summary/all-users-all-subreddits-summary-long.rds")

# filter users to active ones with more than 10 comments per subreddit
nmh.users.df <- nmh.users.df %>% filter(count>10)
# get ones not active in any mental health subreddits
all.mh.nmh.users <- data.frame(author = unique(c(nmh.users.df$author, mh.users$author))) %>%
  mutate(mh = ifelse(author %in% mh.users$author, T, F),
         nmh = ifelse(author %in% nmh.users.df$author, T, F))
nmh <- all.mh.nmh.users %>%
  filter(mh == F, nmh == T)
writeLines(nmh$author, "data/derivatives/users-of-no-mental-health-subreddits-at-all")
writeLines(nmh$author[sample(x = c(1:nrow(nmh)), size = 30000)], 
           "data/derivatives/users-of-no-mental-health-subreddits-at-all-sample-30k")
####



