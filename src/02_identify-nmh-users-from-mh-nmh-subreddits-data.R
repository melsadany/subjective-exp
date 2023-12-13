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
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits2/authors-summary"

files1 <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))
files2 <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits2/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits2/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))
files2.mh.indices <- c(2:6, 8,9,15)
mh.files <- rbind(files1, files2[files2.mh.indices,])

registerDoMC(cores = 14)
mh.users.df <- foreach(i= nrow(mh.files), .combine = rbind) %dopar% {
  t <- read_rds(mh.files$f[i]) %>%
    mutate(subreddit = mh.files$subreddit[i])
  return(t)
}
mh.users.df <- mh.users.df %>%
  filter(!(grepl("\\[deleted]", author)))
# write_rds(mh.users.df, "data/derivatives/identified-users/ALL-mh-subreddits-users.rds")
mh.users <- mh.users.df %>%
  distinct(author)
writeLines(mh.users$author, "data/derivatives/identified-users/ALL-mh-subreddits-users")
####
# non mental-health subreddits
# list files of authors in subreddits
files2.nmh.indices <- c(1,7,10:14,16,17)
nmh.files <- files2[files2.nmh.indices,]

registerDoMC(cores = 4)
nmh.users.df <- foreach(i= 1: nrow(nmh.files), .combine = rbind) %dopar% {
  t <- read_rds(nmh.files$f[i]) %>%
    mutate(subreddit = nmh.files$subreddit[i])
  return(t)
}
nmh.users.df <- nmh.users.df %>%
  filter(!(grepl("\\[deleted]", author)))

# write_rds(nmh.users.df, "data/derivatives/identified-users/ALL-nmh-subreddits-users.rds")
nmh.users <- nmh.users.df %>%
  distinct(author)
# writeLines(nmh.users$author, "data/derivatives/identified-users/ALL-nmh-subreddits-users")


# filter users to active ones with more than 10 comments per subreddit
nmh.users.df <- nmh.users.df %>% filter(count>10)
# get ones not active in any mental health subreddits
all.mh.nmh.users <- data.frame(author = unique(c(nmh.users.df$author, mh.users$author))) %>%
  mutate(mh = ifelse(author %in% mh.users$author, T, F),
         nmh = ifelse(author %in% nmh.users.df$author, T, F))
nmh <- all.mh.nmh.users %>%
  filter(mh == F, nmh == T)
writeLines(nmh$author, "data/derivatives/identified-users/ALL-nmh-subreddits-users-with-no-comments-at-all-in-mh-subreddits")
writeLines(nmh$author[sample(x = c(1:nrow(nmh)), size = 35000)], 
           "data/derivatives/identified-users/ALL-nmh-subreddits-users-with-no-comments-at-all-in-mh-subreddits-35K")
####



