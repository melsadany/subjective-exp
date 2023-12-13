################################################################################
#                            usernames log by subreddit                        #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# list files of authors in subreddits
# combine data to make a dataframe of usernames as rows, and subreddits as colmns
# with filled values of number of comments per user per subreddit
files1 <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))
####
# add more subreddits to the list
files2 <- data.frame(f = list.files(paste0(project.dir, "/data/derivatives/subreddits2/authors-summary"), full.names = T, pattern = "comments")) %>%
  mutate(subreddit = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits2/authors-summary/", "", f),
         subreddit = sub(".rds", "", subreddit),
         subreddit = sub("_comments", "", subreddit))
files <- rbind(files1, files2)
####

registerDoMC(cores = 24)
users.summary <- foreach(i= 1: nrow(files), .combine = rbind) %dopar% {
  t <- read_rds(files$f[i]) %>%
    mutate(subreddit = files$subreddit[i])
  return(t)
}
write_rds(users.summary, "data/derivatives/identified-users/all-users-all-mh-nmh-subreddits-summary-long.rds")

####
# make a dataframe of users data by subreddit
users.log.df <- users.summary %>%
  pivot_wider(names_from = subreddit, values_from = count, id_cols = author, values_fill = NA)
users.log.df<- users.log.df %>%
  mutate(sum_of_comments = rowSums(users.log.df[,-1], na.rm = T))
# write_rds(users.log.df, "data/derivatives/subreddits/authors-summary/all-users-all-subreddits-summary-mtx.rds")
# write_rds(users.log.df, "data/derivatives/all-users-all-mh-nmh-subreddits-summary-mtx.rds")
####

# only keep users with >10 comments per subreddit
# drop deleted users and auto moderators 
users.log.df.filt <- users.summary %>%
  filter(count > 10, 
         !grepl("deleted", author), 
         !grepl("AutoModerator", author)) %>%
  pivot_wider(names_from = subreddit, values_from = count, id_cols = author, values_fill = NA)
users.log.df.filt.2 <- users.log.df.filt %>%
  mutate(sum_of_comments = rowSums(users.log.df.filt[,-1], na.rm = T)) %>%
  mutate(num_of_subreddits = ncol(users.log.df.filt[,-1])-rowSums(is.na(users.log.df.filt[,-1])))
  # filter(sum_of_comments > 50)
# scatter plot for number of subreddits per number of comments
p1 <- users.log.df.filt.2 %>%
  ggplot(aes(x=num_of_subreddits, y = sum_of_comments)) +
  geom_point(size = 0.2, alpha = 0.4)
# how many users per subreddit
p2 <- users.summary %>%
  filter(count > 10, !grepl("deleted", author), !grepl("AutoModerator", author)) %>%
  group_by(subreddit) %>%
  dplyr::summarise(count = n()) %>%
  ggplot(aes(x=subreddit, y = count)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "number of users", 
       caption = "I dropped the authors: [deleted] and 'AutoModerator' \n\tand only kept ones with > 10 comments in this subreddit")
# how many users that posted in how many subreddits
p3 <- users.log.df.filt.2 %>%
  group_by(num_of_subreddits) %>%
  dplyr::summarise(count = n()) %>%
  # mutate(num_of_subreddits = factor(num_of_subreddits, levels = c(1:max(users.log.df.filt.2$num_of_subreddits)))) %>%
  ggplot(aes(x = num_of_subreddits, y = count, label = count)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(angle = 45, size = 2.5, color = redblack.col[2]) +
  labs(caption = "this is showing how many users that posted in 1 subreddit or more")

p <- patchwork::wrap_plots(p2,p1,p3, ncol = 1, heights = c(2.5,1,1))
ggsave(p, filename = "figs/summary-plots-mh-nmh.jpeg", height = 10, width = 8, units = "in")
################################################################################
################################################################################
################################################################################
# 


################################################################################
####
# library(igraph)
# df <- users.summary %>%
#   filter(count > 10, !grepl("deleted", author), !grepl("AutoModerator", author)) %>%
#   select(username=author, subreddit)
# df2 <- full_join(df %>% rename(subreddit_x = subreddit),
#                  df %>% rename(subreddit_y = subreddit), relationship = "many-to-many") %>%
#   filter(subreddit_x != subreddit_y) %>%
#   group_by(subreddit_x, subreddit_y) %>%
#   dplyr::summarise(count = n()) 
# df3 <- df2 %>%
#   pivot_wider(names_from = subreddit_y, values_from = count, values_fill = 0) %>%
#   column_to_rownames("subreddit_x")
# 
# network <- graph_from_data_frame(df2[,-3], directed = F)
# deg <- degree(network, mode = "all")
# plot(network)
# 
# graph <- graph_from_adjacency_matrix(df2, weighted = T, mode = "undirected", diag = F)
# plot(graph, edge.width = E(graph)$user_count)
####