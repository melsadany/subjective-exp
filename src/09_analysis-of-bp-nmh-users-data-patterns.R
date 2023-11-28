################################################################################
#                   analysis of bp and nmh users data patterns                 #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# get a all authors summary of bp, and nmh
# bp users
bp.summ <- data.frame(file = list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary", 
                                        full.names = T, pattern = "_comments.rds")) %>%
  mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary/", "", file)) %>%
  mutate(user = sub("_comments.rds", "", user))
registerDoMC(cores = 4)
bp.summ.all <- foreach(i=1:nrow(bp.summ), .combine = rbind) %dopar% {
  t <- read_rds(bp.summ$file[i])
  return(t)
}
write_rds(bp.summ.all, "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary/all-users-summary-combined-long.rds")
# nmh users
nmh.summ <- data.frame(file = list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary", 
                                        full.names = T, pattern = "_comments.rds")) %>%
  mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary/", "", file)) %>%
  mutate(user = sub("_comments.rds", "", user))
registerDoMC(cores = 4)
nmh.summ.all <- foreach(i=1:nrow(nmh.summ), .combine = rbind) %dopar% {
  t <- read_rds(nmh.summ$file[i])
  return(t)
}
write_rds(nmh.summ.all, "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary/all-users-summary-combined-long.rds")
####
# combine both
users.summ.all <- rbind(bp.summ.all %>% 
                          group_by(subreddit) %>% 
                          dplyr::summarise(users_count = n()) %>% 
                          mutate(bp = 1), 
                        nmh.summ.all %>% 
                          group_by(subreddit) %>% 
                          dplyr::summarise(users_count = n()) %>% 
                          mutate(bp = 0)) %>%
  mutate(bp = as.factor(bp)) 
subreddits.tot <- users.summ.all %>%
  group_by(subreddit, bp) %>%
  dplyr::summarise(tot_users = sum(users_count)) %>%
  pivot_wider(names_prefix = "bp_", names_from = "bp", values_from = "tot_users", values_fill = 0) %>%
  mutate(tot_users = sum(bp_0, bp_1),
         bp_0_per = (bp_0/tot_users)*100,
         bp_1_per = (bp_1/tot_users)*100)
order <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  arrange(desc(bp_1_per)) %>%
  select(subreddit)
p1 <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  pivot_longer(cols = ends_with("per"), values_to = "users_percentage", names_to = "bp") %>%
  mutate(bp = as.factor(readr::parse_number(bp))) %>%
  mutate(subreddit = factor(subreddit, levels = order$subreddit)) %>%
  ggplot(aes(x=subreddit, y = users_percentage, fill = bp)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, color = "black", linetype = 2) +
  scale_fill_manual(values = redblu.col) +
  labs(y = "percentage of users")
p2 <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  ggplot(aes(x=reorder(subreddit, desc(tot_users)), y = tot_users)) +
  geom_bar(stat = "identity") +
  labs(y = "number of users", x="")
p3 <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  pivot_longer(cols = c("bp_0", "bp_1"), values_to = "users_count", names_to = "bp") %>%
  mutate(bp = as.factor(readr::parse_number(bp))) %>%
  ggplot(aes(x=reorder(subreddit, desc(tot_users)), y = users_count, fill = bp)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(values = redblu.col) +
  labs(y = "number of users", x="")
ggsave(patchwork::wrap_plots(p3,p1, ncol = 1), filename = "figs/bp-nmh-percentage-by-subreddit.png", 
       width = 24, height = 9, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################



################################################################################


# check correlation between number of users posting in specific subreddit
# only look at the 10k controls and ~10k bp users
users.meta <- rbind(data.frame(author = readLines("data/derivatives/diagnosed-bp-users")) %>%
                      mutate(class = "bp"),
                    data.frame(author = readLines("data/derivatives/users-of-no-mental-health-subreddits-at-all-sample-10k")) %>%
                      mutate(class = "control"))
bp.authors.summary <- read_rds("data/derivatives/bp_users_data/authors-summary/all")
nmh.users.summary <- read_rds("data/derivatives/nmh_users_data/authors-summary/all")
