################################################################################
#                   analysis of bp and nmh users data patterns                 #
################################################################################
rm(list = ls())
gc()
source("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(qdap)
pdssave <- function(...,file){  
  con = pipe(paste("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/workbench/pixz -2 -q 80 -f 3 > ",file,".pxz",sep=""),"wb") 
  saveRDS(...,file=con)
}
pdsload <- function(fname,envir=.GlobalEnv){
  con <- pipe(paste("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/workbench/pixz -d <",fname),"rb")
  return(readRDS(con))
}
################################################################################
################################################################################
project.dir <- "/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
################################################################################
#################### subreddits usage percentage by group ######################
################################################################################
################################################################################
# get a all authors summary of bp, and nmh
# the author summary files are for full history of users, not just covid period

# covid valid q list
covid.users <- data.frame(file = list.files("data/derivatives/all-users/covid_valid_q_valid", 
                                            pattern = ".rds")) %>%
  mutate(cat = ifelse(grepl("bp_user-", file), "bp", "nmh"),
         user = sub("bp_user-", "", file),
         user = sub("nmh_user-", "", user),
         user = sub("\\.rds", "", user))
######
# bp users
# bp.summ <- data.frame(file = list.files("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-bp-users/authors-summary",
#                                         full.names = T, pattern = "_comments.rds")) %>%
#   mutate(user = sub("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-bp-users/authors-summary/", "", file)) %>%
#   mutate(user = sub("_comments.rds", "", user)) %>%
#   filter(user %in% covid.users$user)
# registerDoMC(cores = 4)
# bp.summ.all <- foreach(i=1:nrow(bp.summ), .combine = rbind) %dopar% {
#   t <- read_rds(bp.summ$file[i])
#   return(t)
# }
# write_rds(bp.summ.all, "/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-bp-users/authors-summary/covid-valid-q-valid-users-summary-combined-long.rds")
bp.summ.all <- read_rds("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-bp-users/authors-summary/covid-valid-q-valid-users-summary-combined-long.rds")
######
# nmh users
# do this once
# nmh.summ <- data.frame(file = list.files("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-nmh-users/authors-summary",
#                                         full.names = T, pattern = "_comments.rds")) %>%
#   mutate(user = sub("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-nmh-users/authors-summary/", "", file)) %>%
#   mutate(user = sub("_comments.rds", "", user)) %>%
#   filter(user %in% covid.users$user)
# registerDoMC(cores = 4)
# nmh.summ.all <- foreach(i=1:nrow(nmh.summ), .combine = rbind) %dopar% {
#   t <- read_rds(nmh.summ$file[i])
#   return(t)
# }
# write_rds(nmh.summ.all, "/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-nmh-users/authors-summary/covid-valid-q-valid-users-summary-combined-long.rds")
nmh.summ.all <- read_rds("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-nmh-users/authors-summary/covid-valid-q-valid-users-summary-combined-long.rds")
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
  labs(y = "percentage of users") +
  theme(axis.text.x.bottom = element_text(size=4))
p2 <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  ggplot(aes(x=reorder(subreddit, desc(tot_users)), y = tot_users)) +
  geom_bar(stat = "identity") +
  labs(y = "number of users", x="") +
  theme(axis.text.x.bottom = element_text(size=4))
p3 <- subreddits.tot %>% 
  filter(tot_users > 1000) %>%
  pivot_longer(cols = c("bp_0", "bp_1"), values_to = "users_count", names_to = "bp") %>%
  mutate(bp = as.factor(readr::parse_number(bp))) %>%
  ggplot(aes(x=reorder(subreddit, desc(tot_users)), y = users_count, fill = bp)) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(values = redblu.col) +
  labs(y = "number of users", x="") +
  theme(axis.text.x.bottom = element_text(size=4))
ggsave(patchwork::wrap_plots(p3,p1, ncol = 1), filename = "figs/bp-nmh-percentage-by-subreddit-covid-valid-q-valid.png", 
       width = 20, height = 9, units = "in", dpi = 320, bg = "white")
################################################################################
################################################################################
################## list covid users of interest both groups ####################
################################################################################
################################################################################
## get a list of all users you wanna look at
users <- data.frame(file = c(list.files("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_q_valid", pattern = "\\.rds", full.names = T))) %>%
  mutate(ta_file = sub("covid_valid_q_valid/", "covid_valid_no-q/text-analyzed/", file),
         te_file = sub("covid_valid_q_valid/", "covid_valid_no-q/text-embeddings/", file)) %>%
  mutate(user = sub("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_q_valid/", "", file)) %>%
  mutate(user = sub("\\.rds", "", user), 
         user = sub("bp_user-", "", user),
         user = sub("nmh_user-", "", user),
         cat = ifelse(grepl("bp_user-", file), "bp", "nmh"))
registerDoMC(cores = 5)
# define dates of the covid period from start of 03/2020 to end of 03/2022
all.dates <- data.frame(date_created = seq(as.Date("2020-03-01"), as.Date("2022-03-31"), by = "day"))
################################################################################
################################################################################
############## summary stats of languaga/comment features per user #############
################################################################################
################################################################################
## you want to get the simple features with fewer dimensions here
# these could be mean, sd of the main features of interest like freq, n_misspelled, etc.
users.stats.pre1 <- foreach(i = 1:nrow(users), .combine = rbind) %dopar% {
  # get the main dataframe of the user history
  user <- users$user[i]
  df <- read_rds(users$file[i]) %>%
    mutate(date_created = as_date(timestamp_created),
           day_created = day(date_created))
  #####################
  # do the qdap methods here
  # number of misspelled words
  sp <- check_spelling(df$body, n.suggest = 0, parallel = F)
  n_misspelled <- as.vector(table(factor(sp$row, levels = c(0:nrow(df)))))
  spe.all <- data.frame(n = sum(n_misspelled),
                        mean = mean(n_misspelled),
                        sd = sd(n_misspelled))
  colnames(spe.all) <- paste0("spelling_", colnames(spe.all))
  # diversity of words in all of their comment history
  diversity <- qdap::diversity(df$body)[,-1]
  colnames(diversity) <- paste0("diversity_", colnames(diversity))
  # formality of their comments of all time
  lexical.class <- qdap::lexical_classification(df$body)
  lexical <- data.frame(lexical_wc = lexical.class$lexical_classification$word.count,
                        lexical_n_content = lexical.class$lexical_classification$n.content,
                        lexical_n_functional = lexical.class$lexical_classification$n.functional)
  # pronouns
  object.pronoun <- qdap::object_pronoun_type(df$body)$prop[,-1]
  subject.pronoun <- qdap::subject_pronoun_type(df$body)$prop[,-1]
  pronouns <- inner_join(object.pronoun, subject.pronoun)
  colnames(pronouns) <- paste0("pronouns_", colnames(pronouns))
  # combine qdap features
  all.qdap <- cbind(spe.all, diversity, lexical, pronouns)
  ####################
  # get the other features from text-analyzed files
  if (!file.exists(users$ta_file[i])) {
    return(NULL)
  }
  df.ta <- read_rds(users$ta_file[i]) %>%
    mutate(date_created = as_date(timestamp_created),
           day_created = day(date_created)) %>%
    filter(!grepl("https\\:", body)) %>% # drop the comments that have links, because they mess up the readability score
    select(-c(6,8)) %>%
    left_join(df %>% select(body, controversiality, score))
  # get the main stats of all variables from language analysis and reddit score/controversiality
  stats <- do.call(rbind, 
                   lapply(df.ta[,c(6:63, 65:66)], function(x) {
                     s <- sd(as.numeric(x), na.rm = T)
                     summ <- as.numeric(summary(x))
                     return(data.frame(min = summ[1],
                                       f_Q = summ[2],
                                       median = summ[3],
                                       mean = summ[4],
                                       t_Q = summ[5],
                                       max = summ[6],
                                       sd = s))
                   })) %>%
    rownames_to_column("var")
  # make stats in a wide format
  stats.wide <- stats %>% 
    pivot_longer(cols = colnames(stats)[-1], names_to = "gp") %>%
    mutate(name = paste0(var, "__", gp)) %>% select(-gp, -var) %>%
    pivot_wider(names_from = name, values_from = value)
  ####################
  # look at comment frequency
  freq <- df %>% 
    group_by(date_created) %>%
    dplyr::summarise(count = n()) %>%
    full_join(all.dates) %>%
    mutate(count = ifelse(is.na(count), 0, count)) %>%
    mutate(week = c(rep(c(1:108), each = 7), rep(109, 5))) %>%
    group_by(week) %>%
    dplyr::summarise(count = sum(count))
  freq.summ <- as.numeric(summary(freq$count))
  freq.summ <- data.frame(min = freq.summ[1],
                          f_Q = freq.summ[2],
                          median = freq.summ[3],
                          mean = freq.summ[4],
                          t_Q = freq.summ[5],
                          max = freq.summ[6],
                          sd = sd(freq$count))
  colnames(freq.summ) <- paste0("freq__", colnames(freq.summ))
  ####################
  # combine all stats
  all <- data.frame(user = user, cat = users$cat[i],
                    cbind(freq.summ, stats.wide, all.qdap))
  print(paste0("Done with user: ", i, " ", user))
  return(all)
}
write_rds(users.stats.pre1, "data/derivatives/all-users/summary-stats-per-user-covid.rds")
# users.stats.pre1 <- read_rds("data/derivatives/all-users/summary-stats-per-user-covid.rds")
################################################################################
################################################################################
#################### compare features between both groups ######################
################################################################################
################################################################################






################################################################################
################################################################################
#################### subreddits usage percentage by group ######################
################################################################################
################################################################################



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
######
users.stats <- users.stats.pre
users.stats <- users.stats[-c(3592,3217),] # possible to be bots
# users.stats <- users.stats.pre %>% filter(q_valid == T)
p3 <- users.stats %>%
  pivot_longer(starts_with("sd"), names_to = "sd_var", values_to = "sd") %>%
  mutate(var = sub("sd_", "", sd_var),
         var = sub("\\.[0-9]+", "", var)) %>%
  ggplot(aes(x=cat, y=sd, fill = cat)) +
  geom_violin() +
  ggpubr::stat_compare_means(size = 2.5) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  scale_fill_manual(values = redblu.col) +
  geom_boxplot(width = 0.1, show.legend = F, fill = "white") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
p4 <- users.stats %>%
  pivot_longer(starts_with("mean"), names_to = "mean_var", values_to = "mean") %>%
  mutate(var = sub("mean_", "", mean_var),
         var = sub("\\.[0-9]+", "", var)) %>%
  ggplot(aes(x=cat, y=mean, fill = cat)) +
  geom_violin() +
  ggpubr::stat_compare_means(size = 2.5) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  scale_fill_manual(values = redblu.col) +
  geom_boxplot(width = 0.1, show.legend = F, fill = "white") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
p5 <- users.stats %>%
  pivot_longer(starts_with("fft")&(contains("freq")|contains("sentiment")), names_to = "fft_var", values_to = "fft") %>%
  mutate(var = sub("fft_", "", fft_var),
         var = sub("\\.[0-9]+", "", var)) %>%
  mutate(fft = Re(fft)) %>%
  ggplot(aes(x=cat, y=fft, fill = cat)) +
  geom_violin() +
  ggpubr::stat_compare_means(size = 2.5) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  scale_fill_manual(values = redblu.col) +
  geom_boxplot(width = 0.1, show.legend = F, fill = "white") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
# p <- patchwork::wrap_plots(p1,p2,p3,p4,nrow = 4, heights = c(3,1,1,1))
ggsave(p3,filename = "figs/boxplot_sd-of-all_bp-nmh_covid_q-valid.png",
       width = 24.7, height = 5.7, units = "in", dpi = 320, bg = "white")
ggsave(p4,filename = "figs/boxplot_mean-of-all_bp-nmh_covid_q-valid.png",
       width = 24.7, height = 5.7, units = "in", dpi = 320, bg = "white")
ggsave(p,filename = "figs/boxplot-of-all_bp-nmh_covid_q-valid.png",
       width = 24.7, height = 20, units = "in", dpi = 320, bg = "white")
################################################################################
users.stats %>%
  group_by(cat) %>%
  dplyr::summarise(count = n()) %>%
  ggplot(aes(x=cat, y=count, fill = cat, label = count)) +
  geom_bar(width = 0.4, stat = "identity", show.legend = F) +
  scale_fill_manual(values = redblu.col) +
  geom_text(size = 4) +
  labs(x="", y="number of users") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
ggsave(filename = "figs/count-of-users-covid-valid-q-valid.png",
# ggsave(filename = "figs/count-of-users_q-valid.png",
       width = 4.1, height = 4.1, units = "in", bg = "white")
users.stats %>%
  ggplot(aes(x=cat, y=sd_syuzhet_sentiment, fill = cat)) +
  geom_violin(show.legend = F, width = 0.4) +
  scale_fill_manual(values = redblu.col) +
  geom_boxplot(width = 0.1, show.legend = F, fill = "white") +
  ggpubr::stat_compare_means(size = 4) +
  labs(x="", y="standard deviation of syuzhet sentiment score")+
  theme(axis.text.x.bottom = element_text(angle = 0))
ggsave(filename = "figs/boxplot_sd-of-syuzhet-sentiment_bp-nmh_covid.png",
       width = 4.1, height = 4.1, units = "in", bg = "white")  


################################################################################


# check correlation between number of users posting in specific subreddit
# only look at the 10k controls and ~10k bp users
users.meta <- rbind(data.frame(author = readLines("data/derivatives/diagnosed-bp-users")) %>%
                      mutate(class = "bp"),
                    data.frame(author = readLines("data/derivatives/users-of-no-mental-health-subreddits-at-all-sample-10k")) %>%
                      mutate(class = "control"))
bp.authors.summary <- read_rds("data/derivatives/bp_users_data/authors-summary/all")
nmh.users.summary <- read_rds("data/derivatives/nmh_users_data/authors-summary/all")
