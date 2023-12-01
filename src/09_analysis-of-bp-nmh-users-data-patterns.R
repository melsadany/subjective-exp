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
# bp.summ <- data.frame(file = list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary", 
#                                         full.names = T, pattern = "_comments.rds")) %>%
#   mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary/", "", file)) %>%
#   mutate(user = sub("_comments.rds", "", user))
# registerDoMC(cores = 4)
# bp.summ.all <- foreach(i=1:nrow(bp.summ), .combine = rbind) %dopar% {
#   t <- read_rds(bp.summ$file[i])
#   return(t)
# }
# write_rds(bp.summ.all, "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary/all-users-summary-combined-long.rds")
bp.summ.all <- read_rds("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/authors-summary/all-users-summary-combined-long.rds")
# nmh users
# do this once
# nmh.summ <- data.frame(file = list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary", 
#                                         full.names = T, pattern = "_comments.rds")) %>%
#   mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary/", "", file)) %>%
#   mutate(user = sub("_comments.rds", "", user))
# registerDoMC(cores = 4)
# nmh.summ.all <- foreach(i=1:nrow(nmh.summ), .combine = rbind) %dopar% {
#   t <- read_rds(nmh.summ$file[i])
#   return(t)
# }
# write_rds(nmh.summ.all, "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary/all-users-summary-combined-long.rds")
nmh.summ.all <- read_rds("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/authors-summary/all-users-summary-combined-long.rds")
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
################################################################################
# compare sd of commenting history of users
users <- data.frame(file = c(list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/filtered", pattern = "_comments.rds", full.names = T),
                             list.files("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/filtered", pattern = "_comments.rds", full.names = T))) %>%
  mutate(ta_file = sub("filtered/", "filtered/text-analyzed/", file),
         ta_file = sub("\\.rds", ".rds.rds", ta_file)) %>%
  mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/filtered/", "", file)) %>%
  mutate(user = sub("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/filtered/", "", user)) %>%
  mutate(user = sub("_comments.rds", "", user), 
         cat = ifelse(grepl("bp_", file), "bp", "nmh"))
registerDoMC(cores = 3)
all.dates <- data.frame(date_created = seq(as.Date("2020-03-01"), as.Date("2022-03-31"), by = "day"))

# decide if just doing covid or not
just_covid = T
# users.sd <- foreach(i = 1:nrow(users), .combine = rbind) %dopar% {
users.stats.pre <- foreach(i = 1:nrow(users), .combine = rbind) %dopar% {
  user <- users$user[i]
  df <- read_rds(users$file[i]) %>%
    mutate(date_created = as_date(timestamp_created),
           year_created = year(date_created),
           month_created = month(date_created),
           day_created = day(date_created))%>%
    mutate(Q = ifelse(month_created %in% c(1:3), 1, 
                      ifelse(month_created %in% c(4:6), 2,
                             ifelse(month_created %in% c(7:9), 3, 4))))
  # if (sum(duplicated(df$body))>3) {
  #   print("This user is a bot")
  #   return(NULL)
  # }
  if (just_covid==T) {
    covid.valid <- df %>%
      group_by(year_created, month_created) %>%
      dplyr::summarise(count = n()) %>%
      filter(year_created <= 2020 & month_created <= 2 | year_created >= 2022 & month_created >= 4) %>%
      mutate(status = ifelse(year_created <= 2020, "before", "after"))
    covid.valid.d <- ifelse(length(unique(covid.valid$status))==2, T, F)
    Q.valid <- df %>% 
      group_by(year_created, Q) %>%
      dplyr::summarise(count = n())
    all.q <- data.frame(year_created = rep(c(2020:2022), each = 4),
                        Q = rep(c(1:4),3))
    c <- right_join(Q.valid, all.q) %>% 
      mutate(count = ifelse(is.na(count), 0, count))
    Q.valid.d <- ifelse(all(c$count>0),T,F)
    if (covid.valid.d) {
      df <- df %>%
        filter(date_created >= as.Date("2020-03-01") & date_created <= as.Date("2022-03-31"))
      print(paste0("This user is valid for covid-period criteria and ",
                   ifelse(Q.valid.d == T, "valid", "not valid"),
                   " for the comment-per-quarter criteria, with ",
                   nrow(df), " comments"))
    }else{
      print("This user is not valid for covid-period criteria")
      # return()
    }
  }
  # df <- read_rds(users$file[i]) %>% mutate(date_created = as_date(timestamp_created))
  if (!file.exists(users$ta_file[i])) {
    return(NULL)
  }
  df.tn <- read_rds(users$ta_file[i]) %>%
    mutate(date_created = as_date(timestamp_created),
           year_created = year(date_created),
           month_created = month(date_created),
           day_created = day(date_created))%>%
    mutate(Q = ifelse(month_created %in% c(1:3), 1, 
                      ifelse(month_created %in% c(4:6), 2,
                             ifelse(month_created %in% c(7:9), 3, 4))))
  df.tn <- inner_join(df.tn, df %>% select(timestamp_created))
  if (just_covid == T) {
    if (covid.valid.d == T & Q.valid.d == T & nrow(df)>0) {
      file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_q_valid/")
    } else if (covid.valid.d == T & Q.valid.d == F & nrow(df)>0) {
      file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_q_notvalid/")
    } else {
      file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_notvalid_q_notvalid/")
    }
    system(paste0("mkdir -p ", file.prefix))
    write_rds(df.tn, 
              file = paste0(file.prefix, users$cat[i],"_user-", user, "_text-analyzed.rds"), 
              compress = "gz")
    if (covid.valid.d == F | nrow(df) == 0) {
      return(NULL)
    }
  }
  tmp <- left_join(all.dates, 
                   data.frame(table(df$date_created)) %>% 
                     mutate(date_created=as_date(Var1))) %>%
    mutate(Freq = ifelse(is.na(Freq), 0, Freq)) %>%
    select(-Var1)
  tmp.weeks <- tmp %>%
    mutate(week = c(rep(c(1:108), each = 7), rep(109, 5))) %>%
    group_by(week) %>%
    dplyr::summarise(count = sum(Freq))
  t <- data.frame(user = users$user[i],
                  cat = users$cat[i],
                  sd_comment_freq = sd(table(df$date_created)),
                  mean_comment_freq = mean(table(df$date_created)),
                  # ac = t(as.numeric(acf(tmp$Freq, lag.max = 108)$acf))
                  ac_comment_freq = t(as.numeric(acf(tmp$Freq, lag.max = 108)$acf)),
                  fft_comment_freq = t(fft(tmp.weeks$count)))
  tmp2 <- left_join(all.dates,
                    df.tn %>% 
                      mutate_at(.vars = vars(nrc.positive, nrc.negative, nrc.anger, nrc.anticipation, nrc.disgust,
                                             nrc.fear, nrc.joy, nrc.sadness, nrc.surprise, nrc.trust),
                                .funs = function(x) x/df.tn$sentimentr.word_count) %>%
                      group_by(date_created) %>%
                      dplyr::summarise_at(.vars = vars(ends_with("_sentiment"),
                                                       "readability", 
                                                       starts_with("nrc")), 
                                          .funs = function(x) mean(x))) %>%
    mutate_at(.vars = vars(ends_with("_sentiment"),
                           "readability", 
                           starts_with("nrc")), 
              .funs = function(x) ifelse(is.na(x), 0, x))
  tmp2.weeks <- tmp2 %>%
    mutate(week = c(rep(c(1:108), each = 7), rep(109, 5))) %>%
    group_by(week) %>%
    dplyr::summarise_at(.vars = vars(ends_with("_sentiment"),
                                     "readability", 
                                     starts_with("nrc")),
                        .funs = function(x) mean(x))
  acv <- foreach (j = 2:ncol(tmp2), .combine = cbind) %dopar% {
    var <- colnames(tmp2)[j]
    if (min(tmp2[,j]) == max(tmp2[,j])) {
      acv.t <- data.frame(sd = 0,
                          mean = 0,
                          matrix(ncol = 109, nrow = 1, 0))
      } else {
        acv.t <- data.frame(sd = sd(tmp2[,j]),
                            mean = mean(tmp2[,j]),
                            t(as.numeric(acf(tmp2[,j], lag.max = 108)$acf)))
      }
    colnames(acv.t) <- c(paste0("sd_", var),
                         paste0("mean_", var),
                         paste0("ac_", var, ".", c(1:109)))
    return(acv.t)
    }
  t <- cbind(t, acv) %>%
    mutate(covid_valid = covid.valid.d,
           q_valid = Q.valid.d)
  return(t)
}
# write_rds(users.stats.pre, "data/derivatives/all-users/combined-covid-valid-users.rds")
# users.stats.pre <- read_rds("data/derivatives/all-users/combined-covid-valid-users.rds")
######
users.stats <- users.stats.pre
# users.stats <- users.stats.pre %>% filter(q_valid == T)

p1 <- users.stats %>%
  pivot_longer(starts_with("ac"), names_to = "ac_var", values_to = "acf") %>%
  mutate(ac_lag = round(readr::parse_number(str_replace_all(pattern = "\\.", replacement = "",string = ac_var)))) %>%
  mutate(var = sub("ac_", "", ac_var),
         var = sub("\\.[0-9]+", "", var)) %>%
  ggplot(aes(x=ac_lag, y=acf, group = user, color = cat)) +
  geom_line(alpha = 0.3) +
  # facet_wrap(~cat) +
  ggh4x::facet_grid2(cols = vars(var), rows = vars(cat), scales = "free", space = "free") +
  scale_color_manual(values = redblu.col)
p2 <- users.stats %>%
  pivot_longer(starts_with("ac"), names_to = "ac_var", values_to = "acf") %>%
  mutate(ac_lag = round(readr::parse_number(str_replace_all(pattern = "\\.", replacement = "",string = ac_var)))) %>%
  mutate(var = sub("ac_", "", ac_var),
         var = sub("\\.[0-9]+", "", var)) %>%
  group_by(user,cat,var) %>%
  dplyr::summarise(avg_acf = mean(acf)) %>%
  ggplot(aes(x=cat, y=avg_acf, fill = cat)) +
  geom_violin() +
  ggpubr::stat_compare_means(size = 2.5) +
  facet_wrap(~var, nrow = 1, scales = "free") +
  scale_fill_manual(values = redblu.col) +
  geom_boxplot(width = 0.1, show.legend = F, fill = "white") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
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
p <- patchwork::wrap_plots(p1,p2,p3,p4,nrow = 4, heights = c(3,1,1,1))
ggsave(p3,filename = "figs/boxplot_sd-of-all_bp-nmh_covid.png",
# ggsave(p3,filename = "figs/boxplot_sd-of-all_bp-nmh_covid_q-valid.png", 
       width = 24.7, height = 5.7, units = "in", dpi = 320, bg = "white")
ggsave(p4,filename = "figs/boxplot_mean-of-all_bp-nmh_covid.png",
# ggsave(p4,filename = "figs/boxplot_mean-of-all_bp-nmh_covid_q-valid.png", 
       width = 24.7, height = 5.7, units = "in", dpi = 320, bg = "white")
ggsave(p2,filename = "figs/boxplot_acf-of-all_bp-nmh_covid.png",
# ggsave(p2,filename = "figs/boxplot_acf-of-all_bp-nmh_covid_q-valid.png", 
       width = 24.7, height = 5.7, units = "in", dpi = 320, bg = "white")
ggsave(p,filename = "figs/boxplot-of-all_bp-nmh_covid.png",
# ggsave(p,filename = "figs/boxplot-of-all_bp-nmh_covid_q-valid.png", 
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
ggsave(filename = "figs/count-of-users.png",
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
