################################################################################
#                   analysis of bp and nmh users data patterns                 #
################################################################################
rm(list = ls())
gc()
source("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
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
# get a all authors summary of bp, and nmh
# the author summary files are for full history of users, not just covid period

# covid valid q list
covid.users <- data.frame(file = list.files("data/derivatives/all-users/covid_valid_q_valid", 
                                            pattern = ".rds")) %>%
  mutate(cat = ifelse(grepl("bp_user-", file), "bp", "nmh"),
         user = sub("bp_user-", "", file),
         user = sub("nmh_user-", "", user),
         user = sub("\\.rds", "", user))

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
       width = 26, height = 9, units = "in", dpi = 320, bg = "white")
################################################################################
################################################################################
################################################################################
################################################################################
# get a list of all users you wanna look at
users <- data.frame(file = c(list.files("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_q_valid", pattern = "\\.rds", full.names = T))) %>%
  mutate(ta_file = sub("covid_valid_q_valid/", "covid_valid_no-q/text-analyzed/", file),
         te_file = sub("covid_valid_q_valid/", "covid_valid_no-q/text-embeddings/", file)) %>%
  mutate(user = sub("/home/msmuhammad/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_q_valid/", "", file)) %>%
  mutate(user = sub("\\.rds", "", user), 
         user = sub("bp_user-", "", user),
         user = sub("nmh_user-", "", user),
         cat = ifelse(grepl("bp_user-", file), "bp", "nmh"))
registerDoMC(cores = 30)
# define dates of the covid period from start of 03/2020 to end of 03/2022
all.dates <- data.frame(date_created = seq(as.Date("2020-03-01"), as.Date("2022-03-31"), by = "day"))
################################################################################
################################################################################
# read the files of these users and save them in a combined file
# the dataframe at the end should be with these columns:
# category: bp or nmh
# user 
# subreddit
# date
# week
# weekday
# body
# all extracted features from the text-analyzed script
# all embeddings extracted from openai embeddins model
for (k in c(1,1001,2001,3001,4001,5001,6001,7001,8001,9001,10001,11001,12001,13001,14001,15001,16001)) {
  start <- k
  end <- min(nrow(users), k+999)
  print(paste0("start: ", start, " end:", end))
  # print(start)
  # print(end)
  # combined.files <- foreach(i=1:nrow(users), .combine = rbind) %dopar% {
  combined.files <- foreach(i=start:end, .combine = rbind) %dopar% {
    # user <- users$user[i]
    df <- read_rds(users$file[i]) %>%
      mutate(date_created = as_date(timestamp_created),
             weekday_created = weekdays(date_created))
    if(file.exists(users$ta_file[i]) & file.exists(users$te_file[i])) {
      # df.ta <- read_rds(users$ta_file[i])
      df.te <- read_rds(users$te_file[i])
    }else{
      return(NULL)
    }
    all <- inner_join(df %>% 
                        select(author, subreddit, date_created, weekday_created, body) %>%
                        mutate(cat = users$cat[i]),
                      # inner_join(df.ta, df.te))
                      df.te)
    rm(df)
    rm(df.te)
    gc()
    return(all)
  }
  pdssave(combined.files, file = paste0("data/derivatives/all-users/combined-covid-valid-q-valid-embeddings-", start,".rds"))
  rm(combined.files)
  gc()
}

# write_rds(combined.files, "data/derivatives/all-users/combined-covid-valid-q-valid-embeddings.rds")
# thousand.1 <- combined.files
################################################################################
# try to read them back? 
f <- list.files("data/derivatives/all-users/embeddings", full.names = T)
all.emb <- foreach (i = c(1:17), .combine = rbind) %dopar% {
  t <- pdsload(f[i])
  gc()
  return(t %>% select(-c(body, weekday_created)))
}


################################################################################



################################################################################

# compare sd of commenting history of users

# # decide if just doing covid or not
# just_covid = T
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
  
  # df <- read_rds(users$file[i]) %>% mutate(date_created = as_date(timestamp_created))
  # if (!file.exists(users$ta_file[i]) | !file.exists(users$te_file[i])) {
  if (!file.exists(users$ta_file[i])) {
    return(NULL)
  }
  df.ta <- read_rds(users$ta_file[i]) %>%
    mutate(date_created = as_date(timestamp_created),
           year_created = year(date_created),
           month_created = month(date_created),
           day_created = day(date_created))%>%
    mutate(Q = ifelse(month_created %in% c(1:3), 1, 
                      ifelse(month_created %in% c(4:6), 2,
                             ifelse(month_created %in% c(7:9), 3, 4))))
  df.ta <- inner_join(df.ta, df %>% select(timestamp_created))
  # df.te <- read_rds(users$te_file[i])
  # all <- inner_join(df.ta, df.te)
  all <- df.ta
  
  # plot var of these embeddings vectors
  # hist(apply(all%>%select(starts_with("V")), MARGIN = 2, FUN = function(x) var(x)) %>% as.numeric(),breaks = 100)
  
  
  # file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_q_valid/all-combined/")
  # system(paste0("mkdir -p ", file.prefix))
  # write_rds(all, 
  #           file = paste0(file.prefix, users$cat[i],"_user-", user, "_all.rds"), 
  #           compress = "gz")
  
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
                  # sd_comment_freq = sd(table(df$date_created)),
                  sd_comment_freq = sd(tmp.weeks$count),
                  # mean_comment_freq = mean(table(df$date_created)),
                  mean_comment_freq = mean(tmp.weeks$count),
                  # ac = t(as.numeric(acf(tmp$Freq, lag.max = 108)$acf))
                  ac_comment_freq = t(as.numeric(acf(tmp.weeks$count, lag.max = 108)$acf)),
                  fft_comment_freq = t(fft(tmp.weeks$count)))
  tmp2 <- left_join(all.dates  %>%
                      mutate(week = c(rep(c(1:108), each = 7), rep(109, 5))),
                    all %>% 
                      mutate_at(.vars = vars(nrc.positive, nrc.negative, nrc.anger, nrc.anticipation, nrc.disgust,
                                             nrc.fear, nrc.joy, nrc.sadness, nrc.surprise, nrc.trust),
                                .funs = function(x) x/all$sentimentr.word_count)) %>%
    group_by(week) %>%
    dplyr::summarise_at(.vars = vars(ends_with("_sentiment"),
                                     "readability", 
                                     starts_with("nrc")), 
                        .funs = function(x) mean(ifelse(is.na(x), 0, x), na.rm = T)) %>%
    ungroup()
  # ###
  # # combine embeddings with ful timeline
  # # replace missing ones with 0
  # # group by week, and get embeddings average
  # tmp3 <- left_join(all.dates %>%
  #                     mutate(week = c(rep(c(1:108), each = 7), rep(109, 5))),
  #                   all %>% select(date_created, paste0("V", c(1:10)))) %>%
  #   group_by(week) %>%
  #   dplyr::summarise_at(.vars = vars(paste0("V", c(1:10))),
  #                       .funs = function(x) mean(ifelse(is.na(x), 0, x), na.rm = T))
  # # combine tmp2 for nrc and tmp3 of ambeddings
  # tmp4 <- inner_join(tmp2, tmp3)
  tmp4 <- tmp2
  ###
  acv <- foreach (j = 2:ncol(tmp4), .combine = cbind) %dopar% {
    var <- colnames(tmp4)[j]
    if (min(tmp4[,j]) == max(tmp4[,j])) {
      acv.t <- data.frame(sd = 0,
                          mean = 0,
                          matrix(ncol = 109, nrow = 1, 0),
                          matrix(ncol = 109, nrow = 1, 0))
    } else {
        acv.t <- data.frame(sd = sd(unlist(tmp4[,j])),
                            mean = mean(unlist(tmp4[,j])),
                            t(as.numeric(acf(unlist(tmp4[,j]), lag.max = 108)$acf)),
                            t(fft(unlist(tmp4[,j]))))
    }
    colnames(acv.t) <- c(paste0("sd_", var),
                         paste0("mean_", var),
                         paste0("ac_", var, ".", c(1:109)),
                         paste0("fft_", var, ".", c(1:109)))
    return(acv.t)
    }
  t <- cbind(t, acv)
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
p5 <- users.stats %>%
  pivot_longer(starts_with("fft"), names_to = "fft_var", values_to = "fft") %>%
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
ggsave(p5,filename = "figs/boxplot_fft-of-all_bp-nmh_covid.png",
# ggsave(p5,filename = "figs/boxplot_fft-of-all_bp-nmh_covid_q-valid.png", 
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
