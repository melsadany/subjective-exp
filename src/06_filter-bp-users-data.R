################################################################################
#                         analysis for bp users comments                       #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(jsonlite)
args <- commandArgs(trailingOnly = T)
# args <- 7249
kk <- as.numeric(args[1])
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/identified-bp-users"
system(paste0("mkdir -p ", reddit.dir, "/filtered"))
system(paste0("mkdir -p ", reddit.dir, "/authors-summary"))
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = ".zst")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("_comments.zst", "", author))
user <- files2$author[kk]
f <- files2$file[as.numeric(kk)]
# read in the zst file, drop deleted body.
df <- stream_in(textConnection(system(paste0("zstd -d --ultra --long=31 -c -T0 ", f), 
                                      intern = T)))
print(paste0("Done reding zst file for: ", f))
gc()
authors.summary <- df %>%
  group_by(author, subreddit) %>%
  dplyr::summarise(count = n())
write_rds(authors.summary, file = paste0(sub("bp-users", "bp-users/authors-summary", sub(".zst", "", f), ignore.case = F), ".rds"))
print(paste0("Done saving authors summary for: ", f))
df2 <- df %>%
  filter(!(grepl("\\[deleted]", body))) %>% 
  distinct(body, .keep_all = T) %>%
  mutate(timestamp_created = as_datetime(as.numeric(created_utc)),
         # timestamp_approved = as_datetime(as.numeric(approved_at_utc)),
         # timestamp_auther_created = as_datetime(as.numeric(author_created_utc)),
         # timestamp_updated = as_datetime(as.numeric(updated_on))
         ) %>%
  # select(any_of("author"))
  select(any_of(c("author", "timestamp_created", "body", "subreddit", "score", "ups", "downs", "gilded", 
                  "distinguished", "edited", "removal_reason", "user_reports", "mod_reports", "num_reports",
                  "report_reasons", "stickied", "is_submitter", 
                  "timestamp_approved", "timestamp_auther_created", "timestamp_updated", 
                  "likes", "replies", "subreddit_type", "no_follow", "send_replies", "author_is_blocked",
                  "parent_id", "archived", "score_hidden", "controversiality", "total_awards_received", 
                  "author_flair_text", "collapsed", "collapsed_reason", "collapsed_reason_code")))

if (nrow(df2)>1) {
  write_rds(df2, file = paste0(sub("bp-users", "bp-users/filtered", sub(".zst", "", f), ignore.case = F), ".rds"))
}
######
# identify if the user is covid-valid
df3 <- df2 %>%
  mutate(date_created = as_date(timestamp_created),
         year_created = year(date_created),
         month_created = month(date_created),
         day_created = day(date_created))%>%
  mutate(Q = ifelse(month_created %in% c(1:3), 1, 
                    ifelse(month_created %in% c(4:6), 2,
                           ifelse(month_created %in% c(7:9), 3, 4))))
# if (sum(duplicated(df3$body))>3) {
#   print("This user is a bot")
#   return(NULL)
# }
covid.valid <- df3 %>%
  group_by(year_created, month_created) %>%
  dplyr::summarise(count = n()) %>%
  filter(year_created <= 2020 & month_created <= 2 | year_created >= 2022 & month_created >= 4) %>%
  mutate(status = ifelse(year_created <= 2020, "before", "after"))
covid.valid.d <- ifelse(length(unique(covid.valid$status))==2, T, F)
Q.valid <- df3 %>% 
  group_by(year_created, Q) %>%
  dplyr::summarise(count = n())
all.q <- data.frame(year_created = rep(c(2020:2022), each = 4),
                    Q = rep(c(1:4),3))
c <- right_join(Q.valid, all.q) %>% 
  mutate(count = ifelse(is.na(count), 0, count))
Q.valid.d <- ifelse(all(c$count>0),T,F)
if (covid.valid.d) {
  df3 <- df3 %>%
    filter(date_created >= as.Date("2020-03-01") & date_created <= as.Date("2022-03-31"))
  print(paste0("This user is valid for covid-period criteria and ",
               ifelse(Q.valid.d == T, "valid", "not valid"),
               " for the comment-per-quarter criteria, with ",
               nrow(df3), " comments"))
  if (covid.valid.d == T & Q.valid.d == T & nrow(df3)>0) {
    file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_q_valid/")
    file.prefix2 <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_no-q/")
    system(paste0("mkdir -p ", file.prefix2))
    write_rds(df3, 
              file = paste0(file.prefix2, "bp_user-", user, ".rds"), 
              compress = "gz")
  } else if (covid.valid.d == T & Q.valid.d == F & nrow(df3)>0) {
    file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_q_notvalid/")
    file.prefix2 <- paste0(project.dir, "/data/derivatives/all-users/covid_valid_no-q/")
    system(paste0("mkdir -p ", file.prefix2))
    write_rds(df3, 
              file = paste0(file.prefix2, "bp_user-", user, ".rds"), 
              compress = "gz")
  } else {
    file.prefix <- paste0(project.dir, "/data/derivatives/all-users/covid_notvalid_q_notvalid/")
  }
  system(paste0("mkdir -p ", file.prefix))
  write_rds(df3, 
            file = paste0(file.prefix, "bp_user-", user, ".rds"), 
            compress = "gz")
}else{
  print("This user is not valid for covid-period criteria")
  # return()
}
######
print(paste0("Done filtering data for: ", f))
################################################################################
