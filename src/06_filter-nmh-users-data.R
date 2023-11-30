################################################################################
#                        analysis for nmh users comments                       #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(jsonlite)
args <- commandArgs(trailingOnly = T)
# args <- 7249
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data"
system(paste0("mkdir -p ", reddit.dir, "/filtered"))
system(paste0("mkdir -p ", reddit.dir, "/authors-summary"))
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = ".zst")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("_comments.zst", "", author))
f <- files2$file[as.numeric(args[1])]
# read in the zst file, drop deleted body.
df <- stream_in(textConnection(system(paste0("zstd -d --ultra --long=31 -c -T0 ", f), 
                                      intern = T)))
print(paste0("Done reding zst file for: ", f))
gc()
authors.summary <- df %>%
  group_by(author, subreddit) %>%
  dplyr::summarise(count = n())
write_rds(authors.summary, file = paste0(sub("nmh_users_data", "nmh_users_data/authors-summary", sub(".zst", "", f), ignore.case = F), ".rds"))
print(paste0("Done saving authors summary for: ", f))
df2 <- df %>%
  filter(!(grepl("\\[deleted]", body))) %>% 
  distinct(.keep_all = T) %>%
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
  write_rds(df2, file = paste0(sub("nmh_users_data", "nmh_users_data/filtered", sub(".zst", "", f), ignore.case = F), ".rds"))
}
# pdssave(df2, file = paste0(sub("nmh_users_data", "nmh_users_data/filtered", sub(".zst", "", f), ignore.case = F), ".rds"))
print(paste0("Done filtering data for: ", f))
################################################################################
