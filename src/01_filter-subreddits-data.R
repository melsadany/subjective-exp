################################################################################
#     filter subreddits data to drop ones of not interest and get some stats   #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(jsonlite)
args <- commandArgs(trailingOnly = T)
# args <- 40
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits"
# reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits2"
# reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/non-mental-health-subreddits"
files2 <- list.files(path = reddit.dir, pattern = ".zst", full.names = T)
# args <- args+1
f <- files2[as.numeric(args[1])]

# read in the zst file, drop deleted authors and body.
# save participants summary of number of comments
# 
df <- stream_in(textConnection(system(paste0("zstd -d --ultra --long=31 -c -T0 ", f), 
                                      intern = T)))
print(paste0("Done reding zst file for: ", f))
authors.summary <- df %>%
  group_by(author) %>%
  dplyr::summarise(count = n())
system(paste0("mkdir -p ", reddit.dir, "/authors-summary"))
system(paste0("mkdir -p ", reddit.dir, "/filtered"))
write_rds(authors.summary, file = paste0(sub("subreddits", "subreddits/authors-summary", sub(".zst", "", f), ignore.case = F), ".rds"))
print(paste0("Done saving authors summary for: ", f))
df2 <- df %>%
  filter(!(grepl("\\[deleted]", body) | grepl("\\[deleted]", author)),
         author %in% authors.summary$author[authors.summary$count>10]) %>% 
  distinct(.keep_all = T) %>%
  mutate(timestamp_created = as_datetime(as.numeric(created_utc)),
         # timestamp_approved = as_datetime(as.numeric(approved_at_utc)),
         # timestamp_auther_created = as_datetime(as.numeric(author_created_utc)),
         # timestamp_updated = as_datetime(as.numeric(updated_on))
         ) %>%
  select(any_of(c("author", "timestamp_created", "body", "subreddit", "score", "ups", "downs", "gilded", 
                 "distinguished", "edited", "removal_reason", "user_reports", "mod_reports", "num_reports",
                 "report_reasons", "stickied", "author_cakeday", "timestamp_approved", "timestamp_auther_created",
                 "timestamp_updated", 
                 "parent_id", "archived", "score_hidden", "controversiality", "total_awards_received", 
                 "author_flair_text", "collapsed", "collapsed_reason", "collapsed_reason_code")))

if (nrow(df2)>1) {
  write_rds(df2, file = paste0(sub("subreddits", "subreddits/filtered", sub(".zst", "", f), ignore.case = F), ".rds"))
}
# pdssave(df2, file = paste0(sub("subreddits", "subreddits/filtered", sub(".zst", "", f), ignore.case = F), ".rds"))
print(paste0("Done filtering data for: ", f))
################################################################################


################################################################################


################################################################################


################################################################################


################################################################################


################################################################################