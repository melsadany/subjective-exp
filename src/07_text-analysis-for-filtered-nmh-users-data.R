################################################################################
#                   sentiment and text  for nmh users comments                  #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(lingmatch, lib.loc = lib.location)
library(sentimentr, lib.loc = lib.location)
library(syuzhet, lib.loc = lib.location)
library(vader, lib.loc = lib.location)
library(quanteda.textstats, lib.loc = lib.location)
library(textshape, lib.loc = lib.location)
args <- commandArgs(trailingOnly = T)
# args <- 7249
i <- as.numeric(args[1])
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/nmh_users_data/filtered"
system(paste0("mkdir -p ", reddit.dir, "/text-analyzed"))
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = "_comments.rds")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("_comments.rds", "", author))
f <- files2$file[i]
user <- files2$author[i]
# read in the rds file
df <- read_rds(f) %>%
  select(author, timestamp_created, body, subreddit)
print(paste0("Done reading the rds file for user #: ", i, ", named: ", user))
df2 <- cbind(df %>%
               mutate(date_created = as_date(timestamp_created)),
             sentimentr = sentimentr::sentiment_by(sentimentr::get_sentences(df$body), ),
             syuzhet_sentiment = syuzhet::get_sentiment(df$body),
             nrc = syuzhet::get_nrc_sentiment(df$body),
             vader = vader::vader_df(df$body)[3:7],
             readability = quanteda.textstats::textstat_readability(df$body)[,2],
             lingmatch = lingmatch::lma_meta(df$body),
             lingmatch = lingmatch::lma_termcat(df$body))
print(paste0("Done doing text analysis for user: ", i))
if (nrow(df2)>1) {
  write_rds(df2, file = paste0(sub("filtered", "filtered/text-analyzed", sub(".zst", "", f), 
                                   ignore.case = F), ".rds"), compress = "gz")
}
print(paste0("Done doing data for: ", f))
################################################################################
