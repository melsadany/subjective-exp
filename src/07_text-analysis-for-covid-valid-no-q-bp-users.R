################################################################################
#                   sentiment and text  for bp users comments                  #
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
source("/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/defined-lexicons.R")
args <- commandArgs(trailingOnly = T)
# args <- 7249
i <- as.numeric(args[1])
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_no-q"
system(paste0("mkdir -p ", reddit.dir, "/text-analyzed"))
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = "bp_user")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("\\.rds", "", sub("bp_user-", "", author)))
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
# extract defined lexicons
df2$economic_stress_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(economic_stress, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$isolation_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(isolation, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$substance_use_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(substance_use, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$guns_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(guns, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$domestic_stress_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(domestic_stress, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$suicidality_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(suicidality, function(y) grepl(pattern = y, x))))) %>% as.numeric()
df2$covid_tot = do.call(rbind, lapply(df$body, function(x) 
  sum(sapply(covid_19, function(y) grepl(pattern = y, x))))) %>% as.numeric()

print(paste0("Done doing text analysis for user: ", i))
if (nrow(df2)>1) {
  write_rds(df2, file = paste0(reddit.dir, "/text-analyzed/bp_user-", files2$author[i], ".rds"), 
            compress = "gz")
}
print(paste0("Done doing data for: ", f))
################################################################################
