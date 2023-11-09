################################################################################
#                        pre-Reddit looks on project aims                      #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(jsonlite)
args <- commandArgs(trailingOnly = T)
# args <- c(1,1)
# args <- c(2,1)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/data/Reddit/academic-torrents/reddit"
dir <- c("comments", "submissions")
files2 <- list.files(path = paste(reddit.dir, dir[as.numeric(args[1])], sep = "/"))
df <- data.frame(id = 1:length(files2), f = files2) %>%
  mutate(loop = c(rep(1,each = 40),
                  rep(2,each = 28),
                  rep(c(3,4,5,6,7,8,9,10), each = 10),
                  11:(length(files2)-148+10))) %>%
  filter(loop == as.numeric(args[2]))
files <- df$f


registerDoMC(cores = 16)
foreach(i = 1:length(files)) %dopar% {
  f <- paste(reddit.dir, dir[as.numeric(args[1])], files[i], sep = "/")
  ### d <- system(paste0("zstd -d --ultra --long=31 -c ", f, ".zst"), intern = T) # local command
  # d <- system(paste0("zstd -d --ultra --long=31 -c ", f), intern = T) # argon command
  # df <- stream_in(textConnection(d)) 
  if (dir[as.numeric(args[1])] == "comments") {
    # df2 <- df %>%
    df2 <- stream_in(textConnection(system(paste0("zstd -d --ultra --long=31 -c ", f), 
                                           intern = T)))  %>%
      filter(!(grepl("deleted", body) | grepl("deleted", author))) %>% # for comments only
      distinct(.keep_all = T) %>%
      select(-any_of(c(contains("media"), "preview", "created_utc"))) # for submissions only
  }else {
    # df2 <- df %>%
    df2 <- stream_in(textConnection(system(paste0("zstd -d --ultra --long=31 -c ", f), 
                                           intern = T)))  %>%
      filter(!grepl("deleted", author)) %>% 
      distinct(.keep_all = T) %>%
      select(-any_of(c(contains("media"), "preview", "created_utc"))) # for submissions only
  }
  pdssave(df2, file = paste0(sub("reddit", "derivatives", f, ignore.case = F), ".rds"))
  # rm(df)
  rm(df2)
  gc()
}

################################################################################


################################################################################


################################################################################


################################################################################


################################################################################


################################################################################