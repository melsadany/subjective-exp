################################################################################
#                    identification/classification of users                    #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# identify mental-health users as the ones commenting in any of the mental health subreddits
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/subreddits/authors-summary"

all.users <- read_rds(paste0(reddit.dir, "/all-users-all-subreddits-summary-long.rds"))
mh.users <- all.users %>%
  distinct(author)
writeLines(mh.users$author, "data/derivatives/66-subreddits-mental-health-users")
####
