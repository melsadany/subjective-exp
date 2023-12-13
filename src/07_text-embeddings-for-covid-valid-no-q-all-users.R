################################################################################
#                     text embeddings for all users comments                   #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
source("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/getEmbedding.R")
args <- commandArgs(trailingOnly = T)
# args <- 7249
i <- as.numeric(args[1])
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/all-users/covid_valid_no-q"
system(paste0("mkdir -p ", reddit.dir, "/text-embeddings"))
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = "_user")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("\\.rds", "", sub(".+_user-", "", author)),
         cat = ifelse(grepl("bp_user", file), "bp", "nmh"))
f <- files2$file[i]
user <- files2$author[i]
group <- files2$cat[i]
# read in the rds file
df <- read_rds(f) %>%
  select(author, timestamp_created, body, subreddit)
print(paste0("Done reading the rds file for user #: ", i, ", named: ", user))
################################################################################
all.embeddings <- getEmbedding(df$body) %>%
  as.data.frame() %>%
  rownames_to_column("body")
df2 <- inner_join(df, all.embeddings)
print(paste0("Done doing text embeddings for user: ", user))
if (nrow(df2)>1) {
  write_rds(df2, file = paste0(reddit.dir, "/text-embeddings/", group,"_user-", user, ".rds"), 
            compress = "gz")
}
print(paste0("Done doing data for: ", user))
################################################################################



################################################################################



################################################################################



################################################################################