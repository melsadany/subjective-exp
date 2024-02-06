################################################################################
#                            prep data for LIWC analysis                       #
################################################################################
#                                 runs on IDAS                                 #
################################################################################
rm(list = ls())
gc()
source("~/LSS/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "~/LSS/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
# you want to save a csv file for all users for do the LIWC analysis
################################################################################
# read rds files
all.files <- data.frame(file = list.files("data/derivatives/all-users/covid_valid_q_valid", 
                                          pattern = "\\.rds", full.names = T)) %>%
  mutate(cat = sub("_.*", "", basename(file)),
         user = sub(".*_user-", "", sub("\\.rds", "", basename(file)))) 
# read files, and try to combine them per category
# bp
bp.files <- all.files %>% filter(cat=="bp")
registerDoMC(cores = 16)
bp.data <- foreach(i = 1:nrow(bp.files), .combine = rbind) %dopar% {
  user = bp.files$user[i]
  df <- read_rds(bp.files$file[i]) %>%
    select(author, timestamp_created, body, subreddit)
  return(df)
}
write_rds(bp.data, "data/derivatives/LIWC/input/bp.rds", compress = "gz")
write_csv(bp.data, "data/derivatives/LIWC/input/bp.csv")
# nmh
nmh.files <- all.files %>% filter(cat=="nmh")
registerDoMC(cores = 16)
nmh.data <- foreach(i = 1:nrow(nmh.files), .combine = rbind) %dopar% {
  user = nmh.files$user[i]
  df <- read_rds(nmh.files$file[i]) %>%
    select(author, timestamp_created, body, subreddit)
  return(df)
}
write_rds(nmh.data, "data/derivatives/LIWC/input/nmh.rds", compress = "gz")
write_csv(nmh.data, "data/derivatives/LIWC/input/nmh.csv")
################################################################################
# here's the LIWC command
# run it local on your workstation
cores <- 4
bp.cmd <- paste("LIWC-22-cli",
                "-m wc",
                "--input", "data/derivatives/LIWC/input/bp.csv",
                "--output", "data/derivatives/LIWC/output/bp-wc.csv",
                "-ci 3",
                "-id 1,2,4",
                "-t", cores,
                sep = " ")
bp.cmd
nmh.cmd <- paste("LIWC-22-cli",
                 "-m wc",
                 "--input", "data/derivatives/LIWC/input/nmh.csv",
                 "--output", "data/derivatives/LIWC/output/nmh-wc.csv",
                 "-ci 3",
                 "-id 1,2,4",
                 "-t", cores,
                 sep = " ")
nmh.cmd
################################################################################
bp.liwc <- read_csv("data/derivatives/LIWC/output/bp-wc.csv") %>%
  mutate(user = sub(";.*", "", `Row ID`),
         timestamp_created = sub(";.*", "", sub(paste0(user, ";"), "", `Row ID`)),
         subreddit = sub(".*;", "", sub(".*;", "", `Row ID`)))
gc()
nmh.liwc <- read_csv("data/derivatives/LIWC/output/nmh-wc.csv") %>%
  mutate(user = sub(";.*", "", `Row ID`),
         timestamp_created = sub(";.*", "", sub(paste0(user, ";"), "", `Row ID`)),
         subreddit = sub(".*;", "", sub(".*;", "", `Row ID`)))
gc()
all.liwc <- rbind(bp.liwc %>% mutate(cat = "bp"),
                  nmh.liwc %>% mutate(cat = "nmh"))
write_rds(all.liwc, "data/derivatives/LIWC/output/all-wc.rds", compress = "gz")
# all.liwc <- read_rds("data/derivatives/LIWC/output/all-wc.rds")
rm(bp.liwc);rm(nmh.liwc);gc()
################################################################################
# distribution and stats
registerDoMC(cores = 10)
dist.plots <- foreach(i=3:120) %dopar% {
  var <- colnames(all.liwc)[i]
  df <- cbind(cat = all.liwc$cat, x = all.liwc[,i])
  colnames(df)[2] <- "x"
  p1 <- df %>% 
    ggplot(aes(x=x, fill = cat)) +
    geom_density(alpha=0.4)+labs(title = var)
  # p2 <- df %>%
  #   ggplot(aes(x=cat, y=x, fill = cat)) +
  #   geom_violin()+geom_boxplot(width=0.2,outlier.size = 0,fill="white")+labs(title = var)+
  #   ggpubr::stat_compare_means()
  # return(p1+p2)
  return(p1)
}
foreach(i = 1:length(dist.plots)) %dopar% {
  ggsave(dist.plots[[i]], filename = paste0("figs/LIWC/distribution/", colnames(all.liwc)[i+2],".png"),
         bg = "white", width = 8, height = 6, units = "in", dpi=360)
}
################################################################################
# filter data to keep comments with 10 words or more
all.liwc.filtered <- all.liwc %>% filter(WC >=10)
# check how many users dropped
nrow(all.liwc %>% filter(cat=="bp") %>% distinct(user))
nrow(all.liwc %>% filter(cat=="nmh") %>% distinct(user))
nrow(all.liwc.filtered %>% filter(cat=="bp") %>% distinct(user)) # no users dropped
nrow(all.liwc.filtered %>% filter(cat=="nmh") %>% distinct(user)) # no users dropped
rm(all.liwc);gc()
################################################################################
# try to make the lmm
library(lmerTest)
# liwc.vars <- data.frame(var = colnames(all.liwc.filtered)[3:120])
registerDoMC(cores = 10)
lm.results <- foreach(i=4:120, .combine = rbind) %dopar% {
  var <- colnames(all.liwc.filtered)[i]
  lm <- lmerTest::lmer(y ~ cat + WC + (1|user) + (1|subreddit),
                       data = cbind(all.liwc.filtered %>% 
                                      select(user, subreddit, cat, WC),
                                    y=all.liwc.filtered[,i]) %>%
                         rename(y=5))
  gc()
  df <- coef(summary(lm)) %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    mutate(fixed = sub("cat", "", fixed),
           confint_min = Estimate - `Std. Error`,
           confint_max = Estimate + `Std. Error`,
           pval = `Pr(>|t|)`,
           var = var)
  write_rds(df, paste0("data/derivatives/LIWC/lmer/", var, ".rds"))
  gc()
  return(df)
}
################################################################################
# combine the saved lmer results
lm.results <- foreach(i=4:120, .combine = rbind) %dopar% {
  var <- colnames(all.liwc.filtered)[i]
  if (file.exists(paste0("data/derivatives/LIWC/lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/LIWC/lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
write_rds(lm.results, "data/derivatives/LIWC/all-lmer-results.rds", compress = "gz")
################################################################################
# print and plot results
liwc.meta <- read_csv("../../data/LIWC/liwc-categories.csv") %>%
  mutate(feature = ifelse(feature == "I", "i", feature),
         feature = ifelse(feature == "Qmark", "QMark", feature))
p1 <- lm.results %>%
  filter(fixed == "nmh") %>%
  # filter(abs(Estimate)<0.5) %>%
  mutate(sig = ifelse(FDR<0.01, "FDR < 0.01", "FDR \u2265 0.01")) %>%
  left_join(liwc.meta %>% rename(var = feature)) %>%
  mutate(category = ifelse(is.na(category), "summary", category)) %>%
  filter(!category %in% c("summary")) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig, color = fixed),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("FDR < 0.01" = 1, "FDR \u2265 0.01" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(category), scales = "free", space = "free", axes = T, remove_labels = T) +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, show.legend = F, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "", y="")
p1
p2 <- lm.results %>%
  filter(fixed == "nmh") %>%
  filter(abs(Estimate)>=0.5) %>%
  mutate(sig = ifelse(FDR<0.01, "FDR < 0.01", "FDR \u2265 0.01")) %>%
  left_join(liwc.meta %>% rename(var = feature)) %>%
  mutate(category = ifelse(is.na(category), "summary", category)) %>%
  filter(category %in% c("summary")) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig, color = fixed),  position = position_dodge(width = 0.6), size =2.5) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("FDR < 0.01" = 1, "FDR \u2265 0.01" = 0.3)) +
  ggh4x::facet_grid2(rows = vars(category), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, show.legend = F, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate", y="",
       caption = paste0("Estimates were derived from the formula:", "\n",
                        "   lmerTest::lmer(y ~ cat + WC + (1|user) + (1|subreddit)", "\n",
                        "y: is the variable from LIWC","\n",
                        "cat: is either nmh or bp"))
patchwork::wrap_plots(p1,p2, ncol = 1, heights = c(10,1))
ggsave("figs/LIWC-lmer-results.png", bg="white",
       width=10, height=16, units = "in", dpi=360)
################################################################################