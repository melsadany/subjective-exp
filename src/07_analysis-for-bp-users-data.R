################################################################################
#                         analysis for bp users comments                       #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
# args <- commandArgs(trailingOnly = T)
# args <- 2000
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/filtered"
files2 <- data.frame(author = list.files(path = reddit.dir, pattern = ".rds")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("_comments.rds", "", author))
registerDoMC(cores = 20)
all.users <- foreach(i = 1:nrow(files2), .combine = full_join) %dopar% {
  user <- files2$author[i]
  f <- files2$file[i]
  # read in the rds file
  df <- read_rds(f)
  print(paste0("Done reding rds file for: ", f))
  df <- df %>%
    mutate(date_created = as_date(timestamp_created),
           year_created = year(date_created),
           month_created = month(date_created), 
           day_created = day(date_created))
  print(paste0("Done reading user index: ", i, " w ", nrow(df), " comments"))
  return(df)
}
write_rds(all.users, paste0(project.dir, "/all-users-combined.rds"))

# make a function to plot the histogram for users by using their rownumber/index as an input
plot_user_history <- function(i, 
                              save_fig = F) {
  user <- files2$author[i]
  f <- files2$file[i]
  # read in the rds file
  df <- read_rds(f)
  df <- cbind(df %>%
                mutate(date_created = as_date(timestamp_created),
                       year_created = year(date_created),
                       month_created = month(date_created), 
                       day_created = day(date_created)),
              sentimentr::sentiment_by(sentimentr::get_sentences(df$body)),
              syuzhet::get_nrc_sentiment(df$body))
  # identify most used subreddits
  top.subreddits <- df %>%
    group_by(subreddit) %>%
    dplyr::summarise(count = n()) %>% 
    # filter(count > 20) %>%
    arrange(desc(count))
  p1 <- df %>%
    mutate(subreddit_f = ifelse(subreddit %in% top.subreddits$subreddit[1:9], subreddit, "other"),
           subreddit_f = factor(subreddit_f, levels = c(top.subreddits$subreddit[1:9], "other"))) %>%
    ggplot(aes(x=date_created, fill = subreddit_f)) +
    geom_histogram(bins = 100) +
    scale_fill_manual(values = ten.colors, name = "") +
    labs(y="number of comments", 
         x = "date of comment",
         caption = paste0("legend colors for subreddits are ordered based on total comments count per subreddit",
                          "\n\t1\t3\t5\t7\t9",
                          "\n\t2\t4\t6\t8\t10")) + 
    theme(legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, 'cm'))
  p2 <- df %>%
    ggplot(aes(x = date_created, y = ave_sentiment)) +
    # geom_line() +
    geom_point(size = 1, alpha = 0.4) +
    geom_smooth(method = "loess") +
    labs(x = "date of comment",
         y= "average sentiment by sentimentr")
  p3 <- df %>%
    pivot_longer(cols = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive),
                 names_to = "word_emotion", values_to = "value") %>%
    ggplot(aes(x = date_created, y = value, color = word_emotion)) +
    # geom_point(size = 0.3, alpha = 0.3) +
    # geom_smooth() +
    geom_density(stat = "identity", show.legend = F) +
    scale_color_manual(values = ten.colors) +
    facet_wrap(~word_emotion, scales = "free_y", ncol = 1) +
    labs(y="score", x = "date of comment")
  p4 <- df %>%
    pivot_longer(cols = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive),
                 names_to = "word_emotion", values_to = "value") %>%
    select(date_created, subreddit, word_emotion, value) %>%
    group_by(date_created, word_emotion) %>%
    dplyr::summarise(avg = mean(value)) %>%
    # group_by(word_emotion) %>%
    # mutate(sc_value = scale(value))  %>%
    # ggplot(aes(x = date_created, y = word_emotion, fill = value)) +
    # ggplot(aes(x = date_created, y = word_emotion, fill = sc_value)) +
    ggplot(aes(x = date_created, y = word_emotion, fill = avg)) +
    geom_tile()+
    scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1], mid = "white", name = "average score") +
    my.guides +
    labs(x = "date of comment",
         y = "",
         caption = "NRC Word-Emotion Association Lexicon using syuzhet")
  # df %>%
  #   ggplot(aes(x=date_created, y =score)) +
  #   # geom_line() +
  #   geom_point(size = 0.4, alpha=0.3)+
  #   geom_smooth(method = "loess")
  p <- patchwork::wrap_plots(patchwork::wrap_plots(p1,p2,p4, ncol = 1), 
                             patchwork::plot_spacer(), 
                             p3, ncol = 3, widths = c(2.5,0.3,1.3))
  if (save_fig == T) {
    # dev.off()
    png(filename = paste0(project.dir, 
                          "/figs/user-", user, "_history.png"), 
        width = 9.5, height = 10.5, units = "in", res = 360);print(p);dev.off()
  }
  print(p)
}

plot_user_history(i= 5401)



df %>%
  ggplot(aes(x=date_created)) +
  geom_density()

################################################################################
