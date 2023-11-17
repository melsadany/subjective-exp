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
  mutate(author = sub("_comments.rds", "", author)) %>%
  mutate(size = file.size(file))
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



colfunc <- colorRampPalette(redblu.col)
# colfunc(10)
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
  df2 <- df %>% 
    select(author, date_created, subreddit, body, word_count, ave_sentiment, positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust) %>%
    mutate_at(.vars = vars(positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
              .funs = function(x) x/df$word_count)
  
  
  # identify most used subreddits
  top.subreddits <- df %>%
    group_by(subreddit) %>%
    dplyr::summarise(count = n()) %>% 
    # filter(count > 20) %>%
    arrange(desc(count))
  # histogram showing total number of comments per day, and filling color based on subreddit
  p1 <- df %>%
    mutate(subreddit_f = ifelse(subreddit %in% top.subreddits$subreddit[1:9], subreddit, "other"),
           subreddit_f = factor(subreddit_f, levels = c(top.subreddits$subreddit[1:9], "other"))) %>%
    ggplot(aes(x=date_created, fill = subreddit_f)) +
    geom_histogram(bins = 100) +
    scale_fill_manual(values = ten.colors, name = "") +
    labs(y="number of comments", 
         x = "",
         caption = paste0("legend colors for subreddits are ordered based on total comments count per subreddit",
                          "\n\t1\t3\t5\t7\t9",
                          "\n\t2\t4\t6\t8\t10")) + 
    theme(legend.text = element_text(size = 7),
          legend.key.size = unit(0.5, 'cm'))
  # scatterplot for avergae sentiment per day
  p2 <- df %>%
    ggplot(aes(x = date_created, y = ave_sentiment)) +
    # geom_line() +
    geom_point(size = 1, alpha = 0.4) +
    geom_smooth(method = "loess") +
    labs(x = "",
         y= "average sentiment by sentimentr")
  # line plot for word_emotion association scores per day
  p3 <- df %>%
    pivot_longer(cols = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive),
                 names_to = "word_emotion", values_to = "value") %>%
    ggplot(aes(x = date_created, y = value, color = word_emotion)) +
    # geom_point(size = 0.3, alpha = 0.3) +
    # geom_smooth() +
    geom_density(stat = "identity", show.legend = F) +
    scale_color_manual(values = ten.colors) +
    facet_wrap(~word_emotion, scales = "free_y", ncol = 1) +
    labs(y="score", x = "")
  # line plot for proportion of word_emotion association per day
  p3_2 <- df2 %>%
    pivot_longer(cols = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive),
                 names_to = "word_emotion", values_to = "value") %>%
    ggplot(aes(x = date_created, y = value, color = word_emotion)) +
    # geom_point(size = 0.3, alpha = 0.3) +
    # geom_smooth() +
    geom_density(stat = "identity", show.legend = F) +
    scale_color_manual(values = ten.colors) +
    facet_wrap(~word_emotion, scales = "free_y", ncol = 1) +
    labs(y="proportion", x = "")
  # heatmap for highest word_emotion association per day
  p4 <- df %>%
    pivot_longer(cols = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive),
                 names_to = "word_emotion", values_to = "value") %>%
    select(date_created, subreddit, word_emotion, value) %>%
    # group_by(date_created, word_emotion) %>%
    # dplyr::summarise(avg = mean(value)) %>%
    # group_by(word_emotion) %>%
    # mutate(sc_value = scale(value))  %>%
    group_by(date_created) %>%
    slice_max(order_by = value, with_ties = F, n = 1) %>%
    # ggplot(aes(x = date_created, y = word_emotion, fill = value)) +
    # ggplot(aes(x = date_created, y = word_emotion, fill = sc_value)) +
    # ggplot(aes(x = date_created, y = word_emotion, fill = avg)) +
    ggplot(aes(x = date_created, y = word_emotion, fill = redblack.col[1])) +
    geom_tile()+
    # scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1], mid = "white", name = "average score") +
    my.guides +
    labs(x = "",
         y = "",
         caption = "NRC Word-Emotion Association Lexicon using syuzhet\nchose to plot the maximum word-emotion association found by day")
  
  
  # scatter plot comments score by day
  # df %>%
  #   ggplot(aes(x=date_created, y =score)) +
  #   # geom_line() +
  #   geom_point(size = 0.4, alpha=0.3)+
  #   geom_smooth(method = "loess") +
  #   labs(x="")
  
  # heatmap for proportion of parts of speech proportion by day
  pos <- tidytext::parts_of_speech %>%
    distinct(word, .keep_all = T)
  t1 <- df2 %>% 
    select(date_created, subreddit, body)  %>%
    mutate(word = strsplit(body, split = " ")) %>%
    unnest(word) %>%
    inner_join(pos, relationship = "many-to-many") %>%
    group_by(date_created, subreddit, body, pos) %>%
    dplyr::summarise(count = n()) %>%
    left_join(df2%>%select(date_created, subreddit, body, word_count), relationship = "many-to-many") %>%
    mutate(pos_prop = count/word_count)
  p5 <- t1 %>%
    ggplot(aes(date_created, fill= pos_prop, y = pos)) +
    geom_tile() +
    scale_fill_gradient2(low = "grey", high = "black") +
    labs(x="", y="parts of speech") + my.guides
    # scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1])
  # scatterplot for number of words said per day
  p6 <- df2 %>%
    ggplot(aes(x=date_created, y=word_count)) +
    geom_point(size = 0.4, alpha = 0.3) +
    geom_smooth(method = "loess") +
    labs(x="", y = "word count per comment")
  
  p <- patchwork::wrap_plots(patchwork::wrap_plots(p1,p2,p4,p5,p6, ncol = 1), 
                             patchwork::plot_spacer(), 
                             p3, p3_2,
                             ncol = 4, widths = c(2.5,0.3,1.3,1.3))
  if (save_fig == T) {
    # dev.off()
    png(filename = paste0(project.dir, 
                          "/figs/users-history/user-", user, "_history.png"), 
        width = 11.5, height = 13, units = "in", res = 360);print(p);dev.off()
  }
  print(p)
}

plot_user_history(i= 5401)



df %>%
  ggplot(aes(x=date_created)) +
  geom_density()

################################################################################
