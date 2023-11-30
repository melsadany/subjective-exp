################################################################################
#                       history plots for users comments                       #
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
reddit.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp/data/derivatives/bp_users_data/filtered/text-analyzed"
files.bp <- data.frame(author = list.files(path = reddit.dir, pattern = "_comments.rds")) %>%
  mutate(file = paste0(reddit.dir,"/", author)) %>%
  mutate(author = sub("_comments.rds.rds", "", author)) %>%
  mutate(size = file.size(file),
         cat = "bp")
files.nmh <- data.frame(author = list.files(path = sub("bp_users", "nmh_users", reddit.dir), pattern = "_comments.rds")) %>%
  mutate(file = paste0(sub("bp_users", "nmh_users", reddit.dir),"/", author)) %>%
  mutate(author = sub("_comments.rds.rds", "", author)) %>%
  mutate(size = file.size(file),
         cat = "nmh")
files.bp.nmh <- rbind(files.bp, files.nmh)
####
# IDAS run
# registerDoMC(cores = 20)
# all.users <- foreach(i = 1:nrow(files2), .combine = rbind) %dopar% {
#   user <- files2$author[i]
#   f <- files2$file[i]
#   # read in the rds file
#   df <- read_rds(f)
#   print(paste0("Done reding rds file for: ", f))
#   df <- df %>%
#     mutate(date_created = as_date(timestamp_created))
#   print(paste0("Done reading user index: ", i, " w ", nrow(df), " comments"))
#   return(df)
# }
# write_rds(all.users, paste0(project.dir, "/all-bp-users-combined.rds"), compress = "gz")
####
# get list of mh subreddits
mh.subreddits <- readLines("/Dedicated/jmichaelson-wdata/msmuhammad/data/Reddit/mental-health-subreedit-list")
####
colfunc <- colorRampPalette(redblu.col)
# colfunc(10)
# make a function to plot the histogram for users by using their rownumber/index as an input
plot_user_history <- function(i, 
                              save_fig = F,
                              fig_path = "",
                              files_meta,
                              all = F,
                              just_covid = F,
                              mh_subreddits = readLines("/Dedicated/jmichaelson-wdata/msmuhammad/data/Reddit/mental-health-subreedit-list")) {
  user <- files_meta$author[i]
  f <- files_meta$file[i]
  # read in the rds file
  df <- read_rds(f) %>%
    mutate(date_created = as_date(timestamp_created),
           year_created = year(date_created),
           month_created = month(date_created),
           day_created = day(date_created))%>%
    mutate(Q = ifelse(month_created %in% c(1:3), 1, 
                      ifelse(month_created %in% c(4:6), 2,
                             ifelse(month_created %in% c(7:9), 3, 4))))
  if (just_covid==T) {
    covid.valid <- df %>%
      group_by(year_created, month_created) %>%
      dplyr::summarise(count = n()) %>%
      filter(year_created <= 2020 & month_created <= 2 | year_created >= 2022 & month_created >= 4) %>%
      mutate(status = ifelse(year_created <= 2020, "before", "after"))
    covid.valid.d <- ifelse(length(unique(covid.valid$status))==2, T, F)
    Q.valid <- df %>% 
      group_by(year_created, Q) %>%
      dplyr::summarise(count = n())
    all.q <- data.frame(year_created = rep(c(2020:2022), each = 4),
                        Q = rep(c(1:4),3))
    c <- right_join(Q.valid, all.q) %>% 
      mutate(count = ifelse(is.na(count), 0, count))
    Q.valid.d <- ifelse(all(c$count>0),T,F)
    if (covid.valid.d) {
      df <- df %>%
        filter(date_created >= as.Date("2020-03-01") & date_created <= as.Date("2022-03-31"))
        # filter((year_created > 2020 & year_created <= 2022) | 
        #          (year_created == 2020 & month_created >= 3) | 
        #          (year_created == 2022 & month_created <= 3))
        # filter(year_created >= 2020 && month_created >= 3) %>%
        # filter(year_created <= 2022 && month_created <= 3)
      print(paste0("This user is valid for covid-period criteria and ",
                   ifelse(Q.valid.d == T, "valid", "not valid"),
                   " for the comment-per-quarter criteria"))
    }else{
      print("This user is not valid for covid-period criteria")
      # return()
    }
  }
  
  # normalize NRC words count
  df2 <- df %>% 
    mutate_at(.vars = vars(starts_with("nrc")),
              .funs = function(x) x/df$sentimentr.word_count)
  
  # identify most used subreddits
  top.subreddits <- df %>%
    group_by(subreddit) %>%
    dplyr::summarise(count = n()) %>% 
    # filter(count > 20) %>%
    arrange(desc(count))
  
  
  #################
  # plots section #
  #################
  # histogram showing total number of comments per day, and filling color based on subreddit
  p1 <- df %>%
    mutate(subreddit_f = ifelse(subreddit %in% top.subreddits$subreddit[1:9], subreddit, "other"),
           subreddit_f = factor(subreddit_f, levels = c(top.subreddits$subreddit[1:9], "other"))) %>%
    ggplot(aes(x=date_created, fill = subreddit_f)) +
    geom_histogram(bins = 108) +
    scale_fill_manual(values = ten.colors, name = "") +
    labs(y="number of comments", 
         x = ""
         # ,caption = paste0("legend colors for subreddits are ordered based on total comments count per subreddit",
         #                  "\n\t1\t3\t5\t7\t9",
         #                  "\n\t2\t4\t6\t8\t10")
         ) + 
    guides(fill = guide_legend(ncol = 4)) +
    theme(legend.key.size = unit(0.35, "cm"), 
          legend.text = element_text(size = 7))
  
  # histogram showing total number of comments per hour of all times, and filling color based on subreddit
  p1_n <- df %>%
    mutate(subreddit_f = ifelse(subreddit %in% top.subreddits$subreddit[1:9], subreddit, "other"),
           subreddit_f = factor(subreddit_f, levels = c(top.subreddits$subreddit[1:9], "other"))) %>%
    mutate(day_time = strptime(strftime(timestamp_created, format = "%H:%M:%S"), format = "%H:%M:%S")) %>%
    mutate(day_hour = lubridate::hour(day_time)) %>%
    ggplot(aes(x=day_hour, fill = subreddit_f)) +
    geom_histogram(bins = 24) +
    scale_fill_manual(values = ten.colors, name = "") +
    labs(y="number of comments", 
         x = "hour of the day"
         # ,caption = paste0("legend colors for subreddits are ordered based on total comments count per subreddit",
         #                  "\n\t1\t3\t5\t7\t9",
         #                  "\n\t2\t4\t6\t8\t10")
    ) + 
    guides(fill = guide_legend(ncol = 4)) +
    theme(legend.key.size = unit(0.35, "cm"), 
          legend.text = element_text(size = 7))
  
  # scatterplot for average sentiment per day (all subreddits)
  p2 <- df %>%
    # filter(subreddit %in% mh_subreddits) %>%
    select(date_created, 
           sentimentr = sentimentr.ave_sentiment, 
           syuzhet = syuzhet_sentiment) %>%
    pivot_longer(cols = c(sentimentr, syuzhet), names_to = "source", values_to = "sentiment_score") %>%
    ggplot(aes(x = date_created, y = sentiment_score)) +
    # geom_line() +
    geom_point(size = 1, alpha = 0.4) +
    geom_smooth(method = "loess") +
    facet_wrap(~source, scales = "free_y", ncol = 1) +
    labs(x = "",
         y= "average sentiment")
  
  # # scatterplot for average sentiment per day (all subreddits)
  # p2_n <- df2 %>%
  #   filter(subreddit %in% mh_subreddits) %>%
  #   select(date_created, 
  #          sentimentr = sentimentr.ave_sentiment, 
  #          syuzhet = syuzhet_sentiment) %>%
  #   pivot_longer(cols = c(sentimentr, syuzhet), names_to = "source", values_to = "sentiment_score") %>%
  #   ggplot(aes(x = date_created, y = sentiment_score)) +
  #   # geom_line() +
  #   geom_point(size = 1, alpha = 0.4) +
  #   geom_smooth(method = "loess") +
  #   facet_wrap(~source, scales = "free_y", ncol = 1) +
  #   labs(x = "",
  #        y= "average sentiment")
  
  # line plot for word_emotion association scores per day
  p3 <- df %>%
    # filter(subreddit %in% mh_subreddits) %>%
    pivot_longer(cols = starts_with("nrc."),
                 names_to = "word_emotion", values_to = "value") %>%
    mutate(word_emotion = sub("nrc\\.", "", word_emotion)) %>%
    ggplot(aes(x = date_created, y = value, color = word_emotion)) +
    # geom_point(size = 0.3, alpha = 0.3) +
    # geom_smooth() +
    geom_density(stat = "identity", show.legend = F) +
    scale_color_manual(values = ten.colors) +
    facet_wrap(~word_emotion, scales = "free_y", ncol = 1) +
    labs(y="", x = "", title = "NRC count")
  
  # line plot for proportion of word_emotion association per day
  p3_2 <- df2 %>%
    # filter(subreddit %in% mh_subreddits) %>%
    pivot_longer(cols = starts_with("nrc."),
                 names_to = "word_emotion", values_to = "value") %>%
    mutate(word_emotion = sub("nrc\\.", "", word_emotion)) %>%
    ggplot(aes(x = date_created, y = value, color = word_emotion)) +
    # geom_point(size = 0.3, alpha = 0.3) +
    # geom_smooth() +
    geom_density(stat = "identity", show.legend = F) +
    scale_color_manual(values = ten.colors) +
    facet_wrap(~word_emotion, scales = "free_y", ncol = 1) +
    labs(y="", x = "", title = "NRC proportion")
  
    
  if (all == T) {
    # add extra plots
    # heatmap for highest word_emotion association per day
    # p4 <- df %>%
    #   # filter(subreddit %in% mh_subreddits) %>%
    #   pivot_longer(cols = starts_with("nrc."),
    #                names_to = "word_emotion", values_to = "value") %>%
    #   mutate(word_emotion = sub("nrc\\.", "", word_emotion)) %>%
    #   select(date_created, subreddit, word_emotion, value) %>%
    #   group_by(date_created) %>%
    #   slice_max(order_by = value, with_ties = F, n = 1) %>%
    #   ggplot(aes(x = date_created, y = word_emotion, fill = redblack.col[1])) +
    #   geom_tile()+
    #   my.guides +
    #   labs(x = "",
    #        y = "",
    #        caption = "NRC Word-Emotion Association Lexicon using syuzhet\nchose to plot the maximum word-emotion association found by day")
    
    # scatter plot comments score by day
    # df %>%
    #   ggplot(aes(x=date_created, y =score)) +
    #   # geom_line() +
    #   geom_point(size = 0.4, alpha=0.3)+
    #   geom_smooth(method = "loess") +
    #   labs(x="")
    
    # readability, and word count scatter
    p5 <- df %>%
      # filter(subreddit %in% mh_subreddits) %>%
      select(date_created, readability, sentimentr.word_count) %>%
      pivot_longer(cols = c(readability, sentimentr.word_count), 
                   names_to = "what", values_to = "value") %>%
      mutate(what = sub("sentimentr\\.", "", what)) %>%
      ggplot(aes(x=date_created, y = value)) +
      # geom_point(size = 0.3, alpha = 0.2, na.rm = T)+
      geom_line() +
      # geom_density(na.rm = T, stat = "identity") +
      # geom_smooth(method = "loess") +
      # geom_bar(stat = "identity") +
      facet_wrap(~what, scales = "free", ncol = 1) +
      labs(y = "", x= "")
    
    # VADER
    p6 <- df %>%
      # filter(subreddit %in% mh_subreddits) %>%
      select(date_created, starts_with("vader")) %>%
      pivot_longer(cols = c(starts_with("vader")), 
                   names_to = "what", values_to = "value") %>%
      mutate(what = sub("vader\\.", "", what)) %>%
      ggplot(aes(x=date_created, y = value)) +
      geom_point(size = 0.3, alpha = 0.2, na.rm = T)+
      # geom_line() +
      # geom_density(na.rm = T, stat = "identity") +
      # geom_smooth(method = "loess") +
      # geom_bar(stat = "identity") +
      facet_wrap(~what, scales = "free", ncol = 1) +
      labs(y = "", x= "", title = "VADER")
    
    # lingmatch scatter
    p7 <- df %>%
      # filter(subreddit %in% mh_subreddits) %>%
      select(date_created, starts_with("lingmatch")) %>%
      pivot_longer(cols = c(starts_with("lingmatch")), 
                   names_to = "what", values_to = "value") %>%
      mutate(what = sub("lingmatch\\.", "", what)) %>%
      ggplot(aes(x=date_created, y = value)) +
      geom_point(size = 0.3, alpha = 0.2, na.rm = T)+
      # geom_density(na.rm = T, stat = "identity") +
      # geom_smooth(method = "loess") +
      # geom_bar(stat = "identity") +
      facet_wrap(~what, scales = "free_y", ncol = 4) +
      labs(x="", y="", title = "lingmatch stats/analysis")
    
    # # heatmap for proportion of parts of speech proportion by day
    # pos <- tidytext::parts_of_speech %>%
    #   distinct(word, .keep_all = T)
    # t1 <- df2 %>% 
    #   select(date_created, subreddit, body)  %>%
    #   mutate(word = strsplit(body, split = " ")) %>%
    #   unnest(word) %>%
    #   inner_join(pos, relationship = "many-to-many") %>%
    #   group_by(date_created, subreddit, body, pos) %>%
    #   dplyr::summarise(count = n()) %>%
    #   left_join(df2%>%select(date_created, subreddit, body, word_count), relationship = "many-to-many") %>%
    #   mutate(pos_prop = count/word_count)
    # 
    # p5 <- t1 %>%
    #   ggplot(aes(date_created, fill= pos_prop, y = pos)) +
    #   geom_tile() +
    #   scale_fill_gradient2(low = "grey", high = "black") +
    #   labs(x="", y="parts of speech") + my.guides
    #   # scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1])
    
    p <- patchwork::wrap_plots(patchwork::wrap_plots(p1,p2,p5,
                                                     ncol = 1, heights = c(0.7,1,1)),
                               patchwork::plot_spacer(), 
                               p6,
                               p3, p3_2,
                               p7,
                               ncol = 6, widths = c(1,0.3,1,1,1,3))
    fig.w <- 19
    fig.h <- 13
  } else {
    p <- patchwork::wrap_plots(patchwork::wrap_plots(p1, p1_n,p2,
                                                     ncol = 1, heights = c(0.7,0.7,1)),
                               patchwork::plot_spacer(), 
                               p3_2,
                               ncol = 3, widths = c(2,0.3,1))
    fig.w <- 9.2
    fig.h <- 9.7
  }
  
  if (save_fig == T) {
    # dev.off()
    if (length(fig_path)>2) {
      fig.p <- fig_path
    }else {
      if (just_covid == T) {
        if (covid.valid.d == T & Q.valid.d == T) {
          fig.prefix <- paste0(project.dir, "/figs/users-history/covid_valid_q_valid/")
          # write_rds(df, 
          #           file = paste0())
        } else if (covid.valid.d == T & Q.valid.d == F) {
          fig.prefix <- paste0(project.dir, "/figs/users-history/covid_valid_q_notvalid/")
        } else {
          fig.prefix <- paste0(project.dir, "/figs/users-history/covid_notvalid_q_notvalid/")
        }
      } else {
        fig.prefix <- paste0(project.dir, "/figs/users-history/not-covid/")
      }
      
      system(paste0("mkdir -p ", fig.prefix))
      if (all==T) {
        fig.p <- paste0(fig.prefix, files_meta$cat[i],"_user-", user, "_history_all.png")
      }else {
        fig.p <- paste0(fig.prefix, files_meta$cat[i],"_user-", user, "_history-part.png")
      }
    }
    png(filename = fig.p, 
        width = fig.w, height = fig.h, units = "in", res = 360);print(p);dev.off()
  }
  print(p)
}

plot_user_history(i= 33, 
                  save_fig = T, all = F, just_covid = T,
                  files_meta = files.bp.nmh)
ids <- c(1000,5401,154,33,9932,5960,1004,1140,9898,1002,2005,357,853,12547,15236,14752,12358,12587,14523,16254)
ids <- c(1:200)
foreach(k = ids) %dopar% {
  plot_user_history(i = k,
                    save_fig = T, all = F, just_covid = T,
                    files_meta = files.bp.nmh)
  plot_user_history(i = k,
                    save_fig = T, all = F, just_covid = F,
                    files_meta = files.bp.nmh)
}


################################################################################
