################################################################################
#                              pre looks at the data                          #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
pacman::p_load(sentimentr, magrittr, syuzhet)
library(ggh4x)
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/subjective-exp"
setwd(project.dir)
################################################################################
data.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/data/ask-a-patient"
data <- read_csv(paste0(data.dir, "/effexor-xr-1-to-112",".csv"), skip_empty_rows = T) %>%
  mutate(date = sub("\nEmail", "", `DATE ADDED`)) %>%
  mutate(duration = sub("s.*", "s", `DURATION/\nDOSAGE`)) %>%
  mutate(days = as.integer(ifelse(grepl("days", duration),readr::parse_number(duration),
                                  ifelse(grepl("weeks", duration),readr::parse_number(duration)*7,
                                         ifelse(grepl("months", duration),readr::parse_number(duration)*30,
                                                ifelse(grepl("years", duration),readr::parse_number(duration)*365,0)))))) %>%
  filter(days>0) %>%
  mutate(comments = tolower(paste(`SIDE EFFECTS FOR EFFEXOR XR`,COMMENTS))) %>%
  select(rating = RATING, reason = 2, comments, sex = SEX, age = AGE, duration, days, date) %>%
  drop_na()
################################################################################
data.se <- cbind(data, 
                 sentimentr::sentiment_by(sentimentr::get_sentences(data$comments)),
                 syuzhet::get_nrc_sentiment(data$comments))
t0<- data.se %>%
  pivot_longer(cols = c(rating, age, days, ave_sentiment, word_count,
                        anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive), 
               names_to = "var", values_to = "val") %>%
  ggplot(aes(x=val))+
  geom_histogram()+
  facet_wrap("var", scales = "free") +labs(x="")
t <- corr.table(data.se%>%select(ave_sentiment), 
           data.se%>%select(age, rating, days, word_count,
                            anger, anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive), 
           method = "spearman") %>%
  filter(V1 != V2) %>%
  mutate(source1 = ifelse(V1 %in% c("rating", "age", "days"), "ask-a-patient",
                         ifelse(V1 %in% c("ave_sentiment", "word_count"), "sentimentr-package",
                                "NRC Word-Emotion Association Lexicon using syuzhet"))) %>%
  mutate(source2 = ifelse(V2 %in% c("rating", "age", "days"), "ask-a-patient",
                          ifelse(V2 %in% c("ave_sentiment", "word_count"), "sentimentr-package",
                                 "NRC Word-Emotion Association Lexicon using syuzhet"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr"))
t1<- t %>%
  filter(source1 == "ask-a-patient")%>%
  ggplot(aes(x=V1, y=V2, fill = r, label=ifelse(FDR<0.05, paste0(rho, ": ", round(r,4), "\n",
                                                                 "FDR: ", round(FDR, 5)), "")))+
  geom_tile()+
  geom_text(size=2.5)+
  redblu.col.gradient+my.guides+null_labs+
  facet_grid(rows = "source2", scales = "free_y", space = "free") +
  labs(title = "ask-a-patient", caption = paste0("n(samples): ", nrow(data.se), "\n",
                                  "* FDR < 0.05", "\n", "Effexor-XR ratings"))+
  theme(strip.text.y.right = element_blank(), plot.title = element_text(size=8))
t2<-t %>%
  filter(source1 == "sentimentr-package")%>%
  ggplot(aes(x=V1, y=V2, fill = r, label=ifelse(FDR<0.05, paste0(rho, ": ", round(r,4), "\n",
                                                                 "FDR: ", round(FDR, 5)), "")))+
  geom_tile()+
  geom_text(size=2.5)+
  redblu.col.gradient+my.guides+null_labs+
  facet_grid(rows = "source2", scales = "free_y", space = "free") +
  labs(title = "sentimentr-package")+theme(axis.text.y = element_blank(),
                                                  # strip.text.y.right = element_blank(), 
                                           plot.title = element_text(size=8))
t3<- t %>%
  filter(source1 == "NRC Word-Emotion Association Lexicon using syuzhet")%>%
  ggplot(aes(x=V1, y=V2, fill = r, label=ifelse(FDR<0.05,paste0(rho, ": ", round(r,4), "\n",
                                                                "FDR: ", round(FDR, 5)), "")))+
  geom_tile()+
  geom_text(size=2.5)+
  redblu.col.gradient+my.guides+null_labs+
  facet_grid(rows = "source2", scales = "free_y", space = "free") +
  labs(title = "NRC Word-Emotion Association Lexicon using syuzhet")+theme(axis.text.y = element_blank(), 
                                                                           plot.title = element_text(size=8))
patchwork::wrap_plots(t1,t2,t0, widths = c(3,2,5))
################################################################################
################################################################################
# chatgpt3 embeddings
library(rgpt3)
gpt3_authenticate("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/openAI/access_key.txt")
gpt3_test_completion()
tmp <- gpt3_embeddings(input_var = data$comments[1:10], 
                      id_var = rownames(data))

library(httr)
api_key <- "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"
# API endpoint
api_url <- "https://api.openai.com/v1/engines/text-davinci-002/completions"
# Prepare the request payload
payload <- list(
  prompt = text_data,
  max_tokens = 0,
  stop = "",
  temperature = 0
)

# Make the API request with API key in headers
response <- POST(
  url = api_url,
  body = payload,
  add_headers(Authorization = paste("Bearer", api_key)),
  encode = "json"
)
# Extract the embeddings from the response
# embeddings <- content(response)$choices$[["0"]]$logits
################################################################################