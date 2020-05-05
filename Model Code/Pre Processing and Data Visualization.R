library(readxl)
library(readr)
library(ggchicklet)
scores <- readxl::read_excel('data/congress_sc0res.xlsx')
people <- read_csv('people.csv')
library(stringr)
for (i in 1:nrow(scores)){
  print(i)
  my_string <- scores$Name[i]
  filtered <- people %>% 
    filter(str_detect(name,my_string))
  
  if (nrow(filtered) == 1){
    the_handle <- filtered$screen_name
    scores$Handle[i] <- the_handle
  }
  
}


scores <- read_csv('people_processed.csv')
scores2 <- scores %>% 
  select(Score, Body, Handle, Party) %>% 
  filter(!is.na(Handle))


congress_tweets2 <- congress_tweets %>% 
  inner_join(scores2, by = c("screen_name" = "Handle"))



library(ggplot2)
library(ggthemes)
library(ggtext)
congress_tweets2 %>% 
  ggplot(aes(Score)) + geom_histogram(bins = 50, fill = "navy") + 
  ggthemes::theme_fivethirtyeight() + ggtitle("Distribution of Tweet Scores")
 


library(tidytext)
library(dplyr)
glimpse(congress_tweets2)
hist(scores$Score)
congress_tweets2 %>% 
  group_by(Party) %>% 
  summarize(Tweets = n(),
            AverageScore = mean(Score)) %>% 
  ggplot(aes(Party,Tweets, label = scales::comma(Tweets))) + 
  geom_chicklet(width = 0.75, aes(fill = Party)) + 
  scale_y_continuous(labels = scales::comma, limits = c(0,1000000)) +
  geom_text(hjust=-0.1) + 
  labs(title = "**Tweets by Party**  
    <span style='font-size:11pt'><span style='color:navy'>Democrats</span>, <span style='color:red'>Republicans</span>, and <span style='color:purple'>Independents</span></span>") +
  ggthemes::theme_fivethirtyeight() + 
  scale_fill_manual(values = c("navy", "purple", "red")) + coord_flip() +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y=element_blank(),
        axis.text.x=element_blank()) + 
  guides(fill =F)




library(dplyr)
library(ggplot2)
library(lubridate)
graph_two_data <- congress_tweets %>% 
  mutate(month = floor_date(as.POSIXct(created_at), "month")) %>% 
  group_by(month) %>% 
  count()

graph_two_data %>% 
  ggplot(aes(month, n)) + 
  geom_line(color = "navy", size = 1.5) + 
  geom_point(fill = "#ffffff", pch=21, color = "navy", size = 2.5) + 
  ggthemes::theme_fivethirtyeight() +
  ggtitle("**Distribution of Tweets by Month**<br/>
          <span style='font-size:11pt'>2010 - 2020</span>") +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        plot.title = element_markdown(lineheight = 1.1))




typeof(congress_tweets2$created_at[1])


scores %>% 
  group_by(Party) %>% 
  summarize(AverageScore = mean(Score)) %>% 
  ggplot(aes(Party, AverageScore, label = round(AverageScore,2))) +
  geom_chicklet(width = 0.75, aes(fill = Party)) + 
  geom_text(hjust=-0.1) + 
  labs(title = "**Average Ideology Score by Party**  
    <span style='font-size:11pt'><span style='color:navy'>Democrats</span>, <span style='color:red'>Republicans</span>, and <span style='color:purple'>Independents</span></span>") +
  ggthemes::theme_fivethirtyeight() + 
  scale_y_continuous(limits = c(0,.75)) +
  scale_fill_manual(values = c("navy", "purple", "red")) + coord_flip() +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y=element_blank(),
        axis.text.x=element_blank()) + 
  guides(fill =F)


congress_tweets2 <- congress_tweets2 %>% 
  mutate(nchar = nchar(text))


hist(congress_tweets2$nchar, 50)
max(congress_tweets$nchar)

test_length <- congress_tweets2 %>% 
  filter(nchar <= 280) %>% 
  arrange(desc(nchar))

hist(test_length$nchar)


library(readr)
test_length <- read_csv('Data/processed_tweets.csv')
test_length %>% 
  ggplot(aes(nchar)) + 
  geom_histogram(binwidth = 10, fill = 'darkgreen') + 
  ggthemes::theme_fivethirtyeight() +
  ggtitle('Distribution of Tweet Length') +
  annotate("text", x = mean(test_length$nchar) - 100, y = 155000, 
           label = paste0("The average number of characters\n per tweet was ",
                          round(mean(test_length$nchar),2))) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff")) 


my_words <- test_length %>%
  select(text, Score, status_id, Party, group2) %>% 
  unnest_tokens("word", "text", drop = F, token = 'tweets')
 
  
library(ggwordcloud)
glimpse(my_words)
top_30 <- my_words %>% 
  group_by(word) %>% 
  count() %>% 
  anti_join(stop_words) %>% 
  arrange(-n) %>% 
  head(30) %>% 
  filter(word != 'amp')

top_30 %>% 
  ggplot(aes(label = word, size = n)) +
  geom_text_wordcloud_area(color = "darkgreen") + ggthemes::theme_fivethirtyeight() +
  scale_size_area(max_size = 12) + ggtitle('Top 30 Words Overall') +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())

top_30_party <- my_words %>% 
  group_by(word, group2) %>% 
  count() %>% 
  anti_join(stop_words) %>% 
  arrange(-n) %>% 
  filter(word != 'amp')


top_30_party %>%
  group_by(Party) %>%
  top_n(n = 30, wt = n) %>% 
  filter(Party != 'I') %>% 
  ggplot(aes(label = word, size = n, color = Party)) +
  geom_text_wordcloud_area() + ggthemes::theme_fivethirtyeight() +
  facet_wrap(~Party) +
  ggtitle('Top 30 Words by Party') +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        plot.title = element_markdown(lineheight = 1.1),
        axis.text.y=element_blank(),
        axis.text.x=element_blank()) +
  scale_color_manual(values = c('navy', 'red'))


my_test <-  top_30_party %>% 
  group_by(Party) %>%
  top_n(n = 30, wt = n) %>% 
  filter(Party != 'I')
glimpse(top_30_party)

library(forcats)

tfidf_plot <- top_30_party %>%
  ungroup() %>% 
  bind_tf_idf(word, group2, n)



tfidf_plot %>% 
  filter(word != '→') %>% 
  group_by(group2) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = group2)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~group2, ncol = 2, scales = "free") +
  coord_flip() +
  ggtitle('Top TF—IDF Words by Group') +
  ggthemes::theme_fivethirtyeight() +
  scale_fill_manual(values = c("purple","pink","lightblue","red","navy")) +
  theme(panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        legend.background = element_rect("#ffffff"),
        legend.key = element_rect("#ffffff"),
        axis.text.x=element_blank())




