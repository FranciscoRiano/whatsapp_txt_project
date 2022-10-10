#https://medium.com/analytics-vidhya/how-i-analyzed-whatsapp-chat-in-r-using-rwhatsapp-and-ggplot-912ba9439026

install.packages("rwhatsapp")
library(rwhatsapp)
library(lubridate)
library(quantmod)
library(stopwords)
library(ggplot2)
library(ggimage)
library(wordcloud) 
library(reshape2)
library(dplyr)


chat <- rwa_read("C:/Users/frian/OneDrive/Documentos - copia/portfolio/whatsapp_text/WhatsApp_Chat_with_Familia_Riano_Sanchez.txt") %>%
  filter(!is.na(author)) %>%
  mutate(count_character= nchar(text), 
         words= nchar(gsub('[^ ]+', '',text))+1)

plain_chat<-rwa_read("C:/Users/frian/OneDrive/Documentos - copia/portfolio/datasets/WhatsApp_Chat_with_Familia_Riano_Sanchez.txt") %>% mutate(count_character= nchar(text), words= nchar(gsub('[^ ]+', '',text))+1)

to_remove <- c(stopwords(language = "en"), "media","message","deleted","https","www",
               "omitted","ref","dass","aan","aa","aan","nee","oru","njan","ok","No","no","yes","Ok","Yes","android.s.wt","he")

chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

chat_clean <- chat_clean %>%
  na.omit(chat_clean)



daysed<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")        
no_of_days_of_messages<-chat %>% mutate(day=date(time)) %>% summarise(no=length(unique(day))) %>%pull(no)
most_oldest_date<-chat %>% mutate(day=date(time))%>% arrange(day) %>% slice(1) %>% select(Oldest=day) %>%pull(Oldest)
most_recent_date<-chat %>% mutate(day=date(time))%>% arrange(desc(day)) %>% slice(1) %>% select(Newest=day) %>%pull(Newest)
#total no of days
total_no_of_days<-as.numeric(most_recent_date-most_oldest_date)   
# total no of days with messages
no_of_days_of_messgaes<- as.numeric(total_no_of_days-no_of_days_of_messages) 
# % days without msg
percent_days_without_messages<-round(no_of_days_of_messgaes/total_no_of_days*100,2) 
#most active day
most_active_day<-chat %>% mutate(date = date(time)) %>% count(date) %>% top_n(1) %>% pull(date) 
#most active day of week
most_active_day_of_week<-chat %>% mutate(day = wday(as.Date(time),week_start = 1)) %>% count(day) %>% top_n(1) %>% pull(day)
most_active_day_of_week<-daysed[most_active_day_of_week]   
#total no of messages
total_no_of_messages <- chat %>% count() 
# no of unique users
total_no_of_users<- n_distinct(chat$author) 
# no of messages per day
messages_per_day<-as_tibble(total_no_of_messages/no_of_days_of_messgaes) 
# no of deleted messages  
deleted_messages<- chat %>% filter(text=="This message was deleted" | text=="You deleted this message") %>% count()  
avg_no_of_words<- chat %>% summarise (n = mean(words)) #no of words
avg_no_of_characters<-chat %>% filter(text != "<Media omitted>")%>% filter(text != "This message was deleted") %>% filter(text != "You deleted this message") %>% summarise (n = mean(count_character)) #no of characters
no_of_smiley<-chat%>% unnest(emoji) %>% count() #no of smileys
unique_smiley<-chat %>%  unnest(emoji) %>% count(emoji, sort = TRUE) %>% count()
no_of_media <-chat %>% filter(text == "<Media omitted>") %>% count() #no of media
no_of_links<- chat%>% filter(str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be")) %>% count() #no of links
no_people_who_left<- plain_chat %>% filter(is.na(author)) %>% filter(str_detect(text,".left")) %>% count() #people who left
no_times_chat_changed <- plain_chat %>% filter(is.na(author)) %>% filter(str_detect(text,".changed.")) %>% count() #times the group name was changed








var<-chat %>% mutate(date = date(time)) %>% count(date) %>% top_n(1)
title<-paste0("Most Active day was ",var %>% pull(date),"\n with ",var %>% pull(n)," messages")

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity", fill="red") +
  xlab("Messages across time") + ylab("") +
  ggtitle(title)+
  theme(axis.text.x = element_text(color = "grey20", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 9, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        plot.title = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain"))





title<-paste0(chat %>% group_by(author) %>% summarise(words=sum(words)) %>% top_n(1) %>% pull(author)," is the most active person!")
chat %>% 
  group_by(author) %>%
  summarise(words=sum(words)) %>%
  top_n(12) %>%
  ggplot(aes(x = reorder(author, words), y = words)) +
  geom_bar(stat = "identity", fill="#F8766D") +
  xlab("") + ylab("Number of Messages Sent") +
  coord_flip() +
  ggtitle(title)+
  theme(axis.text.x = element_text(color = "grey20", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 9, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        plot.title = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain")) 




help("reorder")





emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))




top_chatters<-chat %>% group_by(author) %>% summarise(words=sum(words)) %>%
  top_n(6) %>% pull(author)
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>% filter(author %in% top_chatters ) %>%
  left_join(emoji_data2, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  ggtitle("Most often used emojis")

write.csv(emoji_data,"C:/Users/frian/OneDrive/Documentos - copia/Francisco eres el mejor, jamás lo olvides!/emoji_data.csv", row.names = FALSE)



 emoji_data2 <- read.csv("C:/Users/frian/OneDrive/Documentos - copia/Francisco eres el mejor, jamás lo olvides!/emoji_data.csv")
 
 
 
 
 
 top_chatters<-chat %>% group_by(author) %>% summarise(words=sum(words)) %>% top_n(6) %>% pull(author)
 
 chat_cleaned %>% top_chatters
   select(word, author) %>%
   filter(!word %in% to_remove) %>%
   mutate(word = gsub(".com", "", word)) %>%
   mutate(word = gsub("^gag", "9gag", word)) %>%
   count(author, word, sort = TRUE) %>% 
   bind_tf_idf(term = word, document = author, n = n) %>%
   filter(n > 5) %>% filter(author %in% top_chatters ) %>%
   group_by(author) %>%
   top_n(n = 4, tf_idf) %>%
   ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
   geom_col(show.legend = FALSE) +
   ylab("") +
   xlab("") +
   coord_flip() +
   facet_wrap(~author, ncol = 3, scales = "free_y") +
   scale_x_reordered()+
   ggtitle("Common Words Used")
   
   
   
   
   # to find the hour when most messages are sent
   title<-paste0("Most Messages happen at hour ",chat() %>% mutate(hour = hour(time)) %>% count(hour) %>% top_n(1) %>% pull(hour))
   chat %>%
     mutate(hour = hour(time)) %>%
     count(hour) %>%
     ggplot(aes(x = hour, y = n)) +
     geom_bar(stat = "identity",fill="steelblue") +
     ylab("") + xlab("Messages for every hour") +
     ggtitle(title)+
     scale_x_continuous(breaks = 0:23)
   
   #to find which day of the week most messages are being sent
   daysed<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")        
   most_active_day_of_week<-chat %>% mutate(day = wday(as.Date(time),week_start = 2)) %>% count(day) %>% top_n(1) %>% pull(day)
   most_active_day_of_week<-daysed[most_active_day_of_week]
   title<-paste0("Most messages are sent on a ",most_active_day_of_week)
   days<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") # for axis labels
   
   chat %>%
     mutate(day = wday(as.Date(time),week_start = 2)) %>%
     count(day)  %>%
     ggplot(aes(x = day, y = n)) +
     geom_bar(stat = "identity", fill="steelblue") +
     ylab("") + xlab("Messages Per Day of week") +
     ggtitle(title) +       
     scale_x_continuous(breaks = 1:7,labels=days)+
     scale_x_continuous(breaks = 1:7,labels=days)
   
   
   

   df<-chat %>% unnest_tokens(input = text, output = word) %>% filter(!word %in% to_remove) %>% count(word, sort = TRUE) 
   set.seed(1234) # for reproducibility 
   wordcloud(words = df$word, freq = df$n, min.freq = 5,   
             max.words=250, random.order=FALSE, rot.per=0,      
             colors=brewer.pal(8, "Dark2"))
   

      
   chat %>%
     group_by(author)%>%
     summarize(AMT = n())%>%
     arrange(AMT)%>%
     top_n(18)%>%
     ggplot(aes(x = author, y = AMT)) +
     geom_col()+
     coord_flip()
 
   
     