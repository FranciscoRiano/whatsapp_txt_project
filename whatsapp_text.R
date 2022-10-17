#https://medium.com/analytics-vidhya/how-i-analyzed-whatsapp-chat-in-r-using-rwhatsapp-and-ggplot-912ba9439026


library(rwhatsapp)
library(lubridate)
library(quantmod)
library(stopwords)
library(ggplot2)
library(ggimage)
library(wordcloud) 
library(reshape2)
library(dplyr)
library(tidytext)
library(stringr)
library(tibble)
library(stringi)
library(tidyr)


#first step: import the data set that is the script of a WhatsApp group chat
chat <- rwa_read("C:/Users/frian/OneDrive/Documentos - copia/portfolio/whatsapp_text/WhatsApp_Chat_with_Familia_Riano_Sanchez.txt") %>%
  filter(!is.na(author)) %>%
  mutate(count_character= nchar(text), 
         words= nchar(gsub('[^ ]+', '',text))+1)%>%
  rownames_to_column("id")


#similar to chat but we are not removing the entries where author is null
plain_chat<-rwa_read("C:/Users/frian/OneDrive/Documentos - copia/portfolio/whatsapp_text/WhatsApp_Chat_with_Familia_Riano_Sanchez.txt") %>% mutate(count_character= nchar(text), words= nchar(gsub('[^ ]+', '',text))+1)

to_remove <- c(stopwords(language = "es"), "media","message","deleted","https","www",
               "omitted","ref","dass","aan","aa","aan","nee","oru","njan","ok","No","no","yes","Ok","Yes","android.s.wt","he")

         
    
         
#remove stop_words in Spanish - Befor doing it, it is important to create the customized stop_words just in Spanish
chat_clean <- chat %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(custom_stop_words_sp)


#remove more useless words
chat_clean <- chat_clean %>%
  na.omit(chat_clean)%>%
  filter(word != 'media')%>%
  filter(word != 'omitted')%>%
  filter(word != 'deleted')%>%
  filter(word != 'message')%>%
  filter(word != 'www.facebook.com')%>%
  filter(word != 'youtu.be')%>%
  filter(word != 'whatsapp')


#More data cleaning

chat_clean_id <-
  chat_clean %>%
  mutate(
    # remove links
    word = str_remove_all(word, "https\\S*"),
    word = str_remove_all(word, "http\\S*"),
    word = str_remove_all(word, "t.co*"),
    word = str_remove_all(word, "www"),
    # remove mentions
    word = str_remove_all(word, "@\\S*"),
    # remove punctuation
    word = str_remove_all(word, "[:punct:]"),
    # remove annoying html stuff
    word = str_remove_all(word, "amp"),
    word = str_remove_all(word, "&S*"),
    word = str_replace_all(word, "&#x27;|&quot;|&#x2F;", "'"),
    word = str_replace_all(word, "<a(.*?)>", " "),
    word = str_replace_all(word, "&gt;|&lt;|&amp;", " "),
    word = str_replace_all(word, "&#[:digit:]+;", " "),
    word = str_remove_all(word, "<[^>]*>"),
    # remove numbers
    word = str_remove_all(word, "[:digit:]"),
    # remove excess whitespace
    word = str_squish(word),
    word = str_trim(word))%>%
  filter(word != "")




#Metadata about the group chat

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






#Graph with the number of messages along the time period
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




#Graph with number of words used in messages grouped by author
title<-paste0(chat %>% group_by(author) %>% summarise(words=sum(words)) %>% top_n(1) %>% pull(author)," is the most active person!")
chat %>% 
  group_by(author) %>%
  summarise(words=sum(words)) %>%
  top_n(12) %>%
  ggplot(aes(x = reorder(author, words), y = words)) +
  geom_bar(stat = "identity", fill="#F8766D") +
  xlab("") + ylab("Number of Words used in Messages") +
  coord_flip() +
  ggtitle(title)+
  theme(axis.text.x = element_text(color = "grey20", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 9, angle = 0, hjust = .5, vjust = 0, face = "plain"),  
        plot.title = element_text(color = "grey20", size = 17, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "grey20", size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain")) 


#Just to confirm the info provided by the graph from above
chat %>%
  group_by(author)%>%
  summarize(AMT = sum(words))%>%
  arrange(AMT)






emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s[[:alnum:]]+", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))




#We need to eliminate one specific row, the process is done in excel
top_chatters<-chat %>% group_by(author) %>% summarise(words=sum(words)) %>%
  top_n(6) %>% pull(author)
chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 3, n) %>% filter(author %in% top_chatters ) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 3, scales = "free_y") +
  ggtitle("Most often used emojis")

write.csv(emoji_data,"C:/Users/frian/OneDrive/Documentos - copia/portfolio/whatsapp_text/emoji_data.csv", row.names = FALSE)



 emoji_data2 <- read.csv("C:/Users/frian/OneDrive/Documentos - copia/portfolio/whatsapp_text/emoji_data.csv")
 
 
 
 #Graph with the most used emojis grouped by author
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
   geom_image(aes(y = n + 50, image = emoji_url)) +
   facet_wrap(~author, ncol = 2, scales = "free_y") +
   ggtitle("Most often used emojis")
 
 
 
 
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
   days<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
   months_c <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dic")# for axis labels
   
   chat %>%
     mutate(day = wday(as.Date(time),week_start = 2)) %>%
     count(day)  %>%
     ggplot(aes(x = day, y = n)) +
     geom_bar(stat = "identity", fill="steelblue") +
     ylab("") + xlab("Messages Per Day of week") +
     ggtitle(title) +       
     scale_x_continuous(breaks = 1:7,labels=days)+
     scale_x_continuous(breaks = 1:7,labels=days)
   
   chat %>%
     mutate(month = month(time)) %>%
     count(month)  %>%
     ggplot(aes(x = month, y = n)) +
     geom_bar(stat = "identity", fill="darkred") +
     ylab("") + xlab("Messages Per Month") +
     ggtitle(title) +       
     scale_x_continuous(breaks = 1:12,labels=months_c)+
     scale_x_continuous(breaks = 1:12,labels=months_c)   
   
#Plot with the number of messages per author
   chat %>%
     group_by(author)%>%
     summarize(AMT = n())%>%
     arrange(AMT)%>%
     top_n(18)%>%
     ggplot(aes(x = reorder(author, AMT), y = AMT)) +
     geom_col(fill = "steelblue")+
     coord_flip()+
     xlab("Author")+
     ylab("Number of messages")+
     ggtitle("Number of Messages Sent by Author")+
     theme(panel.background = element_blank())
   
   
 
#Plot with the average number of words used in one message  
chat%>%
  group_by(author)%>%
  summarize(TOT = sum(words), AMT = n(), AVG = sum(words)/n() )%>%
  arrange(AVG)%>%
  top_n(18)%>%
  ggplot(aes(x = reorder(author, AVG), y = AVG))+
  geom_col(fill = "darkred")+
  coord_flip()+
  xlab("Author")+
  ylab("Average number of words per message")+
  ggtitle("Average Number of Words Used by Author per Message")+
  theme(panel.background = element_blank())
   
   
   
#Wordcloud with the most used words in the group chat
   df<-chat_clean_id %>% count(word, sort = TRUE) 
   set.seed(1234) # for reproducibility 
   wordcloud(words = df$word, freq = df$n, min.freq = 5,   
             max.words=250, random.order=FALSE, rot.per=0,      
             colors=brewer.pal(8, "Dark2"))



   

 
   
#It is necessary to create a customized stop word list.
   
   
   
   custom_stop_words_sp <- 
     tibble(word = c('a',
                     'actualmente',
                     'adelante',
                     'además',
                     'afirmó',
                     'agregó',
                     'ahora',
                     'ahí',
                     'al',
                     'algo',
                     'alguna',
                     'algunas',
                     'alguno',
                     'algunos',
                     'algún',
                     'alrededor',
                     'ambos',
                     'ampleamos',
                     'ante',
                     'anterior',
                     'antes',
                     'apenas',
                     'aproximadamente',
                     'aquel',
                     'aquellas',
                     'aquellos',
                     'aqui',
                     'aquí',
                     'arriba',
                     'aseguró',
                     'así',
                     'atras',
                     'aunque',
                     'ayer',
                     'añadió',
                     'aún',
                     'bajo',
                     'bastante',
                     'bien',
                     'buen',
                     'buena',
                     'buenas',
                     'bueno',
                     'buenos',
                     'cada',
                     'casi',
                     'cerca',
                     'cierta',
                     'ciertas',
                     'cierto',
                     'ciertos',
                     'cinco',
                     'comentó',
                     'como',
                     'con',
                     'conocer',
                     'conseguimos',
                     'conseguir',
                     'considera',
                     'consideró',
                     'consigo',
                     'consigue',
                     'consiguen',
                     'consigues',
                     'contra',
                     'cosas',
                     'creo',
                     'cual',
                     'cuales',
                     'cualquier',
                     'cuando',
                     'cuanto',
                     'cuatro',
                     'cuenta',
                     'cómo',
                     'da',
                     'dado',
                     'dan',
                     'dar',
                     'de',
                     'debe',
                     'deben',
                     'debido',
                     'decir',
                     'dejó',
                     'del',
                     'demás',
                     'dentro',
                     'desde',
                     'después',
                     'dice',
                     'dicen',
                     'dicho',
                     'dieron',
                     'diferente',
                     'diferentes',
                     'dijeron',
                     'dijo',
                     'dio',
                     'donde',
                     'dos',
                     'durante',
                     'e',
                     'ejemplo',
                     'el',
                     'ella',
                     'ellas',
                     'ello',
                     'ellos',
                     'embargo',
                     'empleais',
                     'emplean',
                     'emplear',
                     'empleas',
                     'empleo',
                     'en',
                     'encima',
                     'encuentra',
                     'entonces',
                     'entre',
                     'era',
                     'erais',
                     'eramos',
                     'eran',
                     'eras',
                     'eres',
                     'es',
                     'esa',
                     'esas',
                     'ese',
                     'eso',
                     'esos',
                     'esta',
                     'estaba',
                     'estabais',
                     'estaban',
                     'estabas',
                     'estad',
                     'estada',
                     'estadas',
                     'estado',
                     'estados',
                     'estais',
                     'estamos',
                     'estan',
                     'estando',
                     'estar',
                     'estaremos',
                     'estará',
                     'estarán',
                     'estarás',
                     'estaré',
                     'estaréis',
                     'estaría',
                     'estaríais',
                     'estaríamos',
                     'estarían',
                     'estarías',
                     'estas',
                     'este',
                     'estemos',
                     'esto',
                     'estos',
                     'estoy',
                     'estuve',
                     'estuviera',
                     'estuvierais',
                     'estuvieran',
                     'estuvieras',
                     'estuvieron',
                     'estuviese',
                     'estuvieseis',
                     'estuviesen',
                     'estuvieses',
                     'estuvimos',
                     'estuviste',
                     'estuvisteis',
                     'estuviéramos',
                     'estuviésemos',
                     'estuvo',
                     'está',
                     'estábamos',
                     'estáis',
                     'están',
                     'estás',
                     'esté',
                     'estéis',
                     'estén',
                     'estés',
                     'ex',
                     'existe',
                     'existen',
                     'explicó',
                     'expresó',
                     'fin',
                     'fue',
                     'fuera',
                     'fuerais',
                     'fueran',
                     'fueras',
                     'fueron',
                     'fuese',
                     'fueseis',
                     'fuesen',
                     'fueses',
                     'fui',
                     'fuimos',
                     'fuiste',
                     'fuisteis',
                     'fuéramos',
                     'fuésemos',
                     'gran',
                     'grandes',
                     'gueno',
                     'ha',
                     'haber',
                     'habida',
                     'habidas',
                     'habido',
                     'habidos',
                     'habiendo',
                     'habremos',
                     'habrá',
                     'habrán',
                     'habrás',
                     'habré',
                     'habréis',
                     'habría',
                     'habríais',
                     'habríamos',
                     'habrían',
                     'habrías',
                     'habéis',
                     'había',
                     'habíais',
                     'habíamos',
                     'habían',
                     'habías',
                     'hace',
                     'haceis',
                     'hacemos',
                     'hacen',
                     'hacer',
                     'hacerlo',
                     'haces',
                     'hacia',
                     'haciendo',
                     'hago',
                     'han',
                     'has',
                     'hasta',
                     'hay',
                     'haya',
                     'hayamos',
                     'hayan',
                     'hayas',
                     'hayáis',
                     'he',
                     'hecho',
                     'hemos',
                     'hicieron',
                     'hizo',
                     'hoy',
                     'hube',
                     'hubiera',
                     'hubierais',
                     'hubieran',
                     'hubieras',
                     'hubieron',
                     'hubiese',
                     'hubieseis',
                     'hubiesen',
                     'hubieses',
                     'hubimos',
                     'hubiste',
                     'hubisteis',
                     'hubiéramos',
                     'hubiésemos',
                     'hubo',
                     'igual',
                     'incluso',
                     'indicó',
                     'informó',
                     'intenta',
                     'intentais',
                     'intentamos',
                     'intentan',
                     'intentar',
                     'intentas',
                     'intento',
                     'ir',
                     'junto',
                     'la',
                     'lado',
                     'largo',
                     'las',
                     'le',
                     'les',
                     'llegó',
                     'lleva',
                     'llevar',
                     'lo',
                     'los',
                     'luego',
                     'lugar',
                     'manera',
                     'manifestó',
                     'mayor',
                     'me',
                     'mediante',
                     'mejor',
                     'mencionó',
                     'menos',
                     'mi',
                     'mientras',
                     'mio',
                     'mis',
                     'misma',
                     'mismas',
                     'mismo',
                     'mismos',
                     'modo',
                     'momento',
                     'mucha',
                     'muchas',
                     'mucho',
                     'muchos',
                     'muy',
                     'más',
                     'mí',
                     'mía',
                     'mías',
                     'mío',
                     'míos',
                     'nada',
                     'nadie',
                     'ni',
                     'ninguna',
                     'ningunas',
                     'ninguno',
                     'ningunos',
                     'ningún',
                     'no',
                     'nos',
                     'nosotras',
                     'nosotros',
                     'nuestra',
                     'nuestras',
                     'nuestro',
                     'nuestros',
                     'nueva',
                     'nuevas',
                     'nuevo',
                     'nuevos',
                     'nunca',
                     'o',
                     'ocho',
                     'os',
                     'otra',
                     'otras',
                     'otro',
                     'otros',
                     'para',
                     'parece',
                     'parte',
                     'partir',
                     'pasada',
                     'pasado',
                     'pero',
                     'pesar',
                     'poca',
                     'pocas',
                     'poco',
                     'pocos',
                     'podeis',
                     'podemos',
                     'poder',
                     'podria',
                     'podriais',
                     'podriamos',
                     'podrian',
                     'podrias',
                     'podrá',
                     'podrán',
                     'podría',
                     'podrían',
                     'poner',
                     'por',
                     'por qué',
                     'porque',
                     'posible',
                     'primer',
                     'primera',
                     'primero',
                     'primeros',
                     'principalmente',
                     'propia',
                     'propias',
                     'propio',
                     'propios',
                     'próximo',
                     'próximos',
                     'pudo',
                     'pueda',
                     'puede',
                     'pueden',
                     'puedo',
                     'pues',
                     'que',
                     'quedó',
                     'queremos',
                     'quien',
                     'quienes',
                     'quiere',
                     'quién',
                     'qué',
                     'realizado',
                     'realizar',
                     'realizó',
                     'respecto',
                     'sabe',
                     'sabeis',
                     'sabemos',
                     'saben',
                     'saber',
                     'sabes',
                     'se',
                     'sea',
                     'seamos',
                     'sean',
                     'seas',
                     'segunda',
                     'segundo',
                     'según',
                     'seis',
                     'ser',
                     'seremos',
                     'será',
                     'serán',
                     'serás',
                     'seré',
                     'seréis',
                     'sería',
                     'seríais',
                     'seríamos',
                     'serían',
                     'serías',
                     'seáis',
                     'señaló',
                     'si',
                     'sido',
                     'siempre',
                     'siendo',
                     'siete',
                     'sigue',
                     'siguiente',
                     'sin',
                     'sino',
                     'sobre',
                     'sois',
                     'sola',
                     'solamente',
                     'solas',
                     'solo',
                     'solos',
                     'somos',
                     'son',
                     'soy',
                     'su',
                     'sus',
                     'suya',
                     'suyas',
                     'suyo',
                     'suyos',
                     'sí',
                     'sólo',
                     'tal',
                     'también',
                     'tampoco',
                     'tan',
                     'tanto',
                     'te',
                     'tendremos',
                     'tendrá',
                     'tendrán',
                     'tendrás',
                     'tendré',
                     'tendréis',
                     'tendría',
                     'tendríais',
                     'tendríamos',
                     'tendrían',
                     'tendrías',
                     'tened',
                     'teneis',
                     'tenemos',
                     'tener',
                     'tenga',
                     'tengamos',
                     'tengan',
                     'tengas',
                     'tengo',
                     'tengáis',
                     'tenida',
                     'tenidas',
                     'tenido',
                     'tenidos',
                     'teniendo',
                     'tenéis',
                     'tenía',
                     'teníais',
                     'teníamos',
                     'tenían',
                     'tenías',
                     'tercera',
                     'ti',
                     'tiempo',
                     'tiene',
                     'tienen',
                     'tienes',
                     'toda',
                     'todas',
                     'todavía',
                     'todo',
                     'todos',
                     'total',
                     'trabaja',
                     'trabajais',
                     'trabajamos',
                     'trabajan',
                     'trabajar',
                     'trabajas',
                     'trabajo',
                     'tras',
                     'trata',
                     'través',
                     'tres',
                     'tu',
                     'tus',
                     'tuve',
                     'tuviera',
                     'tuvierais',
                     'tuvieran',
                     'tuvieras',
                     'tuvieron',
                     'tuviese',
                     'tuvieseis',
                     'tuviesen',
                     'tuvieses',
                     'tuvimos',
                     'tuviste',
                     'tuvisteis',
                     'tuviéramos',
                     'tuviésemos',
                     'tuvo',
                     'tuya',
                     'tuyas',
                     'tuyo',
                     'tuyos',
                     'tú',
                     'ultimo',
                     'un',
                     'una',
                     'unas',
                     'uno',
                     'unos',
                     'usa',
                     'usais',
                     'usamos',
                     'usan',
                     'usar',
                     'usas',
                     'uso',
                     'usted',
                     'va',
                     'vais',
                     'valor',
                     'vamos',
                     'van',
                     'varias',
                     'varios',
                     'vaya',
                     'veces',
                     'ver',
                     'verdad',
                     'verdadera',
                     'verdadero',
                     'vez',
                     'vosotras',
                     'vosotros',
                     'voy',
                     'vuestra',
                     'vuestras',
                     'vuestro',
                     'vuestros',
                     'y',
                     'ya',
                     'yo',
                     'él',
                     'éramos',
                     'ésta',
                     'éstas',
                     'éste',
                     'éstos',
                     'última',
                     'últimas',
                     'último',
                     'últimos'))
   
     