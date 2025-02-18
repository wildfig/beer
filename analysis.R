# Quench Labatt Social Analysis Data Gathering
# Author: Zach Bricker && TRISTEN RUMBAUGH!! :)
# Organization: WildFig Data

# Load Libraries
libs <- c('tidyr', 'broom', 'dplyr', 'ggplot2', 'ggfortify', 'tidytext', 'readr', 'stringr',
          'jsonlite', 'Rfacebook', 'twitteR', 'lubridate', 'scales', 'wordcloud', 'SnowballC',
          'tm', 'syuzhet', 'tidyr', 'xts')
lapply(libs, library, character.only = TRUE)
remove(libs)

# Load Data Files
filenames <- list.files(path = 'processed_data/', pattern = '*.RData', full.names = TRUE)
lapply(filenames, load, .GlobalEnv)
remove(filenames)

# Create name objects for looping
df_names <- c('labatt', 'molson', 'ultra', 'bud')
client_names <- c('Labatt_USA', 'Molson_Canadian', 'Michelob_ULTRA', 'budlight')
client_names_proper <- c('Labatt USA', 'Molson Canadian', 'Michelob ULTRA', 'Bud Light')
client_ids <- c(134391846723545, 424106561004308, 57921319808, 54876245094)

# Facebook Timestamp Creation
labatt$timestamp <- ymd_hms(labatt$created_time)
labatt$timestamp <- with_tz(labatt$timestamp, "America/New_York")

molson$timestamp <- ymd_hms(molson$created_time)
molson$timestamp <- with_tz(molson$timestamp, "America/New_York")

ultra$timestamp <- ymd_hms(ultra$created_time)
ultra$timestamp <- with_tz(ultra$timestamp, "America/New_York")

bud$timestamp <- ymd_hms(bud$created_time)
bud$timestamp <- with_tz(bud$timestamp, "America/New_York")

all_companies_ts <- rbind(labatt, molson, ultra, bud)
all_companies_ts <- subset(all_companies_ts, select = -c(message, created_time, link, id))
all_companies_ts$total_engagement <- rowSums(all_companies_ts[5:7])

all_companies_ts <- all_companies_ts %>%
  filter(from_id %in% client_ids) %>%
  filter(year(timestamp) %in% c('2015', '2016'))


labatt <- labatt %>%
  filter(from_id %in% client_ids) %>%
  filter(year(timestamp) %in% c('2015', '2016'))

molson <- molson %>%
  filter(from_id %in% client_ids) %>%
  filter(year(timestamp) %in% c('2015', '2016'))

ultra <- ultra %>%
  filter(from_id %in% client_ids) %>%
  filter(year(timestamp) %in% c('2015', '2016'))

bud <- bud %>%
  filter(from_id %in% client_ids) %>%
  filter(year(timestamp) %in% c('2011', '2016'))


# Build Summary Stats DataFrame
Company <- c('Labatt USA', 'Molson Canadian', 'Michelob ULTRA', 'Bud Light')
Comments <- c(sum(labatt$comments_count), sum(molson$comments_count), sum(ultra$comments_count), sum(bud$comments_count))
Likes <- c(sum(labatt$likes_count), sum(molson$likes_count), sum(ultra$likes_count), sum(bud$likes_count))
Shares <- c(sum(labatt$shares_count), sum(molson$shares_count), sum(ultra$shares_count), sum(bud$shares_count))
Total.Posts <- c(nrow(labatt), nrow(molson), nrow(ultra), nrow(bud))
summary_stats <- data.frame(Company, Comments, Likes, Shares, Total.Posts)


# Plot Functions
day_of_week <- function(df, client) {
  r <- get(df) %>%
    filter(from_id %in% client_ids)
  ggplot(data = r, aes(x = wday(timestamp, label = TRUE))) +
    geom_bar(aes(fill = ..count..)) +
    theme(legend.position = "none") + expand_limits(y=c(0,100)) + scale_y_continuous(breaks = c(0, 40, 80, 120, 160)) +
    xlab("Day of the Week") + ylab("Number of Posts") +
    scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
}

timeseries_engagement <- function(client) {
  r <- all_companies_ts 
  r <- filter(r, from_name == client)
  ggplot(data = r, aes(x = timestamp, y = total_engagement)) + 
    geom_line(size = 1)
}


# Create Vertical summary_stats for Labatt

summary_stats <- gather(summary_stats, Engagement, Number, Comments:Total.Posts)
labatt_engagement <- labatt %>%
  select(from_name, type, likes_count, comments_count, shares_count) %>%
  gather(count_name, value, likes_count:shares_count)

all_engagement <- all_companies_ts %>%
  select(from_name, type, likes_count, comments_count, shares_count) %>%
  gather(count_name, value, likes_count:shares_count)

# Plots for Engagement by Type for Labatt
labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "photo"] <- "Photo"

labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "video"] <- "Video"

labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "link"] <- "Link"

labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "status"] <- "Status"

labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "music"] <- "Music"

labatt_engagement$type <- as.character(labatt_engagement$type)
labatt_engagement$type[labatt_engagement$type == "event"] <- "Event"

labatt_engagement$count_name <- as.character(labatt_engagement$count_name)
labatt_engagement$count_name[labatt_engagement$count_name == "likes_count"] <- "Likes"

labatt_engagement$count_name <- as.character(labatt_engagement$count_name)
labatt_engagement$count_name[labatt_engagement$count_name == "shares_count"] <- "Shares"

labatt_engagement$count_name <- as.character(labatt_engagement$count_name)
labatt_engagement$count_name[labatt_engagement$count_name == "comments_count"] <- "Comments"

## All Engagement ###
all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "photo"] <- "Photo"

all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "video"] <- "Video"

all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "link"] <- "Link"

all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "status"] <- "Status"

all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "music"] <- "Music"

all_engagement$type <- as.character(all_engagement$type)
all_engagement$type[all_engagement$type == "event"] <- "Event"

all_engagement$count_name <- as.character(all_engagement$count_name)
all_engagement$count_name[all_engagement$count_name == "likes_count"] <- "Likes"

all_engagement$count_name <- as.character(all_engagement$count_name)
all_engagement$count_name[all_engagement$count_name == "shares_count"] <- "Shares"

all_engagement$count_name <- as.character(all_engagement$count_name)
all_engagement$count_name[all_engagement$count_name == "comments_count"] <- "Comments"

### Matrix Content Engagement for Labatt###
p <- all_engagement %>%
  filter(type != "Music") %>%
  ggplot(., aes(x = type, y = count_name)) + 
  facet_grid(~from_name) +
  stat_sum(aes(group = value, color = type)) + scale_size(range = c(5, 15)) +
  xlab("Post Content Type") + ylab("Engagement Type") +
  coord_flip() + theme(text = element_text(size=20))

plot(p)

p <- labatt_engagement %>%
  filter(type != "Music") %>%
  filter(from_name == "Labatt USA") %>%
  ggplot(., aes(x = type, y = count_name)) + 
  stat_sum(aes(group = value, color = type)) + scale_size(range = c(5, 15)) +
  xlab("Post Content Type") + ylab("Engagement Type") + 
  coord_flip() + theme(text = element_text(size=20))
plot(p)

# Summary Plots
p <- summary_stats %>%
  filter(Engagement != "Total.Posts") %>%
  ggplot(., aes(x = Company, y = Number, fill = Engagement)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("") +
  coord_flip()

plot(p)

# Create Plots for Day Of Week
for(i in seq_along(df_names)) {
  p <- day_of_week(df_names[i], client_names[i])
  plot(p)

}

# Create Plots for Time Series
for(i in seq_along(df_names)) {
  p <- timeseries_engagement(client_names_proper[i])
  plot(p)
}



########### Kevins Plots ###########
# Plots by Month
load('processed_data/labatt_ts.RData')
labatt$dateKP <- date(labatt$timestamp)

pphTS <- labatt %>% 
  select(from_name, type, likes_count:shares_count, dateKP) %>%
  filter(from_name == "Labatt USA") %>%
  mutate(totEng = likes_count + comments_count + shares_count)

pphTS <- pphTS %>%
  mutate(month = as.Date(cut(pphTS$dateKP, breaks = "month")))

pphTS2 <- aggregate(pphTS$totEng~pphTS$month, FUN=sum, na.rm=TRUE)

# graph by month:
ggplot(data = pphTS, aes(month, totEng)) +
  stat_summary(fun.y = sum, geom = "line") #this does the summation not in the data

summary(pphTS2)
pph_ts <- ts(pphTS2, start=c(2011, 7), end=c(2016, 8), frequency=12) 

plot(pph_ts)



########### Kevins Plots ###########

all_companies_ts <- all_companies_ts %>%
  filter(from_id %in% client_ids) %>%
  mutate(month = as.Date(cut(all_companies_ts$timestamp, breaks = "month")))


ggplot(all_companies_ts, aes(x = month, y = total_engagement)) +
  geom_line(aes(group = from_name, color = factor(from_name)))

# Wordclouds

labatt$timestamp <- date(labatt$timestamp)
# labatt Pre
labatt_pre <- labatt %>%
  filter(labatt$timestamp<as.Date("2015-05-01"))

labatt_post <- labatt %>%
  filter(labatt$timestamp>as.Date("2015-05-02"))

labatt_clean_pre <- str_replace_all(labatt_pre$message, "@\\w+", "")
labatt_clean_pre <- gsub("&amp", "", labatt_clean_pre)
labatt_clean_pre <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", labatt_clean_pre)
labatt_clean_pre <- gsub("@\\w+", "", labatt_clean_pre)
labatt_clean_pre <- gsub("[[:punct:]]", "", labatt_clean_pre)
labatt_clean_pre <- gsub("[[:digit:]]", "", labatt_clean_pre)
labatt_clean_pre <- gsub("http\\w+", "", labatt_clean_pre)
labatt_clean_pre <- gsub("[ \t]{2,}", "", labatt_clean_pre)
labatt_clean_pre <- gsub("^\\s+|\\s+$", "", labatt_clean_pre)

labatt_corpus_pre <- Corpus(VectorSource(labatt_clean_pre))
labatt_corpus_pre <- tm_map(labatt_corpus_pre, removePunctuation)
labatt_corpus_pre <- tm_map(labatt_corpus_pre, content_transformer(tolower))
labatt_corpus_pre <- tm_map(labatt_corpus_pre, removeWords, stopwords("english"))
labatt_corpus_pre <- tm_map(labatt_corpus_pre, removeWords, c("amp", "2yo", "3yo", "4yo"))
labatt_corpus_pre <- tm_map(labatt_corpus_pre, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)

wordcloud(words = labatt_corpus_pre, scale=c(5,0.1), max.words=25, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)
tdm <- TermDocumentMatrix(labatt_corpus_pre)
tdm

# molson Post

labatt_clean_post <- str_replace_all(labatt_post$message, "@\\w+", "")
labatt_clean_post <- gsub("&amp", "", labatt_clean_post)
labatt_clean_post <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", labatt_clean_post)
labatt_clean_post <- gsub("@\\w+", "", labatt_clean_post)
labatt_clean_post <- gsub("[[:punct:]]", "", labatt_clean_post)
labatt_clean_post <- gsub("[[:digit:]]", "", labatt_clean_post)
labatt_clean_post <- gsub("http\\w+", "", labatt_clean_post)
labatt_clean_post <- gsub("[ \t]{2,}", "", labatt_clean_post)
labatt_clean_post <- gsub("^\\s+|\\s+$", "", labatt_clean_post)

labatt_corpus_post <- Corpus(VectorSource(labatt_clean_post))
labatt_corpus_post <- tm_map(labatt_corpus_post, removePunctuation)
labatt_corpus_post <- tm_map(labatt_corpus_post, content_transformer(tolower))
labatt_corpus_post <- tm_map(labatt_corpus_post, removeWords, stopwords("english"))
labatt_corpus_post <- tm_map(labatt_corpus_post, removeWords, c("amp", "2yo", "3yo", "4yo"))
labatt_corpus_post <- tm_map(labatt_corpus_post, stripWhitespace)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)

wordcloud(words = labatt_corpus_post, scale=c(5,0.1), max.words=25, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)
tdm <- TermDocumentMatrix(labatt_corpus_post)
tdm

### Day of Week All ###
zach_filter <- all_companies_ts %>%
  filter(from_id %in% client_ids)
ggplot(data = zach_filter, aes(x = wday(timestamp, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of Posts") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4") + facet_wrap(~from_name, ncol = 4)


dowDat <- select(all_companies_ts, total_engagement,from_name, timestamp)
dowDat$dow <- wday(dowDat$timestamp, label=TRUE)
head(dowDat)
dowDat <- aggregate(total_engagement~dow+from_name, data=dowDat, FUN=mean)

ggplot(dowDat, aes(x = dow, y = total_engagement)) +
  geom_bar(stat="identity", aes(fill = total_engagement)) + 
  facet_grid(~from_name) + 
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of Engagements") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")
### Day of Week All ###

### Points for Posts
ggplot(all_companies_ts, aes(x = month, y = total_engagement)) +
  geom_point(aes(color = from_name)) +
  xlab("Year") + ylab("Total Engagement") + 
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))
### Points for Posts

### Total Engagement Line ###
q <- aggregate(all_companies_ts$total_engagement~all_companies_ts$month+
                 all_companies_ts$from_name,
               FUN=sum)

ggplot(q, aes(x = q$`all_companies_ts$month`, y = q$`all_companies_ts$total_engagement`)) +
  geom_line(aes(color=q$`all_companies_ts$from_name`)) +
  ylab("Total Engagement") + xlab("Year") +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))
### Total Engagement Line ###

### molson Content Over Time ###
t <- all_companies_ts %>%
  filter(., from_name == "Molson Canadian")
t <- data.frame(table(t$month, t$type))

t$Var1 <- date(t$Var1)
ggplot(t, aes(x = Var1, y = Freq, group = Var2)) +
  geom_line(aes(color=Var2)) +
  xlab("Year") + ylab("Post Frequency") +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))


#TRISTEN'S GRAPHS!!

### Labatt Content Over Time ###
t <- all_companies_ts %>%
  filter(., from_name == "Labatt USA")
t <- data.frame(table(t$month, t$type))

t$Var1 <- date(t$Var1)
ggplot(t, aes(x = Var1, y = Freq, group = Var2)) +
  geom_line(aes(color=Var2)) +
  xlab("Year") + ylab("Post Frequency") +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))


#MichelobULTRA Content Over Time ###
t <- all_companies_ts %>%
  filter(., from_name == "Michelob ULTRA")
t <- data.frame(table(t$month, t$type))

t$Var1 <- date(t$Var1)
ggplot(t, aes(x = Var1, y = Freq, group = Var2)) +
  geom_line(aes(color=Var2)) +
  xlab("Year") + ylab("Post Frequency") +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))


#Bud Light Content Over Time ###
t <- all_companies_ts %>%
  filter(., from_name == "Bud Light")
t <- data.frame(table(t$month, t$type))

t$Var1 <- date(t$Var1)
ggplot(t, aes(x = Var1, y = Freq, group = Var2)) +
  geom_line(aes(color=Var2)) +
  xlab("Year") + ylab("Post Frequency") +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=12), 
        legend.position=c(0.18, 0.77), 
        legend.background=element_rect(fill=alpha('gray', 0)))


### Bud Data Manipulation
bud_total_engagement <- sum(c(bud$likes_count, bud$comments_count, bud$shares_count))


   