# The largest billers in the city of Kajaani (www.tominblogi.fi)

# load & check data
library(readxl)
url <- "http://www.kajaani.fi/sites/default/files/kajaanin_kaupunki_-_vuoden_2018_ostolaskut.xlsx"
destfile <- "kajaanin_kaupunki_vuoden_2018_ostolaskut.xlsx"
download.file(url, destfile)
y <- read_excel(destfile)
View(y)

head(y)
summary(y)
glimpse(y)
colnames(y)

# Parse data & Wordcloud

toimit <- y[,c('Toimittajan nimi')] 
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
toim <- Corpus(VectorSource(toimit[1:84777,]))
toim <- tm_map(toim, PlainTextDocument)
toim <- tm_map(toim, removePunctuation)
toim<- tm_map(toim, stemDocument)
wordcloud(toim, max.words = 100, random.order = FALSE, colors= brewer.pal(9, "Set1"))

# group and filter data

toimitv <- y[,c('Toimittajan nimi', 'Osto\r\nBrutto')]
names(toimitv)[1] <- 'namekt'
names(toimitv)[2] <- 'brutto'

toimgr <- toimitv %>% 
  group_by(namekt) %>% 
  summarise(brutto = sum(brutto))

lcom <- filter(toimgr, brutto >= 1000000 & brutto < 5000000)
lcom2 <- filter(toimgr, brutto >= 300000 & brutto < 1000000)
lcom3 <- filter(toimgr, brutto >= 200000 & brutto < 300000)
lcom4 <- filter(toimgr, brutto >= 100000 & brutto < 200000)
lcom5 <- filter(toimgr, brutto >= 0 & brutto < 100000)
lcom6 <- filter(toimgr, brutto >= 5000000)

# do plots
library(ggplot2)

theme_set(theme_bw())
pl <- ggplot(lcom, aes(brutto, namekt))
pl + geom_jitter(width = .5, size=1) +
  labs(subtitle="1 000 000 - 5 000 000 euros per year",
       y="name",
       x='EUR',
       title="The largest billers")

theme_set(theme_bw())
pl <- ggplot(lcom2, aes(brutto, namekt))
pl + geom_jitter(width = .5, size=1) +
  labs(subtitle="300 000 - 1 000 000 euros per year",
       y="name",
       x='EUR',
       title="The largest billers")

