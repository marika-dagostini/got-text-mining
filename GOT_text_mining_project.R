#################################################################
###### SENTIMENTAL ANALYSIS BY CHARACTERS GAME OF THRONES  ######
#################################################################

##### Packages #####
library(tm)
library(ggplot2)
library(qdap)
library(tidytext)
library(tidyr)
library(dplyr)
library(broom)
library(Rstem)
library(sentiment)
library(openNLP)
library(igraph)
library(stringr)
library(wordcloud)
library(RDRPOSTagger)
library(tokenizers)
library(readtext)


setwd('C:/Users/Amministratore/Desktop/Text Mining/Project')


##### Split Files into Chapters #####
### Book1 ###

got1<-readLines("./Data/1.AGameOfThrones-GeorgeR.R.Martin.txt")

clean.text<-function(text){
  text<-iconv(text, "latin1", "ASCII", sub="")
  text<-stripWhitespace(text)
  text<-text[which(text !="")]
}

got1<-clean.text(got1)
chap1<-readLines('./Data/Chapters1.txt')


split.chap<-function(text, chapters, book){
  for(i in (2:length(chapters))){
    for(j in (1:length(text))){
      if (chapters[i]==text[j]){
        z<-j-1
        new.chapter<-text[1:z]
        write.table(new.chapter[2:length(new.chapter)], 
                    file = paste("./Splitted Chapters/", book,
                                 "/",new.chapter[1], (i-1),".txt", sep=""))
        text<-text[j:length(text)]
        break
      }
    }
  }
  write.table(text[2:length(text)], file = paste(text[1],(length(chapters)),".txt", sep=""))
}

split.chap(text = got1, chapters = chap1, book = 1)

### Book2 ###
got2<-readLines("./Data/2.AClashOfKings-GeorgeR.R.Martin.txt")
got2<-clean.text(got2)
chap2<-readLines('./Data/Chapters2.txt')
split.chap(got2, chap2, book = 2)

### Book3 ###
got3<-readLines("./Data/3.AStormOfSwords-GeorgeR.R.Martin.txt")
got3<-clean.text(got3)
chap3<-readLines(./'Data/Chapters3.txt')
split.chap(got3, chap3, book = 3)

### Book4 ###
got4<-readLines("./Data/4.AFeastForCrows-GeorgeR.R.Martin.txt")
got4<-clean.text(got4)
chap4<-readLines('Data/Chapters4.txt')
split.chap(got4, chap4, book = 4)

### Book5 ###
got5<-readLines("./Data/5.ADanceWithDragons-GeorgeR.R.Martin.txt")
got5<-clean.text(got5)
chap5<-readLines('./Data/Chapters5.txt')
split.chap(got5, chap5, book = 5)


##### Import Books ####
text1<-readtext("./Splitted Chapters/1/*.txt")

book.table<-function(text,n){
  text$num<-gsub('.* ([0-9]+).txt','\\1',text$doc_id)
  text$num <- as.numeric(as.character( text$num ))
  text$narrator<-gsub("^([[:alpha:]]*).([0-9]+).txt", "\\1", text$doc_id)
  text$book_n<-as.numeric(n)
  text$book_n <- as.numeric(as.character( text$book_n ))
  text<-text[,2:5]
  text<-text[order(text$num, decreasing=F),]
}

text1<-book.table(text1,1)

text2<-readtext("./Splitted Chapters/2/*.txt")
text2<-book.table(text2,2)

text3<-readtext("./Splitted Chapters/3/*.txt")
text3<-book.table(text3,3)

text4<-readtext("./Splitted Chapters/4/*.txt")
text4<-book.table(text4,4)

text5<-readtext("./Splitted Chapters/5/*.txt")
text5<-book.table(text5,5)


##### Chapters Analysis ####

sum1<-cbind(as.matrix(count(text1, text1$narrator)),matrix(rep("1.A Game of Thrones",9),9,1))
sum1[7,1]<-"OTHERS"
sum2<-cbind(as.matrix(count(text2, text2$narrator)),matrix(rep("2.A Clash of Kings",10),10,1))
sum2[7,1]<-"OTHERS"
sum3<-cbind(as.matrix(count(text3, text3$narrator)),matrix(rep("3.A Storm of Swords",12),12,1))
sum3[c(6,9),1]<-"OTHERS"
sum4<-cbind(as.matrix(count(text4, text4$narrator)),matrix(rep("4.A Feast for Crows",18),18,1))
sum4[c(1,4,7,10:18),1]<-"OTHERS"
sum5<-cbind(as.matrix(count(text5, text5$narrator)),matrix(rep("5.A Dance with Dragons",33),33,1))
sum5[c(1,6,9:10,12:30,33),1]<-"OTHERS"

sum<-as.data.frame(rbind(sum1,sum2,sum3,sum4,sum5))
sum[, 2] <- as.numeric(as.character( sum[, 2] ))
sum[, 1] <- as.character( sum[, 1] )

ggplot(data=sum, aes(x=sum$`text1$narrator`, y=n, fill=V3)) +
  geom_bar(na.rm=TRUE, position="stack", width=0.8, stat="identity")+
  labs(x="Narrator", y="Number of Chapters", caption="Figure 3.1")+
  ggtitle("Number of Chapters in the Series by Narrator ")+
  labs(fill="Book") +
  theme(legend.position = 'bottom')+
  scale_fill_manual(values=c("dodgerblue4", "chocolate2", 
                             "firebrick", "seagreen", "lightyellow4"))




##### SERIES ANALYSIS #####
##### Overall Frequency #####
asoiaf<-as.data.frame(rbind(text1,text2,text3,text4,text5))
asoiafCorpus <- Corpus(VectorSource(asoiaf$text))
tdm_asoiaf <- TermDocumentMatrix(asoiafCorpus)
inspect(tdm_asoiaf)

clean.corpus <- function(corpus){
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, tolower) 
  corpus <- tm_map(corpus, removeWords, c("x","dont")) 
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"))) 
  corpus <- tm_map(corpus, removeWords, stop_words$word) 
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes=T)
  corpus <- tm_map(corpus, stripWhitespace) 
  return(corpus)
}

asoiafCorpus<-clean.corpus(asoiafCorpus)

tdm_asoiaf <- TermDocumentMatrix(asoiafCorpus)
inspect(tdm_asoiaf)

abs.freq_asoiaf<-rowSums(as.matrix(tdm_asoiaf))
abs.df_asoiaf <- data.frame(term = names(abs.freq_asoiaf),
                            frequency = abs.freq_asoiaf)
abs.df_asoiaf <- abs.df_asoiaf[order(abs.df_asoiaf[,2], decreasing=F),]

abs.df_asoiaf$term <- factor(abs.df_asoiaf$term,
                             levels = unique(abs.df_asoiaf$term))

ggplot(abs.df_asoiaf[23694:23713,], aes(x = term, y = frequency)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = frequency), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent words in the series\n'A Song of Ice and Fire'",
       x="Term", y="Frequency", caption ="Figure 3.2") 


##### Common Words Between Books #####
asoiaf1.vec <- paste(as.vector(asoiafCorpus[1:73]), collapse=" ")
asoiaf2.vec <- paste(as.vector(asoiafCorpus[74:143]), collapse=" ")
asoiaf3.vec <- paste(as.vector(asoiafCorpus[144:225]), collapse=" ")
asoiaf4.vec <- paste(as.vector(asoiafCorpus[226:271]), collapse=" ")
asoiaf5.vec <- paste(as.vector(asoiafCorpus[272:344]), collapse=" ")

all_asoiaf <- c(asoiaf1.vec, asoiaf2.vec, asoiaf3.vec, asoiaf4.vec, asoiaf5.vec)

asoiafCorpus.all <- VCorpus(VectorSource(all_asoiaf))
tdm.asoiaf.all <- TermDocumentMatrix(asoiafCorpus.all)
tdm.m.asoiaf.all <- as.matrix(tdm.asoiaf.all)
colnames(tdm.m.asoiaf.all) = c("A Game\nof Thrones", "A Clash\nof Kings", 
                               "A Storm\nof Swords", "A Feast\nfor Crows",
                               "A Dance\nwith Dragons")

comparison.cloud(tdm.m.asoiaf.all, max.words=100, random.order=FALSE, title.size=1.0, 
                 colors=c("midnightblue", "darkorange3","darkgreen", 
                          "indianred4","grey34"))







##### ARYA ####
##### Frequency ######
arya1<-text1[which(text1$narrator =="ARYA"),]
arya1 <- arya1[order(arya1[,2], decreasing=F),]

arya2<-text2[which(text2$narrator =="ARYA"),]
arya2<- arya2[order(arya2[,2], decreasing=F),]

arya3<-text3[which(text3$narrator =="ARYA"),]
arya3 <- arya3[order(arya3[,2], decreasing=F),]

arya4<-text4[which(text4$narrator =="ARYA"),]
arya4<- arya4[order(arya4[,2], decreasing=F),]

arya<-rbind(arya1,arya2,arya3,arya4)

aryaCorpus <- Corpus(VectorSource(arya$text)) 
arya.dtm <- DocumentTermMatrix(aryaCorpus)
inspect(arya.dtm)
aryaCorpus<-clean.corpus(aryaCorpus)
arya.dtm <- DocumentTermMatrix(aryaCorpus)
inspect(arya.dtm)

arya.tidy <- tidy(arya.dtm)
colnames(arya.tidy)<-c('chapter_number','word','count')
arya.tidy$chapter_number<-as.numeric(arya.tidy$chapter_number)

count.words.arya<-count(arya.tidy, word)
total.words.arya<-sum(count.words.arya$n)

tdm_arya<-TermDocumentMatrix(aryaCorpus)
inspect(tdm_arya)

abs.freq_arya<-rowSums(as.matrix(tdm_arya))
abs.df_arya<-data.frame(term = names(abs.freq_arya ),
                        frequency = abs.freq_arya)
abs.df_arya<-abs.df_arya[order(abs.df_arya[,2], decreasing=F),]

abs.df_arya$term<-factor(abs.df_arya$term,
                         levels = unique(abs.df_arya$term))

ggplot(abs.df_arya[7324:7343,], aes(x = term, y = frequency)) +
  geom_bar(stat="identity", fill='midnightblue') +
  coord_flip() +
  geom_text(aes(label = frequency), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent words\nin 'Arya Stark'\nchapters",
       x="Term",y="Frequency",caption ="Figure 3.4")

##### Common Words #####
arya1.vec <- paste(as.vector(aryaCorpus[1:5]), collapse=" ")
arya2.vec <- paste(as.vector(aryaCorpus[6:15]), collapse=" ")
arya3.vec <- paste(as.vector(aryaCorpus[16:28]), collapse=" ")
arya4.vec <- paste(as.vector(aryaCorpus[29:30]), collapse=" ")

all_arya <- c(arya1.vec, arya2.vec, arya3.vec, arya4.vec)

aryaCorpus.all <- VCorpus(VectorSource(all_arya))
tdm.arya.all <- TermDocumentMatrix(aryaCorpus.all)
inspect(tdm.arya.all)

tdm.m.arya.all <- as.matrix(tdm.arya.all)
colnames(tdm.m.arya.all) = c("A Game of Thrones", "A Clash of Kings", 
                             "A Storm of Swords", "A Feast for Crows")
#Figure 3.5
comparison.cloud(tdm.m.arya.all, max.words=100, random.order=FALSE, title.size=1.0, 
                 colors=c("midnightblue", "darkorange3","darkgreen", "indianred4"))




##### DAENERYS ####
##### Frequency ######
daenerys1<-text1[which(text1$narrator =="DAENERYS"),]
daenerys1 <- daenerys1[order(daenerys1[,2], decreasing=F),]

daenerys2<-text2[which(text2$narrator =="DAENERYS"),]
daenerys2<- daenerys2[order(daenerys2[,2], decreasing=F),]

daenerys3<-text3[which(text3$narrator =="DAENERYS"),]
daenerys3 <- daenerys3[order(daenerys3[,2], decreasing=F),]

daenerys5<-text5[which(text5$narrator =="DAENERYS"),]
daenerys5<- daenerys5[order(daenerys5[,2], decreasing=F),]

daenerys<-rbind(daenerys1,daenerys2,daenerys3,daenerys5)

daenerysCorpus <- Corpus(VectorSource(daenerys$text)) 
daenerys.dtm <- DocumentTermMatrix(daenerysCorpus)
inspect(daenerys.dtm)
daenerysCorpus<-clean.corpus(daenerysCorpus)
daenerys.dtm <- DocumentTermMatrix(daenerysCorpus)
inspect(daenerys.dtm)

daenerys.tidy <- tidy(daenerys.dtm)
colnames(daenerys.tidy)<-c('chapter_number','word','count')
daenerys.tidy$chapter_number<-as.numeric(daenerys.tidy$chapter_number)

count.words.daenerys<-count(daenerys.tidy, word)
total.words.daenerys<-sum(count.words.daenerys$n)

tdm_daenerys<-TermDocumentMatrix(daenerysCorpus)
inspect(tdm_daenerys)

abs.freq_daenerys<-rowSums(as.matrix(tdm_daenerys))
abs.df_daenerys<-data.frame(term = names(abs.freq_daenerys ),
                            frequency = abs.freq_daenerys)
abs.df_daenerys<-abs.df_daenerys[order(abs.df_daenerys[,2], decreasing=F),]

abs.df_daenerys$term<-factor(abs.df_daenerys$term,
                             levels = unique(abs.df_daenerys$term))

ggplot(abs.df_daenerys[8576:8595,], aes(x = term, y = frequency)) +
  geom_bar(stat="identity", fill='indianred4') +
  coord_flip() +
  geom_text(aes(label = frequency), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent words\nin 'Daenerys Targaryen'\nchapters",
       x="Term", y="Frequency",caption ="Figure 3.6")

##### Common Words #####
daenerys1.vec <- paste(as.vector(daenerysCorpus[1:10]), collapse=" ")
daenerys2.vec <- paste(as.vector(daenerysCorpus[11:15]), collapse=" ")
daenerys3.vec <- paste(as.vector(daenerysCorpus[16:21]), collapse=" ")
daenerys5.vec <- paste(as.vector(daenerysCorpus[22:31]), collapse=" ")

all_daenerys <- c(daenerys1.vec, daenerys2.vec, daenerys3.vec, daenerys5.vec)

daenerysCorpus.all <- VCorpus(VectorSource(all_daenerys))
tdm.daenerys.all <- TermDocumentMatrix(daenerysCorpus.all)
inspect(tdm.daenerys.all)

tdm.m.daenerys.all <- as.matrix(tdm.daenerys.all)
colnames(tdm.m.daenerys.all) = c("A Game of Thrones", "A Clash of Kings", 
                                 "A Storm of Swords", "A Dance with Dragons	")

#Figure 3.7
comparison.cloud(tdm.m.daenerys.all, max.words=100, random.order=FALSE, title.size=1.0, 
                 colors=c("midnightblue", "darkorange3","darkgreen", "indianred4"))





##### JON ####
##### Frequency ######
jon1<-text1[which(text1$narrator =="JON"),]
jon1 <- jon1[order(jon1[,2], decreasing=F),]

jon2<-text2[which(text2$narrator =="JON"),]
jon2<- jon2[order(jon2[,2], decreasing=F),]

jon3<-text3[which(text3$narrator =="JON"),]
jon3 <- jon3[order(jon3[,2], decreasing=F),]

jon5<-text5[which(text5$narrator =="JON"),]
jon5<- jon5[order(jon5[,2], decreasing=F),]

jon<-rbind(jon1,jon2,jon3,jon5)

jonCorpus <- Corpus(VectorSource(jon$text)) 
jon.dtm <- DocumentTermMatrix(jonCorpus)
inspect(jon.dtm)
jonCorpus<-clean.corpus(jonCorpus)
jon.dtm <- DocumentTermMatrix(jonCorpus)
inspect(jon.dtm)

jon.tidy <- tidy(jon.dtm)
colnames(jon.tidy)<-c('chapter_number','word','count')
jon.tidy$chapter_number<-as.numeric(jon.tidy$chapter_number)

count.words.jon<-count(jon.tidy, word)
total.words.jon<-sum(count.words.jon$n)

tdm_jon<-TermDocumentMatrix(jonCorpus)
inspect(tdm_jon)

abs.freq_jon<-rowSums(as.matrix(tdm_jon))
abs.df_jon<-data.frame(term = names(abs.freq_jon ),
                       frequency = abs.freq_jon)
abs.df_jon<-abs.df_jon[order(abs.df_jon[,2], decreasing=F),]

abs.df_jon$term<-factor(abs.df_jon$term,
                        levels = unique(abs.df_jon$term))

ggplot(abs.df_jon[9267:9286,], aes(x = term, y = frequency)) +
  geom_bar(stat="identity", fill='midnightblue') +
  coord_flip() +
  geom_text(aes(label = frequency), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent words\nin 'Jon Snow'\nchapters",
       caption ="Figure 3.8", x="Term", y="Frequency") 

##### Common Words #####
jon1.vec <- paste(as.vector(jonCorpus[1:9]), collapse=" ")
jon2.vec <- paste(as.vector(jonCorpus[10:17]), collapse=" ")
jon3.vec <- paste(as.vector(jonCorpus[18:29]), collapse=" ")
jon5.vec <- paste(as.vector(jonCorpus[30:42]), collapse=" ")

all_jon <- c(jon1.vec, jon2.vec, jon3.vec, jon5.vec)

jonCorpus.all <- VCorpus(VectorSource(all_jon))
tdm.jon.all <- TermDocumentMatrix(jonCorpus.all)
inspect(tdm.jon.all)

tdm.m.jon.all <- as.matrix(tdm.jon.all)
colnames(tdm.m.jon.all) = c("A Game of Thrones", "A Clash of Kings", 
                            "A Storm of Swords", "A Dance with Dragons	")

#Figure 3.9
comparison.cloud(tdm.m.jon.all, max.words=100, random.order=FALSE, title.size=1.0, 
                 colors=c("midnightblue", "darkorange3","darkgreen", "indianred4"))





##### TYRION ####
##### Frequency ######
tyrion1<-text1[which(text1$narrator =="TYRION"),]
tyrion1 <- tyrion1[order(tyrion1[,2], decreasing=F),]

tyrion2<-text2[which(text2$narrator =="TYRION"),]
tyrion2<- tyrion2[order(tyrion2[,2], decreasing=F),]

tyrion3<-text3[which(text3$narrator =="TYRION"),]
tyrion3 <- tyrion3[order(tyrion3[,2], decreasing=F),]

tyrion5<-text5[which(text5$narrator =="TYRION"),]
tyrion5<- tyrion5[order(tyrion5[,2], decreasing=F),]

tyrion<-rbind(tyrion1,tyrion2,tyrion3,tyrion5)

tyrionCorpus <- Corpus(VectorSource(tyrion$text)) 
tyrion.dtm <- DocumentTermMatrix(tyrionCorpus)
inspect(tyrion.dtm)
tyrionCorpus<-clean.corpus(tyrionCorpus)
tyrion.dtm <- DocumentTermMatrix(tyrionCorpus)
inspect(tyrion.dtm)

tyrion.tidy <- tidy(tyrion.dtm)
colnames(tyrion.tidy)<-c('chapter_number','word','count')
tyrion.tidy$chapter_number<-as.numeric(tyrion.tidy$chapter_number)

count.words.tyrion<-count(tyrion.tidy, word)
total.words.tyrion<-sum(count.words.tyrion$n)

tdm_tyrion<-TermDocumentMatrix(tyrionCorpus)
inspect(tdm_tyrion)

abs.freq_tyrion<-rowSums(as.matrix(tdm_tyrion))
abs.df_tyrion<-data.frame(term = names(abs.freq_tyrion ),
                          frequency = abs.freq_tyrion)
abs.df_tyrion<-abs.df_tyrion[order(abs.df_tyrion[,2], decreasing=F),]

abs.df_tyrion$term<-factor(abs.df_tyrion$term,
                           levels = unique(abs.df_tyrion$term))

ggplot(abs.df_tyrion[11577:11596,], aes(x = term, y = frequency)) +
  geom_bar(stat="identity", fill='indianred4') +
  coord_flip() +
  geom_text(aes(label = frequency), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent words\nin 'Tyrion Lannister'\nchapters",
       x="Term", y="Frequency", caption="Figure 3.10") 

##### Common Words #####
tyrion1.vec <- paste(as.vector(tyrionCorpus[1:9]), collapse=" ")
tyrion2.vec <- paste(as.vector(tyrionCorpus[10:24]), collapse=" ")
tyrion3.vec <- paste(as.vector(tyrionCorpus[25:35]), collapse=" ")
tyrion5.vec <- paste(as.vector(tyrionCorpus[36:47]), collapse=" ")

all_tyrion <- c(tyrion1.vec, tyrion2.vec, tyrion3.vec, tyrion5.vec)

tyrionCorpus.all <- VCorpus(VectorSource(all_tyrion))
tdm.tyrion.all <- TermDocumentMatrix(tyrionCorpus.all)
inspect(tdm.tyrion.all)

tdm.m.tyrion.all <- as.matrix(tdm.tyrion.all)
colnames(tdm.m.tyrion.all) = c("A Game of Thrones", "A Clash of Kings", 
                               "A Storm of Swords", "A Dance with Dragons	")

#Figure 3.11
comparison.cloud(tdm.m.tyrion.all, max.words=100, random.order=FALSE, title.size=1.0, 
                 colors=c("midnightblue", "darkorange3","darkgreen", "indianred4"))




##### SERIES


##### SERIES #####
##### Sentimental Analysis #####
nrc.joy <-subset(sentiments,sentiments$lexicon=='nrc' & sentiments$sentiment=='joy')
bing <- subset(sentiments, sentiments$lexicon=='bing')[,-4]

asoiaf.dtm<-DocumentTermMatrix(asoiafCorpus)
asoiaf.tidy <- tidy(asoiaf.dtm)
colnames(asoiaf.tidy)<-c('chapter_number','word','count')
asoiaf.tidy$chapter_number<-as.numeric(asoiaf.tidy$chapter_number)

joy.words.asoiaf <- inner_join(asoiaf.tidy,nrc.joy, by="word")
count.joy.words.asoiaf <- count(joy.words.asoiaf, word)

asoiaf.sentiment <- inner_join(asoiaf.tidy,bing, by="word")
asoiaf.sentiment <- count(asoiaf.sentiment,sentiment, index=chapter_number)
asoiaf.sentiment <- spread(asoiaf.sentiment, sentiment, n, fill = 0)

asoiaf.sentiment$polarity <- asoiaf.sentiment$positive -  asoiaf.sentiment$negative
asoiaf.sentiment$pos <- ifelse(asoiaf.sentiment$polarity >= 0, "pos", "neg")

ggplot(asoiaf.sentiment, aes(x=index, y=polarity, fill=pos)) +
  geom_bar(stat="identity", position="identity", width=1)+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values=c("midnightblue", "firebrick4"),
                    labels=c("Negative", "Positive")) +
  ggtitle("Polarity in the whole Series using 'bing' lexicon ") +
  labs(x="Narrative Time", y="Sentiment", caption="Figure 4.1",
       subtitle = "The vertical lines represent the end of each book")+
  theme_grey()+
  theme(legend.position="bottom")+
  geom_vline(xintercept = c(73,143,225,271))

asoiaf.sentiment <- inner_join(asoiaf.tidy,bing, by="word")
asoiaf.negative<-asoiaf.sentiment[which(asoiaf.sentiment$sentiment=="negative"),]
asoiaf.negative<-aggregate(asoiaf.negative$count, list(asoiaf.negative$word), sum)
asoiaf.negative<-asoiaf.negative[order(asoiaf.negative$x, decreasing=F),]
asoiaf.negative<-as.data.frame(asoiaf.negative)
asoiaf.negative$Group.1 <- factor(asoiaf.negative$Group.1,
                             levels = unique(asoiaf.negative$Group.1))

ggplot(asoiaf.negative[2204:2225,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent negative\nwords in the whole Series", 
       x="Frequency", y="Word",  caption="Figure 4.2")

asoiaf.positive<-asoiaf.sentiment[which(asoiaf.sentiment$sentiment=="positive"),]
asoiaf.positive<-aggregate(asoiaf.positive$count, list(asoiaf.positive$word), sum)
asoiaf.positive<-asoiaf.positive[order(asoiaf.positive$x, decreasing=F),]
asoiaf.positive<-as.data.frame(asoiaf.positive)
asoiaf.positive$Group.1 <- factor(asoiaf.positive$Group.1,
                                  levels = unique(asoiaf.positive$Group.1))

ggplot(asoiaf.positive[928:949,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='firebrick4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent positive\nwords in the Series", 
       x="Frequency", y="Word", caption="Figure 4.3")
##### Emotion Classification ####
data(emotions)

emo.asoiaf.df <- as.data.frame(classify_emotion(asoiaf$text))

score<-round(as.numeric(as.matrix(emo.asoiaf.df[,1:6])),2)
chapter<-as.numeric(matrix(rep(seq(1,334),6), nrow = 334*6, ncol = 1))
Emotion<-as.character(matrix(rep(c("Anger", "Disgust","Fear","Joy","Sadness","Surprise"),
                                 c(334,334,334,334,334,334)),
                             nrow = 334*6, ncol = 1))

asoiaf.plot<-as.data.frame(cbind(score,chapter,Emotion))[1:2004,]
asoiaf.plot[, 1] <- as.numeric(as.character( asoiaf.plot[, 1] ))
asoiaf.plot[, 2] <- as.numeric(as.character( asoiaf.plot[, 2] ))

ggplot(data=asoiaf.plot, aes(x=asoiaf.plot$chapter, y=score, fill=Emotion)) +
  geom_bar(na.rm=TRUE, position = "fill", stat="identity", width = 1)+
  labs(x="Chapters", y="Score", caption="Figure 4.4")+
  ggtitle("Emotions in the whole series ")+
  scale_fill_manual(values=c("firebrick", "seagreen", "dodgerblue4", 
                             "chocolate2", "seashell4", "goldenrod2"))+
  theme_gray()+
  geom_vline(xintercept = c(73,143,225,271))










##### ARYA #####
##### Sentimental Analysis #####

joy.words.arya <- inner_join(arya.tidy,nrc.joy, by="word")
count.joy.words.arya <- count(joy.words.arya, word)

arya.sentiment <- inner_join(arya.tidy,bing, by="word")
arya.sentiment <- count(arya.sentiment,sentiment, index=chapter_number)
arya.sentiment <- spread(arya.sentiment, sentiment, n, fill = 0)

arya.sentiment$polarity <- arya.sentiment$positive -  arya.sentiment$negative
arya.sentiment$pos <- ifelse(arya.sentiment$polarity >= 0, "pos", "neg")

ggplot(arya.sentiment, aes(x=index, y=polarity, fill=pos)) +
  geom_bar(stat="identity", position="identity",width=0.91, fill="midnightblue") + 
  ggtitle("Polarity in 'Arya Stark' chapters using 'bing' lexicon ") +
  labs(x="Narrative Time", y="Sentiment", caption="Figure 4.5",
       subtitle = "The vertical lines represent the end of each book.")+
  theme_grey()+
  geom_vline(xintercept = c(5,15,28))

arya.sentiment <- inner_join(arya.tidy,bing, by="word")
arya.negative<-arya.sentiment[which(arya.sentiment$sentiment=="negative"),]
arya.negative<-aggregate(arya.negative$count, list(arya.negative$word), sum)
arya.negative<-arya.negative[order(arya.negative$x, decreasing=F),]
arya.negative<-as.data.frame(arya.negative)
arya.negative$Group.1 <- factor(arya.negative$Group.1,
                                levels = unique(arya.negative$Group.1))

ggplot(arya.negative[799:808,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent negative\nwords in 'Arya Stark'\nchapters", 
       x="Frequency", y="Word",  caption="Figure 4.6")

arya.positive<-arya.sentiment[which(arya.sentiment$sentiment=="positive"),]
arya.positive<-aggregate(arya.positive$count, list(arya.positive$word), sum)
arya.positive<-arya.positive[order(arya.positive$x, decreasing=F),]
arya.positive<-as.data.frame(arya.positive)
arya.positive$Group.1 <- factor(arya.positive$Group.1,
                                levels = unique(arya.positive$Group.1))

ggplot(arya.positive[286:295,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='firebrick4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent positive\nwords in 'Arya Stark'\nchapters", 
       x="Frequency", y="Word", caption="Figure 4.7")




##### DAENERYS #####
##### Sentimental Analysis #####

joy.words.daenerys <- inner_join(daenerys.tidy,nrc.joy, by="word")
count.joy.words.daenerys <- count(joy.words.daenerys, word)

daenerys.sentiment <- inner_join(daenerys.tidy,bing, by="word")
daenerys.sentiment <- count(daenerys.sentiment,sentiment, index=chapter_number)
daenerys.sentiment <- spread(daenerys.sentiment, sentiment, n, fill = 0)

daenerys.sentiment$polarity <- daenerys.sentiment$positive -  daenerys.sentiment$negative
daenerys.sentiment$pos <- ifelse(daenerys.sentiment$polarity >= 0, "pos", "neg")

ggplot(daenerys.sentiment, aes(x=index, y=polarity, fill=pos)) +
  geom_bar(stat="identity", position="identity",width=0.91, fill="midnightblue") + 
  ggtitle("Polarity in 'Daenerys Targaryen' chapters using 'bing' lexicon ") +
  labs(x="Narrative Time", y="Sentiment", caption="Figure 4.8",
       subtitle = "The vertical lines represent the end of each book.")+
  theme_grey()+
  geom_vline(xintercept = c(10,15,21))

daenerys.sentiment <- inner_join(daenerys.tidy,bing, by="word")
daenerys.negative<-daenerys.sentiment[which(daenerys.sentiment$sentiment=="negative"),]
daenerys.negative<-aggregate(daenerys.negative$count, list(daenerys.negative$word), sum)
daenerys.negative<-daenerys.negative[order(daenerys.negative$x, decreasing=F),]
daenerys.negative<-as.data.frame(daenerys.negative)
daenerys.negative$Group.1 <- factor(daenerys.negative$Group.1,
                                    levels = unique(daenerys.negative$Group.1))

ggplot(daenerys.negative[1015:1024,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent negative\nwords in 'Daenerys Targaryen'\nchapters", 
       x="Frequency", y="Word",  caption="Figure 4.9")

daenerys.positive<-daenerys.sentiment[which(daenerys.sentiment$sentiment=="positive"),]
daenerys.positive<-aggregate(daenerys.positive$count, list(daenerys.positive$word), sum)
daenerys.positive<-daenerys.positive[order(daenerys.positive$x, decreasing=F),]
daenerys.positive<-as.data.frame(daenerys.positive)
daenerys.positive$Group.1 <- factor(daenerys.positive$Group.1,
                                    levels = unique(daenerys.positive$Group.1))

ggplot(daenerys.positive[481:490,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='firebrick4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent positive\nwords in 'Daenerys Targaryen'\nchapters", 
       x="Frequency", y="Word", caption="Figure 4.10")






##### JON ####
##### Sentimental Analysis #####

joy.words.jon <- inner_join(jon.tidy,nrc.joy, by="word")
count.joy.words.jon <- count(joy.words.jon, word)

jon.sentiment <- inner_join(jon.tidy,bing, by="word")
jon.sentiment <- count(jon.sentiment,sentiment, index=chapter_number)
jon.sentiment <- spread(jon.sentiment, sentiment, n, fill = 0)

jon.sentiment$polarity <- jon.sentiment$positive -  jon.sentiment$negative
jon.sentiment$pos <- ifelse(jon.sentiment$polarity >= 0, "pos", "neg")

ggplot(jon.sentiment, aes(x=index, y=polarity, fill=pos)) +
  geom_bar(stat="identity", position="identity",width=0.91, fill="midnightblue") + 
  ggtitle("Polarity in 'Jon Snow' chapters using 'bing' lexicon") +
  labs(x="Narrative Time", y="Sentiment", caption="Figure 4.11",
       subtitle = "The vertical lines represent the end of each book.")+
  theme_grey()+
  geom_vline(xintercept = c(9,17,29))


jon.sentiment <- inner_join(jon.tidy,bing, by="word")
jon.negative<-jon.sentiment[which(jon.sentiment$sentiment=="negative"),]
jon.negative<-aggregate(jon.negative$count, list(jon.negative$word), sum)
jon.negative<-jon.negative[order(jon.negative$x, decreasing=F),]
jon.negative<-as.data.frame(jon.negative)
jon.negative$Group.1 <- factor(jon.negative$Group.1,
                               levels = unique(jon.negative$Group.1))

ggplot(jon.negative[1097:1106,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent negative\nwords in 'Jon Snow'\nchapters", 
       x="Frequency", y="Word",  caption="Figure 4.12")

jon.positive<-jon.sentiment[which(jon.sentiment$sentiment=="positive"),]
jon.positive<-aggregate(jon.positive$count, list(jon.positive$word), sum)
jon.positive<-jon.positive[order(jon.positive$x, decreasing=F),]
jon.positive<-as.data.frame(jon.positive)
jon.positive$Group.1 <- factor(jon.positive$Group.1,
                               levels = unique(jon.positive$Group.1))

ggplot(jon.positive[428:437,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='firebrick4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent positive\nwords in 'Jon Snow'\nchapters", 
       x="Frequency", y="Word", caption="Figure 4.13")







##### TYRION ####
##### Sentimental Analysis #####

joy.words.tyrion <- inner_join(tyrion.tidy,nrc.joy, by="word")
count.joy.words.tyrion <- count(joy.words.tyrion, word)

tyrion.sentiment <- inner_join(tyrion.tidy,bing, by="word")
tyrion.sentiment <- count(tyrion.sentiment,sentiment, index=chapter_number)
tyrion.sentiment <- spread(tyrion.sentiment, sentiment, n, fill = 0)

tyrion.sentiment$polarity <- tyrion.sentiment$positive -  tyrion.sentiment$negative
tyrion.sentiment$pos <- ifelse(tyrion.sentiment$polarity >= 0, "pos", "neg")

ggplot(tyrion.sentiment, aes(x=index, y=polarity, fill=pos)) +
  geom_bar(stat="identity", position="identity",width=0.91, fill="midnightblue") + 
  ggtitle("Polarity in 'Tyrion Lannister' chapters using 'bing' lexicon") +
  labs(x="Narrative Time", y="Sentiment", caption="Figure 4.14",
       subtitle = "The vertical lines represent the end of each book.")+
  theme_grey()+
  geom_vline(xintercept = c(9,24,35))

tyrion.sentiment <- inner_join(tyrion.tidy,bing, by="word")
tyrion.negative<-tyrion.sentiment[which(tyrion.sentiment$sentiment=="negative"),]
tyrion.negative<-aggregate(tyrion.negative$count, list(tyrion.negative$word), sum)
tyrion.negative<-tyrion.negative[order(tyrion.negative$x, decreasing=F),]
tyrion.negative<-as.data.frame(tyrion.negative)
tyrion.negative$Group.1 <- factor(tyrion.negative$Group.1,
                                  levels = unique(tyrion.negative$Group.1))

ggplot(tyrion.negative[1432:1441,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='slateblue4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent negative\nwords in 'Tyrion Lannister'\nchapters", 
       x="Frequency", y="Word",  caption="Figure 4.15")

tyrion.positive<-tyrion.sentiment[which(tyrion.sentiment$sentiment=="positive"),]
tyrion.positive<-aggregate(tyrion.positive$count, list(tyrion.positive$word), sum)
tyrion.positive<-tyrion.positive[order(tyrion.positive$x, decreasing=F),]
tyrion.positive<-as.data.frame(tyrion.positive)
tyrion.positive$Group.1 <- factor(tyrion.positive$Group.1,
                                  levels = unique(tyrion.positive$Group.1))

ggplot(tyrion.positive[613:622,], aes(x = Group.1, y = x)) +
  geom_bar(stat="identity", fill='firebrick4') +
  coord_flip() +
  geom_text(aes(label = x), colour = "white",
            hjust = 1.25, size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Most frequent positive\nwords in 'Tyrion Lannister'\nchapters", 
       x="Frequency", y="Word", caption="Figure 4.16")


#############
