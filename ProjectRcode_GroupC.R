library('data.table')     #Crates a table on line 12  (as.data.table)
library('readr')          #Read flat/tabular Reviews files from disk
library('wordcloud')      #Use for visual respresentation (wordcloud)
library('tm')             #Use for temming,  stopword removal,et cetera  (tm_map)
library('ggplot2')        #Use for visual representation like bar charts
library('RSentiment')     #Analyses sentiment of a sentence in English and assigns score to it. 

#Load files
phones <- read.csv("C:/Users/tejas/Desktop/NJIT/Web Mining/Project/updates/MobileReviewsData.csv",stringsAsFactors = F)
positiveW <- read.table("C:/Users/tejas/Desktop/NJIT/Web Mining/Project/positive-words.txt", header = FALSE)
negativeW <- read.table("C:/Users/tejas/Desktop/NJIT/Web Mining/Project/negative-words.txt", header = FALSE)
phones$brand <- as.factor(phones$brand)
phones <- as.data.table(phones)



###part 1 cleaning data.
# cleaning the Apple's data
ApplePhone <- phones$Reviews[phones$brand=="Apple"]      #Isolate Apple's lines
corpApple = Corpus(VectorSource(list(ApplePhone)))         #Creates a corpus
corpApple = tm_map(corpApple, content_transformer(tolower))
corpApple = tm_map(corpApple, removeWords, stopwords("english"))
corpApple = tm_map(corpApple, removePunctuation)
corpApple = tm_map(corpApple, removeNumbers) 
#corpApple <- tm_map(corpApple, PlainTextDocument)  
finalApple<-data.frame(text=sapply(corpApple, identity), stringsAsFactors=FALSE) #converts corpus back to dataframe

# cleaning the Samsung's data
SmsungPhone <- phones$Reviews[phones$brand=="Samsung"]      #Isolate Samsung's lines
corpSamsung = Corpus(VectorSource(list(SmsungPhone)))         #Creates a corpus
corpSamsung = tm_map(corpSamsung, content_transformer(tolower))
corpSamsung = tm_map(corpSamsung, removeWords, stopwords("english"))
corpSamsung = tm_map(corpSamsung, removePunctuation)
corpSamsung = tm_map(corpSamsung, removeNumbers) 
#corpSamsung <- tm_map(corpSamsung, PlainTextDocument)  
finalSamsung<-data.frame(text=sapply(corpSamsung,identity), stringsAsFactors=F)
#end of cleaning


#wdDF is a list of negative and positve words combined plus a sentiment score next of each words
wdDF<- data.frame(words = positiveW, value = 1,stringsAsFactors=FALSE) 
wdDF[]<-lapply(wdDF,as.character)
wdDF<- rbind(wdDF,data.frame(words = negativeW, value = -1))
wdDF$lengths<-unlist(lapply(wdDF$V1, nchar))
wdDF<-wdDF[ order(-wdDF[,3]),]
wdDF[,2] <- as.numeric(as.character(wdDF[,2]))  # converts the value of column[2] to num



#This function calculates the sentiment 
assignscore <- function(sentence){
  score<-0
  for(x in 1:nrow(wdDF)){
    count<-length(grep(wdDF[x,1],sentence))
    if(count){
      score<-score + (count * wdDF[x,2])
      sentence<-sub(wdDF[x,1],'',sentence)
    }
  }
  score
}
#End of function

#Apples's sentiment
AppleSentiment_score<- (lapply(finalApple$text, assignscore))

AppleSentiment <- data.frame(cbind(finalApple, AppleSentiment_score)) 
View(AppleSentiment)  # DF of Sentiment analysis of Apple's lines 

PosSent<- with(AppleSentiment, c(sum(AppleSentiment_score <= -1)))
NegSent<- with(AppleSentiment, c(sum(AppleSentiment_score >= 1)))
Neutral<- with(AppleSentiment, c(sum(AppleSentiment_score == 0)))
TotalSent<-as.data.frame(rbind(PosSent,NegSent,Neutral))
TotalSent$sentiment <- c("Positive", "Negative", "Neutral")
ggplot(TotalSent,aes(x = sentiment,y = V1, fill=sentiment))+ geom_bar(stat = "identity")+ ggtitle("Apple's sentiment score count")


#Samsung's sentiemnt
SamSentiment_score<- unlist(lapply(finalSamsung$text, assignscore))

SamSentiment <- data.frame(cbind(finalSamsung, SamSentiment_score))
View(SamSentiment)  # DF of Sentiment analysis of Samsung's lines 

PosSent<- with(SamSentiment, c(sum(SamSentiment_score <= -1)))
NegSent<- with(SamSentiment, c(sum(SamSentiment_score >= 1)))
Neutral<- with(SamSentiment, c(sum(SamSentiment_score == 0)))
TotalSent<-as.data.frame(rbind(PosSent,NegSent,Neutral))
TotalSent$sentiment <- c("Positive", "Negative", "Neutral")
ggplot(TotalSent,aes(x = sentiment,y = V1, fill=sentiment))+ geom_bar(stat = "identity")+ ggtitle("Samsung's sentiment score count")


###Part 2 Number of reviews for Samsung and Apple  

brand_partipation <- phones[brand%in%c("Apple","Samsung"),.N,by=brand][order(-N)]
ggplot(brand_partipation,aes(x = brand,y = N, fill=brand))+ geom_bar(stat = "identity")+ ggtitle("Number of reviews for Samsung and Apple")
### END OF Part 2


###Part 3 No.of Apple's positive and negative sentiments 
ApplePhone_matrix = DocumentTermMatrix(VCorpus(VectorSource(corpApple[[1]]$content)))
freq_ApplePhone <- colSums(as.matrix(ApplePhone_matrix))
ApplePhone_stmts = calculate_sentiment(names(freq_ApplePhone))
ApplePhone_stmts = cbind(ApplePhone_stmts, as.data.frame(freq_ApplePhone))
sent_pos_Apple = ApplePhone_stmts[ApplePhone_stmts$sentiment == 'Positive',] #We use library('RSentiment')      
sent_neg_Apple = ApplePhone_stmts[ApplePhone_stmts$sentiment == 'Negative',] #We use library('RSentiment')  
cat("Apples negative Sentiments: ",sum(sent_neg_Apple$freq_ApplePhone)," and positive: ",sum(sent_pos_Apple$freq_ApplePhone))

wordcloud(sent_neg_Apple$text,sent_neg_Apple$freq, min.freq=5,colors=brewer.pal(6,"Dark2")) #Most freq Negative words
warnings()
wordcloud(sent_pos_Apple$text,sent_pos_Apple$freq, min.freq=5,colors=brewer.pal(5,"Dark2")) #Most freq Positive words 


# No.of Samsung's positive and negative sentiments 
SmsungPhone_matrix = DocumentTermMatrix(VCorpus(VectorSource(corpSamsung[[1]]$content)))
freq_SmsungPhone <- colSums(as.matrix(SmsungPhone_matrix))
SmsungPhone_stmts = calculate_sentiment(names(freq_SmsungPhone))
SmsungPhone_stmts = cbind(SmsungPhone_stmts, as.data.frame(freq_SmsungPhone))
sent_pos_Samsung = SmsungPhone_stmts[SmsungPhone_stmts$sentiment == 'Positive',] #We use library('RSentiment')      
sent_neg_Samsung = SmsungPhone_stmts[SmsungPhone_stmts$sentiment == 'Negative',] #We use library('RSentiment')
cat("Samsungs negative Sentiments: ",sum(sent_neg_Samsung$freq_SmsungPhone)," and positive: ",sum(sent_pos_Samsung$freq_SmsungPhone))

wordcloud(sent_neg_Samsung$text,sent_neg_Samsung$freq, min.freq=5,colors=brewer.pal(8,"Dark2")) #Most freq Negative words
wordcloud(sent_pos_Samsung$text,sent_pos_Samsung$freq, min.freq=5,colors=brewer.pal(4,"Dark2"))
###END OF CODE


