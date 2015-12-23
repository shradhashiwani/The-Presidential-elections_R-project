library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tm)


# import positive and negative words
projectDir = getwd()
inputDir = file.path(projectDir, 'input')
positive_words = readLines(file.path(inputDir, 'positive_words.txt'))
negative_words =  readLines(file.path(inputDir, 'negative_words.txt'))

#declaring path of output folder
outputDir = file.path(projectDir, 'output')


#connecting to twitter
#please try to generate your auth using your twitter account
##<<<IMPORTANT>>##
#the code will not work without this
consumer_key <- 'thisisdummy-please generate it'
consumer_secret <- 'thisisdummy-please generate it'
access_token <- 'thisisdummy-please generate it'
access_secret <-'thisisdummy-please generate it'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# tweets with candidates name in list 
DonaldTrump_tweets = searchTwitter('@realDonaldTrump',lang="en", n=500, resultType = "recent")
HillaryClinton_tweets = searchTwitter('@HillaryClinton',lang="en", n=500, resultType = "recent")

#saving result to tweet
save(DonaldTrump_tweets,file=file.path(outputDir, 'DonaldTrump_tweets.RData'), ascii=T)
save(HillaryClinton_tweets,file=file.path(outputDir, 'HillaryClinton_tweets.RData'), ascii=T)
#save(DonaldTrump_tweets, file=file.path("C:/Users/Shradha Shiwani/Desktop/The Presidential elections_R project/output", 'DonaldTrump_tweets.RData' ), ascii=T)
#save(HillaryClinton_tweets, file=file.path("C:/Users/Shradha Shiwani/Desktop/The Presidential elections_R project/output", 'HillaryClinton_tweets.RData' ), ascii=T)

# converting list to character array
Hillary_Democrat_txt = sapply(HillaryClinton_tweets, function(x) x$getText())
Donald_Republican_txt = sapply(DonaldTrump_tweets, function(x) x$getText())

#create word cloud for candidates and export image: start
#hillarycliton
{
#clean hillary's tweet
Hillary_corpus <- Corpus(VectorSource(Hillary_Democrat_txt ))
Hillary_clean <- tm_map(Hillary_corpus, removePunctuation)
Hillary_clean <- tm_map(Hillary_clean, removeNumbers)
Hillary_clean <- tm_map(Hillary_clean, stripWhitespace)
#Hillary_clean <- tm_map(Hillary_clean, removeWords, stopwords("english"))
# add extra stop words
myStopwords <- c(stopwords('english'), "she","hillary", "will","has","they", "we","who","how","they","because", "should",
                 "and","the","for","our","but","are","list", "hillaryclinton", "clinton")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
Hillary_clean <- tm_map(Hillary_clean, removeWords, myStopwords)
#remove urls
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
Hillary_clean <- tm_map(Hillary_clean, content_transformer(removeURL))
#calculating frequency of the words
dtm <- TermDocumentMatrix(Hillary_clean,
                          control = list(removePunctuation = TRUE,
                                         stopwords = c("hillaryclinton", "realdonaldtrump", stopwords("english")),
                                         removeNumbers = TRUE, tolower = TRUE))
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#generating word cloud
wordcloud(d$word,d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          random.color=TRUE,colors=rainbow(7), scale = c(2, 0.2))

# save the word cloud in png format
#png(file=file.path("C:/Users/Shradha Shiwani/Desktop/The Presidential elections_R project/output", 'Hillary_wordcloud.png' ), width=12, height=8, units="in", res=300)
png(file=file.path(outputDir, 'Hillary_wordcloud.png' ), width=12, height=8, units="in", res=300)

wordcloud(d$word, d$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
}


#donaldtrumph
{
#clean donald's tweet
Donald_corpus <- Corpus(VectorSource(Donald_Republican_txt ))
Donald_clean <- tm_map(Donald_corpus, removePunctuation)
Donald_clean <- tm_map(Donald_clean, removeNumbers)
Donald_clean <- tm_map(Donald_clean, stripWhitespace)
Donald_clean <- tm_map(Donald_clean, removeWords, stopwords("english"))
# add extra stop words
Donald_clean <- tm_map(Donald_clean, removeWords, c( "realdonaldtrump","hillaryclinton", "donald","he","she","hillary", "will","has","they", "we","who","how","they","because", "should",
                                                     "and","the","for","our","but","are","list", "clinton" ))
#remove urls
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
Donald_clean <- tm_map(Donald_clean, content_transformer(removeURL))
#calculating frequency of the words
dtm <- TermDocumentMatrix(Donald_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#generating word cloud
wordcloud(d$word,d$freq, min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.15, 
          random.color=TRUE,colors=rainbow(7), scale = c(3, 0.2))

# save the word cloud in png format
#png(file=file.path("C:/Users/Shradha Shiwani/Desktop/The Presidential elections_R project/output", 'Donald_wordcloud.png' ), width=12, height=8, units="in", res=300)
png(file=file.path(outputDir, 'Donald_wordcloud.png' ), width=12, height=8, units="in", res=300)

wordcloud(d$word, d$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
}

#create word cloud for candidates and export image: end


# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#plotting individual graph of the candidates: start
{
donald.scores = score.sentiment(Donald_Republican_txt, positive_words, negative_words, .progress='text')
hillary.scores = score.sentiment(Hillary_Democrat_txt, positive_words, negative_words, .progress='text')

donald.scores$Presidential_Candidate = 'Donald Trumph'
donald.scores$code = 'DT'
hillary.scores$Presidential_Candidate = 'Hillary Clinton'
hillary.scores$code = 'HC'

all.scores = rbind( donald.scores, hillary.scores )

# ggplot works on data.frames, always
g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=Presidential_Candidate) )

# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_bar( binwidth=1 )

# make a separate plot for each candidate
g.hist = g.hist + facet_grid(Presidential_Candidate~.)

# plain display, nice colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() 

print(g.hist)
ggsave(file.path(outputDir, 'twitter_score_histograms.pdf'), g.hist, width=6, height=5.5)
}
#plotting individual graph of the candidates: end


#graph for comparing tweets about hillary and donald: start
{
# join tweet txts
all_Presidential_Candidates = c(Donald_Republican_txt, Hillary_Democrat_txt) 

nd = c(length(Donald_Republican_txt), length(Hillary_Democrat_txt))

# apply function score.sentiment
#donald.score = score.sentiment(Donald_Republican_txt, positive_words, negative_words, .progress='text')
#hillary.score = score.sentiment(Hillary_Democrat_txt, positive_words, negative_words, .progress='text')
all_Presidential_Candidates.score = score.sentiment(all_Presidential_Candidates, positive_words, negative_words, .progress='text')
#all_Presidential_Candidates.scores = rbind( donald.score, hillary.score )


# add variables to data frame
all_Presidential_Candidates.score$Presidential_Candidate = factor(rep(c("Donald Trumph", "Hillary Clinton"), nd))
all_Presidential_Candidates.score$pos = as.numeric(all_Presidential_Candidates.score$score >= 2)
all_Presidential_Candidates.score$neg = as.numeric(all_Presidential_Candidates.score$score <= -2)


# how many very positives and very negatives
numpos = sum(all_Presidential_Candidates.score$pos)
numneg = sum(all_Presidential_Candidates.score$neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )

# colors
cols =c("pink", "blue")
names(cols) = c("Hillary Clinton", "Donald Trumph")

# boxplot
candidateplot <- ggplot(all_Presidential_Candidates.score, aes(x=Presidential_Candidate, y=score, group=Presidential_Candidate)) +
  geom_boxplot(aes(fill=Presidential_Candidate)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  labs(title = "Boxplot - Candidate's Tweet Scores")

#pdf("C:/Users/Shradha Shiwani/Desktop/The Presidential elections_R project/output/graph4.pdf")
ggsave(file.path(outputDir, 'graph4.pdf'), candidateplot)
dev.off()
}
#graph for comparing tweets about hillary and donald: end


#clear console
cat("\014")

