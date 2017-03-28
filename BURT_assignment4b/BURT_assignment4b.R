library(Rfacebook)
library(RCurl)
library(RJSONIO)
library(XML)
library(stringr)
library(tm)
library('ggplot2')


load(file="my.facebook_credentials")
fb.oauth <- fbOAuth(app_id = fb.app.id, app_secret = fb.app.secret, extended_permissions = TRUE)


### Retrieve 100 recent posts from Facebook

page <- getPage("nyrangers", n=100, token = fb.oauth)


### plot frequency of post type
page.category <- table(page$type)
setwd('/home/marcburt/BU DA/Web_Analytics/facebook_sent')
png('category.png')
barplot(page.category, main = 'Frequency of Post Type', xlab = 'Type', ylab = 'Count', col = c('red', 'blue'))
dev.off()
### most liked post

most.liked <- page[which.max(page$likes_count), ]
most.liked$message

### most commented post

most.commented <- page[which.max(page$comments_count), ]
most.commented$message

### get all the comments from most commented

coms <- getPost(most.commented$id, n = 1000, token = fb.oauth)
comments <- coms$comments$message


### clean corpus function 
clean.corpuses <- function(corpus){
	#remove: URL, caps, punct, stop words, numbers, stem everything, whitespace
	#output -> create term document matrix
	cleaned <- tm_map(corpus, content_transformer(function(x) iconv(x, to = 'ASCII', sub="" ))) ##remove non-ASCII
	cleaned <- tm_map(cleaned, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x))) ## remove URL
	cleaned <- tm_map(cleaned, content_transformer(tolower))
	cleaned <- tm_map(cleaned, removePunctuation)
	cleaned <- tm_map(cleaned, removeWords, stopwords("english"))
	cleaned <- tm_map(cleaned, removeNumbers)
	cleaned <- tm_map(cleaned, stemDocument)
	cleaned <- tm_map(cleaned, stripWhitespace)
	tdm <- TermDocumentMatrix(cleaned)
	return(tdm)

}

comments.source <- VectorSource(comments)
comments.corp <- Corpus(comments.source)
comments.tdmc <- clean.corpuses(comments.corp)

word.freq <- function(tdm){
	freq <- rowSums(as.matrix(tdm))
	freq <- sort(freq, decreasing = TRUE)
	return(freq)
}

comments.freq <- word.freq(comments.tdmc)

head(names(comments.freq), 20)
plot(head(comments.freq, 20), type ='hist')

comments.df <- head(data.frame(comments.freq), 20)


png('wordcount.png')
word.count.plot <- ggplot(comments.df, aes(rownames(comments.df), comments.freq, fill = rownames(df))) +
	geom_bar(stat="identity")+
	ggtitle('Top 20 Most Frequent Words for NY Rangers') +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	labs(x = 'Words', y = 'Frequency') +
	scale_fill_discrete(guide=FALSE)
word.count.plot
dev.off()