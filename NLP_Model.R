#Natural Language processing

#Importing dataset
# stringasfactors --> to not identify the reviews in the file as factors.
# quote --> to omit if any double quotes are present in the reviews.
dataset_origial = read.delim('Restaurant_Reviews.tsv', quote = '',stringsAsFactors = FALSE)

#Cleaning texts
install.packages('tm')
install.packages('SnowballC')
library(tm)
library(SnowballC)#to incorporate the stopwards() function 
corpus = VCorpus(VectorSource(dataset$Review))#converting the reviews required into corpus
corpus = tm_map(corpus, content_transformer(tolower))#converts all the text to small case.
corpus = tm_map(corpus, removeNumbers)#removes all numbers from reviews columns
corpus = tm_map(corpus, removePunctuation)#removes punctuations
corpus = tm_map(corpus, removeWords,stopwords())# stopwords removes all irrelevant from the files which are not important  for machine learning
corpus = tm_map(corpus, stemDocument)# to select only the root word
corpus = tm_map(corpus, stripWhitespace)# to remove extra spaces in the document

#Creating a Bag of Words model
dtm = removeSparseTerms(DocumentTermMatrix(corpus),0.999)# process of removing non frequent words from the file
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_origial$Liked #converting to the original data frame.
 

