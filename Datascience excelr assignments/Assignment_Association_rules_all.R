##Association rules
library(readxl)
library(caret)
library(arules)
library(arulesViz)

#data set books

books <- read.csv('D:/Datasets/books.csv')

dim(books)
head(books)
colnames(books)
table(is.na(books)) # 0 NaNs 

rules<-apriori(books,parameter = list(con=0.95,supp=0.95,minlen=4),control =list(verbose=F))
arules::inspect(rules)

windows()
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

rules.sorted<-sort(rules,by="lift")
arules::inspect(rules.sorted)


df<-data.frame(lhs=labels(lhs(rules.sorted)),rhs=labels(rhs(rules.sorted)),rules.sorted@quality)
head(df)
###################################################################

## data set groceries
groceries <- read.transactions(file.choose(),format="basket")

dim(groceries)
head(groceries)

inspect(groceries[1:10])

# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 

itemFrequencyPlot(groceries,topN=20)

rules_groceries<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

arules::inspect(rules_groceries)

rules.sorted_groceries<-sort(rules_groceries,by="lift")
arules::inspect(rules.sorted_groceries)

df_1<-data.frame(lhs=labels(lhs(rules.sorted_groceries)),rhs=labels(rhs(rules.sorted_groceries)),rules.sorted_groceries@quality)
head(df_1)

window()
plot(rules_groceries,method = "scatterplot")
plot(rules_groceries,method = "grouped")
plot(rules_groceries,method = "graph")
plot(rules_groceries,method = "mosaic")


######################################################

## data set movies 

movies <-read.csv(file.choose())
dim(movies)
head(movies)
colnames(movies)
movie_df <- movies[-c(1:5)]

table(is.na(movie_df)) # 0 NaNs 

rules_movies<-apriori(movie_df,parameter = list(con=0.03,supp=0.07,minlen=2),control =list(verbose=F))
arules::inspect(rules_movies)

plot(rules_movies,method='graph')

rules.sorted<-sort(rules_movies,by="lift")
arules::inspect(rules.sorted)


df_movies<-data.frame(lhs=labels(lhs(rules.sorted)),rhs=labels(rhs(rules.sorted)),rules.sorted@quality)
head(df_movies)

