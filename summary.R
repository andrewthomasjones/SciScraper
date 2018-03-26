m1<-mongolite::mongo(collection ="System" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
test <- m1$find(
    query = '{}',
    fields = '{"body" : true}',
)
allsystemtext<-test$body


allsystemtext = tolower(allsystemtext) #make it lower case
allsystemtext = gsub('[[:punct:]]', '', allsystemtext) #remove punctuation



write(allsystemtext, file="SystemBodyText2.txt")

allsystemtext2 = gsub('solar system', '', allsystemtext) #remove phrase
allsystemtext2 = gsub('immune system', '', allsystemtext2) #remove phrase


write(allsystemtext2, file="SystemBodyText_remove_phrase.txt")


m1<-mongolite::mongo(collection ="System" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
test <- m1$find(
    query = '{}',
    fields = '{"date" : true , "url" : true, "title" : true}',
)


System_data<-test

m1<-mongolite::mongo(collection ="Systemically" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
test <- m1$find(
    query = '{}',
    fields = '{"date" : true , "url" : true, "title" : true}',
)

library(stringr)

Systemically_data<-test
which.max(str_count(allsystemtext,"system"))
hist(str_count(allsystemtext2,"system"))

