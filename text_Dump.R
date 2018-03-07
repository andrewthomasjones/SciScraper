m1<-mongolite::mongo(collection ="System" , db = "SciScraper2", url = "mongodb://localhost", verbose = TRUE)
test <- m1$find(
    query = '{}',
    fields = '{"body" : true}',
)
allsystemtext<-test$body


write(allsystemtext, file="SystemBodyText.txt")

