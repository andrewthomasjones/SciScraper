library(mongolite)
library(plyr)
library(stringr)

library(lubridate)

library(ggplot2)

m1<-mongolite::mongo(collection ="System" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
m2<-mongolite::mongo(collection ="systematically" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
m3<-mongolite::mongo(collection ="systemic" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)

test1 <- m1$find(
    query = '{}',
    fields = '{"body" : true, "date" : true , "url" : true}',
)

test2 <- m2$find(
    query = '{}',
    fields = '{"body" : true, "date" : true ,"url" : true}',
)

test3 <- m3$find(
    query = '{}',
    fields = '{"body" : true, "date" : true, "url" : true }',
)

test1$count<-str_count(test1$body,"system")
test2$count<-str_count(test2$body,"systematically")
test3$count<-str_count(test3$body,"systemic")

test1$url[test1$count>0]


sum(test2$url[test2$count>0] %in% test2$url[test2$count>0])
sum(test1$url[test1$count>0] %in% test3$url[test3$count>0])
sum(test3$url[test3$count>0] %in% test2$url[test2$count>0])



grid.newpage()
draw.triple.venn(area1 = 22, area2 = 20, area3 = 13, n12 = 11, n23 = 4, n13 = 5,
                 n123 = 1, category = c("Dog People", "Cat People", "Lizard People"), lty = "blank",
                 fill = c("skyblue", "pink1", "mediumorchid"))


sum(test2$url %in% test3$url)
sum(test3$url %in% test1$url)


intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect # the R function in base is slightly more careful
intersect(1:10, 7:20)

length(intersect(unique(test2$url[test2$count>0]) , unique(test3$url[test3$count>0])))



png("venn.png", width = 480, height = 360)
draw.triple.venn(area1 = 5462, area2 = 1211, area3 = 374, n12 = 474, n23 = 15, n13 = 337,
                 n123 = 15, category = c("System", "Systematic", "Systemic"), lty = "blank",
                 fill = c("blue", "red", "green"), euler.d = T, scaled=T)

dev.off()

test$Date <- as.Date((test$date))
test$Month <- month(test$Date)
test$Year<- year(test$Date)
test$count<-str_count(test$body,"system")

test_systemic<- ddply(test, "Year", summarize, mu = mean(count[count>0], na.rm=T), N = sum(count>0, na.rm=T))
#
# totals2<-data.frame(Year =totals$Year,  System = test_system$N, Systemic =
# test_systemic$N, Systematic = test_systematic$N, Total = totals$Total)
#
#
# write.csv(totals2, "totals2.csv")


allsystemtext<-test$body

dim(allsystemtext)




Systemically_data<-test
which.max(str_count(allsystemtext,"system*"))
hist(str_count(allsystemtext,"system")+str_count(allsystemtext,"System"))

sum(str_count(allsystemtext,"system")==0, na.rm=T)



#write(allsystemtext, file="SystemBodyText2.txt")





#

totals<-read.csv("Counts.csv")

test2$N2 <-test2$N / totals$Total

png("System.png", width = 480, height = 360)
ggplot(data=test2, aes(x=Year, y=N))+ scale_x_continuous(breaks=2007:2017) + geom_line()+theme_bw()+geom_point(size=2, colour ="red")+ggtitle("Articles containing 'System*' at least once in Science") + ylab("Number of articles")
dev.off()

png("System2.png", width = 480, height = 360)
ggplot(data=test2, aes(x=Year, y=mu))+ scale_x_continuous(breaks=2007:2017) + geom_line()+theme_bw()+geom_point(size=2, colour ="red")+ggtitle("Mean occurances of 'System*' in articles where it occurs at least once \n in Science") + ylab("Mean number of Occurances")
dev.off()




(test2$Year,test2$N)
plot(test2$Year,test2$mu)




#
#
#
#
#
#
# m1<-mongolite::mongo(collection ="ComplexSystem" , db = "SciScraper2", url = "mongodb://localhost", verbose = TRUE)
# test <- m1$find(
#     query = '{}',
#     fields = '{"issue" : true, "url" : true}',
# )
# sum(is.na(test$issue))
#
#
#
#
# write(allsystemtext, file="SystemBodyText2.txt")
#
# m1<-mongolite::mongo(collection ="System" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
# test <- m1$find(
#     query = '{}',
#     fields = '{"body" : true}',
# )
# allsystemtext<-test$body
#
#
# allsystemtext = tolower(allsystemtext) #make it lower case
# allsystemtext = gsub('[[:punct:]]', '', allsystemtext) #remove punctuation
#
#
#
# write(allsystemtext, file="SystemBodyText2.txt")
#
# allsystemtext2 = gsub('solar system', '', allsystemtext) #remove phrase
# allsystemtext2 = gsub('immune system', '', allsystemtext2) #remove phrase
#
#
# write(allsystemtext2, file="SystemBodyText_remove_phrase.txt")
#
#
# m1<-mongolite::mongo(collection ="System" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
# test <- m1$find(
#     query = '{}',
#     fields = '{"date" : true , "url" : true, "title" : true}',
# )
#
#
# System_data<-test
#
# m1<-mongolite::mongo(collection ="Systemically" , db = "SciScraper3", url = "mongodb://localhost", verbose = TRUE)
# test <- m1$find(
#     query = '{}',
#     fields = '{"date" : true , "url" : true, "title" : true}',
# )
#
# library(stringr)
#
# Systemically_data<-test
# which.max(str_count(allsystemtext,"system"))
# hist(str_count(allsystemtext2,"system"))
#
