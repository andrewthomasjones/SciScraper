library(SciScraper)
#load search term list
#terms<-read.csv("wordlist.csv")
terms<-read.csv("wordlist.csv")
m<-length(terms$Seach.Terms)
B<-100 #chunksize

chunk1 <- function(article_list, i, B, K, QAlist, index2, big_list, j, terms, m) {
    sub_article_list<-article_list
    start<-(i-1)*B+1
    end<-min(i*B,K)
    sub_article_list$articles<-sub_article_list$articles[start:end]
    get_details(sub_article_list, db="SciScraper3")
    QAlist<-runQA(QAlist, sub_article_list, index2[start:end], db="SciScraper3")
    write.csv(QAlist, file = paste0("/home/andrew/Dropbox/Consulting/Systems/Code/",article_list$term ,"_QA.csv"))
    big_list[[j]]$QAlist<-QAlist
    save(big_list,terms, m, B, file='/home/andrew/Dropbox/Consulting/Systems/Code/big_list.RData')
}

big_list<-list()
for(j in 1:m){
    article_list<-get_article_list(terms$Seach.Terms[j])
    K=length(article_list$articles)
    QAlist<-data.frame(url= unlist(article_list$articles))
    QAlist$entry<-NA
    QAlist$date<-NA
    QAlist$body<-NA

    big_list[[j]]<-list(article_list=article_list, K=K,QAlist=QAlist )
}
save(big_list,terms,m, B, file='/home/andrew/Dropbox/Consulting/Systems/Code/big_list.RData')
##########################################################################################

load(file='/home/andrew/Dropbox/Consulting/Systems/Code/big_list.RData')

for(j in 1:m){
     article_list<-big_list[[j]]$article_list
     K<-big_list[[j]]$K
     QAlist<-big_list[[j]]$QAlist

     big_list[[j]]$article_list


     K<-sum(is.na(QAlist$entry))
     n<-ceiling(K/B)
     article_list$articles<-subset(article_list$articles,is.na(QAlist$entry))
     index2<-which(is.na(QAlist$entry))

        for(i in 1:n){
            try(x<-chunk1(article_list, i, B, K, QAlist, index2, big_list, j, terms, m), silent = TRUE)
        }



}



#check_for_gaps(article_list)
#
#
#
#
#
# #details<-get_details(articles)
# for(j in 1:m){
#     articles<-get_article_list(terms$Seach.Terms[j])
#
#     details<-get_details(articles)
#     #save_to_db(details, db="SciScraper1",collection="FullCollection")
# }
#
#
#
