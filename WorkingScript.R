library(SciScraper)
#load search term list
#terms<-read.csv("wordlist.csv")
terms<-read.csv("wordlist_full.csv")
m<-length(terms$Seach.Terms)
for(j in 1:m){
    article_list<-get_article_list(terms$Seach.Terms[j])
    check_for_gaps(article_list)
}




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
