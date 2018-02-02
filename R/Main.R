get_article_list<-function(term, max=NA, A=2007, B=2017){

  #get list of articles
  articles<-list()
  #term<-"systems"
  for(j in A:B){


    term<-gsub(" ","%2B",term)

    #search
    base_html<-paste0("http://science.sciencemag.org/search/text_abstract_title%3A",
                      term, "%20text_abstract_title_flags%3Amatch-phrase%20limit_from%3A",
          j,"-01-01%20limit_to%3A",j,
          "-12-31%20jcode%3Asci%20numresults%3A100%20sort%3Apublication-date%20direction%3Aascending%20format_result%3Astandard")

    html1<-xml2::read_html(base_html)
    how_many_res<- html1 %>% rvest::html_nodes("h2") %>% rvest::html_text()
    x<-substr(how_many_res[1],1,nchar(how_many_res[1])-9)
    count<-as.numeric(gsub(",","",stringi::stri_extract_last_words(x)))
    #print(count)
    pages<-ceiling(count/100)

    for(i in 0:(pages-1)){

      if(i==0){
        html_temp<-base_html
      }else{
        html_temp<-paste0(base_html, "?page=", i)
      }

      page_temp<-xml2::read_html(html_temp)

      res_temp <- page_temp %>% rvest::html_nodes(".variant-full-text") %>% rvest::html_attr("href")

      articles<-c(articles,res_temp)
      if(!is.na(max)){
          if(length(articles>=max)){
              return(list(term=term, articles=articles))
          }
      }

    }
  }

  return(list(term=term, articles=articles))
}



get_details<-function(articles){

    term<-articles$term
    article_list<-articles$articles

    #data components

    type<-".overline"
    title<-".highwire-cite-title"
    abstract<-"#p-1"
    abstract_rev<-"#p-11"
    date_etc<-".meta-line"
    body_text<-".article__body"
    author<-"#contrib-group-1"
    affliation<-".affiliation-list"
  #get actual stuff
  results_list<-list()

  for(i in 1:length(article_list)){


    url1<-article_list[[i]]

    x1<-xml2::read_html(paste0("http://science.sciencemag.org",url1))

    #x1<-xml2::read_html("http://science.sciencemag.org/content/359/6373/309.full")


    #add chcek is already in data base and add under search term
    #add check type is %in% c("Review", "Report", "Research Article")

    title_text<- x1 %>% rvest::html_node(title) %>% rvest::html_text()
    type_text<-x1 %>% rvest::html_node(type) %>% rvest::html_text()
    print(type_text)
    if(type_text%in% c("Review", "Report", "Research Article")){

      if(type_text=="Review"){
        #if type is Review
        abstract_text<-x1 %>% rvest::html_node(abstract_rev) %>% rvest::html_text()
      }else{
        abstract_text<-x1 %>% rvest::html_node(abstract) %>% rvest::html_text()
        k=2
        while(nchar( abstract_text)< 500){
          abstract_text<-x1 %>% rvest::html_node(paste0("#p-",k)) %>% rvest::html_text()
          k=k+1
        }
      }





      authors<-x1 %>% rvest::html_node(author) %>% rvest::html_text() #needs parsing



      author_split<-strsplit(authors, "\\,")[[1]]

      if(length(author_split)>1){
        author_split2<-subset(author_split, nchar(author_split)>3)

        author_split3<-trimws(gsub('[[:digit:]]+', '', author_split2))
        author_list <- gsub('[[:punct:] ]+',' ',author_split3)

        affliation_text<-x1 %>% rvest::html_node(affliation) %>% rvest::html_text() #needs parsing
        affliations<-strsplit(affliation_text, "\\.")

        affiliations<-gsub("^\\d+", "", affliations[[1]])

        affil_list <- rep(list(NA),length(author_split3))
        names(affil_list) <- author_list

        temp1<-substr(author_split2, nchar(author_split2), nchar(author_split2))

        for(i in 1:length(author_split3)){
          temp2<-temp1[i]
          start1<-which(author_split2[i] == author_split)

          if(i!=length(author_split3)){
            start2<-which(author_split2[i+1] == author_split)
          }else{
            start2<-length(author_split)
          }

          k=start1+1
          while(k<start2){
            temp2<-c(temp2,author_split[k])
            k<-k+1
          }

          temp2<-as.numeric(temp2)
          temp3<-temp2[!is.na(temp2)]

          affil_list[[i]]<-c(temp3)
        }

      }else{
        author_list<-gsub('[[:punct:] ]+',' ',authors)
        affiliations<-affliation_text
        affil_list<-list(1)
      }

      date_etc<-try(x1 %>% rvest::html_node(date_etc) %>% rvest::html_text(), silent = TRUE)

      if(class(date_etc)!="try-error"){
        #date_etc<-x1 %>% rvest::html_node(date_etc) %>% rvest::html_text()#needs parsing
        d1<-gregexpr(pattern ='DOI:',date_etc)
        doi_address<-trimws(substring(date_etc, (d1[[1]]+4) , nchar(date_etc) ))

        v1<-gregexpr(pattern ='Vol.',date_etc)
        volume<-trimws(substring(date_etc, (v1[[1]]+4) , (v1[[1]]+7) ))

        I1<-gregexpr(pattern ='Issue',date_etc)
        issue<-trimws(substring(date_etc, (I1[[1]]+5) , (I1[[1]]+9) ))

        d2<-gregexpr(pattern ='Science',date_etc)
        d3<-gregexpr(pattern =':',date_etc)[[1]]
        date<-as.Date( trimws(gsub("[[:punct:][:blank:]]+", " ", (substring(date_etc, (d2[[1]]+7) , (d3[[1]]-1) )))), "%d %b %Y")
      }else{
        date<-NA
        volume<-NA
        doi_address<-NA
        issue<-NA
      }


      body1<- x1 %>% rvest::html_node(body_text) %>% rvest::html_text() #cut after "References and Notes" or Supplementary Materials
      b1<-gregexpr(pattern ='http://www.sciencemag.org/about/science-licenses-journal-article-reuse',body1)
      b2<-gregexpr(pattern ='Supporting Online Material',body1)
      b3<-gregexpr(pattern ="Supplementary Materials",body1)

      cut_options<-c(b1[[1]], b2[[1]], b3[[1]])
      cut<-min(cut_options[cut_options>0])

      body2<-substring(body1,1, cut-1 )

      body3<-textclean::replace_white(body2)
      body4<-textclean::replace_non_ascii(body3)

      results_list[[i]]<-list(url=url1, term=c(term), title=title_text,  type=type_text, body=body4, abstract=abstract_text, date=date, issue=issue, volume=volume, DOI=doi_address, affil_list= affil_list, author_list=author_list, affiliations=affiliations)
    }
  }
  return(results_list)
}


save_to_db<-function(results_list, db="SciScraper1", collection="FirstRun"){

    #connect to mongo
    m1<-mongolite::mongo(collection = collection, db = db, url = "mongodb://localhost", verbose = TRUE)

    for(i in 1:length(results_list)){

        #do quality check
        if(length(results_list[[i]])>0){
        #check if in DB??
        #convert to JSON
        entry<-jsonlite::toJSON(results_list[[i]], pretty=T, auto_unbox = T)
        m1$insert(entry)
        }
    }

    #close mongo connection

}




