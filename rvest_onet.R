source("functions.R")

load_pkg("rvest")
load_pkg("stringi")

### TEST
### LinkToArcitle <- "http://biznes.onet.pl/wiadomosci/medycyna/b-szydlo-proponuje-likwidacje-nfz-w-sluzbie-zdrowia-nie-moze-przede-wszystkim-liczyc/ym44d8"

# Funkcja zwraca ramkę danych z komentarzami do atykułu znajdującego się pod adresem LinkToArcitle 
getComments.Onet <- function(LinkToArcitle) {
    
    PrefixLink <- stri_sub(LinkToArcitle, 1, stri_locate_first_regex(LinkToArcitle, ".pl/")[2] - 1) # adres serwisu na którym znajdują się komentarze
    Articles.HTML <- html(LinkToArcitle) ### TODO: może lista serwisów Onet ???
    
    ### TODO: co gdy nie ma komentarzy?
    ### TODO: co gdy komentarzy jest mniej niż 25?
    Comments.HTML <- paste0(PrefixLink, html_attr(html_nodes(ArticleHTML, ".k_makeComment"), "href"))
    
    Comments.HTML.Current <- Comments.HTML
    Results <- data.frame()
    i_security <- 0
    Session <- html_session(Comments.HTML.Current)
    
    repeat {
        autor <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_locked") %>% html_text()
        parent.autor <- html_nodes(s, ".k_nForum_ReaderContentFrame") %>% html_node(".k_parentAuthor") %>% html_text()
        komentarz <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_content") %>% html_text()
        czas <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_nForum_CommentInfo") %>% html_node("span") %>% html_text()
        ocena <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipUpPercent") %>% html_text()
        ocena.liczba <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipCount") %>% html_text()
        
        Results.Partial <- as.data.frame(cbind(autor, komentarz, czas, ocena, ocena.liczba))
        Results <- rbind(Results, Results.Partial)
        
        if (length(html_attr(html_nodes(Session, ".k_makeComment"), "href")) == 2 && i_security > 0) {
            break
        }
        
        Comments.HTML.Current <- paste0(PrefixLink, html_attr(html_nodes(Session, ".k_makeComment"), "href"))[[2]]
        Session <- jump_to(Session, Comments.HTML.Current)
        i_security <- i_security + 1
        
        ### TODO: może jakiesz oszacowanie czasu? Można ze strony głównej sczytać liczbę komentarzy
    }
    
    ### TODO: wstępne czyszczenie danych
    # 1 wszystkie wartości z podwójnych spacji, znacznikóW \r \n \t, spacji przed i po wpisie
    # 2 przekonwertować czas w oparciu o Sys.Date()
    
    return(Results)
}

### TEST
Wyniki <- getComments.Onet("http://biznes.onet.pl/wiadomosci/medycyna/b-szydlo-proponuje-likwidacje-nfz-w-sluzbie-zdrowia-nie-moze-przede-wszystkim-liczyc/ym44d8")
write.csv(Wyniki, "przyklad1.csv")
