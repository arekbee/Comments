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
    Comments.HTML <- paste0(PrefixLink, html_attr(html_nodes(Articles.HTML, ".k_makeComment"), "href"))
    NumberOfComments <- html_text(html_nodes(Articles.HTML, ".k_nForum_Header2 span")[1]) # czasami jest wektor dwuelementowy (drugi element to liczba dyskusji)
    NumberOfComments <- as.numeric(stri_replace_all_regex(NumberOfComments, "\\(|\\)|[:space:]", ""))
    
    Comments.HTML.Current <- Comments.HTML
    Results <- data.frame()
    i_security <- 0
    Session <- html_session(Comments.HTML.Current)
    
    # Szacowanie postępu
    ProgressBar <- txtProgressBar(min = 0, max = NumberOfComments, style = 3)
    repeat {
        Author <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_author") %>% html_text()
        Author.Parent <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_parentAuthor") %>% html_text()
        Comment <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_content") %>% html_text()
        Time <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_nForum_CommentInfo") %>% html_node("span") %>% html_text()
        NumberOfVotes <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipCount") %>% html_text()
        Rate.Positive <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipUpPercent") %>% html_text()
        Rate.Negative <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipDownPercent") %>% html_text()
        LinkToResponses <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_CommentInfo") %>% html_node("a") %>% html_attr("href")
        
        
        Results.Partial <- as.data.frame(cbind(Author, Author.Parent, Comment, Time, NumberOfVotes,Rate.Positive ,Rate.Negative ,LinkToResponses))
        Results <- rbind(Results, Results.Partial)
        
        ### TODO: może jakiesz oszacowanie czasu? Można ze strony głównej sczytać liczbę komentarzy
        setTxtProgressBar(ProgressBar, i_security) #test
        
        if (length(html_attr(html_nodes(Session, ".k_makeComment"), "href")) == 2 && i_security > 0) {
            setTxtProgressBar(ProgressBar, NumberOfComments) #test
            break
        }
        
        Comments.HTML.Current <- paste0(PrefixLink, html_attr(html_nodes(Session, ".k_makeComment"), "href"))[[2]]
        Session <- jump_to(Session, Comments.HTML.Current)
        i_security <- i_security + 25
        
    }
    
    ### TODO: wstępne czyszczenie danych
    # 1 wszystkie wartości z podwójnych spacji, znacznikóW \r \n \t, spacji przed i po wpisie
    # 2 przekonwertować czas w oparciu o Sys.Date()
    
    return(Results)
}

### TESTY - testowane działy

Wyniki <- getComments.Onet("http://biznes.onet.pl/wiadomosci/medycyna/b-szydlo-proponuje-likwidacje-nfz-w-sluzbie-zdrowia-nie-moze-przede-wszystkim-liczyc/ym44d8")
write.csv(Wyniki, "onet_biznes_example1.csv")
Wyniki <- getComments.Onet("http://wiadomosci.onet.pl/wroclaw/stowarzyszenie-ktore-broni-romow-odpowiada-na-oswiadczenie-prezydenta-wroclawia/dfk9ly")
write.csv(Wyniki, "onet_wiadomosci_example1.csv")
Wyniki <- getComments.Onet("http://kobieta.onet.pl/dziecko/plotki/pierwsze-dziecko-w-wieku-40-lat-i-wiecej/jzge0")
write.csv(Wyniki, "onet_kobieta_example1.csv")
Wyniki <- getComments.Onet("http://muzyka.onet.pl/koncerty/przystanek-woodstock-kto-finansuje-festiwal/k87pcp")
write.csv(Wyniki, "onet_muzyka_example1.csv")
