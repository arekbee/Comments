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
    Results <- data.frame()
    
    if (NumberOfComments <= 25) {
        return(Results) ### rozwiązanie tymczasowe 
    }
    
        
    Comments.HTML.Current <- Comments.HTML
    i <- 0
    Session <- html_session(Comments.HTML.Current)
    
    # Szacowanie postępu
    ProgressBar <- txtProgressBar(min = 0, max = NumberOfComments, style = 3)
    repeat {
        Author <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_author") %>% html_text()
        Comment <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_content") %>% html_text()
        Time <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_nForum_CommentInfo") %>% html_node("span") %>% html_text()
        NumberOfVotes <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipCount") %>% html_text()
        Rate.Positive <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipUpPercent") %>% html_text()
        Rate.Negative <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_MarkTipDownPercent") %>% html_text()

        Author.Parent <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_node(".k_parentAuthor") %>% html_text()
        
        ### TODO: niestety niepoprawnie to się wczytuje z uwagi na to, że pojawia się czasami dodatkowy element <a href> </>
        NumberOfResponses <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_CommentInfo") %>% html_node("a") %>% html_text("href")
        LinkToResponses <- html_nodes(Session, ".k_nForum_ReaderContentFrame") %>% html_nodes(".k_nForum_CommentInfo") %>% html_node("a") %>% html_attr("href")
        
        Results.Partial <- as.data.frame(cbind(Author, Comment, Time, NumberOfVotes, Rate.Positive, Rate.Negative, 
                                               Author.Parent, NumberOfResponses, LinkToResponses))
        Results <- rbind(Results, Results.Partial)
        
        ### TODO: może jakiesz oszacowanie czasu? Można ze strony głównej sczytać liczbę komentarzy
        setTxtProgressBar(ProgressBar, i) #test
        
        if (length(html_attr(html_nodes(Session, ".k_makeComment"), "href")) == 2 && i > 0) {
            setTxtProgressBar(ProgressBar, NumberOfComments) #test
            break
        }
        ### Zabezpieczenie gdyby coś nie tak było z przełączaniem kolejnych stron
        if (i > NumberOfComments + 100) {
            break
        }
        
        Comments.HTML.Current <- paste0(PrefixLink, html_attr(html_nodes(Session, ".k_makeComment"), "href"))[[2]]
        Session <- jump_to(Session, Comments.HTML.Current)
        i <- i + 25
    }
    
    ### TODO: wstępne czyszczenie danych
    # 1 wszystkie wartości ze znacznikóW \r \n \t, spacji przed i po wpisie oraz podwójnych spacji ### TODO: usprawnić wyrażeniami regularnymi
    Results <- apply(Results, 2, function (x) {stri_replace_all_fixed(x, "\t", " ")})
    Results <- apply(Results, 2, function (x) {stri_replace_all_fixed(x, "\r", " ")})
    Results <- apply(Results, 2, function (x) {stri_replace_all_fixed(x, "\n", " ")})
    #Results <- apply(Results, 2, function (x) {stri_replace_all_fixed(x, "\"", "'")})
    Results <- apply(Results, 2, function (x) {stri_replace_all_fixed(x, "  ", " ")})
    Results <- apply(Results, 2, function (x) {stri_trim(x)})
    # 2 przekonwertować czas w oparciu o Sys.Date()
    
    return(Results)
}

### TESTY - testowane działy

Wyniki1 <- getComments.Onet("http://biznes.onet.pl/wiadomosci/medycyna/b-szydlo-proponuje-likwidacje-nfz-w-sluzbie-zdrowia-nie-moze-przede-wszystkim-liczyc/ym44d8")
write.csv2(Wyniki1, "onet_biznes_example1.csv")
Wyniki2 <- getComments.Onet("http://wiadomosci.onet.pl/wroclaw/stowarzyszenie-ktore-broni-romow-odpowiada-na-oswiadczenie-prezydenta-wroclawia/dfk9ly")
write.csv2(Wyniki2, "onet_wiadomosci_example1.csv")
Wyniki3 <- getComments.Onet("http://kobieta.onet.pl/dziecko/plotki/pierwsze-dziecko-w-wieku-40-lat-i-wiecej/jzge0")
write.csv2(Wyniki3, "onet_kobieta_example1.csv")
Wyniki4 <- getComments.Onet("http://muzyka.onet.pl/koncerty/przystanek-woodstock-kto-finansuje-festiwal/k87pcp")
write.csv2(Wyniki4, "onet_muzyka_example1.csv")
