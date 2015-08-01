load_pkg <- function(pkg, cran = TRUE)
{
    if(suppressWarnings(!require(pkg, character.only = TRUE)))
    {
        if(cran)
        {
            install.packages(pkg)  
        } else
        {
            install.packages(pkg)  
            #load_pkg("devtools")
            #install_github("???")
        }
        library(pkg, character.only = TRUE)
    }
}


### TODO: funkcja która przekonwertuje czas wstawienia postu na odpowiedni format, potem już tylko zastosować apply
convert_time <- function(x) {
    if (x == "wczoraj") {
        t <- 2
    } else if (x == "dzisiaj") {
        t <- 1
    } else {
        t <- 0
    }
    
    return(t)
}
