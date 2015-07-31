load_pkg = function(pkg, cran = TRUE)
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
