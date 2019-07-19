##-----------------------------------------------------------------------------
# LOAD PACKAGES
##-----------------------------------------------------------------------------
    # install.packages("pacman")
    # install.packages("devtools")
    # devtools::install_github("lionel-/ggstance")
    # devtools::install_github("quanteda/quanteda.corpora")

    pacman::p_load(text2vec,fasttextM,
                   tm, Matrix,tidytext,
                   textstem,dplyr,
                   tidyverse,textclean,
                   quanteda.corpora,
                   stringr,extrafont,
                   ggplot2,ggrepel,
                   gutenbergr,ggstance,
                   ggpubr,GGally,
                  install=TRUE)

                #   forcats
                #   reshape2


    # for plot aesthetics
    # devtools::install_github("UrbanInstitute/urbnthemes")
        # Mac users will probably need to install the Lato font.
        # After the font is installed and you run the library(urbnthemes)
        # line, run this: urbnthemes::lato_install() 
        # (see https://github.com/UI-Research/urbnthemes). 
    library(urbnthemes)
    set_urbn_defaults()
    
    ## Download the fasttext word embeddings dataset: 
    ## https://fasttext.cc/docs/en/english-vectors.html
    ## we are using the fasttextM package.
    # devtools::install_github("statsmaths/fasttextM") 
    ## This only needs to be done once. We recommend at least 1000mb.
    # ft_download_model("en", mb = 3000)
    ## To load this locally downloaded set run this:
    ft_load_model("en")

    ## load CMD function
    source("1_FINAL_CMD_Function.R") 

##-----------------------------------------------------------------------------

## END ##
