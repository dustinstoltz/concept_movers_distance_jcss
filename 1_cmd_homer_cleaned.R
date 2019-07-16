##-----------------------------------------------------------------------------
# LOAD DATA
##-----------------------------------------------------------------------------
    
    # Download from Project GUTENBERG 

    iliad <- gutenberg_download(c(6130),
                        meta_fields = "title") %>%
                        group_by(title) %>%
                        summarise(text = paste(text, collapse=", ") ) %>%
                        mutate(text = sub("^.*(THE CONTENTION OF ACHILLES AND AGAMEMNON.*)","\\1",text) ) %>%
                        mutate(text = sub("(^.*)END OF THE ILLIAD.*","\\1",text) )

    odyssey <- gutenberg_download(c(1727),
                        meta_fields = "title") %>%
                        group_by(title) %>%
                        summarise(text = paste(text, collapse=", ") ) %>%
                        mutate(text = sub("^.*(THE GODS IN COUNCIL.*)","\\1",text) ) %>%
                        mutate(text = sub("(^.*) FOOTNOTES.*","\\1",text) )


## ----------------------------------------------------------------------------
# Parse into Chapters (Books)
## ----------------------------------------------------------------------------

    il.chapters <- strsplit(iliad$text, "BOOK")[[1]]
    # length(il.chapters) ## Should be 24
    iliad.chapters <- tibble::tibble(
                        chapter = 1:length(il.chapters),
                        text = il.chapters
                        ) %>%
                        add_column(title = "Iliad") %>%
                        mutate(c_title = paste("Book", chapter, sep=" "),
                               id = paste("Iliad", c_title, sep=" ") )
    ##
    od.chapters <- strsplit(odyssey$text, "Book ")[[1]]
    # length(od.chapters) ## Should be 24
    odyssey.chapters <- tibble::tibble(
                        chapter = 1:length(od.chapters),
                        text = od.chapters
                        ) %>%
                        add_column(title = "Odyssey") %>%
                        mutate(c_title = paste("Book", chapter, sep=" "),
                               id = paste("Odyssey", c_title, sep=" ") )
    #
    df.homer <- rbind(iliad.chapters, odyssey.chapters)

## ----------------------------------------------------------------------------
# Create DTMs
## ----------------------------------------------------------------------------
    dtm.homer <- df.homer %>% 
            mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
            mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
            mutate(text = replace_white(text)) %>%
            mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
            mutate(text = replace_number(text)) %>%
            unnest_tokens(word, text, to_lower = TRUE) %>%
            anti_join(stop_words) %>%
            filter(!str_detect(word, "[0-9]+") ) %>%
            dplyr::count(id, word) %>%
            bind_tf_idf(word, id, n) %>% 
            cast_dtm(term = word, document = id, 
                     value = n, weighting = tm::weightTf) %>%
                    removeSparseTerms(.99)
    # dim(dtm.homer) # 48 chapters by 11,215 terms (@ 99%)

    ## ------------------------------------------------------------------------

##-----------------------------------------------------------------------------
#  Get CMD
##-----------------------------------------------------------------------------

        cmd.homer <- CMDist(dtm.homer, cw =c("thought", "action"))

        #
        df1 <- left_join(df.homer, cmd.homer, by=c("id" = "docs"))
        df1$row <- as.numeric(rownames(df1))
        #
        # dim(df1)
##-----------------------------------------------------------------------------
# PLOT
##-----------------------------------------------------------------------------

    p1 <- df1 %>%
            ggplot(aes(x=factor(id, levels=id[order(row)] ), y= thought, fill = title) ) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~title, ncol = 2, scales = "free_x") + 
                labs(y = "Closeness to 'Thought'", x = "") +
                geom_text_repel(data = . %>% 
                                mutate(label = ifelse(title=="Iliad" & thought > 0,
                                        c_title, "") ),
                                aes(label = label), nudge_y=0.5 ) +
                theme(axis.text.x=element_blank(),
                      axis.ticks.x=element_blank() )
    
    p2 <- df1 %>%
            ggplot(aes(x=factor(id, levels=id[order(row)] ), y= action, fill = title) ) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~title, ncol = 2, scales = "free_x") + 
                labs(y = "Closeness to 'Action'", x = "") +
                theme(
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank() )
    
    png("Figure_barchart_thought_action_homer.png", 
        width = 9, height = 9, units = 'in', res = 900)
        ggarrange(p1, p2, ncol=1, nrow=2)
    dev.off()

#------------------------------------------------------------------------------

### END ###