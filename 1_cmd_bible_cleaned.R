##-----------------------------------------------------------------------------
# LOAD DATA
##-----------------------------------------------------------------------------
    meta <- read_csv(file="2_kjv_meta.csv")

    ## download KJV bible books from Project Gutenberg
    bible <- meta %>%
                select(gutenberg_id) %>%
                gutenberg_download(meta_fields = c("title", "gutenberg_id") ) %>%
                group_by(title) %>%
                summarise(text = paste(text, collapse=", "),
                          gutenberg_id = mean(as.numeric(gutenberg_id ) ) ) %>%
                left_join(meta)

##-----------------------------------------------------------------------------
# Prepare TEXT DATA
##-----------------------------------------------------------------------------

    unnest.bible <- bible %>% 
            mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
            mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
            mutate(text = replace_white(text)) %>%
            mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
            mutate(text = replace_number(text))  %>%
            unnest_tokens(word, text, to_lower = TRUE) %>%
            anti_join(stop_words) %>%
            filter(!str_detect(word, "[0-9]+") ) %>%
            count(short_title, word) %>%
            bind_tf_idf(word, short_title, n)

##-----------------------------------------------------------------------------
#  Build DTM, Run CMD Function, MERGE with Metadata
##-----------------------------------------------------------------------------
    bible.close <- unnest.bible %>% 
                    cast_dtm(term = word, 
                                document = short_title, 
                                value = n, 
                                weighting = tm::weightTf) %>%
                            removeSparseTerms(.999) %>%
                    CMDist(cw =c("introspection") ) %>%
                    rename(short_title = "docs") %>%
                    left_join(bible) %>%
                    mutate(name = factor(short_title, 
                           levels=short_title[order(-year)])) %>%
                    as_tibble()

## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
    # FLIPPED Bar Chart
    bars <- bible.close %>%
            ggplot(aes(fill=jaynes, x=introspection, y=name) ) +
                geom_barh( position = "identity", stat="identity", width = 9) +
                geom_text(data=subset(bible.close, labels==1), 
                               aes(x=-2.5, label = circa), 
                                   size = 3, nudge_y=1, family="Arial") +
                xlim(-3, 2.2) +
                labs(x = "Closeness to 'Introspection'", 
                     y = "Books of the KJV Bible, Arranged by Approx. Year") +
                facet_grid(name~., space="free", scales="free_y") +
                theme(strip.text.y = element_text(angle = 360),
                    axis.text.y=element_blank(),
                    panel.spacing = unit(.1, "lines"),
                    legend.position="none",
                    strip.text = element_text(size = 6, family="Arial"),
                    strip.background =element_rect(fill="gray94"),
                    panel.grid.major = element_blank(), 
                    axis.line = element_line(colour = "black")
                        )

    png("Figure_barchart_introspect_kjv_bible.png", 
    width = 6, height = 8.5, units = 'in', res = 1300)
    bars
    dev.off()

#------------------------------------------------------------------------------

### END ###