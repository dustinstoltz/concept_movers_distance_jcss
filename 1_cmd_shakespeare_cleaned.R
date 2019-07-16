##-----------------------------------------------------------------------------
# LOAD DATA
##-----------------------------------------------------------------------------
 
    play.meta <- read_csv("2_shakes_meta.csv")
    basic.concepts <- read_csv("2_list_basic_concepts.csv")

    # Grab the text from Project GUTENBERG
    plays <- play.meta %>% 
                select(gutenberg_id) %>%
                gutenberg_download() %>%
                group_by(gutenberg_id) %>%
                summarise(text = paste(text, collapse=", ") )

    # Clean PLAY Text and UNNEST wrods
    play.text <- plays %>% 
            mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
            mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
            mutate(text = replace_white(text)) %>%
            mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
            mutate(text = replace_number(text)) %>%
            unnest_tokens(word, text, to_lower = TRUE) %>%
            anti_join(stop_words) %>%
            filter(!str_detect(word, "[0-9]+") ) %>%
            count(gutenberg_id, word) %>%
            bind_tf_idf(word, gutenberg_id, n)

#------------------------------------------------------------------------------
# Create DTM
#------------------------------------------------------------------------------

    play.dtm <- play.text %>%
                cast_dtm(term = word, document = gutenberg_id, 
                value = n, weighting = tm::weightTf) %>%
                removeSparseTerms(0.99) # 37 by 22782 (99%)
                dim(play.dtm)


##-----------------------------------------------------------------------------
#  Get CMD for `DEATH'
##-----------------------------------------------------------------------------
    # ~13 mins to run
    play.cmd <- play.dtm %>%
                CMDist(cw=c("death") ) %>%
                mutate(gutenberg_id=as.numeric(docs)) %>%
                left_join(play.meta)

##-----------------------------------------------------------------------------
# PLOT
##-----------------------------------------------------------------------------
## Comparing death and body counts in SHAKESPEARE

    s1 <- play.cmd %>%
            mutate(shape = factor(boas_problem_plays, 
                                  levels = c(0,1), 
                                  labels = c("", "Problem Play"))) %>%
            ggplot(aes(x=body_count, y=death) ) +
                geom_point(aes(color=as.factor(genre), shape=shape),size=3,  alpha=.9) +
                geom_text_repel(aes(label = short_title)) +
                geom_smooth(method="loess") +
                scale_size(guide = "none") +
                labs(x = "Number of Deaths in the Play", y = "Closeness to 'Death'",
                     title ="",
                     color = "Genre",
                     shape = "Boas' Problem Play",
                     size = FALSE) +
                theme(legend.position = "right",
                      legend.box = "vertical",
                      legend.direction = "vertical",
                      legend.justification="right",
                      axis.title.y = element_text(size = 10),
                      axis.title.x = element_text(size = 10) ) +
                guides(shape=FALSE)

    png("Figure_death_shakespeare.png", 
         width = 10, height = 9, units = 'in', res = 700)
        s1
    dev.off()

##-----------------------------------------------------------------------------
#  Get CMD for 200 Basic Concepts
    # ~12 mins
    basic.cmd <- play.dtm %>%
                CMDist(cw=c(unique(basic.concepts$term), "death") )
                
    basic.df <- basic.cmd %>% 
                mutate(gutenberg_id=as.numeric(docs)) %>% 
                left_join(play.meta, by = "gutenberg_id") %>%
                select(-c("docs", "gutenberg_id", "year.y",
                            "boas_problem_plays", "genre",  
                            "gutenberg_title") ) %>%
                melt(id.vars=c("short_title", 
                                "body_count"), 
                                value.name="value")

## ----------------------------------------------------------------------------
# PLOT Basic Concepts by Body Count
## ----------------------------------------------------------------------------

    s2 <- basic.df %>%
            ggplot(aes(x=body_count, y=as.numeric(value), group=variable) ) +
            geom_smooth(method="loess", se=FALSE, alpha=0.4, color="grey88") +

            geom_smooth(data=subset(basic.df, variable==c("death") ), 
                        aes(y=value),
                        method="loess", colour = "#1696d2", se=F) +

            geom_smooth(data=subset(basic.df, variable==c("kill") ), 
                        aes(y=value),
                        method="loess", colour = "#ec008b", se=F) +
            geom_smooth(data=subset(basic.df, variable==c("die") ), 
                        aes(y=value),
                        method="loess", colour = "#ec008b", se=F) +
            geom_smooth(data=subset(basic.df, variable==c("stab") ), 
                        aes(y=value),
                        method="loess", colour = "#ec008b", se=F) +
            geom_smooth(data=subset(basic.df, variable==c("blood") ), 
                        aes(y=value),
                        method="loess", colour = "#ec008b", se=F) +
            ggplot2::annotate("text", x = 14, y = 1.75, label = "death") +
            ggplot2::annotate("text", x = 14, y = 2.3, label = "kill") +
            ggplot2::annotate("text", x = 14.5, y = 2.1, label = "die") +
            ggplot2::annotate("text", x = 14.5, y = 1.65, label = "stab") +
            ggplot2::annotate("text", x = 14.5, y = 1.5, label = "blood") +
            labs(x = "Number of Deaths in the Play", y = "Closeness to Basic Concept",
                     title ="") +
            theme(axis.title.y = element_text(size = 10),
                  axis.title.x = element_text(size = 10) ) +
            guides(shape=FALSE)
    
    png("Figure_basic_concepts_shakepeare.png", 
         width = 10, height = 10, units = 'in', res = 700)
        s2
    dev.off()

#------------------------------------------------------------------------------

### END ###