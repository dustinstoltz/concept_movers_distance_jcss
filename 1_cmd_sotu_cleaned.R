
#------------------------------------------------------------------------------
# DATA
#------------------------------------------------------------------------------

    sotu <- quanteda.corpora::data_corpus_sotu
    sotu.texts <- sotu$documents
    sotu.texts$docname <- sotu.texts$`_document`
    sotu.texts$`_document` <- NULL

#------------------------------------------------------------------------------
# Create DTMs
#------------------------------------------------------------------------------

    sotu.count <- sotu.texts %>% 
        mutate(documents = str_replace_all(texts, "[^[:ascii:]]", " ")) %>%
        mutate(documents = str_replace_all(texts, "[[:punct:]]", " ")) %>%
        unnest_tokens(word, texts, to_lower = TRUE) %>%
        anti_join(stop_words) %>%
        filter(!str_detect(word, "[0-9]+") ) %>%
        dplyr::count(docname, word) %>%
        group_by(docname) %>%
        mutate(n = sum(n))

    sotu.dtm <- sotu.texts %>% 
        mutate(documents = str_replace_all(texts, "[^[:ascii:]]", " ")) %>%
        mutate(documents = str_replace_all(texts, "[[:punct:]]", " ")) %>%
        unnest_tokens(word, texts, to_lower = TRUE) %>%
        anti_join(stop_words) %>%
        filter(!str_detect(word, "[0-9]+") ) %>%
        dplyr::count(docname, word) %>%
        cast_dtm(term = word, document = docname, 
                 value = n, weighting = tm::weightTf)

    sotu.dtm.99 <- removeSparseTerms(sotu.dtm, .99) 
    sotu.dtm.95 <- removeSparseTerms(sotu.dtm, .95)
    sotu.dtm.85 <- removeSparseTerms(sotu.dtm, .85) 
    sotu.dtm.75 <- removeSparseTerms(sotu.dtm, .75) 
    sotu.dtm.65 <- removeSparseTerms(sotu.dtm, .65) 
    ##
# inspect(sotu.dtm)
# dim(sotu.dtm)

#------------------------------------------------------------------------------
#Getting CMDistances
#------------------------------------------------------------------------------
# ~24 mins each
CMDs.1.99 = CMDist(sotu.dtm.99, cw = c("father", "strict"), compound = T)
CMDs.2.99 = CMDist(sotu.dtm.99, cw = c("parent", "nurture"), compound = T)
#----------------------------------------------
# 9 mins
CMDs.1.95 = CMDist(sotu.dtm.95, cw = c("father", "strict"), compound = T)
CMDs.2.95 = CMDist(sotu.dtm.95, cw = c("parent", "nurture"), compound = T)
#----------------------------------------------
# 3 mins
CMDs.1.85 = CMDist(sotu.dtm.85, cw = c("father", "strict"), compound = T)
CMDs.2.85 = CMDist(sotu.dtm.85, cw = c("parent", "nurture"), compound = T)
#----------------------------------------------
# 1.28 mins
CMDs.1.75 = CMDist(sotu.dtm.75, cw = c("father", "strict"), compound = T)
CMDs.2.75 = CMDist(sotu.dtm.75, cw = c("parent", "nurture"), compound = T)
#----------------------------------------------
# 33 seconds
CMDs.1.65 = CMDist(sotu.dtm.65, cw = c("father", "strict"), compound = T)
CMDs.2.65 = CMDist(sotu.dtm.65, cw = c("parent", "nurture"), compound = T)
#----------------------------------------------

#----------------------------------------------
#Join the data 
sotu.texts <- left_join(sotu.texts, CMDs.1.99, by = c("docname" = "docs") )
sotu.texts <- left_join(sotu.texts, CMDs.2.99, by = c("docname" = "docs") )
#----------------------------------------------
sotu.texts <- left_join(sotu.texts, CMDs.1.95, by = c("docname" = "docs") )
sotu.texts <- left_join(sotu.texts, CMDs.2.95, by = c("docname" = "docs") )
#----------------------------------------------
sotu.texts <- left_join(sotu.texts, CMDs.1.85, by = c("docname" = "docs") )
sotu.texts <- left_join(sotu.texts, CMDs.2.85, by = c("docname" = "docs") )
#----------------------------------------------
sotu.texts <- left_join(sotu.texts, CMDs.1.75, by = c("docname" = "docs") )
sotu.texts <- left_join(sotu.texts, CMDs.2.75, by = c("docname" = "docs") )
#----------------------------------------------
sotu.texts <- left_join(sotu.texts, CMDs.1.65, by = c("docname" = "docs") )
sotu.texts <- left_join(sotu.texts, CMDs.2.65, by = c("docname" = "docs") )
#----------------------------------------------

# Tidy it up
colnames(sotu.texts) <- c("texts", "FirstName", "President", "Date",
                          "delivery", "type", "party", "docname",
                          "father_strict.99", "parent_nurture.99",
                          "father_strict.95", "parent_nurture.95",
                          "father_strict.85", "parent_nurture.85",
                          "father_strict.75", "parent_nurture.75",
                          "father_strict.65", "parent_nurture.65")

#------------------------------------------------------------------------------
# PLOTS
#------------------------------------------------------------------------------

  #Family concepts over time
  
  family.plot <- ggplot(sotu.texts[which(sotu.texts$party=="Democratic" | 
                            sotu.texts$party=="Republican"),], 
        aes(x = Date, y = father_strict.99) ) +
        geom_smooth(aes(color = "father"), method = "loess", formula = y ~ x) +
        geom_smooth(aes(x = Date, y = parent_nurture.99, color = "parent"), method = "loess", 
                    formula = y ~ x) +
        geom_point(aes(x = Date, y = father_strict.99, color = "father"), size = 2) +
        geom_point(aes(x = Date, y = parent_nurture.99, color = "parent"), size = 2) +
        xlab("Date") +
        ylab('Closeness to Compound Concept') +
        scale_color_manual(name = "Compound\nConcept",
                          breaks = c("father", "parent"),
                          values = c("#fdbf11", "#1696d2"),
                          labels = c('"Strict" + "Father"',
                                      '"Nurture" + "Parent"')) +
        theme(legend.spacing.x = unit(.25, 'cm'),
              legend.text.align =  1.5)
  
  png("Figure_sotu_family_time.png", 
       width = 7, height = 5, units = 'in', res = 900)
  family.plot
  dev.off()
#----------------------------------------------
# Sparsity CORRELATIONS

    sparse.corr <- sotu.texts %>%
                mutate(f.99 = as.numeric(father_strict.99),
                        f.95 = as.numeric(father_strict.95),
                        f.85 = as.numeric(father_strict.85),
                        f.75 = as.numeric(father_strict.75),
                        f.65 = as.numeric(father_strict.65)
                        ) %>%
                select("f.99","f.95","f.85","f.75","f.65") %>%
                ggpairs(
                # upper = list(continuous = wrap("cor", size = 5, fontface = "bold", color="black") ),
                showStrips = FALSE,
                #title = "Correlations at Different Levels of Sparsity for 'Strict Father'",
                columnLabels=c("99% Sparsity", "95% Sparsity","85% Sparsity",
                               "75% Sparsity","65% Sparsity")
                )

    png("Figure_sotu_sparse_correlations.png", 
       width = 9, height = 9, units = 'in', res = 900)
    sparse.corr
    dev.off()

#------------------------------------------------------------------------------

### END ###
