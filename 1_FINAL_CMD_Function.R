##-----------------------------------------------------------------------------
#  CMD Function as of 20190214
##-----------------------------------------------------------------------------

        CMDist <- function(dtm, cw, compound = FALSE, scale = TRUE) {
                start_time <- Sys.time()
                cw <- str_trim(cw)
                if((sum(lengths(regmatches(cw, gregexpr(" ", cw)))) >= 1)){
                  stop("Only input unigrams as cw.")
                  }
                
                ## add word if not in DTM
                dtm2 <- dtm
                for (i in cw) {
                  if(!i %in% colnames(dtm2)) {
                    new <- matrix(0, nrow=nrow(dtm2))
                    colnames(new) <- i
                    dtm2 <- cbind(dtm2, new)
                  }
                }
                
                # prepare word embeddings
                wem <- ft_embed(colnames(dtm2), lang = "en")
                rownames(wem) <- colnames(dtm2) #Add words as rownames to word embeddings
                wv <- wem[rowSums(is.na(wem)) != ncol(wem), ] # Remove the NAs or RWMD won't like it
                dtm2 <- as.matrix(dtm2)

                ## create pseudo dtm
                if(compound == FALSE) {
                  r <- sparseMatrix(dims = c(length(cw),ncol(dtm2)), i={}, j={})
                  pd <- rbind(r, dtm2)
                      for (i in 1:length(cw)) {
                          pd[i,cw[i]] <- 1
                    }
                }
                
                if(compound == TRUE) {
                  r <- sparseMatrix(dims = c(1,ncol(dtm2)), i={}, j={})
                  pd <- rbind(r, dtm2)
                  for (i in 1:length(cw)) {
                    ##
                    pd[1,cw[i]] <- 1/length(cw)
                  }
                }
                
                ## pseudodoc must be at least two columns, even if one concept word
                if(compound == FALSE) {
                  pd <- pd[1:(length(cw)+1),]
                  pd <- pd[,as.vector(rownames(wv))]
                  dtm2 <- dtm2[,as.vector(rownames(wv))]
                  dtm2 <- Matrix(dtm2, sparse = TRUE)
                }
                
                if(compound == TRUE) {
                  pd <- pd[1:2,]
                  pd <- pd[,as.vector(rownames(wv))]
                  dtm2 <- dtm2[,as.vector(rownames(wv))]
                  dtm2 <- Matrix(dtm2, sparse = TRUE)
                }

                ## the Work Horse of the function:
                dist = dist2(dtm2, pd, method = RWMD$new(wv), norm = 'none')
                ##
                
                if(compound == FALSE) {
                  df <- as.data.frame(dist[,1:length(cw)])
                }
                
                if(compound == TRUE) {
                  cw <- paste(cw, sep="_", collapse = "_")
                  df <- as.data.frame(dist[,1])
                }

                colnames(df) <- c(cw)
                df$docs <- rownames(dtm2)

                if(scale == TRUE) {
                  df[,cw] <- scale(df[,cw])*-1
                }

                if(scale == FALSE) {
                  df[,cw] <- (df[,cw])*-1
                }
                #
                df[,cw] <- sapply(df[,cw],as.numeric)

                end_time <- Sys.time()
                print(end_time - start_time)
                return(df)
            }
