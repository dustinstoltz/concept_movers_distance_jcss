# Concept Mover's Distance: Reproduction Guide

Code and data to reproduce Stoltz and Taylor (2019) "Concept Mover's Distance."

[Dustin S. Stoltz](https://www.dustinstoltz.com) and [Marshall A. Taylor](https://www.marshalltaylor.net)

This is the code and data necessary to reproduce the measures, graphs, and plots for Stoltz and Taylor (2019) "Concept Mover's Distance."

## CMDist Function
----

``` r

CMDist <- function(dtm, cw) {
                start_time <- Sys.time()
                
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
                r <- sparseMatrix(dims = c(length(cw),ncol(dtm2)), i={}, j={})
                pd <- rbind(r, dtm2)
                    for (i in 1:length(cw)) {
                        pd[i,cw[i]] <- 1
                    }
                
                ## pseudodoc must be at least two columns, even if one concept word
                pd <- pd[1:(length(cw)+1),]
                pd <- pd[,as.vector(rownames(wv))]
                dtm2 <- dtm2[,as.vector(rownames(wv))]
                dtm2 <- Matrix(dtm2, sparse = TRUE)

                ## the Work Horse of the function:
                dist = dist2(dtm2, pd, method = RWMD$new(wv), norm = 'none')
                ##
                df <- as.data.frame(dist[,1:length(cw)])
                colnames(df) <- c(cw)
                df$docs <- rownames(dtm2)
                df[,cw] <- scale(df[,cw])*-1
                end_time <- Sys.time()
                print(end_time - start_time)
                return(df)
            }
```
### Performance


-------------------------------------------------------------------
### THE END 
