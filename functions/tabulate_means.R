### Tabulate Means ###

## Only one each of the column name or column index for each item (subject, product, rating, demographic)
##   needs to be assigned. If both are assigned, precedence goes to the index
##
## To write to xlsx, use the output_folder option
##
## Please note that the letter options are both FALSE, if you want reduced letters, 
##   set both assign_letters and reduce_letters to TRUE

tabulate_means <- function(dataset, subject_column_name = NULL, subject_column_index = NULL, 
                           product_column_name = NULL, product_column_index = NULL,
                           rating_column_names = NULL, rating_column_indices = NULL, 
                           demographic_column_names = NULL, demographic_column_indices = NULL,
                           sort_column_name = NULL,
                           pVal = 0.05, output_folder = NULL, output_filename_append = "",
                           reduce_letters = TRUE, label_MSE = FALSE,
                           assign_letters = TRUE, collapse_single_variables_across_groups = FALSE) {
   library(tidyr)
   library(magrittr)
   library(dplyr)
   library(igraph)
   library(lpSolve)
   library(rlang)
   
   if(is.null(subject_column_index) & is.null(subject_column_name)) {
      stop("no subject id column specified")
   }
   if(is.null(product_column_index) & is.null(product_column_name)) {
      stop("no product id column specified")
   }
   if(is.null(rating_column_indices) & is.null(rating_column_names)) {
      stop("no rating columns specified")
   }
   
   if(is.null(subject_column_index)) {
      subject_column_index = which(names(dataset) == subject_column_name)[1]
      if(is.na(subject_column_index)) {
         stop("Specified subject column not found in dataset. Please check the spelling.")
      }
   }
   if(is.null(product_column_index)) {
      product_column_index = which(names(dataset) == product_column_name)[1]
      if(is.na(product_column_index)) {
         stop("Specified product column not found in dataset. Please check the spelling.")
      }
   } 
   
   if(!is.null(rating_column_indices)) {
      rating_column_names <- names(dataset)[rating_column_indices]
   } else {
      rating_column_indices <- which(names(dataset) %in% rating_column_names)
      if(length(rating_column_indices) != length(rating_column_names)) {
         warning("Not all rating column names were found in the main dataset. Please check the spelling.")  
      }
   }
   if(is.null(demographic_column_indices) & !is.null(demographic_column_names)) {
      demographic_column_indices <- which(names(dataset) %in% demographic_column_names)
      if(length(demographic_column_indices) != length(demographic_column_names)) {
         warning("Not all demographic column names were found in the main dataset. Please check the spelling.")  
      }
   }
   
   ratingColumnNames <- names(dataset)[rating_column_indices]
   mainData <- dataset %>%
      select_at(c(subject_column_index, product_column_index, rating_column_indices, demographic_column_indices))
   names(mainData) <- c("subjectCol", "productCol", names(dataset)[c(rating_column_indices, demographic_column_indices)])
   mainData$subjectCol <- as.factor(mainData$subjectCol)
   mainData$productCol <- as.factor(mainData$productCol)
   
   if(is.null(sort_column_name)) {
      sort_column_name <- ratingColumnNames[1]
   }
   
   demoNames <- names(dataset)[demographic_column_indices]
   
   mainData <- mainData %>%
      gather(key = scaleCol, value = scaleRating, ratingColumnNames)
   if(!is.numeric(mainData$scaleRating)) {
      warning("Scale rating columns were not all numeric. They will be converted to numeric.")
      mainData <- mainData %>%
         mutate(scaleRating = as.numeric(scaleRating))
   }
   
   ### Define some helper functions
   createTabulations <- function(inputData) {
      inputData %>%
         mutate(productCol = as.character(productCol)) %>%
         group_by(productCol, scaleCol) %>%
         summarise(Totals = n(), Means = mean(scaleRating, na.rm = TRUE)) %>%
         left_join(
            inputData %>%
               mutate(productCol = as.character(productCol)) %>%
               group_by(productCol, scaleCol, scaleRating) %>%
               count() %>%
               spread(scaleRating, n, fill = 0),
            by = c("productCol", "scaleCol")) %>%
         arrange(desc(Means)) %>%
         rename(Product = productCol, Scale = scaleCol) %>%
         select(Scale, Product, Totals, Means, names(.)[-c(1:4)]) %>%
         ungroup()
   }
   
   createMeans <- function(inputData) {
      output_data <- inputData %>%
         mutate(productCol = as.character(productCol)) %>%
         group_by(productCol) %>%
         group_by(scaleCol, add = TRUE) %>%
         summarise(Means = mean(scaleRating, na.rm = TRUE)) %>%
         spread(scaleCol, Means) %>%
         rename(Product = productCol) %>%
         ungroup() %>%
         select(Product, !!!rating_column_names)
      if(!is.null(sort_column_name)) {
         output_data <- output_data %>%
            arrange(desc(!!rlang::sym(sort_column_name)))
      } else {
         output_data <- output_data %>%
            arrange(Product)
      }
      return(output_data)
   }
   
   getSignificanceMatrix <- function(relevant_data, mse) {
      sampleSizes <- relevant_data %>% group_by(productCol) %>% summarize(Count = sum(!is.na(scaleRating)))
      prodMeans <- relevant_data %>% group_by(productCol) %>% summarise_at(mean, .vars = "scaleRating", na.rm = TRUE)
      prodNames <- prodMeans %>% arrange(desc(scaleRating)) %>% pull(productCol)
      if(any(sampleSizes$Count < 2)) {
         significanceMat <- as.data.frame(matrix(1, nrow = length(prodNames), ncol = length(prodNames)))
         warning(paste0("Sample size for the following products was less than 2 which probably indicates a typo:\n",
                     sampleSizes$productCol[which(sampleSizes$Count < 2)], "\n"))
         
      } else {
         significanceMat <- as.data.frame(outer(prodNames, prodNames, Vectorize(function(prod1, prod2) {
            meanDiff <- abs(prodMeans$scaleRating[prodMeans$productCol == prod1] - prodMeans$scaleRating[prodMeans$productCol == prod2])
            tVal <- meanDiff / sqrt(mse / sampleSizes$Count[sampleSizes$productCol == prod1] + mse / sampleSizes$Count[sampleSizes$productCol == prod2])
            df <- sampleSizes$Count[sampleSizes$productCol == prod1] + sampleSizes$Count[sampleSizes$productCol == prod2] - 2
            return((1.0 - pt(tVal, df)) * 2.0)
         })))
      }
      names(significanceMat) <- prodNames
      rownames(significanceMat) <- prodNames
      return(significanceMat)
   }
   
   assignLettersFcn <- function(cliqueMemMat) {
      if(dim(cliqueMemMat)[2] == 1 & all(cliqueMemMat == 1)) {
         return(data.frame(Letters = rep(letters[1], times = dim(cliqueMemMat)[1])) %>%
                   set_rownames(rownames(cliqueMemMat)))
      } else {
         return(data.frame(Letters = apply(cliqueMemMat[,order(as.numeric(apply(cliqueMemMat, 2, paste0, collapse = "")), decreasing = TRUE)], 1,
                                           function(row) {
                                              paste0(letters[which(as.logical(row))], collapse = "")
                                           })))
      }
   }
   
   createLettersList <- function(relevant_data, reduce_letters, sortScaleName = NULL) {
      if(is.null(sortScaleName)) {
         sortOrder <- as.character(unique(relevant_data$productCol))[order(as.character(unique(relevant_data$productCol)))]
      } else {
         sortOrder <- as.character(relevant_data %>% 
           filter(.$scaleCol == sortScaleName) %>% 
            group_by(productCol) %>% 
            summarise(mean = mean(scaleRating, na.rm = TRUE)) %>% 
            arrange(desc(mean)) %>%
            pull(productCol))
      }
      
      outData <- relevant_data %>%
         group_by(scaleCol) %>%
         nest() %>%
         mutate(mse = map_dbl(data, function(data) {
            if(length(unique(data$subjectCol)) > 1) {
               summary(aov(scaleRating ~ subjectCol + productCol, 
                           data = data)) %>% 
                  map("Mean Sq") %>% 
                  map_dbl(3) %>%
                  return()
            } else {
               summary(aov(scaleRating ~ productCol, 
                           data = data)) %>% 
                  map("Mean Sq") %>% 
                  map_dbl(1) %>%
                  return()
            }
         })) %>%
         mutate(sigMat = map2(data, mse, getSignificanceMatrix)) %>%
         mutate(cliqueMemMat = map(.$sigMat, function(mat) {
            if(any(is.na(mat))) {
               #browser()
            }
               if(reduce_letters) {
                  outMat <- data.frame(minCliqueAssignsLP(mat >= pVal)$cliqueMemMat)
               } else {
                  outMat <- as.data.frame(allCliqueAssigns(mat >= pVal))
               }
               rownames(outMat) <- rownames(mat)
               return(outMat)
            })) %>%
         mutate(Letters = map(cliqueMemMat, assignLettersFcn)) %>% #(.x[match(sortOrder, rownames(.x)),]))) %>%
         mutate(Letters = map(.$Letters, function(letterDF) { 
            return(as_tibble(letterDF, rownames = "Product"))
            })) %>%
         mutate(Means = map(data, function(scaleData) {
               scaleData %>%
                  group_by(productCol) %>%
                  summarise(Means = mean(scaleRating, na.rm = TRUE)) %>%
                  rename(Product = productCol)
            })) %>%
         mutate(finalDF = pmap(list(.$Letters, .$Means, .$mse, .$scaleCol, .$data), function(letter_vec, means, mse, scaleCol, data) {
            out_tibble <- left_join(letter_vec, means %>% mutate(Product = as.character(Product)), by = "Product") %>%
               mutate(LetterMeans = paste0(formatC(Means, digits = 2, format = "f"), " ", Letters)) %>%
               select(Product, Means, Letters, LetterMeans) %>%
               mutate(MSE = mse, Scale = scaleCol)
            names(out_tibble)[1] <- paste0("Product (N = ", data %>% pull(subjectCol) %>% unique() %>% length(), ")")
            return(out_tibble[match(sortOrder, out_tibble %>% pull(1)),])
            })) %>%
         pull(finalDF)
      names(outData) <- outData %>%
         map_chr(., function(x) { x$Scale[[1]] })
      if(length(outData) == 0) {
         outData <- NULL
      }
      return(outData)
   }
   
   collapseLetters <- function(lettersList) {
      if(is.null(lettersList)) {
         return(NULL)
      }
      originalOrder <- lettersList[[1]] %>% pull(1)
      if(label_MSE) {
         outDF <- reduce(lettersList, rbind) %>% 
            mutate(ScaleLabel = paste0(Scale, " (MSE = ", formatC(MSE, format = "f", digits = 2), ")")) %>% 
            select(1, ScaleLabel, LetterMeans) %>% # 1 = Product
            spread(ScaleLabel, LetterMeans)
         column_order <- c(1, match(rating_column_names, map_chr(names(outDF), function(name) {
            stringi::stri_sub(name, 1, stringi::stri_locate_first_fixed(name, " (MSE = ")[[1]] - 1)
         })))
         
      } else {
         outDF <- reduce(lettersList, rbind) %>% 
            mutate(ScaleLabel = Scale) %>% 
            select(1, ScaleLabel, LetterMeans) %>% # 1 = Product
            spread(ScaleLabel, LetterMeans)
         column_order <- c(1, match(rating_column_names, names(outDF)))
      }
      return(outDF[match(originalOrder, outDF %>% pull(1)), column_order]) # 1 = Product
   }
   
   
   ### Tabulations
   print("Tabulating...")
   tabulationsList <- lapply(demoNames, function(demoName) {
      uniqueDemoValues <- unique(mainData[[demoName]])
      demoResults <- lapply(uniqueDemoValues, function(demoValue) {
         createTabulations(mainData %>% filter(.[[demoName]] == demoValue))
      })
      names(demoResults) <- uniqueDemoValues
      return(demoResults)
   })
   names(tabulationsList) <- demoNames
   
   # Add overall tabulation
   tabulationsList <- c(list(Total = list(Total = createTabulations(mainData))), tabulationsList)
   
   print("Tabulations Complete")
   
   ### Means
   print("Calculating Means...")
   meansList <- lapply(demoNames, function(demoName) {
      uniqueDemoValues <- sort(unique(mainData[[demoName]]))
      #uniqueDemoValues <- uniqueDemoValues[!is.na(uniqueDemoValues)]
      demoResults <- lapply(uniqueDemoValues, function(demoValue) {
         createMeans(mainData %>% filter(.[[demoName]] == demoValue))
      })
      names(demoResults) <- uniqueDemoValues

      if(length(rating_column_indices) == 1 && collapse_single_variables_across_groups) {
         combinedTbl <- reduce(demoResults, left_join, by = "Product")
         names(combinedTbl)[-1] <- names(demoResults)
         combinedTbl <- combinedTbl %>%
            arrange(desc(Product))
         demoResults <- list(combinedTbl)
         names(demoResults) <- demoName
      }

      return(demoResults)
   })
   names(meansList) <- demoNames
   
   # Add overall means
   meansList <- c(list(Total = list(Total = createMeans(mainData))), meansList)
   
   # forbidden characters in Excel sheet names
   forbiddenChars <- "/|\\|\\*|\\[|]|:|\\?'"
   
   print("Mean Calculation Complete")
   
   if(!is.null(output_folder)) {
      library(writexl)
      
      if(!dir.exists(output_folder)) {
         dir.create(output_folder)
      }
      
      print("Writing Tabulations File")
      outFileList <- unlist(tabulationsList, recursive = FALSE)
      names(outFileList) <- unlist(map(names(tabulationsList), function(name) {
         names(tabulationsList[[name]]) <- stringr::str_replace_all(
            str_sub(paste0(name, " - ", names(tabulationsList[[name]])), 1, 31),
            forbiddenChars, '-')
      }))
      writexl::write_xlsx(outFileList, col_names = TRUE, path = 
                             file.path(output_folder, stringr::str_replace_all(
                                paste0(Sys.Date(), 
                                       ifelse(output_filename_append == "", "", paste0(" - ", output_filename_append)),
                                       " - Tabulations.xlsx"), forbiddenChars, '-')
                                ))
      
      print("Writing Means File")
      outFileList <- unlist(meansList, recursive = FALSE)
      names(outFileList) <- unlist(map(names(meansList), function(name) {
         names(meansList[[name]]) <- stringr::str_replace_all(
            str_sub(paste0(name, " - ", names(meansList[[name]])), 1, 31),
            forbiddenChars, '-')
      }))
      writexl::write_xlsx(outFileList, col_names = TRUE, path = 
                             file.path(output_folder, stringr::str_replace_all(
                                paste0(Sys.Date(), 
                                       ifelse(output_filename_append == "", "", paste0(" - ", output_filename_append)),
                                       " - Means.xlsx"), forbiddenChars, '-')
                             ))
   }
   
   ### Letters
   if(assign_letters) {
      print("Calculating Letters...")
      
      lettersList <- lapply(demoNames, function(demoName) {
         uniqueDemoValues <- unique(mainData[[demoName]])
         #uniqueDemoValues <- uniqueDemoValues[!is.na(uniqueDemoValues)]
         print(paste0("  ", demoName))
         demoResults <- lapply(uniqueDemoValues, function(demoValue) {
            print(paste0("    ", demoValue))
            createLettersList(mainData %>% filter(.[[demoName]] == demoValue), reduce_letters, sort_column_name)
         })
         
         names(demoResults) <- uniqueDemoValues
         
         if(length(rating_column_indices) == 1 && collapse_single_variables_across_groups) {
            combinedTbl <- reduce(demoResults, left_join, by = "Product")
            names(combinedTbl)[-1] <- names(demoResults)
            combinedTbl <- combinedTbl %>%
               arrange(desc(Product))
            demoResults <- list(combinedTbl)
            names(demoResults) <- demoName
         }
         
         return(demoResults)
      })
      names(lettersList) <- demoNames
   
      # Add overall letters
      lettersList <- c(list(Total = list(Total = createLettersList(mainData, reduce_letters, sort_column_name))), lettersList)
      print("Letter Calculation Complete")
   
      # Add collapsed letters list

      collapsedLettersList <- modify_depth(lettersList, .depth = 2, collapseLetters)
      collapsedLettersList <- map(collapsedLettersList, function(demo_list) {
         null_indices <- which(unlist(map(demo_list, is.null)))
         if(length(null_indices) > 0) {
            return(demo_list[-null_indices])
         } else {
            return(demo_list)
         }
      })

      if(!is.null(output_folder)) {
         print("Writing letters file")
         library(writexl)
         outFileList <- unlist(collapsedLettersList, recursive = FALSE)
         names(outFileList) <- unlist(map(names(collapsedLettersList), function(name) {
            names(collapsedLettersList[[name]]) <- stringr::str_replace_all(
               str_sub(paste0(name, " - ", names(collapsedLettersList[[name]])), 1, 31),
               forbiddenChars, '-')
         }))
         writexl::write_xlsx(outFileList, col_names = TRUE, path = 
                                file.path(output_folder, stringr::str_replace_all(
                                   paste0(Sys.Date(), 
                                          ifelse(output_filename_append == "", "", paste0(" - ", output_filename_append)),
                                          " - Letters.xlsx"), forbiddenChars, '-')
                                ))
      }
      
      return(list(tabulations = tabulationsList, means = meansList, lettersList = lettersList, collapsedLetters = collapsedLettersList))
   }
   
   return(list(tabulations = tabulationsList, means = meansList))
}

isRemovable <- function(assignInd, cliqueMemMat, connMat){
   # Note: assignInd is a 2-dim index (row, col)
   
   vertInd <- assignInd[1]
   cliqueInd <- assignInd[2]
   
   # The only connections that can be affected are those with connected vertices in the same clique
   relVertInds <- which(connMat[vertInd, ] & cliqueMemMat[ , cliqueInd])
   
   for (relVertInd in relVertInds){
      # If the connVertInd vertex is  in the cliqueInd clique then need to have at least one other clique in common
      if(!any(cliqueMemMat[vertInd, -cliqueInd] & cliqueMemMat[relVertInd, -cliqueInd])){
         # This vertex has no other clique in common, so this assignment is not removable
         return(F)
      }
   }
   
   # If we make it this far, assignment is removable
   return(T)
}

allCliqueAssigns <- function(boolMat) {
   numProds <- dim(boolMat)[[1]]
   compGraph <- graph.adjacency(as.matrix(boolMat), mode = "undirected")
   mCliques <- lapply(maximal.cliques(compGraph), function(x){sort(as.numeric(as.character(x)))})
   names(mCliques) <- 1:length(mCliques)
   return(map_dfr(mCliques, function(productMem) { 
      outVec <- rep(0, numProds)
      outVec[productMem] <- 1
      return(outVec)
   }))
}

# Linear Programming to find minimum number of unique letters (unique maximal cliques) to represent display
minCliqueAssignsLP <- function(boolMat){
   
   # Minimizing number of unique letters
   
   ptm <- proc.time()
   
   # Make connectivity matrix from Boolean matrix with diagonal set to zero
   connMat <- boolMat
   diag(connMat) <- F
   numVerts <- dim(connMat)[1]
   
   # Identify singletons, if any
   singletonVertFlags <- !apply(connMat, 1, any)
   singletonInds <- which(singletonVertFlags)
   numSVerts <- length(singletonInds)
   nonSingletonInds <- which(!singletonVertFlags)
   numNSVerts <- length(nonSingletonInds)
   
   # If we have all singletons, there is nothing to do, just set the clique membership matrix to be diagonal
   if (numSVerts == numVerts){
      lpSol <- list()
      cliqueMemMat <- matrix(F, numVerts, numVerts)
      diag(cliqueMemMat) <- T
   } else {
      
      ##### We remove all singletons for the purposes of the analysis, and put them back in
      ##### at the end if they exist.  Note, at ths point, the indexing does shift and will
      ##### need to be shifted back
      
      if (numSVerts > 0){
         connMat <- connMat[nonSingletonInds, nonSingletonInds]
         numVerts <- numNSVerts
      }
      
      compGraph <- graph.adjacency(as.matrix(connMat), mode = "undirected")
      mCliques <- lapply(maximal.cliques(compGraph), function(x){sort(as.numeric(as.character(x)))})
      cliqueLengths <- sapply(mCliques, length)
      singletonCliques <- (cliqueLengths == 1)
      
      # Remove cliques that contain singletons as linear programming counts on edges being covered to retain assignments
      cliqueLengths <- cliqueLengths[!singletonCliques]
      mCliques <- mCliques[order(cliqueLengths)]
      numCliques <- length(mCliques)
      
      # If there is only one clique, no need to search as no assignments can be removed
      if (numCliques == 1){
         lpSol <- list()
         cliqueMemMat <- matrix(T, numVerts, 1)
      } else {
         
         # Clique membership matrix for vertices
         cliqueMemMat <- sapply(mCliques, function(x){output <- rep(F,numVerts);output[x] <- T;return(output)})
         
         # Look up table to convert assigment index to vertex-clique pair
         assignRoster <- which(cliqueMemMat, arr.ind = T)
         colnames(assignRoster) <- c("Vertex", "Clique")
         numAssigns <- dim(assignRoster)[1]
         rownames(assignRoster) <- paste("Assign", 1:numAssigns)
         
         # Create reverse look up function to convert vertex-clique pair to assignment index
         revAssignRoster <- 1:numAssigns
         names(revAssignRoster) <- apply(assignRoster, 1, function(x){paste(x[1],x[2],sep=",")})
         revAssignFcn <- function(vertexInd,CliqueInd){as.numeric(revAssignRoster[paste(vertexInd,CliqueInd,sep=",")])}
         
         # Look up table to convert edge to vertex-vertex pair
         edgeRoster <- which(connMat & upper.tri(connMat), arr.ind = T)
         colnames(edgeRoster) <- c("Vertex 1", "Vertex 2")
         numEdges <- dim(edgeRoster)[1]
         rownames(edgeRoster) <- paste("Edge", 1:numEdges)
         
         # Clique coverage matrix for edges
         edgeCoverMat <- t(apply(edgeRoster, 1, function(x){apply(cliqueMemMat, 2, function(y){all(y[x])})}))
         
         # Look up table to convert edge coverage index to edge-clique pair
         edgeCoverRoster <- which(edgeCoverMat, arr.ind = T) # Look up table to convert edge covering to edge-clique pair
         colnames(edgeCoverRoster) <- c("Edge", "Clique")
         numEdgeCovers <- dim(edgeCoverRoster)[1]
         rownames(edgeCoverRoster) <- paste("EdgeCover", 1:numEdgeCovers)
         
         # Look up table to convert edge coverage index to vertex-vertex-clique pair
         vertexVertexCoverRoster <- t(apply(edgeCoverRoster, 1, function(x){c(edgeRoster[x[1],], x[2])})) # Look up table to convert edge covering to vertex-vertex-clique pair
         colnames(vertexVertexCoverRoster) <- c("Vertex 1", "Vertex 2", "Clique")
         
         ####### Constraints that capture whether edges are covered by cliques depending on which assignments of vertices to cliques are made #######
         
         # Determine which vertices are involved in each edge covering, for the sake of setting constraints
         firstAssigns <- apply(vertexVertexCoverRoster,1,function(x){revAssignFcn(x[1], x[3])})
         secondAssigns <- apply(vertexVertexCoverRoster,1,function(x){revAssignFcn(x[2], x[3])})
         
         # Create constraint that first vertex not assigned to a clique means edge not covered by that clique
         firstAssignsConstMat1 <- t(sapply(as.numeric(firstAssigns), function(x){output <- rep(0, numAssigns);output[x]<-1;return(output)}))
         firstAssignsConstMat2 <- t(sapply(1:numEdgeCovers, function(x){output <- rep(0, numEdgeCovers);output[x]<--1;return(output)}))
         firstAssignsConstMat <- cbind(firstAssignsConstMat1, firstAssignsConstMat2)
         firstAssignsDirVect <- rep(">=", numEdgeCovers)
         firstAssignsRHS <- rep(0, numEdgeCovers)
         
         # Create constraint that second vertex not assigned to a clique means edge not covered by that clique
         secondAssignsConstMat1 <- t(sapply(as.numeric(secondAssigns), function(x){output <- rep(0, numAssigns);output[x]<-1;return(output)}))
         secondAssignsConstMat <- cbind(secondAssignsConstMat1, firstAssignsConstMat2) # Note firstAssignsConstMat2 is same as secondAssignsConstMat2 would be
         secondAssignsDirVect <- firstAssignsDirVect
         secondAssignsRHS <- firstAssignsRHS
         
         # Create constraint that first and second vertex assigned to a clique means edge is covered by that clique
         jointAssignsConstMat <- cbind(firstAssignsConstMat1+secondAssignsConstMat1, firstAssignsConstMat2)
         jointAssignsDirVect <- rep("<=", numEdgeCovers)
         jointAssignsRHS <- rep("1", numEdgeCovers)
         
         ####### Constraints that ensure all edges are covered at all depending on which edge clique coverings hold #######
         
         # Create reverse look up function to convert edge-clique pair to edge coverage index
         revEdgeCoverRoster <- 1:numEdgeCovers
         names(revEdgeCoverRoster) <- apply(edgeCoverRoster, 1, function(x){paste(x[1],x[2],sep=",")})
         revEdgeCoverFcn <- function(edgeInd,CliqueInd){as.numeric(revEdgeCoverRoster[paste(edgeInd,CliqueInd,sep=",")])}
         
         # Determine edge coverage indices for each pair (will be used to create constraints)
         edgeCoverSplit <- split(as.numeric(edgeCoverRoster[,2]), edgeCoverRoster[,1])
         edgeCoverInds <- lapply(1:numEdges, function(x){sapply(edgeCoverSplit[[x]], function(y){revEdgeCoverFcn(x,y)})})
         
         # Create constraint that every edge must be covered by at least one clique
         edgeCoverConstMat1 <- matrix(0, numEdges, numAssigns)
         edgeCoverConstMat2 <- t(sapply(edgeCoverInds, function(x){output <- rep(0, numEdgeCovers);output[x]<-1;return(output)}))
         edgeCoverConstMat <- cbind(edgeCoverConstMat1, edgeCoverConstMat2)
         edgeCoverDirVect <- rep(">=", numEdges)
         edgeCoverRHS <- rep("1", numEdges)
         
         ###### Constraints that ensure non-removable assigments are not removed
         
         essentialAssignInds <- which(apply(assignRoster, 1, function(x){!isRemovable(x,cliqueMemMat,connMat)}))
         numEssential <- length(essentialAssignInds)
         essentialConstMat1 <- t(sapply(essentialAssignInds, function(x){output <- rep(0, numAssigns);output[x]<-1;return(output)}))
         essentialConstMat2 <- matrix(0, numEssential, numEdgeCovers)
         essentialConstMat <- cbind(essentialConstMat1, essentialConstMat2)
         essentialDirVect <- rep("==", numEssential)
         essentialRHS <- rep("1", numEssential)
         
         # Now conduct reduction of assignments
         
         objVect <- c(rep(1,numAssigns), rep(0, numEdgeCovers))
         constMat <- rbind(firstAssignsConstMat, secondAssignsConstMat, jointAssignsConstMat, edgeCoverConstMat, essentialConstMat)
         constDir <- c(firstAssignsDirVect, secondAssignsDirVect, jointAssignsDirVect, edgeCoverDirVect, essentialDirVect)
         constRHS <- c(firstAssignsRHS, secondAssignsRHS, jointAssignsRHS, edgeCoverRHS, essentialRHS)
         
         lpSol <- lp(direction = "min", objective.in = objVect, const.mat = constMat,
                     const.dir = constDir, const.rhs = constRHS, all.bin = T)
         
         # Create the clique membership matrix resulting from the LP solution
         keepAssigns <- assignRoster[which(lpSol$solution[1:numAssigns]==1), ]
         cliqueMemMat <- matrix(0, numVerts, numCliques)
         cliqueMemMat[keepAssigns] <- 1
      }
      
      # If there were singletons, they get added back in now
      if (numSVerts > 0){
         numVerts <- numNSVerts + numSVerts
         fullCliqueMemMat <- matrix(0, numVerts, numCliques+numSVerts)
         fullCliqueMemMat[nonSingletonInds, 1:numCliques] <- cliqueMemMat
         for (sInd in 1:numSVerts){
            fullCliqueMemMat[singletonInds[sInd],] <- rep(0, numCliques+numSVerts)
            fullCliqueMemMat[singletonInds[sInd],numCliques+sInd] <- 1
         }
         cliqueMemMat <- fullCliqueMemMat
      }
   }
   checkMat <- cliqueMemMat%*%t(cliqueMemMat)
   diag(checkMat) <- ifelse(boolMat[1,1], 1, 0)
   #print(paste("Correct Reduction:", all((checkMat > 0) == boolMat)))
   
   elapsedTime <- as.numeric(proc.time() - ptm)[3]
   return(list(cliqueMemMat = cliqueMemMat, elapsedTime = elapsedTime))
}
