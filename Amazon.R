Code: 
 
library(tm) library(SnowballC) library(wordcloud) library(tidytext) library(cluster) library(factoextra) library(dbscan) library(proxy) library(ggplot2) library(dplyr) library(stringr) library(slam) library(RColorBrewer) 
set.seed(123) 
 
 
csv_path <- "D:/Academic/@Semesters/@8th Semester (Summer)/FinalTerm/Inrtoduction to Data Science/Project/amazon_reviews.csv"  
raw <- read.csv(csv_path, stringsAsFactors = FALSE) 
 
 
text_col_candidates   <- c("Text","text","reviewText","ReviewText","Review.Text","body","review_body","review_text","c ontent") rating_col_candidates <- c("Rating","rating","overall","Score","score","star_rating","stars") 
 
text_col   <- intersect(text_col_candidates,   names(raw))[1] rating_col <- intersect(rating_col_candidates, names(raw))[1] 
 
if (is.na(text_col) || is.na(rating_col)) {   stop("Couldn't find text or rating column. Check column names with names(read.csv(csv_path, nrows=1)).") 
} 
 
df <- raw %>%   select(text = all_of(text_col), rating = all_of(rating_col)) %>%   filter(!is.na(text), !is.na(rating)) %>%   mutate(rating = as.numeric(rating)) %>% 
  filter(!is.na(rating)) %>%   mutate(text = trimws(text)) %>% 
  filter(nchar(text) > 0) 
 
 
bad_pool  <- df %>% filter(rating <= 2) 
good_pool <- df %>% filter(rating >= 4) 
 
if (nrow(bad_pool)  < 500) stop(paste0("Not enough bad (<=2★) reviews. Found ", nrow(bad_pool), ", need 500.")) if (nrow(good_pool) < 500) stop(paste0("Not enough good (>=4★) reviews. Found ", nrow(good_pool), ", need 500.")) 
 
bad_500  <- dplyr::slice_sample(bad_pool,  n = 500) good_500 <- dplyr::slice_sample(good_pool, n = 500) 
 
reviews <- dplyr::bind_rows(bad_500, good_500) %>%   dplyr::mutate(label = ifelse(rating >= 4, "good", "bad")) %>%   dplyr::slice_sample(n = nrow(.))   
 
cat("Final dataset size:", nrow(reviews), "rows (",     sum(reviews$label=="bad"), "bad, ", 
    sum(reviews$label=="good"), "good)\n") 
 
 
corpus <- VCorpus(VectorSource(reviews$text)) 
 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) corpus <- tm_map(corpus, toSpace, "[^[:alnum:][:space:]']")  corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removeNumbers) corpus <- tm_map(corpus, removePunctuation) corpus <- tm_map(corpus, removeWords, stopwords("english")) corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, stemDocument) 
 
 
dtm <- DocumentTermMatrix(corpus, 
                          control = list(                             weighting = weightTfIdf, 
                            minDocFreq = 5    
                          )) 
 
 
nonempty <- slam::row_sums(dtm) > 0 dtm <- dtm[nonempty, ] 
reviews_clean <- reviews[nonempty, ] 
 
 
tfidf <- as.matrix(dtm) 
 
l2 <- sqrt(rowSums(tfidf^2)) ; l2[l2==0] <- 1 
tfidf_norm <- tfidf / l2 
 
cat("TF-IDF shape:", dim(tfidf_norm)[1], "docs x", dim(tfidf_norm)[2], "terms\n") 
 
pca <- prcomp(tfidf_norm, center = TRUE, scale. = FALSE) 
pc_scores <- pca$x pc2  <- as.data.frame(pc_scores[, 1:2,  drop = FALSE]) pc20 <- as.data.frame(pc_scores[, 1:20, drop = FALSE]) 
 
 
km2 <- kmeans(pc20, centers = 2, nstart = 25) pc2$km2 <- factor(km2$cluster) 
 
km3 <- kmeans(pc20, centers = 3, nstart = 25) pc2$km3 <- factor(km3$cluster) 
 
p1 <- ggplot(pc2, aes(PC1, PC2, color = km2)) +   geom_point(alpha=.7) +   labs(title="K-means (k=2) on 20 PCs", x="PC1", y="PC2") +   theme_minimal() p2 <- ggplot(pc2, aes(PC1, PC2, color = km3)) +   geom_point(alpha=.7) +   labs(title="K-means (k=3) on 20 PCs", x="PC1", y="PC2") +   theme_minimal() 
 
print(p1); print(p2) 
 
d_cos <- proxy::dist(as.matrix(pc20), method = "cosine") hc <- hclust(d_cos, method = "ward.D2") plot(hc, labels = FALSE, hang = -1, main="Hierarchical (Ward.D2) on cosine distance (20 PCs)") rect.hclust(hc, k = 2, border = 2:3) hc_cut2 <- cutree(hc, k = 2) hc_cut3 <- cutree(hc, k = 3) pc2$hc2 <- factor(hc_cut2) pc2$hc3 <- factor(hc_cut3) 
 
knn5 <- dbscan::kNNdist(as.matrix(pc20), k = 5) eps_guess <- as.numeric(quantile(knn5, 0.95)) cat("DBSCAN eps guess:", round(eps_guess, 3), "\n") 
 
db <- dbscan::dbscan(as.matrix(pc20), eps = eps_guess, minPts = 5) pc2$db <- factor(ifelse(db$cluster==0, "noise", paste0("C", db$cluster))) 
 
fviz_cluster(list(data = pc20, cluster = db$cluster),              geom = "point", ellipse = FALSE,              main = "DBSCAN clusters on 20 PCs") 
 
 
km2_lab <- km2$cluster centroids <- matrix(0, nrow = ncol(tfidf_norm), ncol = 2) for (k in 1:2) {   idx <- which(km2_lab == k)   if (length(idx) > 0) { 
    centroids[, k] <- colMeans(tfidf_norm[idx, , drop=FALSE]) 
  } } 
colnames(centroids) <- c("km2_cluster1","km2_cluster2") rownames(centroids) <- colnames(tfidf_norm) 
 
top_terms <- function(centroid_vec, n=15){   s <- sort(centroid_vec, decreasing = TRUE)   head(data.frame(term = names(s), weight = as.numeric(s)), n) } 
 
cat("\nTop terms for K-means (k=2) - Cluster 1:\n") print(top_terms(centroids[,"km2_cluster1"], 15)) cat("\nTop terms for K-means (k=2) - Cluster 2:\n") print(top_terms(centroids[,"km2_cluster2"], 15)) 
 
label_vec <- reviews_clean$label tab_km2 <- table(km2 = km2_lab, label = label_vec) tab_hc2 <- table(hc2 = hc_cut2, label = label_vec) tab_db  <- table(db = ifelse(db$cluster==0,"noise",paste0("C",db$cluster)), label = label_vec) 
 
cat("\nContingency: K-means (k=2) vs labels:\n"); print(tab_km2) cat("\nContingency: Hierarchical (k=2) vs labels:\n"); print(tab_hc2) cat("\nContingency: DBSCAN vs labels:\n"); print(tab_db) 
 
mk_wordcloud <- function(rows, title_txt) {   m <- sort(colSums(tfidf_norm[rows, , drop = FALSE]), decreasing = TRUE)   wordcloud(     words = names(m), freq = m,     max.words = 120, scale = c(3, 0.6),     random.order = FALSE, 
    colors = brewer.pal(8, "Dark2") 
  ) 
  title(main = title_txt) 
} 
par(mfrow = c(1,2)) mk_wordcloud(which(label_vec == "good"), "Good Reviews") mk_wordcloud(which(label_vec == "bad"),  "Bad Reviews") par(mfrow = c(1,1)) 
 
 
