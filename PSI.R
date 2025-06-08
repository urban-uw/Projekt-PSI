
#instalowanie pakietów, jeśli użytkownik nie ma
if (!require("pdftools")) install.packages("pdftools")
if (!require("tm")) install.packages("tm")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("cluster")) install.packages("cluster")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("factoextra")) install.packages("factoextra")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("DT")) install.packages("DT")
if (!require("stopwords")) install.packages("stopwords")


# Wymagane pakiety
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming (nie obsługuje polskiego, użyjemy własnej funkcji)
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury słów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele
library(pdftools)     # Do czytania plików PDF
library(tidytext)     # Dodatkowe funkcje tekstowe
library(stringi)      # Przetwarzanie stringów

# Funkcja do podstawowego stemmingu polskich słów
simple_polish_stemmer <- function(x) {
  x <- tolower(x)
  # Usuwanie typowych końcówek
  x <- gsub("(owie|ów|owi|em|ami|ach|ami|ach|a|ę|ą|i|y|om|ą|ę|ść|ci|dzie|ści|ż|ź|ń)$", "", x)
  return(x)
}

# Polskie stopwords - ręcznie zdefiniowana lista, uwzględnione też słowa bardzo często pojawiające się w skryptach matematycznych
polish_stopwords <- c("oraz", "czy", "w", "na", "i", "z", "o", "po", "za", "do",
                      "się", "nie", "jest", "jak", "co", "to", "tylko", "ale",
                      "aby", "więc", "gdy", "ponieważ", "który", "która", "które",
                      "jego", "jej", "ich", "ten", "ta", "te", "tamto", "tym",
                      "jestem", "jesteś", "być", "był", "była", "było", "byli",
                      "będzie", "mam", "ma", "mają", "mi", "mnie", "ci", "cię",
                      "mu", "ją", "go", "im", "nią", "niemu", "nich", "nią",
                      "sobie", "soba", "nas", "nami", "wam", "wami", "pan",
                      "pani", "pana", "panią", "oni", "one", "ono", "onego",
                      "onej", "onemu", "onych", "oną", "on", "ona", "ono", "dla", "gdzie", "niech", "jeśli", "wtedy")



# Pobierz listę plików PDF w folderze
pdf_files <- list.files(pattern = "\\.pdf$")

# Wczytaj zawartość plików PDF z obsługą polskich znaków
texts <- lapply(pdf_files, function(x) {
  txt <- pdf_text(x)
  txt <- paste(txt, collapse = " ")
  txt <- stri_encode(txt, to = "UTF-8") # Konwersja do UTF-8
  return(txt)
})

# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(VectorSource(texts))
names(corpus) <- pdf_files  # Nadaj nazwy dokumentom zgodnie z nazwami plików

# 1. Przetwarzanie i oczyszczanie tekstu ----
# Normalizacja i usunięcie zbędnych znaków ----

# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))

# Usuń zbędne znaki specyficzne dla PDF
corpus <- tm_map(corpus, toSpace, "[[:punct:]]")
corpus <- tm_map(corpus, toSpace, "[[:digit:]]")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "\u2022") # bullet points

# Standardowe czyszczenie tekstu
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, polish_stopwords)
corpus <- tm_map(corpus, stripWhitespace)

# Usuń pojedyncze litery które pozostały
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\b\\w{1}\\b", "", x)))
corpus <- tm_map(corpus, stripWhitespace)

# Proste stemmingowanie polskich słów
corpus_stemmed <- tm_map(corpus, content_transformer(simple_polish_stemmer))

# 2. Tworzenie macierzy dokument-term ----
dtm <- DocumentTermMatrix(corpus_stemmed)
dtm_m <- as.matrix(dtm)
rownames(dtm_m) <- pdf_files  # Nazwy dokumentów jako nazwy wierszy

# Usuń wyrazy które występują rzadziej niż w 1 dokumencie
if(length(pdf_files)>1){
  dtm_m <- dtm_m[, colSums(dtm_m > 0) >= 1]
}
# 3. Analiza częstości słów ----
word_freq <- sort(colSums(dtm_m), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq, row.names = NULL)

# Chmura słów
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq,
          min.freq = 1, max.words = 100,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)

# Wykres najczęstszych słów
top_words <- head(word_freq_df, 20)
ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 najczęstszych słów w dokumentach",
       x = "Słowo",
       y = "Częstość") +
  theme_minimal()

# 4. Analiza podobieństwa dokumentów ----
# Ponieważ mamy tylko 2 dokumenty, obliczymy ich podobieństwo
if(length(pdf_files) >2) {
  cosine_sim <- sum(dtm_m[1,] * dtm_m[2,]) /
    (sqrt(sum(dtm_m[1,]^2)) * sqrt(sum(dtm_m[2,]^2)))

  cat("Podobieństwo kosinusowe między dokumentami:", round(cosine_sim, 3), "\n")

  # Wspólne słowa
  common_words <- intersect(names(dtm_m[1, dtm_m[1,] > 0]),
                            names(dtm_m[2, dtm_m[2,] > 0]))
  cat("Liczba wspólnych słów:", length(common_words), "\n")

  # Unikalne słowa dla każdego dokumentu
  unique_words1 <- setdiff(names(dtm_m[1, dtm_m[1,] > 0]), common_words)
  unique_words2 <- setdiff(names(dtm_m[2, dtm_m[2,] > 0]), common_words)

  cat("\nUnikalne słowa w", pdf_files[1], ":", length(unique_words1), "\n")
  cat("Unikalne słowa w", pdf_files[2], ":", length(unique_words2), "\n")

  # Można zapisać te słowa do przeglądnięcia
  writeLines(unique_words1, "unikalne_slowa_dok1.txt")
  writeLines(unique_words2, "unikalne_slowa_dok2.txt")
  writeLines(common_words, "wspolne_slowa.txt")
}
# 5. Graficzne porównanie podobieństwa dokumentów ----

# Funkcja do obliczania podobieństwa kosinusowego
cosine_similarity <- function(x) {
  sim <- tcrossprod(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2))))
  diag(sim) <- 1
  return(sim)
}

similarity_matrix <- cosine_similarity(dtm_m)

# Zamiana podobieństwa na odległość
dist_matrix <- as.dist(1 - similarity_matrix)

# MDS z kontrolą wymiaru k
n_docs <- nrow(dtm_m)
k_dim <- min(2, n_docs - 1)

if (k_dim > 0) {
  mds <- cmdscale(dist_matrix, k = k_dim)

  # Jeśli k=1 (np. 2 dokumenty), dodaj kolumnę zerową, aby był wykres 2D
  if (k_dim == 1) {
    mds <- cbind(mds, 0)
  }

  mds_df <- data.frame(Dim1 = mds[,1], Dim2 = mds[,2], Document = pdf_files)

  library(ggrepel)
  library(ggplot2)

  ggplot(mds_df, aes(x = Dim1, y = Dim2, label = Document)) +
    geom_point(size = 4, color = "darkgreen") +
    geom_text_repel() +
    labs(title = "Mapa podobieństwa dokumentów (MDS)",
         x = "Wymiar 1",
         y = "Wymiar 2") +
    theme_minimal()

} else {
  message("Za mało dokumentów, aby wykonać MDS")
}

# Dendrogram (bazowy wykres R, bez factoextra)
if(length(pdf_files)>2){
  hc <- hclust(dist_matrix, method = "ward.D2")

  plot(hc, labels = pdf_files, main = "Drzewo podobieństwa dokumentów")
  rect.hclust(hc, k = 2)  # Pokazuje podziały na 2 klastry (możesz zmienić k)
}

# Zapis wyników do plików
write.csv(word_freq_df, "czestosc_slow.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(dtm_m, "macierz_dokument_term.csv", fileEncoding = "UTF-8")
write.csv(similarity_matrix, "macierz_podobienstwa.csv", fileEncoding = "UTF-8")
write.csv(mds_df, "mds_wspolrzedne.csv", row.names = FALSE, fileEncoding = "UTF-8")

2+2 #!=4
