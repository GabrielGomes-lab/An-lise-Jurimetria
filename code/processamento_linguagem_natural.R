# Análise textual dos dados

# Listamos os pacotes que precisamos
packages = c("quanteda", "quanteda.textmodels", "quanteda.textstats", "quanteda.textplots",
             "newsmap", # para classificar documentos, com base em “seed words”
             "readtext", # para ler diferentes formatos de texto
             "spacyr", # para anotação de classes gramaticais, reconhecimento de entidades e anotação sintática (python deve estar instalado)
             "ggplot2", #para gráfico simples das frequências
             "seededlda", # para modelagem de tópico
             "stringr", # para as expressões regulares
             "jsonlite",
             "dplyr",
             "quanteda.corpora",
             "FactoMineR",
             "factoextra",
             "flextable",
             "GGally",
             "ggdendro",
             "Matrix",
             "tm",
             "sna",
             "stringi",)

# Instalamos (se necessário) e carregamos os pacotes
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      require(x, character.only = TRUE)
    }
  }
)

#Abrimos nosso arquivo com os textos
arq_textos <- readtext("C:/Users/Gabriel/Documents/txt_priseg", encoding = "utf-8")

# Agora vamos fazer uma primeira limpeza nos arquivos
arq_textos <- arq_textos %>%
  mutate(text = str_replace_all(text, "\\t", "")) %>%
  mutate(text = str_replace_all(text, "\n", " ")) %>%  # Remover quebras de linha
  mutate(text = str_replace_all(text, "\\s+", " ")) %>% # Remover múltiplos espaços
  mutate(text = str_replace_all(text, "\\b\\d+\\b", "")) %>% # Remover números
  mutate(text = str_replace_all(text, "[^[:alnum:][:space:]]", "")) %>% # Remover pontuações e caracteres especiais
  mutate(text = stri_trans_general(text, "latin-ascii")) %>%
  mutate(text = str_to_lower(text)) %>%
  mutate(text = str_trim(text, side = c("both"))) # Remover espaços extras no início e no final


# Agora vamos transformar em corpus ---------
corpus_df <- corpus(arq_textos)

# Agora vamos tokenizar os três dataframes
toks_df <- tokens(corpus_df)
toks_df <- tokens(corpus_df, remove_symbols = TRUE, remove_punct = TRUE)


# Remover stopwords
toks_df <- tokens_select(toks_df,
                         pattern = c(stop_words2 ,stopwords("pt")),
                         selection = "remove")

toks_df2 <- tokens_select(toks_df,
                          pattern = c(stop_words2 ,stopwords("pt")),
                          selection = "remove")

# Fazemos um dfm
dfm_unico <- dfm(toks_df2)

#lematização
lemma_dic <- read.delim(file = "C:/Users/Gabriel/Downloads/lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE)
names(lemma_dic) <- c("stem", "term")

lemma_dic <- lemma_dic %>%
  mutate(term = stri_trans_general(term, "latin-ascii")) %>%
  mutate(stem = stri_trans_general(stem, "latin-ascii"))

for (j in 1:length(dfm_unico@Dimnames$features)){
  comparacao <- dfm_unico@Dimnames$features[j] == lemma_dic$term
  if (sum(comparacao) == 1){
    dfm_unico@Dimnames$features[j] <- as.character(lemma_dic$stem[comparacao])
  } else {
    dfm_unico@Dimnames$features[j] <- dfm_unico@Dimnames$features[j]
  }
}


# Em uma primeira análise vamos encontrar a document feature matrix
topfeatures(dfm_unico, 10) #Encontra as top 10 palavras

# Núvem de plvras
set.seed(100) #para reprodução dos resultados
textplot_wordcloud(dfm_unico, min_count = 6, random_order = FALSE, rotation = .25, color = RColorBrewer::brewer.pal(8, "Dark2"))

# Número de ocorrencias mais frequentes
dfm_unico %>%
  textstat_frequency(n = 20) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequência") +
  theme_minimal()

# Topic Modeling (LDA) -----
set.seed(100)
tm_nostop <- textmodel_lda(dfm_unico, k = 8)
terms(tm_nostop, 10)

#Semantic Network ------
#criar fcm a partir de dfm
fcm_nostop <- fcm(dfm_unico)
# listar as top features
feat <- names(topfeatures(fcm_nostop, 50))
#selecionar
fcm_select <- fcm_select(fcm_nostop, pattern = feat, selection = "keep")

size <- log(colSums(dfm_select(dfm_unico, feat, selection = "keep")))

textplot_network(fcm_select, min_freq = 0.1, vertex_size = size / max(size) * 3, vertex_labelsize = 10)


# n-gramas
# Encontrar quais os cid que aparecem na base

ngram_kwic <- kwic(toks_df, pattern = c("cid"), valuetype = c("fixed"), window = 1) %>%
  as.data.frame() %>%
  dplyr::select(-to, -from, -pattern)


