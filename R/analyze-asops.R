
library(stringr)
library(tibble)
library(dplyr)
library(purrr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(topicmodels)
library(flextable)
library(locktonr)
library(officer)


locktonr::gg_set_lockton_theme()

# ---------- import asops -------------------
asops <- readRDS("ASOPS/asops_pc.rds") %>%
  select(number, title, text = text2)

# ---------- text analysis -------------------
asops_words <- asops %>%
  unnest(text) %>%
  group_by(number) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

asops_words_ex_actuary <- asops_words %>%
  filter(!(word %in% c("actuary","actuarial", "actuary’s", "actuaries",
                       "standard","asop","section")))

asops_words_ex_actuary <- asops_words_ex_actuary %>%
  mutate(word = if_else(word == "risks", "risk", word)) %>%
  mutate(word = if_else(str_sub(word, -1)=="s",str_sub(word, 1,-2), word))

asops_word_count <- asops_words_ex_actuary %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)

asops_count <- asops_words_ex_actuary %>%
  count(number, word, sort = TRUE) %>%
  filter(n > 10)

asops_words_tf <- asops_count %>%
  bind_tf_idf(word, number, n)

asops_words_tf %>%
  arrange((tf_idf))

asops_words_tf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(number) %>%
  top_n(5) %>%
  ungroup() %>%
  filter(number == 47) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  # facet_wrap(~number, ncol = 2, scales = "free") +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16))

# LDA
asops_dtm <- asops_count %>%
  cast_dtm(number, word, n)

asops_lda <- LDA(asops_dtm, k = 5, control = list(seed = 12))
asops_topics <- tidy(asops_lda, matrix = "beta")
asops_topics

top_terms <- asops_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

assignments <- augment(asops_lda, data = asops_dtm)
assignments

asops_docs <- tidy(asops_lda, matrix = "gamma")
asops_docs

# ---------- n-gram -------------------
asops_ngram <- asops %>%
  unnest(text) %>%
  group_by(number) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

asops_trigram <- asops %>%
  unnest(text) %>%
  group_by(number) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3)


bigrams_separated <- asops_ngram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  drop_na()

trigrams_separated <- asops_trigram %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  drop_na()


# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 10)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ", remove = FALSE) %>%
  inner_join(unite(bigram_counts, bigram, word1, word2, sep = " "), by = "bigram")

bigrams_united %>%
  arrange(desc(n))

bigrams_united %>%
  filter(word1 == "risk") %>%
  count(title, bigram, sort = TRUE)

bigrams_united %>%
  filter(bigram == "uncertainty") %>%
  count(title, bigram, sort = TRUE)

trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE) %>%
  filter(n > 5)

trigram_counts

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ", remove = FALSE) %>%
  inner_join(unite(trigram_counts, trigram, word1, word2, word3, sep = " "), by = "trigram")

trigrams_united %>%
  arrange(desc(n))


# ---------- exhibits -------------------

ft_asops <- asops %>%
  distinct(number, title) %>%
  mutate(number = number_format(number)) %>%
  regulartable() %>%
  ft_lockton_theme()

gg_words <- asops_word_count %>%
  slice(1:10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(y = n, x = word)) +
  geom_col() +
  geom_text(aes(label = n, y = n + 40), size = 5) +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Top 10 words from ASOPs")

gg_bi <- bigrams_united %>%
  arrange(desc(n)) %>%
  distinct(bigram, n) %>%
  slice(1:10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(y = n, x = bigram)) +
  geom_col() +
  geom_text(aes(label = n, y = n + 10), size = 5) +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16)) +
  ggtitle("Top 10 bi-grams from ASOPs")

ft_uncertainty <- asops_words_ex_actuary %>%
  filter(word == "uncertainty") %>%
  distinct(number, title, word) %>%
  mutate(number = number_format(number)) %>%
  regulartable() %>%
  ft_lockton_theme()



pptx <- officer::read_pptx("template.pptx")

pptx <- pptx  %>%

  add_slide("Title and Content", "1_Template with examples_US letter") %>%
  ph_with_text("ngram", type = "title") %>%
  ph_with_gg(gg_words, type = "body") %>%

  add_slide("Title and Content", "1_Template with examples_US letter") %>%
  ph_with_text("bigram", type = "title") %>%
  ph_with_gg(gg_bi, type = "body") %>%

  add_slide("Title and Content", "1_Template with examples_US letter") %>%
  ph_with_text("bigram", type = "title") %>%
  ph_with_flextable(ft_asops, type = "body") %>%
  ph_with_flextable(ft_uncertainty, type = "body")


print(pptx, target = paste0("slides",
                            stringr::str_replace_all(Sys.time(),":","."), ".pptx")) %>% invisible()


