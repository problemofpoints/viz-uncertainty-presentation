

library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(purrr)

# ---------- scrape asops from web -------------------

# get links
asop_links <- read_html("http://www.actuarialstandardsboard.org/standards-of-practice/") %>%
  html_node(".blog") %>%
  html_nodes("div a") %>%
  html_attr("href")
asop_links <- unique(asop_links)
asop_links <- asop_links[str_detect(asop_links, ".pdf", negate = TRUE)]

get_asop_number <- function(html){

  as.numeric(tail(str_split(html %>% html_nodes("h1") %>% html_text(), " ")[[1]], 1))

}

# get text
asops <- tibble(url = asop_links) %>%
  mutate(html = map(url, ~ read_html(.x) %>% html_node(".pf-content")))

asops <- asops %>%
  mutate(number = map_dbl(html, possibly(get_asop_number, NA_real_))) %>%
  mutate(text = map(html, ~ .x %>% html_nodes("p") %>% html_text())) %>%
  mutate(title = map_chr(text, ~ str_trim(.x[if_else(.x[1]=="" | .x[1]=="Revised Edition", 2, 1)])))

# just P&C asops
p_and_c <- c(7, 9, 13, 19, 20, 29, 30, 36, 38, 39, 43, 53, 46, 47, 55, 12, 23, 25, 41)

asops_pc <- asops %>%
  filter(number %in% p_and_c)

# attempt to remove boilerplate text at beginning
str_to_find <- "The ASB establishes and improves standards of actuarial practice.
These ASOPs identify what the actuary should consider, document, and disclose when performing an actuarial assignment.
The ASB’s goal is to set standards for appropriate practice for the U.S."

asops_pc <- asops_pc %>%
  mutate(text2 = map(text, ~ .x[ifelse(length(str_which(.x, str_to_find))==0, 30, str_which(.x, str_to_find)+1):length(.x)]))

# save tibble
asops_pc %>%
  select(-html) %>%
  saveRDS("ASOPS/asops_pc.rds")
