# char1 
## unnest_tokens
text <- c('because i could not stop for death',
          'he kindly stopped for me',
          'the he held but just ourselves',
          'and immortality')
library(dplyr)
library(tidytext)
text_df <- data_frame(line=1:4,text = text)
text_df %>% unnest_tokens(word,text)


library(janeaustenr)
library(stringr)
original_books <- austen_books() %>% dplyr::group_by(book) %>% mutate(linenumber = row_number(),chapter = cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore_case = TRUE))))%>% ungroup()

tidy_books <- original_books %>% unnest_tokens(word,text)

data("stop_words")
ttidy_books <- tidy_books %>% anti_join(stop_words)

tidy_books %>% count(word,sort = TRUE)

library(ggplot2)
tidy_books %>% count(word,sort = TRUE) %>% filter(n>600) %>% mutate(word = reorder(word,n)) %>% ggplot(aes(word,n))+geom_col()+xlab(NULL)+coord_flip()

library(gutenbergr)
