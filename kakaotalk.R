# library
library(knitr)
library(tm) # ¿¬°ü¼º °Ë»ç °á°ú°ªÀ» ½Ã°¢È­ÇÏ±â À§ÇÑ ±×·¡ÇÁ
library(qgraph) # ¿¬°ü¼º °Ë»ç °á°ú°ªÀ» ½Ã°¢È­ÇÏ±â À§ÇÑ ±×·¡ÇÁ
library(qdapRegex) # 
library(KoNLP) # ÇÑ±Û ÇüÅÂ¼Ò ºĞ¼®
library(stringr) # Regexp¸¦ ÇÏ±â À§ÇØ¼­ ÇÑ±ÛÀ» ¹®ÀÚº°·Î, ´Ü¾îº°·Î Àß¶ó³»°í ¹Ù²Ù´Â ÀÏÀ» ´ã´ç
library(wordcloud) # ¿öµåÅ¬¶ó¿ìµå
library(wordcloud2)
library(RColorBrewer) # ¿öµåÅ¬¶ó¿ìµå »ö±òÀ» ¿¹»Ú°Ô ÇØ ÁÖ´Â °É·Î ¾Ë°í ÀÖÀ½
library(NIADic)# KoNLPÀÇ ÇÑ±Û»çÀü ÃÖ½ÅÆÇ. SejongDicº¸´Ù ´Ü¾î·®µµ ¸¹°í Á¤È®µµµµ ³ô´Ù.
library(googleVis) # Â÷Æ® ±×¸®±â



# Ä«Ä«¿ÀÅå ´ëÈ­ ºÒ·¯¿À±â
text <- file("´ëÈ­.txt", encoding = 'UTF-8')
kakaotalk = readLines(text, encoding = 'UTF-8')
head(kakaotalk)
kakaotalk <- kakaotalk[-1:-3] # ÇÊ¿ä¾ø´Â ºÎºĞ Á¦°Å
kakaotalk <- str_replace_all(kakaotalk, 'ÇÚµåÆù¿¡ ÀúÀåÇÑ »ó´ë¹æ ÀÌ¸§', 'º»¸í')
```



# ´©°¡ ´õ ¸¹ÀÌ ÅåÀ» ÇÏ´Â°¡?
me <- length(kakaotalk[grep("\\[³ª", kakaotalk)])
partner <- length(kakaotalk[grep("\\[»ó´ë¹æ", kakaotalk)])

# data.frameÀ¸·Î º¯È¯
volume <- c(me, partner)  # Ä«Åå·®
name <- c('³ª', '»ó´ë¹æ')  # ÀÌ¸§
kakao_df <- data.frame(name, volume)  # Ä«Åå data.frameÀ¸·Î º¯È¯
str(kakao_df)

# ½Ã°¢È­
pie <- gvisPieChart(kakao_df, options = list(width = 400, height = 300))

header <- pie$html$header
header <- gsub("charset = UTF-8", "charset = EUC-KR", header)
pie$html$header <- header

plot(pie)


# ¿ÀÀü°ú ¿ÀÈÄ Áß¿¡ ¾ğÁ¦ ´õ ¸¹Àº ÅåÀ» ÇÏ´Â°¡?
am <- length(kakaotalk[grep("\\[¿ÀÀü", kakaotalk)])  # ¿ÀÀü
pm <- length(kakaotalk[grep("\\[¿ÀÈÄ", kakaotalk)])  # ¿ÀÈÄ

volume2 <- c(am, pm)
name2 <- c('¿ÀÀü', '¿ÀÈÄ')
time_df <- data.frame(name, volume2)
str(time_df)

# ½Ã°¢È­
pie1 <- gvisPieChart(time_df, options = list(width = 400, height = 300))

header <- pie1$html$header
header <- gsub("charset = UTF-8", "charset = EUC-KR", header)
pie1$html$header <- header

plot(pie1)


# ºĞ¼®À» À§ÇÑ µ¥ÀÌÅÍ ÀüÃ³¸®
prep <- str_replace_all(kakaotalk, "ÀÌ¸ğÆ¼ÄÜ", "") %>%   # ÀÌ¸ğÆ¼ÄÜ ¾ø¾Ö±â
  str_replace_all("\\[¿ÀÈÄ", "") %>%   # ¿ÀÈÄ Áö¿ì±â
  str_replace_all("\\[¿ÀÀü", "") %>%   # ¿ÀÀü Áö¿ì±â
  str_replace_all("[¤¡-¤¾]+", "") %>%   # ÀÚÀ½ ¾ø¾Ö±â
  str_replace_all("\\[³ª", "") %>%   # ´ëÈ­¹æ »ç¶÷ ÀÌ¸§ ¾ø¾Ö±â(³ª)
  str_replace_all("\\[»ó´ë¹æ", "") %>%   # ´ëÈ­¹æ »ç¶÷ ÀÌ¸§ ¾ø¾Ö±â(»çÃÌÇü)
  str_replace_all("\\[|\\]", "") %>%   # Ä«Åå ÅØ½ºÆ® µ¥ÀÌÅÍÀÇ ´ë°ıÈ£ Áö¿ì±â
  str_replace_all("[0-9]+:[0-9]+\\]", "") %>%   # ¸ğµç ½Ã°£ ¾ø¾Ö±â 
  str_replace_all("»çÁø", "") %>%   # »çÁø ¾ø¾Ö±â
  str_replace_all("[°¡-ÆR]¿ä", "") %>%   # txtµ¥ÀÌÅÍ°¡ ¿äÀÏº°·Î ³ª´²Á® ÀÖ±â ¶§¹®
  str_replace_all("³â|¿ù|ÀÏ", "") %>%   # ¿¬¿ùÀÏ Áö¿ì±â
  str_replace_all("[0-9]+", "") %>%   # ¼ıÀÚ Áö¿ì±â
  str_replace_all("(http).+(\\w)", "") %>%   # ¸µÅ© Áö¿ì±â
  str_replace_all(",+", "") %>%   # ¹®ÀåºÎÈ£ Áö¿ì±â
  as.character()
head(prep)


# ¸í»ç ÃßÃâ
noun1 <- sapply(prep, extractNoun, USE.NAMES = F) %>% unlist()
head(noun1)
noun <- Filter(function(prep){nchar(prep) >= 2}, noun1)   # µÎÀ½Àı ÀÌ»óÀÇ ´Ü¾î¸¸ ÃßÃâ
head(noun)

nouns <- sort(table(noun), decreasing = T)

# ¸í»ç ºóµµ
wordFreq <- table(noun)
head(wordFreq)

# ¸í»ç ºóµµ 50¼øÀ§
wordFreq_top <- head(sort(wordFreq, decreasing = T), 50)
head(wordFreq_top)

print(wordFreq_top)


# ¿öµåÅ¬¶ó¿ìµå
wordcloud2(nouns)

# ¸í»ç¸¸ °¡Á®¿À±â
tt <- paste(unlist(SimplePos22(prep)))
allnoun <- str_match_all(tt, "[°¡-ÆR]+/[N][C]|[°¡-ÆR]+/[N][Q]+") %>% unlist()
N <- str_replace_all(allnoun, "/[N][C]", "") %>%
  str_replace_all("/[N][Q]", "") %>% unlist()   # ¸í»ç·Î ÃßÃâµÈ ´Ü¾îµéÀÇ ºĞ·ùÇ¥ÀÎ /NC, /NQ µîÀ» Á¦°ÅÇÑ´Ù.

CorpusNC <- Corpus(VectorSource(N))
myDtm <- TermDocumentMatrix(CorpusNC, control = list(wordLengths = c(4, 10),
                                                     removePunctuation = T,
                                                     removeNumbers = T,
                                                     weighting = weightBin))
Encoding(myDtm$dimnames$Terms) = "UTF-8"

# È®ÀÎ
findFreqTerms(myDtm, lowfreq = 10)

myDtmM <- as.matrix(myDtm) # Çà·Ä·Î º¯È¯
myrowDtmM <- rowSums(myDtmM)
myDtmM.order <- myrowDtmM[order(myrowDtmM, decreasing = T)]
freq.wordsNC <- sample(myDtmM.order[myDtmM.order > 25], 20)
freq.wordsNC <- as.matrix(freq.wordsNC)
freq.wordsNC
co.matrix <- freq.wordsNC %*% t(freq.wordsNC)

# qgraph
qgraph(co.matrix,
       labels = rownames(co.matrix),
       diag = F,
       layout = 'spring',
       vsize = log(diag(co.matrix)))