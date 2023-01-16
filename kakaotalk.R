# library
library(knitr)
library(tm) # ������ �˻� ������� �ð�ȭ�ϱ� ���� �׷���
library(qgraph) # ������ �˻� ������� �ð�ȭ�ϱ� ���� �׷���
library(qdapRegex) # 
library(KoNLP) # �ѱ� ���¼� �м�
library(stringr) # Regexp�� �ϱ� ���ؼ� �ѱ��� ���ں���, �ܾ�� �߶󳻰� �ٲٴ� ���� ���
library(wordcloud) # ����Ŭ����
library(wordcloud2)
library(RColorBrewer) # ����Ŭ���� ������ ���ڰ� �� �ִ� �ɷ� �˰� ����
library(NIADic)# KoNLP�� �ѱۻ��� �ֽ���. SejongDic���� �ܾ�� ���� ��Ȯ���� ����.
library(googleVis) # ��Ʈ �׸���



# īī���� ��ȭ �ҷ�����
text <- file("��ȭ.txt", encoding = 'UTF-8')
kakaotalk = readLines(text, encoding = 'UTF-8')
head(kakaotalk)
kakaotalk <- kakaotalk[-1:-3] # �ʿ���� �κ� ����
kakaotalk <- str_replace_all(kakaotalk, '�ڵ����� ������ ���� �̸�', '����')
```



# ���� �� ���� ���� �ϴ°�?
me <- length(kakaotalk[grep("\\[��", kakaotalk)])
partner <- length(kakaotalk[grep("\\[����", kakaotalk)])

# data.frame���� ��ȯ
volume <- c(me, partner)  # ī�差
name <- c('��', '����')  # �̸�
kakao_df <- data.frame(name, volume)  # ī�� data.frame���� ��ȯ
str(kakao_df)

# �ð�ȭ
pie <- gvisPieChart(kakao_df, options = list(width = 400, height = 300))

header <- pie$html$header
header <- gsub("charset = UTF-8", "charset = EUC-KR", header)
pie$html$header <- header

plot(pie)


# ������ ���� �߿� ���� �� ���� ���� �ϴ°�?
am <- length(kakaotalk[grep("\\[����", kakaotalk)])  # ����
pm <- length(kakaotalk[grep("\\[����", kakaotalk)])  # ����

volume2 <- c(am, pm)
name2 <- c('����', '����')
time_df <- data.frame(name, volume2)
str(time_df)

# �ð�ȭ
pie1 <- gvisPieChart(time_df, options = list(width = 400, height = 300))

header <- pie1$html$header
header <- gsub("charset = UTF-8", "charset = EUC-KR", header)
pie1$html$header <- header

plot(pie1)


# �м��� ���� ������ ��ó��
prep <- str_replace_all(kakaotalk, "�̸�Ƽ��", "") %>%   # �̸�Ƽ�� ���ֱ�
  str_replace_all("\\[����", "") %>%   # ���� �����
  str_replace_all("\\[����", "") %>%   # ���� �����
  str_replace_all("[��-��]+", "") %>%   # ���� ���ֱ�
  str_replace_all("\\[��", "") %>%   # ��ȭ�� ��� �̸� ���ֱ�(��)
  str_replace_all("\\[����", "") %>%   # ��ȭ�� ��� �̸� ���ֱ�(������)
  str_replace_all("\\[|\\]", "") %>%   # ī�� �ؽ�Ʈ �������� ���ȣ �����
  str_replace_all("[0-9]+:[0-9]+\\]", "") %>%   # ��� �ð� ���ֱ� 
  str_replace_all("����", "") %>%   # ���� ���ֱ�
  str_replace_all("[��-�R]��", "") %>%   # txt�����Ͱ� ���Ϻ��� ������ �ֱ� ����
  str_replace_all("��|��|��", "") %>%   # ������ �����
  str_replace_all("[0-9]+", "") %>%   # ���� �����
  str_replace_all("(http).+(\\w)", "") %>%   # ��ũ �����
  str_replace_all(",+", "") %>%   # �����ȣ �����
  as.character()
head(prep)


# ���� ����
noun1 <- sapply(prep, extractNoun, USE.NAMES = F) %>% unlist()
head(noun1)
noun <- Filter(function(prep){nchar(prep) >= 2}, noun1)   # ������ �̻��� �ܾ ����
head(noun)

nouns <- sort(table(noun), decreasing = T)

# ���� ��
wordFreq <- table(noun)
head(wordFreq)

# ���� �� 50����
wordFreq_top <- head(sort(wordFreq, decreasing = T), 50)
head(wordFreq_top)

print(wordFreq_top)


# ����Ŭ����
wordcloud2(nouns)

# ���縸 ��������
tt <- paste(unlist(SimplePos22(prep)))
allnoun <- str_match_all(tt, "[��-�R]+/[N][C]|[��-�R]+/[N][Q]+") %>% unlist()
N <- str_replace_all(allnoun, "/[N][C]", "") %>%
  str_replace_all("/[N][Q]", "") %>% unlist()   # ����� ����� �ܾ���� �з�ǥ�� /NC, /NQ ���� �����Ѵ�.

CorpusNC <- Corpus(VectorSource(N))
myDtm <- TermDocumentMatrix(CorpusNC, control = list(wordLengths = c(4, 10),
                                                     removePunctuation = T,
                                                     removeNumbers = T,
                                                     weighting = weightBin))
Encoding(myDtm$dimnames$Terms) = "UTF-8"

# Ȯ��
findFreqTerms(myDtm, lowfreq = 10)

myDtmM <- as.matrix(myDtm) # ��ķ� ��ȯ
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