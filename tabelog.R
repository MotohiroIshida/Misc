
library(rvest)

baseurl <- "http://tabelog.com/osaka/???/???/???/dtlrvwlst/"
browseURL(baseurl)

pages <- 1:2#サイトのページ数

## idを取得
ids <- character()
for (i in pages){
    target1 <- paste0(baseurl, "COND-0/smp1/?lc=0&rvw_part=all&PG=",i)
    res1 <- read_html(target1)
    ## id を取得し
    res1 %>% html_nodes (xpath ="//span[@class = 'rvw-item__showall-trigger js-show-full-comment']") %>% html_attr("data-review-id") -> tmp # %>% html_text()
    ids <- c(ids,tmp)
}

## urls を生成
urls <- paste0(baseurl, ids)
## urls


## 書き込みページすべてにアクセスし、必要な情報を抽出
contents <- data.frame(id = NULL, cat = NULL, texts = NULL)
for (url in urls){
    print (url)
    try(res2 <- read_html(url))
    id <- res2 %>% html_nodes (xpath = '//span[contains(@class, "lev")]')  %>% html_text () %>% `[`(1)
    if(length(id) < 1) id <- ""
    cat <- res2 %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-profile")]')  %>% html_text ()
    if(length(cat) < 1) cat <- ""
    texts <- res2 %>% html_nodes (xpath = '//div[@class="rvw-item__rvw-comment"]') %>% html_text ()
    if(length(texts) < 1) texts <- ""
    contents <- rbind(contents, data.frame(id = id, cat = cat, texts = texts, stringsAsFactors = FALSE))
}

summary(contents)

# write.csv(contents, file = "contents.csv", row.names = FALSE)

NROW(contents)
NCOL (contents)

library(dplyr); library(stringr)

library(RMeCab); library(magrittr)

## テキスト部分をファイルに保存
write(contents$texts, file = "tabelog.txt")
## 形態素解析
frq <- RMeCabFreq("tabelog.txt")

frq %>% filter (Freq > 2, Info1 %in% c("名詞","形容詞","動詞")) -> frq2
frq2 %<>% filter(!Term %in% c("それ","する","いる" ,"の","なる","よう","ある","ステーキ"))

## ワードクラウド
library(wordcloud)
wordcloud(frq2$Term,frq2$Freq,color = rainbow(10),scale = c(6,1),min.freq = 8)


## ネットワークグラフ
bigram <- NgramDF("tabelog.txt",type = 1,pos =c("名詞","動詞","形容詞"))
## 頻度調整
bigram %<>% filter(Freq > 5)

library(igraph)

bigram2 <- bigram %>% graph.data.frame()

# ラベルサイズの指定
V(bigram2)$label.cex = 2

## 実際にグラフにしてみる
tkplot(bigram2, vertex.label =V(bigram2)$name, edge.label =E(bigram2)$weight , vertex.size = 23,vertex.color = "SkyBlue")


# library(dplyr) ; library(magrittr);library(stringr)
# contents <- read.csv("contents.csv", stringsAsFactors = FALSE)
## 口コミを男女に分ける
contents %>% filter (str_detect(cat, "女性")) -> women
NROW(women)#;write(women$texts, file = "cat/women.txt")
contents %>% filter (str_detect(cat, "男性")) -> men#;write(men$texts, file = "cat/men.txt")

## 男女に分けてクチコミを解析
setwd("~/Dropbox/R/API")

library(RMeCab) ; require(dplyr) ;require (magrittr)
                                                   
cats <- docDF("cat", type = 1, pos = c("名詞","形容詞","動詞"))

# library(dplyr) ; library(magrittr)
head (cats)

## 語彙絞り込み
cats %<>% filter(!POS2 %in% c( "サ変接続", "数"))
head (cats);NROW(cats)
cats$sum <- cats$men.txt + cats$women.txt
cats2 <- cats %>% filter(sum > 5); NROW(cats2)
cats2$TERM[duplicated(cats2$TERM)]
# cats2 %<>% filter(!str_detect(TERM, "店"))
cats2 %<>% filter(! TERM %in% c("の","ん","なる","する","肉", "店","食べる","よう","さ","それ","い","これ","さん","こと","くれる","てる","思う","いる","ある","れる","言う","ない"))
rownames(cats2) <- cats2$TERM
cats2 <- cats2 %>% select(men.txt,women.txt)


# cats.corr <- MASS::corresp(cats2,nf=1)
# barplot(cats.corr)
## 主成分分析
cats.pca <- princomp(cats2)
biplot(cats.pca, cex = 1.3)

library(ggfortify)

autoplot(cats.pca,data= cats2,label = TRUE,loadings = TRUE,  loadings.label = TRUE, loadings.label.size  = 8,label.fontface = 6, loadings.label.lineheight = 0)

plot(cats.pca$scores, type = "n")
text(cats.pca$scores, rownames(cats.pca$scores), cex = 1.4 )

## 男女別特徴語
rownames (cats.pca$scores [cats.pca$scores[,2] > 0,])

rownames (cats.pca$scores [cats.pca$scores[,2] < 0,])

## 語彙検索

library(stringr)

contents$texts [ str_detect (contents$texts, "ワイン") ]


##################################################
## 以下走り書きメモ
## str_subset (contents$texts, "店員")
## contents %>% select(texts) %>% str_detect ("ワイン")  %>% class 
## contents %>% select(texts) %>% as.vector  %>% str_subset("ワイン") # %>% length# class 
## target2 <- "http://tabelog.com/osaka/A2701/A270201/27064826/dtlrvwlst/COND-0/smp1/?lc=0&rvw_part=all&PG=2"
## res2 <- read_html(target2)

## ## id を取得し
## res2 %>% html_nodes (xpath ="//span[@class = 'rvw-item__showall-trigger js-show-full-comment']") %>% html_attr("data-review-id") -> urls2 # %>% html_text()
## 1
## (urls <- c(urls1,urls2))

## ## URLを作成

## urls <- paste0("http://tabelog.com/osaka/A2701/A270201/27064826/dtlrvwlst/", urls)
## urls



## res <- read_html("http://tabelog.com/osaka/A2701/A270202/27005525/dtlrvwlst/")

## ## id を取得し
## res %>% html_nodes (xpath ="//span[@class = 'rvw-item__showall-trigger js-show-full-comment']") %>% html_attr("data-review-id") -> urls # %>% html_text()

## ## URLを作成

## urls <- paste0("http://tabelog.com/osaka/A2701/A270202/27005525/dtlrvwlst/", urls)
## urls


## ##
## contents <- data.frame(id = NULL, cat = NULL, texts = NULL)
## for (url in urls){
##     res2 <- read_html(url)
##     id <- res2 %>% html_nodes (xpath = '//span[contains(@class, "lev")]')  %>% html_text () %>% `[`(1)
##     if(length(id) < 1) id <- ""
##     cat <- res2 %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-profile")]')  %>% html_text ()
##     if(length(cat) < 1) cat <- ""
##     texts <- res2 %>% html_nodes (xpath = '//div[@class="rvw-item__rvw-comment"]') %>% html_text ()
##     if(length(texts) < 1) texts <- ""
##     contents <- rbind(contents, data.frame(id = id, cat = cat, texts = texts, stringsAsFactors = FALSE))
## }
## res %>% html_nodes (xpath = '//span[contains(@class, "lev")]')  %>% html_text () %>% `[`(1)
## res %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-profile")]')  %>% html_text ()
## res %>% html_nodes (xpath = '//div[@class="rvw-item__rvw-comment"]') %>% html_text ()


## library(dplyr)
## # res2 <- res %>% html_nodes(xpath = '//*[@id="column-main"]/div[1]/div[7]/div[2]/p[2]/a[1]/span') %>% html_text()
## res2 <- res %>% html_nodes(xpath = '//*[@id="column-main"]/div[1]')# %>% html_text()

## length(res2[2])

## # res2 %>% html_nodes (xpath = '//*[@property = "v:reviewer"]') %>% html_text ()
## res %>% html_nodes (xpath = 'meta[name=description]') %>% html_text ()

## res2 %>% html_nodes (xpath = '.property  p') %>% html_text ()

## res %>% html_nodes (xpath = '//span[@class = "rvw-item__rvwr-rvwcount count"]') %>% html_text ()
## res %>% html_nodes (xpath = '//span[@class = "rvw-item__rvwr-profile"]') %>% html_text ()

## res %>% html_nodes (xpath = '//strong[@class = "rvw-item__usedprice-price"]') %>% html_text ()

## res2 %>% html_nodes (xpath = '//div[@class="rvw-item__rvw-comment js-rvw-comment"]') %>% html_text ()
## # res %>% html_nodes (xpath = '//div[@id = "v:description"]') %>% html_text ()



## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[3]/div[3]/div[1]') %>% html_text ()

## res %>% html_nodes (xpath = '//span[@class = "lev2"]')  %>% html_text ()
## res %>% html_nodes (xpath = '//span[@class = "lev3"]')  %>% html_text ()
## res %>% html_nodes (xpath = '//span[@class = "lev4"]')  %>% html_text ()

## res %>% html_nodes (xpath = '//span[@class = "lev5"]')  %>% html_text ()

## //*[@id="column-main"]/div[3]/div[3]/div[1]
## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[3]/div[3]/div[1]/p') %>% html_text ()

## res2 %>% html_nodes (xpath = '@rvw-item_rvwr-profile') %>% html_text ()

## # res2 %>% html_nodes (xpath = "//div[7]/div[1]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[7]/div[2]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[7]/div[3]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[8]/div[1]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[8]/div[2]") %>% html_text()
## res2 %>% html_nodes (xpath = "//div[8]/div[3]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[9]/div[1]") %>% html_text()

## res2 %>% html_nodes (xpath = "//div[7]/div[1]") %>% html_text()
## res2 %>% html_nodes (xpath = "//div[7]/div[3]") %>% html_text()


## div.rvw-item:nth-child(7) > div:nth-child(3) > p:nth-child(2) > a:nth-child(1) > span:nth-child(1)




## library(xml2)

## ## x <- read_xml ("yahoo2.xml") ;名前空間なし
## # 名前空間付き
## yahoo  <- read_xml ("http://tabelog.com/osaka/A2701/A270202/27005525/dtlrvwlst/", as_html = TRUE)
## xml_ns(yahoo)

## xml_ns(yahoo)
## ## error ; xml_find_all (yahoo, "//word", "d1")

## ( ns <- xml_ns_rename(xml_ns(yahoo), xsi = "xsi", d1 = "bar"))

## xml_find_all (yahoo, "//v:reviewer")
## xml_find_all (yahoo, "//bar:surface", ns)
## xml_text(  xml_find_all (yahoo, "//bar:surface", ns)  )

## ## xml_name(x) # xml_siblings(x)




## library(rvest)
## res <- read_html("http://tabelog.com/osaka/A2701/A270202/27005525/dtlrvwlst/")

## library(dplyr)

## res %>% html_nodes (xpath = '//span[@class = "rvw-item__rvwr-rvwcount count"]') %>% html_text ()

## res %>% html_nodes (xpath = '//span[@class = "rvw-item__rvwr-profile"]') %>% html_text ()

## res %>% html_nodes (xpath = '//strong[@class = "rvw-item__usedprice-price"]') %>% html_text ()

## res2 %>% html_nodes (xpath = '//div[@class="rvw-item__rvw-comment js-rvw-comment"]') %>% html_text ()




##  res %>% html_nodes (xpath = '//span[@class = "lev1"]')  %>% html_text ()
## res %>% html_nodes (xpath = '//span[@class = "lev2"]')  %>% html_text ()

## res %>% html_nodes (xpath = '//span[@class = "lev1"]|//span[@class = "lev2"]//span[@class = "lev3"]|//span[@class = "lev4"]|//span[@class = "lev5"]')  %>% html_text ()


## res %>% html_nodes (xpath = '//span[contains(@class, "lev")]')  %>% html_text ()

## res %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-name-subtext")]//span')  %>% html_text ()

## res %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-profile")]')  # %>% html_text ()

## res %>% html_nodes (xpath = '//span[contains(@class, "rvw-item__rvwr-profile")]|//span[contains(@class, "rvw-item__rvwr-name-subtext")]') %>% html_text ()

## res %>% html_nodes (xpath = '.rvw-item__rvwr-name-subtext span')  %>% html_text ()


## res %>% html_nodes (xpath = '//span[@class="rvw-item__rvwr-profile"]') #%>% html_text ()


## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[7]/div[2]/p[2]/a[1]/span') %>% html_text()

## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[7]/div[2]/p[2]/span[2]') %>% html_text()

## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[24]/div[2]/p[2]/a/span') %>% html_text()

## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[24]/div[2]/p[2]/a/span[2]') %>% html_text()

## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]') -> res3

## x <- data.frame(id = NULL, attr = NULL, text = NULL)

## for (i in 1:length(xml_children(res3))){
##     selector <- paste0("//*[@id='column-main']/div[1]/div[",i,"]/div[2]/p[2]/a[1]/span")
##     tmp <- res %>% html_nodes (xpath = selector) %>% html_text()
##     if(length(tmp) < 1) next#tmp <- ""
##     selector <- paste0("//*[@id='column-main']/div[1]/div[",i,"]/div[2]/p[2]/span[2]")
##     tmp2 <- res %>% html_nodes (xpath = selector) %>% html_text()
##     if(length(tmp2) < 1) tmp2 <- ""
##     selector <- paste0("//*[@id='column-main']/div[1]/div[",i, "]/div[3]/div[1]/p")
##     tmp3 <- res %>% html_nodes (xpath = selector) %>% html_text()
##     if(length(tmp3) < 1) tmp3 <- ""
##     x <- rbind(x, data.frame(id = tmp, attr = tmp2, text = tmp3))
## }


## ## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[7]/div[3]/div[1]') %>% html_text()
## ## //*[@id="column-main"]/div[1]/div[9]/div[3]/div[1]/text()[1]

## ## //*[@id="column-main"]/div[1]/div[7]/div[3]/div[1]/p

## ## //*[@id="column-main"]/div[3]/div[3]/div[1]/p

## ## //*[@id="column-main"]/div[3]/div[3]/div[1]/p


## ## //*[@id="column-main"]/div[3]/div[3]/div[1]

## ## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[3]/div[3]/div[1]') %>% html_text()
## rvw-item__rvw-comment js-rvw-comment
## //*[@id="column-main"]/div[1]/div[9]/div[3]/div[1]/span
## //*[@id="column-main"]/div[1]/div[9]/div[3]/div[1]


## res %>% html_nodes (xpath = '//*[@id="column-main"]/div[1]/div[18]/div[3]/div/span') %>% html_attr("data-review-id")


## res %>% html_nodes (xpath ="//span[@class = 'rvw-item__showall-trigger js-show-full-comment']") %>% html_attr("data-review-id") # %>% html_text()
