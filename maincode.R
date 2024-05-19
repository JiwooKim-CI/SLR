###install packages
install.packages("rvest")
library(rvest)
library(xml2)

# 크롤링할 웹 페이지 URL 설정
html_file_path <- "HTML_2019.html"
webpage <- read_html(html_file_path)


# 참고문헌 제거
reference_nodes <- xml_find_all(webpage, "//a[starts-with(@id, 'c')]/following-sibling::p[@class='body-paragraph']")

for (node in reference_nodes) {
  xml_remove(node)
}

webpage<-as.character(webpage)
writeLines(webpage,con="webpage_2019.html")


html_file_path_2<-c("webpage_2010.html","webpage_2011.html","webpage_2012.html","webpage_2013.html",
                    "webpage_2014.html","webpage_2015.html","webpage_2016.html","webpage_2017.html",
                    "webpage_2018.html","webpage_2019.html","webpage_2020.html","webpage_2021.html",
                    "webpage_2022.html","webpage_2023.html")

anal <- read_html(html_file_path_2[[10]])

###################################################################
paper_elements <- html_nodes(anal, "dl.print-citation")
num_papers <- length(paper_elements)
output_directory <- "/Users/jiwookim/Documents/R/first_project"
dir.create(output_directory, showWarnings = FALSE)

# 특정 단어
target_word <- "mediation analysis"


# 각 논문을 나타내는 태그 또는 클래스 선택 (예를 들어, "div.paper"라고 가정)
papers <- html_nodes(anal, "div.print-citation")
# 특정 단어를 포함한 논문의 개수를 세기 위한 변수 초기화
target_word_count <- 0
matching_paper_count<-0

# 각 논문에 대한 반복문
for (paper in papers) {
  paper_text <- html_text(paper)
  if (grepl(target_word, paper_text, ignore.case = TRUE)) {
    target_word_count <- target_word_count + 1
  }
}
cat("특정 단어 '", target_word, "'를 포함한 논문의 개수:", target_word_count, "\n")

paper_text <- html_text(papers[[13]])
grepl(target_word, paper_text, ignore.case = TRUE)

citation_divs <- html_nodes(anal, "div.print-citation")

# 각 "div.print-citation" 태그에 대해 처리

html_text(papers[1])

##논문 저장
matching_paper_count <- 0
paper_fnl <- list()
paper_fnl_text <- list()
for (i in 1:length(papers)) {
  paper <- papers[i]
  paper_text <- html_text(paper)
  
  if (grepl(target_word, paper_text, ignore.case = TRUE)) {
    matching_paper_count <- matching_paper_count + 1
    paper_filename <- paste0(output_directory, "paper_", matching_paper_count, ".html")
    paper_fnl <- append(paper_fnl,paper)
    paper_fnl_text<-append(paper_fnl_text,paper_text)
    # 논문을 파일로 저장
#   paper_html <- as.character(paper[[1]])
#    writeLines(paper_html, con = paper_filename)
  }
}
a <- html_text(paper_fnl)
writeLines(paper_fnl_text,con="tmp.html")


subjects <- NULL
DOIs <- NULL
studies <- NULL
paper_text <- NULL
for (i in 1:length(paper_fnl)){

  paper_subtitles <- html_text(html_nodes(paper_fnl[[i]],"a[title^='Study'], a[title^='Experiment']"))
  paper_texts <- html_text(paper_fnl[[i]])
  if (length(paper_subtitles != 0)){
    for (j in 1:length(paper_subtitles)){
      text <- strsplit(paper_texts, paste0("\n ",paper_subtitles[j],"\n"),fixed = TRUE)[[1]][2]
      if (j!=length(paper_subtitles)){
        text <- strsplit(text,paste0("\n ",paper_subtitles[j+1],"\n"),fixed = TRUE)[[1]][1]  
      }
      subjects <- c(subjects, strsplit(paper_texts, "\n",fixed = TRUE)[[1]][2])
      DOIs <- c(DOIs,strsplit(paper_texts, "Digital Object Identifier:",fixed = TRUE)[[1]][2])
      studies <- c(studies, paper_subtitles[j])
      paper_text <- c(paper_text,text)
    }
  }else{
    subjects <- c(subjects, strsplit(paper_texts, "\n",fixed = TRUE)[[1]][2])
    DOIs <- c(DOIs,strsplit(paper_texts, "Digital Object Identifier:",fixed = TRUE)[[1]][2])
    studies <- c(studies, "-")
    paper_text <- c(paper_text,paper_texts)
  }
}

SLR <- cbind(titles = subjects, DOIs, studies, paper_text)



########################################## 여기까지


#### 제목 저장
titles <- NULL
for (i in 1:length(paper_fnl_text)){
  titles <- c(titles,strsplit(paper_fnl_text[[i]], "\n")[[1]][2])
}
SLR_fulltext <- data.frame(titles)

DOIs<-NULL
for (i in 1:length(paper_fnl_text)){
  DOIs <- c(DOIs,strsplit(paper_fnl_text[[i]], "Digital Object Identifier:")[[1]][2])
}
SLR_fulltext <- cbind(SLR_fulltext,DOIs)

#### linear model_fulltext
target_words_model<-c("logistic probit","logistic-probit","log linear","log-linear",
                "multinomial","poisson","negative binomial","cox porportional hazards",
                "additive hazards","non-linear","non linear","generalized additive","loess","polynomial",
                "quadratic","cubic")
paper_fnl2<-list()
model<- matrix(0,nrow=length(paper_fnl_text),ncol=length(target_words_model))

for (i in 1:length(paper_fnl_text)){
  for (j in 1:length(target_words_model)){
  paper <- paper_fnl_text[[i]]
  # paper_text <- html_text(paper)
  if (grepl(target_words_model[[j]], paper, ignore.case = TRUE)) {
    paper_fnl2 <- append(paper_fnl2,paper)
    model[i,j] <- 1
  }
  }
}
model_sum<-rowSums(model)
SLR_fulltext <- cbind(SLR_fulltext,model_sum)

####linear model_by_study
model_1<- matrix(0,nrow=nrow(SLR),ncol=length(target_words_model))

for (i in 1:nrow(SLR)){
  for (j in 1:length(target_words_model)){
    paper <- SLR[i,4]
    if (grepl(target_words_model[j], paper, ignore.case = TRUE)) {
      model_1[i,j] <- 1
    }
  }
}

model_sum_1<-rowSums(model_1)
SLR <- cbind(SLR,model_sum_1)
SLR

###sensitivity analysis
target_word2<-"sensitivity analysis"
paper_fnl2<-list()
sensitivity<- rep(0,length(paper_fnl_text))
for (i in 1:length(paper_fnl_text)){
  paper <- paper_fnl_text[[i]]
  # paper_text <- html_text(paper)
  if (grepl(target_word2, paper, ignore.case = TRUE)) {
    paper_fnl2 <- append(paper_fnl2,paper)
    sensitivity[i] <- 1
  }
}

SLR_fulltext <- cbind(SLR_fulltext,sensitivity)
SLR_fulltext

###NDE,CDE
target_words_DE<-c("natural direct effect","controlled direct effect","natural indirect effect")
paper_fnl2<-list()
DE<- matrix(0,nrow=length(paper_fnl_text),ncol=length(target_words_DE))

for (i in 1:length(paper_fnl_text)){
  for (j in 1:length(target_words_DE)){
    paper <- paper_fnl_text[[i]]
    # paper_text <- html_text(paper)
    if (grepl(target_words_DE[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      DE[i,j] <- 1
    }
  }
}
DE_sum<-rowSums(DE)

SLR_fulltext <- cbind(SLR_fulltext,DE_sum)

###alternative methods
target_words_AM<-c("instrumental variable","front-door","front door")
paper_fnl2<-list()
AM<- matrix(0,nrow=length(paper_fnl_text),ncol=length(target_words_AM))

for (i in 1:length(paper_fnl_text)){
  for (j in 1:length(target_words_AM)){
    paper <- paper_fnl_text[[i]]
    # paper_text <- html_text(paper)
    if (grepl(target_words_AM[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      AM[i,j] <- 1
    }
  }
}
AlternativeMethods_sum<-rowSums(AM)

SLR_fulltext <- cbind(SLR_fulltext,AlternativeMethods_sum)

#### RCT
target_words_RCT<-c('randomised controlled trial','randomized controlled trial',
                     'placebo controlled','randomly allocated','controlled design',
                     'randomly assigned','controlled study','controlled trial',
                     'parallel groups','control groups','double blinded','parallel group',
                     'compared with','double marked','doubleblinded','single masked',
                     'doubleblind','single blind','assigned to','double blind','cross over',
                     'randomised','randomized','crossover','randomly','placebo','trial',
                     'CCT' ,'RCT')###need to revision
paper_fnl2<-list()
RCT<- matrix(0,nrow=length(paper_fnl_text),ncol=length(target_words_RCT))

for (i in 1:length(paper_fnl_text)){
  for (j in 1:length(target_words_RCT)){
    paper <- paper_fnl_text[[i]]
    # paper_text <- html_text(paper)
    if (grepl(target_words_RCT[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      RCT[i,j] <- 1
    }
  }
}
RCT_sum<-rowSums(RCT)

SLR_fulltext <- cbind(SLR_fulltext,RCT_sum)



SLR
write.csv(SLR,file="SLR_2023.csv")
SLR_fulltext
write.csv(SLR_fulltext,file="SLR_fulltext_2023.csv")
####convert html file to pdf
#library(pagedown)
#chrome_print("file:///Users/jiwookim/Desktop/tm/first_projectpaper_1.html", output ="tmp1.pdf")
