###import packages

library(rvest)
library(xml2)
library(R3port)
library(pagedown)

# set URL for crawling
webpage <- read_html("HTML_2024.html")

# deleting references
reference_nodes <- xml_find_all(webpage, "//a[starts-with(@id, 'c')]/following-sibling::p[@class='body-paragraph']")

for (node in reference_nodes) {
  xml_remove(node)
}

anal<-as.character(webpage)|>
  read_html()
###################################################################
## counting 'mediation analysis'
# set target word
target_words <- c(" mediate", "Baron & Kenny", "indirect effect", "Sobel", "mediation", "Baron and Kenny")
# counting total paper numbers
papers <- html_nodes(anal, "div.print-citation")
paper_without_content <- html_nodes(anal, "dl.print-citation")

target_word_count <- 0

# iteration
for (i in 1:length(papers)) {
  paper_text <- html_text(papers[i])
  paper_subtitles <- html_text(html_nodes(papers[i], "a[title^='Study'], a[title^='Studies'], a[title^='Experiment'], a[title^='Experiments']"))
  paper_discussion <- html_text(html_nodes(papers[i], "a[title^='Discussion'], a[title^='General Discussion']"))
  
  text <- strsplit(paper_text, paste0("\n ", paper_subtitles[1], "\n"), fixed = TRUE)[[1]][2] 
  text <- strsplit(text, paste0("\n ", paper_discussion[length(paper_discussion)], "\n"), fixed = TRUE)[[1]][1]
  
  if (any(grepl(paste(target_words, collapse = "|"), text, ignore.case = TRUE))) {
    target_word_count <- target_word_count + 1
  }
}

cat("Papers that include words '", target_words, "'are:", target_word_count, "\n")

## save papers
paper_fnl <- list()
paper_fnl_text <- list()

for (i in 1:length(papers)) {
  paper <- papers[i]
  paper_text <- html_text(papers[i])
  paper_subtitles <- html_text(html_nodes(papers[i],"a[title^='Study'], a[title^='Studies'], a[title^='Experiment'], a[title^='Experiments']"))
  paper_discussion <- html_text(html_nodes(papers[i],"a[title^='Discussion'], a[title^='General Discussion']"))
  text <- strsplit(paper_text,paste0("\n ",paper_subtitles[1],"\n"),fixed = TRUE)[[1]][2] 
  text <- strsplit(text, paste0("\n ",paper_discussion[length(paper_discussion)],"\n"),fixed = TRUE)[[1]][1]
  if (any(grepl(paste(target_words, collapse = "|"), text, ignore.case = TRUE))) {
    title <- strsplit(paper_text, "\n",fixed = TRUE)[[1]][2]
    title <- strsplit(title, "/",fixed = TRUE)[[1]][1]
    paper_filename <- paste0("paper_", title, ".html")
    paper_filename <- gsub(" ", "_", paper_filename)
    paper_pdfname <- paste0("paper_", title, ".pdf")
    paper_fnl <- append(paper_fnl,paper)
    paper_fnl_text<-append(paper_fnl_text,paper_text)
    # save each paper as a html file
    #paper_html <- as.character(paper[[1]])
    #writeLines(paper_html, con = paper_filename)
  }
}

####convert html file to pdf

html_folder <- getwd()
pdf_folder <-  getwd()
xml_folder <-  getwd()

html_files <- list.files(html_folder, pattern = "^paper_.*\\.html$", full.names = TRUE)


for (html_file in html_files) {
  
  pdf_file <- file.path(pdf_folder, sub(".html$", ".pdf", basename(html_file)))
  
  try({
    chrome_print(
      input = html_file,
      output = pdf_file
    )
  }, silent = TRUE)
}

xml_output <- "output.xml"

# XML 파일을 만들기 위한 빈 XML 객체 생성
xml_doc <- xml2::xml_new_document()

# HTML 파일을 읽어서 XML에 추가
for (html_file in html_files) {
  # HTML 파일 읽기
  html_content <- read_html(html_file)
  
  # 필요한 정보 추출 (예시: 모든 링크 추출)
  links <- html_content %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # XML에 정보 추가
  for (link in links) {
    xml2::xml_add_child(xml_doc, "link", link)
  }
}

# XML 파일 저장
xml2::write_xml(xml_doc, xml_output)
###################################################################
####automation
#### dividing each paper by its studies
SLR <- NULL
SLR_fulltext <- NULL

subjects <- NULL
DOIs <- NULL
Field <- NULL
studies <- NULL
paper_text <- NULL
for (i in 1:length(paper_fnl)){

  paper_subtitles <- html_text(html_nodes(paper_fnl[[i]],"a[title^='Study'], a[title^='Studies'], a[title^='Experiment'], a[title^='Experiments']"))
  paper_discussion <- html_text(html_nodes(paper_fnl[[i]],"a[title^='Discussion'], a[title^='General Discussion']"))
  paper_texts <- html_text(paper_fnl[[i]])
  if (length(paper_subtitles != 0)){
    for (j in 1:length(paper_subtitles)){
      text <- strsplit(paper_texts, paste0("\n ",paper_subtitles[j],"\n"),fixed = TRUE)[[1]][2]
      if (j!=length(paper_subtitles)){
        text <- strsplit(text,paste0("\n ",paper_subtitles[j+1],"\n"),fixed = TRUE)[[1]][1]  
      }
      else {
        text <- strsplit(text, paste0("\n ",paper_discussion[length(paper_discussion)],"\n"),fixed = TRUE)[[1]][1]
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
paper_text

SLR <- cbind(titles = subjects, DOIs, studies, paper_text)

########################################## 

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

Field<-NULL
for (i in 1:length(paper_fnl_text)){
  Field <- c(Field,strsplit(paper_texts, "PsycInfo Classification:",fixed = TRUE)[[1]][2])
}
SLR_fulltext <- cbind(SLR_fulltext,Field)
#### search keywords

#### mediation_by study

target_words_medi<-c(" mediate", "Baron & Kenny", "indirect effect", "Sobel", "mediation", "Baron and Kenny")
medi_1<- matrix(0,nrow=nrow(SLR),ncol=length(target_words_medi))

for (i in 1:nrow(SLR)){
  for (j in 1:length(target_words_medi)){
    paper <- SLR[i,4]
    if (grepl(target_words_medi[j], paper, ignore.case = TRUE)) {
      medi_1[i,j] <- 1
    }
  }
}

medi_sum_1<-rowSums(medi_1)
SLR <- cbind(SLR,medi_sum_1)
SLR



#### linear model_fulltext
target_words_model<-c("logistic probit","logistic-probit","log linear","log-linear",
                "multinomial","poisson","negative binomial","cox proportional hazards",
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

target_word2<-"sensitivity analysis"
paper_fnl2<-list()
sensitivity<- rep(0,length(SLR))
for (i in 1:nrow(SLR)){
  paper <- paper <- SLR[i,4]
  # paper_text <- html_text(paper)
  if (grepl(target_word2, paper, ignore.case = TRUE)) {
    paper_fnl2 <- append(paper_fnl2,paper)
    sensitivity[i] <- 1
  }
}

SLR <- cbind(SLR,sensitivity)

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

target_words_DE<-c("natural direct effect","controlled direct effect","natural indirect effect")
paper_fnl2<-list()
DE<- matrix(0,nrow=length(paper_fnl_text),ncol=length(target_words_DE))

for (i in 1:nrow(SLR)){
  for (j in 1:length(target_words_DE)){
    paper <- SLR[i,4]
    # paper_text <- html_text(paper)
    if (grepl(target_words_DE[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      DE[i,j] <- 1
    }
  }
}
DE_sum<-rowSums(DE)

SLR <- cbind(SLR,DE_sum)

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

target_words_AM<-c("instrumental variable","front-door","front door")
paper_fnl2<-list()
AM<- matrix(0,nrow=nrow(SLR),ncol=length(target_words_AM))

for (i in 1:nrow(SLR)){
  for (j in 1:length(target_words_AM)){
    paper <- SLR[i,4]
    # paper_text <- html_text(paper)
    if (grepl(target_words_AM[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      AM[i,j] <- 1
    }
  }
}
AlternativeMethods_sum<-rowSums(AM)

SLR <- cbind(SLR,AlternativeMethods_sum)

#### RCT
target_words_RCT<-c('randomised controlled trial','randomized controlled trial',
                    'controlled design','randomly assigned','controlled study','controlled trial',
                     'randomised','randomized','randomly',
                     'CCT' ,'RCT')
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

paper_fnl2<-list()
RCT<- matrix(0,nrow=nrow(SLR),ncol=length(target_words_RCT))

for (i in 1:length(paper_fnl_text)){
  for (j in 1:length(target_words_RCT)){
    paper <- SLR[i,4]
    # paper_text <- html_text(paper)
    if (grepl(target_words_RCT[[j]], paper, ignore.case = TRUE)) {
      paper_fnl2 <- append(paper_fnl2,paper)
      RCT[i,j] <- 1
    }
  }
}
RCT_sum<-rowSums(RCT)

SLR <- cbind(SLR,RCT_sum)

SLR_fulltext_1 <- SLR_fulltext
SLR_1 <- SLR

write.csv(SLR,file="SLR_bystudy_2024.csv")

write.csv(SLR_fulltext,file="SLR_fulltext_2024.csv")

