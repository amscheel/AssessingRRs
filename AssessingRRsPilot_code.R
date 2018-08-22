
## Import dataset
install.packages("rio", dependencies = TRUE)
library(rio)
x <- import("AssessingRRsPilot_data.csv")

## Delete all invalid/test sessions
invalid.session <- c(x[x$item_name=="ARTICLE_NAME" & x$answer=="","session"],
                     x[x$item_name=="ARTICLE_NAME" & x$answer=="x","session"],
                     x[x$item_name=="ARTICLE_NAME" & x$answer=="y","session"],
                     x[x$item_name=="ARTICLE_NAME" & x$answer=="a new name","session"],
                     x[x$item_name=="ARTICLE_NAME" & x$answer=="testrun1","session"],
                     x[x$item_name=="ARTICLE_NAME" & x$answer=="testrun2","session"])
x <- x[-grep((paste(invalid.session,collapse="|")), x$session),]

## standardise all DOI entries to have one reliable identifier for each paper
x[x$item_name=="ARTICLE_DOI", "answer"] <- gsub("^.*?1","1", x[x$item_name=="ARTICLE_DOI", "answer"])

## remove all rows of variables that haven't been shown
x <- x[x$shown!="",]




## Create a new variable to be able to identify each occurence of variables with several entries (e.g. several hypotheses)
sessions <- unique(x$session)
item <- unique(x$item_name)
x$repeat.var <- NA

for (i in 1:length(sessions)) {
  for (j in 1:length(item)) {
    repeat.seq <- seq(from=1, to=nrow(x[x$session==sessions[i] & x$item_name==item[j], ]))
    x$repeat.var[which(x$session==sessions[i] & x$item_name==item[j])] <- repeat.seq
  }
}

x$study.no <- NA
x$hyp.no <- NA
x$result.no <- NA


for (i in 1:length(sessions)) {
  
  ## Session 3 is incomplete because the paper shouldn't be in the sample. This bit makes sure it is skipped.
  if(tail(x[which(x$session==sessions[i]), "item_name"], 1) != "END_NOTE2") next
  
  max.study <- length(x$repeat.var[which(x$session == sessions[i] & x$item_name=="STUDY_NO")])
  
  a <- 0 # this is a helper variable to identify new hypotheses in papers with >1 study 
         # (because repeat.var keeps ticking up and hyp.no resets when study.no changes)
  b <- 0
  
  for (j in 1:max.study) {
        x$study.no[
                    ## This indexes the row number of the first row to be named after the current study
                    (which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat.var == j)):
                    ## This indexes the row number of the last row to be named after the current study:
                    ## if the current study is not the last, the last row is one above the next HYP_NO,
                    ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
                    (ifelse(j < max.study, 
                           which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat.var == j+1) -1,
                           which(x$session == sessions[i] & x$item_name == "EXPLOR_RESEARCHQ") -1))
                  ] <- j
        max.hyp <- length(x$repeat.var[which(x$session == sessions[i] & x$study.no == j & x$item_name=="HYP_NO")])
        
        for (k in 1:max.hyp) {
          
          a <- a + 1
          
          x$hyp.no[
            ## This indexes the row number of the first row to be named after the current study
            (which(x$session == sessions[i] & x$study.no == j & x$item_name == "HYP_NO" & x$repeat.var == a)):
              ## This indexes the row number of the last row to be named after the current study:
              ## if the current study is not the last, the last row is one above the next HYP_NO,
              ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
              (ifelse(k < max.hyp, 
                      which(x$session == sessions[i] & x$study.no == j & x$item_name == "HYP_NO" & x$repeat.var == a+1)-1,
                      ifelse(j < max.study,
                             which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat.var == j+1) -1,
                             which(x$session == sessions[i] & x$item_name == "EXPLOR_RESEARCHQ") -1))
               )] <- k
          
          max.result <- length(x$repeat.var[which(x$session == sessions[i] & x$study.no == j & x$hyp.no == k & x$item_name=="RESULT_NO")])
          if(max.result < 1) next
          
          for (l in 1:max.result) {
            
            b <- b + 1
            
            x$result.no[
              ## This indexes the row number of the first row to be named after the current study
              (which(x$session == sessions[i] & x$study.no == j & x$hyp.no == k & x$item_name == "RESULT_NO" & x$repeat.var == b)):
                ## This indexes the row number of the last row to be named after the current study:
                ## if the current study is not the last, the last row is one above the next HYP_NO,
                ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
                (which(x$session == sessions[i] & x$study.no == j & x$hyp.no == k & x$item_name == "RESULT_NOTE3" & x$repeat.var == b)
                )] <- l
            
          }
        }   
    }
}

tail(x[x$session==sessions[8], c("item_name", "answer", "study.no", "hyp.no", "result.no")], 200)


x[x$item_name == "CONCLUSION_QUOTE", c("session", "study.no", "hyp.no", "repeat.var")]

x$item_name[which(x$item_name == "CONCLUSION_QUOTE")-1]

x$repeat.var[which(x$item_name == "HYP_NO")] == x$repeat.var[which(x$item_name == "CONCLUSION_QUOTE")]

## check if it worked
x$repeat.var[which(x$session==sessions[10] & x$item_name==item[50])]

## Turn dataset into wide format (not sure if this is still necessary after creating repeat.var)
library(reshape2)
x.wide <- dcast(x, session + repeat.var ~ item_name, value.var = "answer")
nrow(x.wide)

max(x$repeat.var[which(x$session == sessions[6] & x$item_name == "RESULT_NO")])
max(x$result.no[which(x$session == sessions[6])])

x$item_name[which(x$item_name == "RESULT_NOTE3")+1]

x$item_name[which(x$item_name == "RESULT_NO") : which(x$item_name == "RESULT_NOTE3")]



x$result.no[
  ## This indexes the row number of the first row to be named after the current study
  (which(x$session == sessions[i] & x$study.no == j & x$hyp.no == k & x$item_name == "RESULT_NO" & x$repeat.var == b)):
    ## This indexes the row number of the last row to be named after the current study:
    ## if the current study is not the last, the last row is one above the next HYP_NO,
    ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
    (which(x$session == sessions[i] & x$study.no == j & x$hyp.no == k & x$item_name == "RESULT_NOTE3" & x$repeat.var == b)
    )]

