
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
x$repeat_var <- NA

for (i in 1:length(sessions)) {
  for (j in 1:length(item)) {
    repeat.seq <- seq(from=1, to=nrow(x[x$session==sessions[i] & x$item_name==item[j], ]))
    x$repeat_var[which(x$session==sessions[i] & x$item_name==item[j])] <- repeat.seq
  }
}

x$study.no <- NA
x$hyp.no <- NA
x$result.no <- NA

for (i in 1:length(sessions)) {
  
  ## Session 3 is incomplete because the paper shouldn't be in the sample. This bit makes sure it is skipped.
  if(tail(x[which(x$session==sessions[i]), "item_name"], 1) != "END_NOTE2") next
  
  max.study <- length(x$repeat_var[which(x$session == sessions[i] & x$item_name=="STUDY_NO")])
  
  for (j in 1:max.study) {
        x$study.no[
                    ## This indexes the row number of the first row to be named after the current study
                    (which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat_var == j)):
                    ## This indexes the row number of the last row to be named after the current study:
                    ## if the current study is not the last, the last row is one above the next HYP_NO,
                    ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
                    (ifelse(j < max.study, 
                           which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat_var == j+1) -1,
                           which(x$session == sessions[i] & x$item_name == "EXPLOR_RESEARCHQ") -1))
                  ] <- j
        max.hyp <- length(x$repeat_var[which(x$session == sessions[i] & x$study.no == j & x$item_name=="HYP_NO")])
        
        for (k in 1:max.hyp) {
          x$hyp.no[
            ## This indexes the row number of the first row to be named after the current study
            (which(x$session == sessions[i] & x$study.no == j & x$item_name == "HYP_NO" & x$repeat_var == k)):
              ## This indexes the row number of the last row to be named after the current study:
              ## if the current study is not the last, the last row is one above the next HYP_NO,
              ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
              (ifelse(k < max.hyp, 
                      which(x$session == sessions[i] & x$study.no == j & x$item_name == "HYP_NO" & 
                              ifelse(max.study > 1 & j > 1,
                                     x$repeat_var == length(x$study.no[which(x$session == sessions[i] & 
                                                                             x$study.no == j-1 & x$item_name == "HYP_NO")])+k+1,
                                     x$repeat_var == k+1))-1,
                      ifelse(j < max.study,
                             which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat_var == j+1) -1,
                             which(x$session == sessions[i] & x$item_name == "EXPLOR_RESEARCHQ") -1))
               )] <- k
        }   
    }
}


tail(x$session[which(!is.na(x$hyp.no))], 550)
sessions[8]

length(x$item_name[which(x$item_name=="STUDY_NO" & x$session=="vb3bDEb7I3tNNeS-Te52Ss_7PenrA1_MAyPQRT9jqSr_LS4nBbIQq9e24SulGOqZ")])
length(x$item_name[which(x$study.no == 2 & x$item_name=="HYP_NO" & x$session==sessions[8])])

j<max.study
which(x$session == sessions[i] & x$item_name == "STUDY_NO" & x$repeat_var == j+1)
which(x$session == sessions[i] & x$item_name == "EXPLOR_RESEARCHQ")
k<max.hyp
which(x$session == sessions[i] & x$study.no == j & x$item_name == "HYP_NO" & x$repeat_var == k+1)

x$repeat_var[which(x$session==sessions[8] & x$item_name=="STUDY_NO")]

x$repeat_var[which(x$session==sessions[3] & x$item_name=="STUDY_NO")]
tail(x[which(x$session==sessions[16]), "item_name"], 1)


max(x$repeat_var[which(x$item_name=="HYP_NO")])
length(x$answer[which(x$session==sessions[i])])



length(x[x$item_name=="STUDY_NEXT2","answer"])

# row number of rows in which STUDY_NEXT2 == 3
which(x[, "item_name"] == "STUDY_NEXT2" & x[, "answer"] == 3)

x[which(x$item_name == "HYP_NO") - 1, "item_name"]


length(x[which(x[, "item_name"] == "STUDY_NO" & x[, "repeat_var"] == 3) , "item_name"])



start.study <- which(x[, "session"] == sessions[i] & x[, "item_name"] =="STUDY_NO" & x[, "repeat_var"] == k)
end.study <- which(x[, "session"] == sessions[i] & x[, "item_name"] =="STUDY_NO" & x[, "repeat_var"] == k+1)

repeat.study <- seq(from=1, to=nrow(x[x$session==sessions[i] & x$item_name=="STUDY_NO", ]))
repeat.hyp <- seq(from=1, to=nrow(x[x$session==sessions[i] & x$item_name=="HYP_NO", ]))
repeat.result <- seq(from=1, to=nrow(x[x$session==sessions[i] & x$item_name=="RESULT_NO", ]))

## check if it worked
x$repeat_var[which(x$session==sessions[10] & x$item_name==item[50])]

## Turn dataset into wide format (not sure if this is still necessary after creating repeat_var)
library(reshape2)
x.wide <- dcast(x, session + repeat_var ~ item_name, value.var = "answer")
nrow(x.wide)




