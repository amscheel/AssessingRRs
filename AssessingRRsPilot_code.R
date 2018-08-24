
## Import dataset
install.packages("rio", dependencies = TRUE)
library(rio)
pilot <- import("AssessingRRsPilot_data.csv")

## Delete all invalid/test sessions
invalid.session <- c(pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="","session"],
                     pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="x","session"],
                     pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="y","session"],
                     pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="a new name","session"],
                     pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="testrun1","session"],
                     pilot[pilot$item_name=="ARTICLE_NAME" & pilot$answer=="testrun2","session"])
pilot <- pilot[-grep((paste(invalid.session,collapse="|")), pilot$session),]

## standardise all DOI entries to have one reliable identifier for each paper
pilot[pilot$item_name=="ARTICLE_DOI", "answer"] <- gsub("^.*?1","1", pilot[pilot$item_name=="ARTICLE_DOI", "answer"])

## remove all rows of variables that haven't been shown
pilot <- pilot[pilot$shown!="",]




## Create a new variable to be able to identify each occurence of variables with several entries (e.g. several hypotheses)
sessions <- unique(pilot$session)
item <- unique(pilot$item_name)
pilot$doi <- NA
pilot$coder <- NA
pilot$repeat.var <- NA


for (i in 1:length(sessions)) {
  pilot$doi[which(pilot$session == sessions[i])] <- pilot$answer[which(pilot$session == sessions[i] & pilot$item_name == "ARTICLE_DOI")]
  pilot$coder[which(pilot$session == sessions[i])] <- pilot$answer[which(pilot$session == sessions[i] & pilot$item_name == "CODED_BY")]
  
  for (j in 1:length(item)) {
    repeat.seq <- seq(from=1, to=nrow(pilot[pilot$session==sessions[i] & pilot$item_name==item[j], ]))
    pilot$repeat.var[which(pilot$session==sessions[i] & pilot$item_name==item[j])] <- repeat.seq
  }
}


pilot$study.no <- NA
pilot$hyp.no <- NA
pilot$result.no <- NA

for (i in 1:length(sessions)) {
  
  ## Session 3 is incomplete because the paper shouldn't be in the sample. This bit makes sure it is skipped.
  if(tail(pilot[which(pilot$session==sessions[i]), "item_name"], 1) != "END_NOTE2") next
  
  max.study <- length(pilot$repeat.var[which(pilot$session == sessions[i] & pilot$item_name=="STUDY_NO")])
  
  a <- 0 # this is a helper variable to identify new hypotheses in papers with >1 study 
         # (because repeat.var keeps ticking up and hyp.no resets when study.no changes)
  b <- 0
  
  for (j in 1:max.study) {
        pilot$study.no[
                    ## This indexes the row number of the first row to be named after the current study
                    (which(pilot$session == sessions[i] & pilot$item_name == "STUDY_NO" & pilot$repeat.var == j)):
                    ## This indexes the row number of the last row to be named after the current study:
                    ## if the current study is not the last, the last row is one above the next HYP_NO,
                    ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
                    (ifelse(j < max.study, 
                           which(pilot$session == sessions[i] & pilot$item_name == "STUDY_NO" & pilot$repeat.var == j+1) -1,
                           which(pilot$session == sessions[i] & pilot$item_name == "EXPLOR_RESEARCHQ") -1))
                  ] <- j
        max.hyp <- length(pilot$repeat.var[which(pilot$session == sessions[i] & pilot$study.no == j & pilot$item_name=="HYP_NO")])
        if(max.hyp < 1) next
        
        for (k in 1:max.hyp) {
          a <- a + 1
          
          pilot$hyp.no[
            ## This indexes the row number of the first row to be named after the current study
            (which(pilot$session == sessions[i] & pilot$study.no == j & pilot$item_name == "HYP_NO" & pilot$repeat.var == a)):
              ## This indexes the row number of the last row to be named after the current study:
              ## if the current study is not the last, the last row is one above the next HYP_NO,
              ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
              (ifelse(k < max.hyp, 
                      which(pilot$session == sessions[i] & pilot$study.no == j & pilot$item_name == "HYP_NO" & pilot$repeat.var == a+1)-1,
                      ifelse(j < max.study,
                             which(pilot$session == sessions[i] & pilot$item_name == "STUDY_NO" & pilot$repeat.var == j+1) -1,
                             which(pilot$session == sessions[i] & pilot$item_name == "EXPLOR_RESEARCHQ") -1)))] <- k
          
          max.result <- length(pilot$repeat.var[which(pilot$session == sessions[i] & pilot$study.no == j & 
                                                        pilot$hyp.no == k & pilot$item_name=="RESULT_NO")])
          if(max.result < 1) next
          
          for (l in 1:max.result) {
            
            b <- b + 1
            
            pilot$result.no[
              ## This indexes the row number of the first row to be named after the current study
              (which(pilot$session == sessions[i] & pilot$study.no == j & pilot$hyp.no == k & 
                       pilot$item_name == "RESULT_NO" & pilot$repeat.var == b)):
                ## This indexes the row number of the last row to be named after the current study:
                ## if the current study is not the last, the last row is one above the next HYP_NO,
                ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
                (which(pilot$session == sessions[i] & pilot$study.no == j & pilot$hyp.no == k & 
                         pilot$item_name == "RESULT_NOTE3" & pilot$repeat.var == b))] <- l
            
          }
        }   
    }
}

### Retrieve DOIs for RRs and PRs, respectively
x <- 14.5*10
set.seed(x)
RRselection <- sample(126, size = 4, replace=FALSE)
PRselection <- sample(81, size = 4, replace=FALSE)

RRlist <- read.csv("List-of-RRs_20180608.csv")
PRlist <- read.csv("List-of-PRs_20180608.csv")

doi.mat <- cbind(c(RRlist[RRselection,"DOI"], PRlist[PRselection,"DOI"]), c(rep("RR", 4), rep("PR", 4)))
doi.mat <- data.frame()

RR.doi <- RRlist[RRselection,"DOI"]
PR.doi <- PRlist[PRselection,"DOI"]

doi <- c(as.character(RR.doi), as.character(PR.doi))
group <- c(rep("RR", 4), rep("PR", 4))
doi.df <- as.data.frame(cbind(doi, group))

pilot <- merge(pilot, doi.df, by="doi", sort = FALSE)

pilot$coder[which(pilot$coder == unique(pilot$coder)[1])] <- "Anne"
pilot$coder[which(pilot$coder == unique(pilot$coder)[2])] <- "Emma"


pilot <- pilot[order(pilot$group, pilot$doi, pilot$coder, pilot$created),]
summary(pilot)

write.csv(pilot, file = "AssessingRRsPilot_data_cleaned.csv")


## Turn dataset into wide format (not sure if this is still necessary after creating repeat.var)
library(reshape2)
pilot.wide <- dcast(pilot, session + repeat.var ~ item_name, value.var = "answer")
nrow(pilot.wide)

max(pilot$repeat.var[which(pilot$session == sessions[6] & pilot$item_name == "RESULT_NO")])
max(pilot$result.no[which(pilot$session == sessions[6])])

pilot$item_name[which(pilot$item_name == "RESULT_NOTE3")+1]

pilot$item_name[which(pilot$item_name == "RESULT_NO") : which(pilot$item_name == "RESULT_NOTE3")]



pilot$result.no[
  ## This indexes the row number of the first row to be named after the current study
  (which(pilot$session == sessions[i] & pilot$study.no == j & pilot$hyp.no == k & pilot$item_name == "RESULT_NO" & pilot$repeat.var == b)):
    ## This indexes the row number of the last row to be named after the current study:
    ## if the current study is not the last, the last row is one above the next HYP_NO,
    ## if the current study is the last one, the last row is one above EXPLOR_RESEARCHQ
    (which(pilot$session == sessions[i] & pilot$study.no == j & pilot$hyp.no == k & pilot$item_name == "RESULT_NOTE3" & pilot$repeat.var == b)
    )]

