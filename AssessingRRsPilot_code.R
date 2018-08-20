
## Import dataset
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
x[x$item_name=="ARTICLE_DOI", "answer"]


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

## check if it worked
x$repeat_var[which(x$session==sessions[10] & x$item_name==item[50])]

## Turn dataset into wide format (not sure if this is still necessary after creating repeat_var)
library(reshape2)
x.wide <- dcast(x, session + repeat_var ~ item_name, value.var = "answer")
nrow(x.wide)




