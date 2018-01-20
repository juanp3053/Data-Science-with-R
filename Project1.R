#Name: Project 1
#Authoer: Juan P. Garces
#Date: 09/12/2016

data("USJudgeRatings")
#Creation of data frame later used
judges <- data.frame(Top1 = NULL,Top2 = NULL,Top1 = NULL)
opt <- options(stringsAsFactors = FALSE)
ADD1 <- ""

for (i in 1:12) { #Deciding Top 3 for every row and populating data frame
  
  Ranking <- rank(USJudgeRatings[i], ties.method = "max") #Using the classical maximum to decide ties
  GG <- sort(Ranking, index.return = TRUE, decreasing = TRUE) 
  ADD1 <- append(ADD1, rownames(USJudgeRatings)[GG$ix[1:3]]) #vector for all the top judges values going to data frame
  #values added to each row /****
  ADDS <- rownames(USJudgeRatings)[GG$ix[1:3]] 
  judges <- rbind(judges, ADDS)
  #****/
  
}
row.names(judges) <- c("CONT", "INTG","DMNR", "DILG", "CFMG", "DECI", "PREP" , "FAMI", "ORAL", "WRIT", "PHYS", "RTEN")
`colnames<-`(judges, c("TOP 1", "TOP 2", "TOP 3"))

#ADD1 <-append(ADD1, "NARUK,H.J.")  #TEST FOR MULTIPLE WINNERS
last <-sort(table(ADD1),decreasing=TRUE)
which(last == max(last))






