######################### NOTICE #################################
#JDK version should be more than 1.8.0.
#Included RHINO should be used only under non-commercial purpose.
#Developer: Sukjae Choi (lingua72@gmail.com)
##################################################################

#[1] setting functions
initRhino <- function(path)
{
  setwd(paste(path, "/RHINO", sep=""))
  if(!require(rJava)) {install.packages("rJava"); library(rJava)}
  .jinit()
  .jaddClassPath(paste(path, "/RHINO", sep=""))
  .jclassPath()
  RHINO <- .jnew("rhino/RHINO")
  .jcall(RHINO, returnSig = "V", "ExternInit")
  
  return(RHINO)
}

getMorph <- function(sentence, type)
{
  result <- .jcall(RHINO, returnSig = "S", "getMorph", sentence, type)
  Encoding(result) <- "UTF-8"
  resultVec <- unlist(strsplit(result, '\r\n'))
  return(resultVec)
}


#[2] Setting path & Initializing RHINO
path <- getwd()          #You must change the path by yours. Use getwd().
RHINO <- initRhino(path)


#[3] Analyzing sentence variable
setwd(path)
result <- readLines("sam1.txt")                           #./RHINO2.5.3/WORK/sample.txt. There are only 3 sentences

nounVec <- getMorph(result[1], "noun")                      #get nouns(NNG, NNp, NP). The result is vector. NNB is excluded
nounVec[1]
for(i in nounVec)
  print(i)

verbVec <- getMorph(result[1], "verb")                      #get verbs(VV, VA, XR). The result is vector. VX is excluded
verbVec[1]
for(i in verbVec)
  print(i)

getMorph(result[1], "NNG")                                  #You can get individual POS.
getMorph(result[1], "NNP")
getMorph(result[1], "NP")
getMorph(result[1], "NNB")                                  #You can get NNB also here.
getMorph(result[1], "VV")
getMorph(result[1], "VA")
getMorph(result[1], "VX")                                   #You can get VX also here.
getMorph(result[1], "XR")

a <- getMorph(result[1], "VV")
b <- unique(a)
b <- paste0(b,"다")
b
write.csv(b, "b.csv")
#[4] Analyze all files in ./RHINO2.5.3/WORK/RHINO/_input/
setwd(paste(path, "/RHINO", sep=""))
.jcall(RHINO, returnSig = "V", "analyzingText_rJava", "N")  #The rightest option: N-> Noun(NNG, NNP, NP), V-> Verb(VV, VA, XR), NV-> Noun and Verb
print("Created result.txt in ./RHINO2.5.3/WORK/RHINO/")


#[5] Example of using wordcloud library
txt <- readLines("result.txt", encoding="UTF-8")
txt <- gsub(", ", "\r\n", txt)
writeLines(txt, "result2.txt")
noun <- readLines("result2.txt")
nounVec <- unlist(noun)
nounFrame <- data.frame(nounVec)
nounFreq <- table(nounFrame)
head(sort(nounFreq, decreasing = T), 20)

if(!require(RColorBrewer)) {install.packages("RColorBrewer"); library(RColorBrewer)}
pal <- brewer.pal(12,"Set3")
if(!require(wordcloud)) {install.packages("wordcloud"); library(wordcloud)}
wordcloud(names(nounFreq), freq=nounFreq, scale=c(5,0.5), rot.per = 0.25, min.freq = 1, random.order = F, random.color = T, colors=pal)

print("Finished.")

