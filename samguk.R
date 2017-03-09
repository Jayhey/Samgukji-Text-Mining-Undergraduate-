library(stringi)
library(stringr)
library(reshape2)
library(igraph)
library(KoNLP)
library(tm)
library(topicmodels)
library(wordcloud)
library(rgl)
sam <-readLines("sam_all.txt")

#대화 지시문 분류
sentence_sam <- function(sam_txt){
        sentence <- sam_txt[which(regexpr(' ',sam_txt) == 1)] #대화, 지시문 중에서 지시문만 가져옴.
        #talk_sam6 <- sam6[which(regexpr(' ',sam6) != 1)]
        return(sentence)
}


#battle word 갯수 
battle_words <- readLines("battle_words.txt")

battle_sam <- function(battle_words, sentence_sam){
        battle_sam6 <- data.frame(sentence_sam)
        for(i in 1:length(battle_words)){
                battle_sam6[,i+1] <- str_detect(sentence_sam ,battle_words[i])
        }
        
        for(i in 1:length(battle_words)){
                names(battle_sam6)[i+1] <- battle_words[i]
        }
        
        battle_sam6[,(length(battle_sam6)+1)] <- rowSums(battle_sam6[,2:length(battle_sam6)])
        s <- data.frame(sentence = battle_sam6[,1],
                        battle_words = battle_sam6[,length(battle_sam6)])
        return(s)      
} 

####지시문 등장인물 파악####호 -> 이름 다 바꾸기 #####
sam_names <- readLines("sam_names.txt")
name_unity <- read.csv("name_unity.csv", header = FALSE)
name_unity[,1] <- as.character(name_unity[,1])
name_unity[,2] <- as.character(name_unity[,2])

battle_general <- function(sam_names, name_unity,s6){
        for(i in 1:nrow(s6)){
                for(j in 1:length(sam_names)){
                        if(str_detect(s6[i,1], sam_names[j]) == TRUE){
                                s6[i,3] <- paste(s6[i,3], sam_names[j])
                                s6[i,3] <- gsub("NA ","", s6[i,3])
                        }
                }
                for(k in 1:nrow(name_unity)){
                        s6[i,3] <- gsub(name_unity[k,2], name_unity[k,1], s6[i,3])
                }
                s6[i,3] <- gsub(" 서서", "서서", s6[i,3]) #서서 예외 수정
                cat(i,"/",length(s6$sentence),'\n')
        }
        return(s6)
}
#전투 씬만 분류하기. 얘를 안하면 전체 지시문 파악 가능.
#전투 , 전투+일반 , 일반 이렇게 세 개로 나누자.
battle_only <- function(s6){
        battle <- subset(s6, 
                         select = c(sentence,battle_words, V3),
                         subset =(battle_words >= 4 & is.na(V3) == FALSE))
        return(battle)
}
battle_plus_normal <- function(s6){
        bn <- subset(s6, 
                     select = c(sentence,battle_words, V3),
                     subset =(is.na(V3) == FALSE))
        return(bn)
}
normal_only <- function(s6){
        normal <- subset(s6, 
                         select = c(sentence,battle_words, V3),
                         subset = (battle_words <= 4 & is.na(V3) == FALSE))
        return(normal)
}

######################장수 matrix로 바꾸기########################
unique_name <- readLines("unique_name.txt")


adjmatrix <- function(unique_name, general_list){
        u <- unique(do.call('c', strsplit(general_list, " "))) #장수 이름 중복 제거
        matrix <- data.frame(name = unique_name)
        transaction <- data.frame(row.names = u)
        for (i in 1:length(general_list)) { #trasaction matrix 만들기.
                for (j in 1:length(u)) {
                        transaction[j, i] <- 1 * (u[j] %in% strsplit(general_list[i], ' ')[[1]])
                }
        }
        mat <- as.matrix(transaction)
        
        # transform into a term-term adjacency matrix
        adjmat <- mat %*% t(mat)
        return(adjmat)        
}

#################igraph 돌려보기#################
first <- read.csv("first.csv")
middle <- read.csv("middle.csv")
late <- read.csv("late.csv")
#팩션 정해주기##
faction <- function(general_list, when){
        faction1 <- c()
        faction2 <- c()
        faction3 <- c()
        faction4 <- c()
        u <- unique(do.call('c', strsplit(general_list, " "))) #장수 이름 중복 제거
        for(i in 1:length(u)){
                if(u[i] %in% when[,1] == TRUE){
                        faction1 <- append(faction1, u[i])
                }
                if(u[i] %in% when[,2] == TRUE){
                        faction2 <- append(faction2, u[i])
                }
                if(u[i] %in% when[,3] == TRUE){
                        faction3 <- append(faction3, u[i])
                }
                if(u[i] %in% when[,4] == TRUE){
                        faction4 <- append(faction4, u[i])
                }
        }
        when_general <- list(Wei = faction1, Shu = faction2, Wu = faction3, etc = faction4)
        return(when_general)
}
igraph_viz <- function(adjmat, general_nation, general_list, when_general){
        g <- graph_from_adjacency_matrix(adjmat, mode = "undirected",
                                         weighted = TRUE, diag = FALSE)
        #general_nation <- faction(general_list, when_general)
        faction1 <- c("하후돈","조조","장료","허저","서황","정욱")
        faction2 <- c("유비","관우","장비","조운","황충","위연","방통","제갈량")
        faction3 <- c("손권","노숙","주유","여몽","육손")
        faction4 <- c("마초","방덕")
        
        #지도자는 네모
        V(g)$shape <- "circle"
        V(g)[c("조조", "유비","손권")]$shape <- "rectangle"
        V(g)[faction1]$color <- "deeppink"
        V(g)[faction2]$color <- "darkolivegreen1"
        V(g)[faction3]$color <- "lightblue"
        V(g)[faction4]$color <- "lightgoldenrod1"
        
        #노드 사이즈
        V(g)$size <- 1.6*sqrt(graph.strength(g))
        #간선 넓이
        X<- E(g)$weight
        E(g)$width <- sqrt(X)
        
        #세력 간 관계 표시
        #F1 <- V(g)[faction1]
        #F2 <- V(g)[faction2]
        #F3 <- V(g)[faction3]
        #E(g)[ F1 %--% F1 ]$color <- "pink"
        #E(g)[ F2 %--% F2 ]$color <- "lightblue"
        #E(g)[ F3 %--% F3 ]$color <- "lightgreen"
        #E(g)[ F1 %--% F2 ]$color <- "orangered1"
        #E(g)[ F1 %--% F3 ]$color <- "yellow2"
        #E(g)[ F2 %--% F3 ]$color <- "cyan"
        
        V(g)$label.dist <- ifelse(V(g)$size >= 9, 0, 0.75)
        
        
        plot(g)
        
        return(g)
}
#호, 주격조사 통일

del_jcs <- function(sam_conver, name_unity){
        final_conver <- data.frame(name = name_unity[,1])
        for(i in 1:nrow(sam_conver)){
                for(j in 1:nrow(name_unity)){
                        if(str_detect(sam_conver[i,1],name_unity[j,1])==TRUE){
                                final_conver[j,2] <- paste(sam_conver[i,2]
                                                           ,final_conver[j,2])
                        }
                        if(str_detect(sam_conver[i,1],name_unity[j,2])==TRUE){
                                final_conver[j,2] <- paste(sam_conver[i,2]
                                                           ,final_conver[j,2])
                        }
                        
                }
        }
        final_conver2 <- final_conver
        #for(i in 1:(nrow(final_conver)-1)){
        #        if(final_conver[i,1] == final_conver[i+1,1]){
        #                final_conver2 <- final_conver2[-i,]
        #        }
        #}
        return(final_conver2)
}
#####################conversation##############################

after <- readLines("after_verb.txt")
before <- readLines("before_verb.txt")
subject <- readLines("conv_name.txt")

conversation <- function(sam_txt, after, before, subject, name_unity){
        sam_conv <- c()
        sam_conver <- data.frame(subject)
        for(i in 1:length(sam_txt)){ #리스트 + 문장으로 나누기
                sam_conv[i] <- stri_split_boundaries(sam_txt[i],type="sentence")
        }
        
        for(i in 1:length(subject)){ #장수주격조사, 말하다 조합 
                for(j in 1:length(before)){
                        for(k in 1:(length(sam_txt)-1)){
                                if(str_detect(sam_conv[[k]][length(sam_conv[[k]])],subject[i]) == TRUE && 
                                   str_detect(sam_conv[[k]][length(sam_conv[[k]])],before[j]) == TRUE){
                                        sam_conver[i,2] <- paste(sam_conver[i,2], sam_txt[k+1])
                                }
                        }        
                }
                for(j in 1:length(after)){
                        for(k in 2:length(sam_txt)){
                                if(str_detect(sam_conv[[k]][1],subject[i]) == TRUE &&
                                   str_detect(sam_conv[[k]][1],after[j]) == TRUE){
                                        sam_conver[i,2] <- paste(sam_conver[i,2], sam_txt[k-1])
                                }        
                        }      
                }
                cat('"',subject[i],'"', "완료",i,"/",length(subject), "\n")
        }
        #final_conver <- del_jcs(sam_conver,name_unity)
        return(sam_conver)
}


battle_name <- readLines("battle_name.txt")

allocate_battle <- function(s6, battle_name){
        sam_battle <- c()
        sam_battle_scene <- data.frame(battle_name)
        for(i in 1:nrow(s6)){ #리스트 + 문장으로 나누기
                sam_battle[i] <- stri_split_boundaries(s6[i,1],type="sentence")
        }
        for(i in 1:length(sam_battle)){
                for(j in 1:length(sam_battle[[i]])){
                        for(k in 1:length(battle_name)){
                                if(str_detect(sam_battle[[i]][j],battle_name[k])==TRUE){
                                        sam_battle_scene[k,2] <- paste(sam_battle_scene[k,2], 
                                                                       sam_battle[[i]][j])
                                }                
                                
                        }
                        
                }
                cat(i,"/",length(sam_battle),"\n")
        }
        #sam_battle_scene <- del_jcs(sam_battle_scene, name_unity)
        return(sam_battle_scene)
}


#KONLP, RHINO 추출
verb_and_adj <- function(pre_mid_conv){
        vdj <- data.frame(name = pre_mid_conv[,1])
        
        b <- list()
        a <- lapply(pre_mid_conv[,2], extractNoun)
        for(i in 1:length(a)){
                for(j in 1:length(a[[i]])){
                        vdj[i,2] <- paste(a[[i]][j], vdj[i,2])
                }
                cat(i,"/",length(a),"  1st","\n")
        }
        b <- list()
        c <- list()
        for(i in 1:nrow(pre_mid_conv)){
                b[[i]] <- getMorph(pre_mid_conv[i,2],"VV")
                c[[i]] <- getMorph(pre_mid_conv[i,2],"VA")
                cat(i,"/",nrow(pre_mid_conv),"  2nd","\n")
        }
        for(j in 1:nrow(pre_mid_conv)){
                for(k in 1:length(b[[i]])){
                        b[[j]][k] <- paste(b[[j]][k], "다", sep="")
                        vdj[j,3] <- paste(b[[j]][k], vdj[j,3])
                }
        }
        for(j in 1:nrow(pre_mid_conv)){
                for(k in 1:length(c[[i]])){
                        vdj[j,3] <- paste(c[[j]][k], vdj[j,3])
                }
        }
        return(vdj)
}


#############execution##############


#대화 지시문 분류
sentence_sam <-sentence_sam(sam)
sentence_gwando <- sentence_sam(gwando)
sentence_red <- sentence_sam(red)

#전투씬 추출, 전투 장수 통일(?)
sam_names <- readLines("sam_names.txt")
name_unity <- read.csv("name_unity.csv", header = FALSE)

sam_all <- battle_sam(battle_words , sentence_sam)
sam_all <- battle_general(sam_names, name_unity, sam_all)
sam_all <- battle_plus_normal(sam_all)

general_list <- sam_all$V3 

unique_name <- readLines("unique_name.txt")
adjmat_all <- adjmatrix(unique_name, general_list)

sam_all_battle <- allocate_battle(sam_all, battle_name)


#관도
gwando <- readLines("관도전투.txt")
sentence_gwando <- sentence_sam(gwando)

sam_names <- readLines("sam_names_gwando.txt")
name_unity <- read.csv("name_unity_gwando.csv", header = FALSE)

sam_gwando <- battle_sam(battle_words, sentence_gwando)
sam_gwando <- battle_general(sam_names, name_unity, sam_gwando)
sam_gwando <- battle_only(sam_gwando)

gwando2 <- read.csv("gwando.csv")
for(i in 1:nrow(sam_gwando)){
        sam_gwando[,3] <- gsub("  "," ", sam_gwando[,3])
}
general_list_gwando <- sam_gwando$V3 

when_general_gwando <- faction(general_list_gwando, when = gwando2)

unique_name <- readLines("unique_name_gwando.txt")
adjmat_gwando <- adjmatrix(unique_name, general_list_gwando)

igraph_ex <- igraph_viz(adjmat_gwando, general_nation = when_general_gwando, 
                        general_list_gwando, general_list_gwando)

#적벽대전
red <- readLines("적벽대전.txt")
sentence_red <- sentence_sam(red)

sam_names <- readLines("sam_names_red.txt")
name_unity <- read.csv("name_unity_red.csv", header = FALSE)

sam_red <- battle_sam(battle_words, sentence_red)
sam_red <- battle_general(sam_names, name_unity, sam_red)
sam_red <- battle_only(sam_red)

red2 <- read.csv("red.csv")
general_list_red <- sam_red$V3 

when_general_red <- faction(general_list_red, when = red2)

unique_name <- readLines("unique_name_red.txt")
adjmat_red <- adjmatrix(unique_name, general_list_red)

igraph_ex <- igraph_viz(adjmat_red, general_nation = when_general_red, 
                        general_list_red, general_list_red)

#박망파 전투
pa <- readLines("박망파.txt")
sentence_pa <- sentence_sam(pa)

sam_names <- readLines("sam_names_pa.txt")
name_unity <- read.csv("name_unity_pa.csv", header = FALSE)

sam_pa <- battle_sam(battle_words, sentence_pa)
sam_pa <- battle_general(sam_names, name_unity, sam_pa)
sam_pa <- battle_plus_normal(sam_pa)

pa2 <- read.csv("pa.csv")
general_list_pa <- sam_pa$V3 
when_general_pa <- faction(general_list_pa, when = pa2)

unique_name <- readLines("unique_name_pa.txt")
adjmat_pa <- adjmatrix(unique_name, general_list_pa)

igraph_ex <- igraph_viz(adjmat_pa, general_nation = when_general_pa, 
                        general_list_pa, general_list_pa)

#
unique_name <- readLines("unique_name.txt")
general_list_gwando <- sam_gwando$V3 
adjmat <- adjmatrix(unique_name, general_list)

adjmat_gwando <- adjmatrix(unique_name, general_list_gwando)

#초,중,후반부 장수 구분. 팩션 할당
middle <- read.csv("middle.csv")

gwando2 <- read.csv("gwando.csv")
when_general_gwando <- faction(general_list_gwando, when = gwando2)

#igraph 플롯그리기
igraph_ex <- igraph_viz(adjmat,general_nation= when_general)

igraph_ex <- igraph_viz(adjmat_gwando, general_nation = when_general_gwando)

#대화 뽑기
a <- proc.time()
all_conv <- conversation(sam, after, before, subject, name_unity)
a-proc.time()
all_conv_del <- del_jcs(all_conv, name_unity)
nchar(all_conv[1,2])

#명사, 동사 추출
for(i in 1:nrow(fgtest)){
        fgtest[i,2] <- as.character(fgtest[i,2])
}


raw_conv_extract2 <- data.frame(name = raw_conv_extract[,1])
for(i in 1:nrow(raw_conv_extract)){
        raw_conv_extract2[i,2] <- paste(raw_conv_extract[i,2], raw_conv_extract[i,3])
        raw_conv_extract2[i,2] <- gsub("NA다 ","",raw_conv_extract2[i,2])
        raw_conv_extract2[i,2] <- gsub("NA ","",raw_conv_extract2[i,2])
}
raw_conv_ex_test <- 
        cortest <- Corpus(VectorSource(raw_conv_extract2$V2))
cortest
cortest <- tm_map(cortest, removeWords, choong2)
myTdm <- TermDocumentMatrix(cortest)
myTdm

#########lda########
raw_conv <- read.csv("raw_conv_final.csv")
raw_conv <- as.data.frame(sapply(raw_conv, as.character))
raw_conv2 <- data.frame(name = r)
raw_conv_ex <- verb_and_adj(raw_conv)

aa2 <- read.csv("conversation.csv")
for(i in 1:nrow(aa2)){
        aa2[i,2] <- as.character(aa2[i,2])
}




sam_all_final_battle2 <- read.csv("sam_all_final_battle2.csv")
real_final_extract <- read.csv("real_final_extract.csv")
myCorpus <- Corpus(VectorSource(sam_all_battle_final2$V2))
myCorpus <- Corpus(VectorSource(real_final_extract$V2))
choong <- readLines("choong.txt")
choong2 <- unique(choong)
myStopwords <- choong2
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords, choong2)

myTdm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 2))

NTopic <- 2 #토픽 갯수는 우리가 원하는 만큼 정해주자. 이거는 다 다름.

ptm <- proc.time() #alpha가 1이면 완벽하게 랜덤. 커지면 토픽 분포가 같게 하는 것. 작아지면 한 토픽 안에 소수의 토픽만 섞이게 하는 것. 1 아니면 0.1이 좋다고 하심.
control_LDA_Gibbs <- list(alpha = 0.1, estimate.beta = TRUE, verbose = 0, #beta는 1이면 모든 단어가 랜덤하게 분포. 1보다 작으면 핵심어가 도드라지게 만들겠다. estimate.beta는 최적값 만들어주는 것.
                          prefix = tempfile(), save = 0, keep = 0, #나머지는 help파일 찾아봐
                          seed = as.integer(Sys.time()), nstart = 1,
                          best = TRUE, delta = 0.1, iter = 3000, #iteration 1000~5000사이면 수렴이 됨. 
                          burnin = 0, thin = 2000)

Gibbs_LDA <-LDA(myTdm, NTopic, method = "Gibbs", control = control_LDA_Gibbs)
proc.time() - ptm #끝시간 - 시작시간 

terms(Gibbs_LDA, 30)
# Initialize file names
TermFileName <- paste("AssignedTerms_", NTopic, ".csv", sep="")
CoTableFileName <- paste("CoTable_", NTopic, ".csv", sep="")
Top5.PaperFileName <- paste("Top5Papers_", NTopic, ".csv", sep="")

# Frequent terms
Gibbs.terms <- terms(Gibbs_LDA, 20) #30개 어휘를 뽑아달라.
write.csv(Gibbs.terms, TermFileName)

# Keyword co-occurence table for topics 상위 단어들 중에서 토픽 몇개 겹치나 확인
Unique.terms <- rownames(table(as.character(Gibbs.terms)))
CoTable <- matrix(0, NTopic, NTopic)

for (a in 1:NTopic){
        for (b in 1:NTopic){
                CoTable[a,b] <- length(which(table(c(Gibbs.terms[,a],Gibbs.terms[,b])) == 2))    
        }
}

rownames(CoTable) <- paste("Topic", c(1:NTopic), sep="")
colnames(CoTable) <- paste("Topic", c(1:NTopic), sep="")

write.csv(CoTable, CoTableFileName)


#장수 전투 할당
mid_battle <- allocate_battle(s6, battle_name)

tet <- data.frame(name = mid_conv[,1])
for(i in 1:length(a)){
        for(j in 1:(length(a[[i]]))){
                tet[i,2] <- paste(a[[i]][j], tet[i,2])
        }
}


