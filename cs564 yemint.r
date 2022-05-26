library(hash)

word_to_index<-hash()

word_by_mbti <- read.csv('word_by_mbti.csv',header=TRUE)

all_mbti=c('ISTJ','ISTP','ISFJ','ISFP','INTJ','INTP','INFJ','INFP','ESTJ','ESTP','ESFJ','ESFP','ENTJ','ENTP','ENFJ','ENFP')


total_by_type=c()
m_index <- 2

while (m_index<18){
	total_by_type[m_index-1]<-sum(word_by_mbti[m_index])
	m_index<-m_index+1
}

total_all=sum(total_by_type)

count_small_pvalue <- 0
word_index <- 1
while(word_index<87189+1){
	word_to_index[[ word_by_mbti[word_index,1]  ]] = word_index

	now_observed=c()
	now_expected=c()
	
	m_index <- 2
	while(m_index<18){
		now_observed[m_index-1] <- word_by_mbti[word_index,m_index]
		now_expected[m_index-1] <- (total_by_type[m_index-1]/total_all)
		m_index<-m_index+1
	}
	pv<-chisq.test(x=now_observed, p=now_expected)$p.value
	if(pv<0.05){
		count_small_pvalue<-count_small_pvalue+1
	}
	word_index<-word_index+1
}



# look for specific word 
now_word_index <- word_to_index[['happy']]

now_observed=c()
now_expected=c()
now_residuals=c()
now_word_total<-sum(word_by_mbti[now_word_index,-1])
m_index <- 2
while(m_index<18){
	now_word_total<-sum(word_by_mbti[now_word_index,-1])

	oi <- word_by_mbti[now_word_index,m_index]
	ei <- now_word_total*(total_by_type[m_index-1]/total_all)


	now_residuals[m_index-1] <- (oi-ei)/sqrt(ei)
	m_index<-m_index+1
}

now_residuals

barplot(now_residuals, col=ifelse(now_residuals>0,"red","blue"),names.arg=all_mbti)
