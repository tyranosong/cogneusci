# 데이터베이스에서 실험 자극 추출 

library(tidyverse)
library(readxl) # 자극에 대한 사전 평정 결과 파일을 불러오기 위한 패키지.
library(writexl) # 추출한 단어 자극을 내보내서 저장하기 위한 패키지.


# 텍스트 원자료를 티블로 불러오기.
mald <- read_delim("MALD1_ItemData.txt", delim = "\t")
View(mald)


expdata <- mald %>%
  filter(IsWord == TRUE) %>% # 비단어를 제외하고, 단어인 항목만 남기기.
  filter(POS == "Noun") %>% # 단어의 품사가 명사인 단어만 포함시키기.
  mutate(Zipf_COCA = log10(FreqCOCA/560) + 3) %>% # 단어별 COCA 빈도를 기준으로 로그 빈도(Zipf Frequency)를 계산하여, 해당 값으로 이루어진 열(Zipf_COCA)을 추가하기.
  filter(Zipf_COCA <= 2) %>% # 계산된 로그 빈도(Zipf_COCA) 값이 2 이하인 단어들만 남기기.
  filter(Duration >= 500) %>%
  filter(Duration < 1000) %>% # 단어 음성 파일 길이가 500ms 이상, 1000ms 미만인 단어들을 선별하기. 
  filter(NumMorphs == 1)  # 데이터베이스에 단일 형태소 단어로 등록된 단어들만 남기기.

View(expdata)


# 사전 평정 이전, visual inspection 과정에서 frontiersman의 복수형인 frontiersmen과, glycerine과 형태가 유사한 동음이의어인 glycerol까지 단어 2개 제외.
# 단어 자극에 대한 사전 평정 결과 분석. 

# 사전 평정 결과 엑셀 파일 불러오기.
pre <- read_excel("pre-evaluation.xlsx")
View(pre)
pre

# 열 이름을 간단하게 변경하기.
names(pre) <- c("Item","A","B","C","D","E")
pre


# 친숙도 점수의 평균 계산하기.
pre$familiarity <- rowMeans(pre[,c('A','B','C','D','E')])

pre_eval <- pre %>%
  filter(familiarity < 3) # 친숙도 점수 평균이 3 이상인 단어를 제외하기.
View(pre_eval)


# 데이터베이스에서 추출하였던 단어 자료와 사전 평정을 거친 단어 자료의 교집합에 속하는 단어들만 남기기.
finaldata <- expdata %>%
  inner_join(pre_eval, by = "Item")
View(finaldata)

# 친숙도 평정 점수가 나타난 열들을 제거하기.
stimuli_final <- finaldata[,-c(25:30)]
View(stimuli_final)

# 최종 실험 자극 데이터 파일을 저장하기.
write_xlsx(stimuli_final, "stimuli_final.xlsx")