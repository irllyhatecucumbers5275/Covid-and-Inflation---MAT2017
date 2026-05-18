
# 20년도와 23년도 데이터샘플을 추출 후 비교 필요

# 우리가 할거는 2020 2023
# 추출해서 비교하기. 일단 그러면 데이터셋 두개가 필수
 # 인덱싱을 할 수 있으면 골드데이터도
  
# 골드 데이터 = gold
# covid확진자 데이터 = covid01
# cpi데이터: newpostcpi - 에서 또 cpi bmd로 변형



  
# 0605 2326 cpibmd0605_00.csv 생성완료
# 0605 2341 goldbmd0605_00.csv 생성완료
# 0605 23XX write_csv(lolcovid,"covidbmd0605_00.csv") 로 Covid dataset 생성완료


--------------------------------
  
  
# 글로벌 모빌리티를 봐야함

> library(tidyverse)
── Attaching core tidyverse packages ────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr   1.1.4     ✔ readr   2.1.5
✔ forcats 1.0.0     ✔ stringr 1.5.1
✔ ggplot2 3.5.1     ✔ tibble  3.2.1
✔ purrr   1.0.2     ✔ tidyr   1.3.1
── Conflicts ──────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> library(dplyr)
> library(stringr)
global_mobility.csv
# A tibble: 11,730,025 × 9
# =====================================================
# things to do  
# 1. 열삭제 (모빌리티리포트)

#2. min max 조망. ((일단 보기만 할거임.))
#3. 절댓값 계산, mutate
#4. 열 삭제삭제
#5. 데이터프레임 mutate  (월별 합치는 연산)
#6. ((필요시 / / 열 삭제삭제))

#7. 2020 추출
#8. 2023 추출




write_csv(gm0202,"mobile.csv")
#from 2020 0201 to 2022 1001


> write_csv(gm0202,"mobile_covidbmd0605_00.csv")

https://support.google.com/covid19-mobility?hl=ko#topic=9822927 
# reference check for mobility data
--------------

  
  
