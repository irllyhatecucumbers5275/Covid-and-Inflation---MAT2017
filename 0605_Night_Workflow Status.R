
# 20년도와 23년도 데이터샘플을 추출 후 비교 필요

# 우리가 할거는 2020 2023
# 추출해서 비교하기. 일단 그러면 데이터셋 두개가 필수적이다. 
 # 인덱싱을 할 수 있으면 골드데이터도 하자. 
  
# 골드 데이터 = gold
# covid확진자 데이터 = covid01
# cpi데이터: newpostcpi - 에서 또 cpi bmd로 변형



  
# 0605 2326 cpibmd0605_00.csv 생성완료

# 0605 2341 goldbmd0605_00.csv 생성완료

# 0605 23XX write_csv(lolcovid,"covidbmd0605_00.csv") 로 Covid dataset 생성완료





--------------------------------
  
  
%% 글로벌 모빌리티를 볼 것임

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
0. 열삭제삭제 (모빌리티리포트)

1. min max 조망. ((일단 보기만 할거임.))
2. 절댓값 계산, mutate
3. 열 삭제삭제
4. 데이터프레임 mutate  (월별 합치는 연산)
5. ((필요시 / / 열 삭제삭제))

6. 2020 추출
7. 2023 추출


2022까지밖에 없노!!
  write_csv(gm0202,"mobile.csv") ㅋㅋㅋㅋㅋ 통제변인 떡칠 ㅋㅋㅋ
from 2020 0201 to 2022 1001


> write_csv(gm0202,"mobile_covidbmd0605_00.csv")

https://support.google.com/covid19-mobility?hl=ko#topic=9822927 
모빌리티 참고하시오
--------------

  
  