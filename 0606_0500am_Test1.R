#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# test 1

# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

#■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#v667_compl
#"ver999_nomiss_mobis_abssum.csv"

v665_ccc
write_csv(v665_ccc,"ver999_nomiss_mobis_allfactors.csv")

library(dplyr)
library(lubridate)
library(tidyverse)

# all variables in: tester 1

# sum of absolutes + compute Mobility rate : tester 2

#"v665_no_abssum_nomiss.csv"

# tester 1부터 회귀분석

names(tester1)
[1] "cown"                 "Country"              "WHO_region"          
[4] "year_month"           "NewCases_change_rate" "cpi_pop_alltime"     
[7] "emp_change_p"         "exr_change_p"         "rt_rc_mob"           
[10] "groc_pha_mob"         "parks_mob"            "transit_mob"         
[13] "work_mob"             "resid_mob"            "gold_usd_changerate" 

model1<-lm(cpi_pop_alltime~NewCases_change_rate+emp_change_p+exr_change_p+rt_rc_mob+groc_pha_mob+parks_mob+transit_mob+work_mob+resid_mob+gold_usd_changerate,data=tester1_clean)


# 1) NA/NaN만 제거
tester1_cc <- tester1[complete.cases(tester1), ]

# 2) 1)에서 만든 tester1_cc를 바탕으로 Inf/–Inf까지 제거
tester1_clean <- tester1_cc[apply(tester1_cc, 1, function(x) all(is.finite(x))), ]




stargazer(model1,
      type = "text",
      title = "Covid19 effect on CPI (Tester 1)",
      dep.var.labels = "CPI monthly Change Rate ",
      covariate.labels = c("covid 19 new cases monthly rate ", "unemplotment rate change", "exchange rate of lcu /usd", "mobility recrec", "grocery/pharmacy MOB","park mobility","transit stn mobil","workpl mob","redisual mob", "gold price cr")
)




vars <- c("cpi_pop_alltime",
          "NewCases_change_rate", "emp_change_p", "exr_change_p",
          "rt_rc_mob", "groc_pha_mob", "parks_mob",
          "transit_mob", "work_mob", "resid_mob", "gold_usd_changerate")


# 각 열별로 Inf 개수 확인
sapply(tester1[, vars], function(x) sum(is.infinite(x)))




  # 1) Inf인 값만 NA로 치환
  tester1$NewCases_change_rate[ is.infinite(tester1$NewCases_change_rate) ] <- NA

# 2) 회귀에 필요한 열만 뽑아서(11개 변수)
vars <- c("cpi_pop_alltime",
          "NewCases_change_rate", "emp_change_p", "exr_change_p",
          "rt_rc_mob", "groc_pha_mob", "parks_mob",
          "transit_mob", "work_mob", "resid_mob", "gold_usd_changerate")
df_sub <- tester1[ , vars]

# 3) NA가 하나라도 있는 행 전부 제거
df_sub_clean <- df_sub[ complete.cases(df_sub), ]

# 4) 남은 행 번호(인덱스) 뽑아서 원본에서 그 행만 골라 회귀 돌리기
keep_idx <- as.integer(rownames(df_sub_clean))

model1 <- lm(cpi_pop_alltime ~ NewCases_change_rate
             + emp_change_p
             + exr_change_p
             + rt_rc_mob
             + groc_pha_mob
             + parks_mob
             + transit_mob
             + work_mob
             + resid_mob
             + gold_usd_changerate,
             data = tester1[keep_idx, ])
summary(model1)




# 1) Inf를 NA로 치환
tester1[is.infinite(as.matrix(tester1))] <- NA

# 2) NA가 있는 행 모두 제거
tester1 <- tester1[complete.cases(tester1), ]

# 3) 회귀 실행
model1 <- lm(
  cpi_pop_alltime ~ NewCases_change_rate + emp_change_p + exr_change_p +
    rt_rc_mob + groc_pha_mob + parks_mob + transit_mob +
    work_mob + resid_mob + gold_usd_changerate,
  data = tester1
)




Covid19 effect on CPI (Tester 1)
===========================================================
  Dependent variable:    
  ---------------------------
  CPI monthly Change Rate  
-----------------------------------------------------------
  covid 19 new cases monthly rate          -0.00000          
(0.00000)         

unemplotment rate change                   0.001           
(0.003)          

exchange rate of lcu /usd                0.131***          
  (0.012)          

mobility recrec                           -0.003           
(0.003)          

grocery/pharmacy MOB                      0.006**          
  (0.003)          

park mobility                             -0.001*          
  (0.001)          

transit stn mobil                         -0.002           
(0.002)          

workpl mob                               0.013***          
  (0.002)          

redisual mob                              -0.007           
(0.006)          

gold price cr                             0.022**          
  (0.009)          

Constant                                 0.616***          
  (0.082)          

-----------------------------------------------------------
  Observations                               1,194           
R2                                         0.188           
Adjusted R2                                0.182           
Residual Std. Error                  0.934 (df = 1183)     
F Statistic                      27.471*** (df = 10; 1183) 
===========================================================
  Note:                           *p<0.1; **p<0.05; ***p<0.01




# 모델 수정.
model10<- lm(NewCases_change_rate~ cpi_pop_alltime + emp_change_p + exr_change_p +rt_rc_mob + groc_pha_mob + parks_mob + transit_mob +work_mob + resid_mob + gold_usd_changerate, data = tester1
)

stargazer(model10,
          +           type = "text",
          +           title = "Covid19 effect wtf CPI (Tester 1)",
          +           dep.var.labels = "covid 19 new cases monthly rate ",
          +           covariate.labels = c("CPI monthly Change Rate ", "unemplotment rate change", "exchange rate of lcu /usd", "mobility recrec", "grocery/pharmacy MOB","park mobility","transit stn mobil","workpl mob","redisual mob", "gold price cr")
          + )




