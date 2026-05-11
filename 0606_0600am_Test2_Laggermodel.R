tester11 <- tester1

# 1) 정렬 및 그룹별로 1기 시차 생성
tester11 <- tester11 %>%
  arrange(Country, year_month) %>%
  group_by(Country) %>%
  mutate(lag1_NewCases = lag(NewCases_change_rate, 1)) %>%
  ungroup()

# 2) 시차 생성 과정에서 생긴 NA(그룹별 첫 달 등) 제거
tester11 <- tester11 %>%
  filter(!is.na(lag1_NewCases))

# 3) Inf → NA 치환 (벡터 인덱싱으로만!)
tester11[ is.infinite(as.matrix(tester11)) ] <- NA

# 4) NA가 하나라도 있는 행 모두 제거
tester11 <- tester11[ complete.cases(tester11), ]

# 5) 최종 회귀 실행
model_lag <- lm(
  cpi_pop_alltime ~ lag1_NewCases + emp_change_p + exr_change_p +
    rt_rc_mob + groc_pha_mob + parks_mob + transit_mob +
    work_mob + resid_mob + gold_usd_changerate,
  data = tester11
)





stargazer(
  model_lag,
  type = "text",
  title = "Covid19 Effect on CPI with 1-Month Lag (Tester 1)",
  dep.var.labels = "CPI Monthly Change Rate",
  covariate.labels = c(
    "Covid-19 New Cases Monthly Rate (Lag 1)",
    "Unemployment Rate Change",
    "Exchange Rate (LCU/USD)",
    "Mobility: Recreation & Retail",
    "Mobility: Grocery/Pharmacy",
    "Mobility: Park",
    "Mobility: Transit Stations",
    "Mobility: Workplace",
    "Mobility: Residential",
    "Gold Price Change Rate"
  )
)




#국가 고정효과
#지금은 나라별 변수(모빌리티, 확진률, 환율, 금 가격 등)를 모두 포함했지만, 
#여전히 각 국가의 “불변한 특성(제도, 구조, 기본 물가 수준 등)”이 결과에 영향을 줄 수 있다


model_fe <- lm(
  cpi_pop_alltime ~ lag1_NewCases + emp_change_p + exr_change_p +
    rt_rc_mob + groc_pha_mob + parks_mob + transit_mob +
    work_mob + resid_mob + gold_usd_changerate +
    factor(Country),
  data = tester11
)
summary(model_fe)


# “국가별 상수항 차이”를 통제 
#각 나라 내에서의 시계열 변화 효과만 추정 

# 시점고정효과

model_fe2 <- lm(
  cpi_pop_alltime ~ lag1_NewCases + emp_change_p + exr_change_p +
    rt_rc_mob + groc_pha_mob + parks_mob + transit_mob +
    work_mob + resid_mob + gold_usd_changerate +
    factor(Country) + factor(year_month),
  data = tester11
)

summary(model_fe2)



# 전 세계적인 물가 충격까지 통제 
# ==> 코로나 확진률의 효과가 더 분리


Call:
  lm(formula = cpi_pop_alltime ~ lag1_NewCases + emp_change_p + 
       exr_change_p + rt_rc_mob + groc_pha_mob + parks_mob + transit_mob + 
       work_mob + resid_mob + gold_usd_changerate + factor(Country), 
     data = tester11)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.5845 -0.3371 -0.0353  0.2785  8.2047 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)                        3.511e+00  1.677e-01  20.943  < 2e-16 ***
  lag1_NewCases                     -4.961e-07  4.862e-07  -1.020  0.30774    
emp_change_p                       1.277e-03  2.409e-03   0.530  0.59623    
exr_change_p                       9.954e-02  1.066e-02   9.336  < 2e-16 ***
  rt_rc_mob                          1.690e-03  3.959e-03   0.427  0.66954    
groc_pha_mob                       8.072e-03  2.999e-03   2.692  0.00721 ** 
  parks_mob                         -1.783e-03  9.550e-04  -1.867  0.06215 .  
transit_mob                       -6.618e-04  2.205e-03  -0.300  0.76417    
work_mob                           6.555e-03  2.619e-03   2.503  0.01246 *  
  resid_mob                          7.087e-03  8.118e-03   0.873  0.38287    
gold_usd_changerate                2.101e-02  7.726e-03   2.720  0.00663 ** 
  factor(Country)Austria            -2.901e+00  2.113e-01 -13.730  < 2e-16 ***
  factor(Country)Belgium            -2.911e+00  2.140e-01 -13.602  < 2e-16 ***
  factor(Country)Brazil             -3.320e+00  1.991e-01 -16.678  < 2e-16 ***
  factor(Country)Bulgaria           -2.767e+00  2.177e-01 -12.708  < 2e-16 ***
  factor(Country)Canada             -3.068e+00  2.186e-01 -14.036  < 2e-16 ***
  factor(Country)Chile              -3.046e+00  2.066e-01 -14.741  < 2e-16 ***
  factor(Country)Colombia           -3.369e+00  1.999e-01 -16.854  < 2e-16 ***
  factor(Country)Denmark            -2.860e+00  2.341e-01 -12.219  < 2e-16 ***
  factor(Country)Dominican Republic -2.871e+00  2.175e-01 -13.200  < 2e-16 ***
  factor(Country)Ecuador            -3.688e+00  2.309e-01 -15.969  < 2e-16 ***
  factor(Country)Finland            -2.994e+00  2.229e-01 -13.436  < 2e-16 ***
  factor(Country)France             -3.151e+00  2.184e-01 -14.428  < 2e-16 ***
  factor(Country)Germany            -3.001e+00  2.141e-01 -14.016  < 2e-16 ***
  factor(Country)Greece             -3.203e+00  2.447e-01 -13.089  < 2e-16 ***
  factor(Country)Hungary            -2.682e+00  2.191e-01 -12.239  < 2e-16 ***
  factor(Country)Ireland            -3.091e+00  2.126e-01 -14.541  < 2e-16 ***
  factor(Country)Israel             -3.214e+00  2.081e-01 -15.444  < 2e-16 ***
  factor(Country)Italy              -2.969e+00  2.103e-01 -14.121  < 2e-16 ***
  factor(Country)Japan              -3.493e+00  2.039e-01 -17.129  < 2e-16 ***
  factor(Country)Luxembourg         -3.010e+00  2.137e-01 -14.082  < 2e-16 ***
  factor(Country)Malta              -3.205e+00  2.134e-01 -15.017  < 2e-16 ***
  factor(Country)Mexico             -3.139e+00  2.017e-01 -15.564  < 2e-16 ***
  factor(Country)Morocco            -3.522e+00  2.175e-01 -16.191  < 2e-16 ***
  factor(Country)Norway             -2.936e+00  2.223e-01 -13.206  < 2e-16 ***
  factor(Country)Pakistan           -3.093e+00  2.893e-01 -10.690  < 2e-16 ***
  factor(Country)Peru               -3.262e+00  2.169e-01 -15.039  < 2e-16 ***
  factor(Country)Philippines        -3.325e+00  2.272e-01 -14.637  < 2e-16 ***
  factor(Country)Portugal           -3.254e+00  2.115e-01 -15.388  < 2e-16 ***
  factor(Country)Romania            -2.768e+00  2.093e-01 -13.229  < 2e-16 ***
  factor(Country)Saudi Arabia       -3.416e+00  2.110e-01 -16.189  < 2e-16 ***
  factor(Country)Singapore          -3.240e+00  2.170e-01 -14.931  < 2e-16 ***
  factor(Country)South Africa       -3.245e+00  2.237e-01 -14.503  < 2e-16 ***
  factor(Country)Spain              -3.023e+00  2.075e-01 -14.570  < 2e-16 ***
  factor(Country)Sri Lanka          -1.664e+00  2.269e-01  -7.331 4.41e-13 ***
  factor(Country)Sweden             -2.908e+00  2.224e-01 -13.076  < 2e-16 ***
  factor(Country)Switzerland        -3.249e+00  2.106e-01 -15.432  < 2e-16 ***
  factor(Country)Thailand           -3.436e+00  2.456e-01 -13.989  < 2e-16 ***
  factor(Country)Uruguay            -3.032e+00  1.970e-01 -15.393  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7488 on 1106 degrees of freedom
Multiple R-squared:  0.5017,	Adjusted R-squared:  0.4801 
F-statistic:  23.2 on 48 and 1106 DF,  p-value: < 2.2e-16



Call:
  lm(formula = cpi_pop_alltime ~ lag1_NewCases + emp_change_p + 
       exr_change_p + rt_rc_mob + groc_pha_mob + parks_mob + transit_mob + 
       work_mob + resid_mob + gold_usd_changerate + factor(Country) + 
       factor(year_month), data = tester11)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.3923 -0.3149 -0.0070  0.2703  7.6921 

Coefficients: (1 not defined because of singularities)
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                        2.794e+00  4.286e-01   6.520 1.08e-10 ***
  lag1_NewCases                      8.272e-07  5.895e-07   1.403  0.16080    
emp_change_p                       4.269e-03  2.451e-03   1.742  0.08177 .  
exr_change_p                       8.738e-02  1.191e-02   7.339 4.23e-13 ***
  rt_rc_mob                          4.710e-03  4.447e-03   1.059  0.28981    
groc_pha_mob                       7.049e-03  3.463e-03   2.035  0.04206 *  
  parks_mob                         -1.383e-03  1.177e-03  -1.174  0.24051    
transit_mob                       -2.012e-03  2.173e-03  -0.926  0.35467    
work_mob                          -4.801e-03  3.085e-03  -1.556  0.11996    
resid_mob                          1.328e-02  8.918e-03   1.489  0.13673    
gold_usd_changerate               -9.109e-01  3.949e-01  -2.307  0.02125 *  
  factor(Country)Austria            -3.306e+00  2.099e-01 -15.749  < 2e-16 ***
  factor(Country)Belgium            -3.380e+00  2.171e-01 -15.567  < 2e-16 ***
  factor(Country)Brazil             -3.316e+00  1.915e-01 -17.317  < 2e-16 ***
  factor(Country)Bulgaria           -3.068e+00  2.133e-01 -14.384  < 2e-16 ***
  factor(Country)Canada             -3.481e+00  2.245e-01 -15.505  < 2e-16 ***
  factor(Country)Chile              -3.179e+00  2.037e-01 -15.606  < 2e-16 ***
  factor(Country)Colombia           -3.403e+00  1.929e-01 -17.645  < 2e-16 ***
  factor(Country)Denmark            -3.357e+00  2.466e-01 -13.611  < 2e-16 ***
  factor(Country)Dominican Republic -3.306e+00  2.176e-01 -15.191  < 2e-16 ***
  factor(Country)Ecuador            -3.996e+00  2.351e-01 -16.999  < 2e-16 ***
  factor(Country)Finland            -3.437e+00  2.308e-01 -14.889  < 2e-16 ***
  factor(Country)France             -3.517e+00  2.199e-01 -15.994  < 2e-16 ***
  factor(Country)Germany            -3.322e+00  2.198e-01 -15.113  < 2e-16 ***
  factor(Country)Greece             -3.465e+00  2.451e-01 -14.138  < 2e-16 ***
  factor(Country)Hungary            -3.030e+00  2.218e-01 -13.663  < 2e-16 ***
  factor(Country)Ireland            -3.539e+00  2.130e-01 -16.613  < 2e-16 ***
  factor(Country)Israel             -3.608e+00  2.074e-01 -17.402  < 2e-16 ***
  factor(Country)Italy              -3.333e+00  2.116e-01 -15.752  < 2e-16 ***
  factor(Country)Japan              -3.742e+00  2.008e-01 -18.632  < 2e-16 ***
  factor(Country)Luxembourg         -3.431e+00  2.163e-01 -15.865  < 2e-16 ***
  factor(Country)Malta              -3.547e+00  2.119e-01 -16.734  < 2e-16 ***
  factor(Country)Mexico             -3.327e+00  1.955e-01 -17.017  < 2e-16 ***
  factor(Country)Morocco            -3.801e+00  2.179e-01 -17.446  < 2e-16 ***
  factor(Country)Norway             -3.420e+00  2.281e-01 -14.993  < 2e-16 ***
  factor(Country)Pakistan           -2.989e+00  2.823e-01 -10.590  < 2e-16 ***
  factor(Country)Peru               -3.471e+00  2.164e-01 -16.040  < 2e-16 ***
  factor(Country)Philippines        -3.720e+00  2.300e-01 -16.171  < 2e-16 ***
  factor(Country)Portugal           -3.620e+00  2.109e-01 -17.163  < 2e-16 ***
  factor(Country)Romania            -3.103e+00  2.054e-01 -15.107  < 2e-16 ***
  factor(Country)Saudi Arabia       -3.689e+00  2.096e-01 -17.598  < 2e-16 ***
  factor(Country)Singapore          -3.605e+00  2.181e-01 -16.532  < 2e-16 ***
  factor(Country)South Africa       -3.493e+00  2.243e-01 -15.576  < 2e-16 ***
  factor(Country)Spain              -3.354e+00  2.054e-01 -16.329  < 2e-16 ***
  factor(Country)Sri Lanka          -2.083e+00  2.284e-01  -9.120  < 2e-16 ***
  factor(Country)Sweden             -3.390e+00  2.289e-01 -14.810  < 2e-16 ***
  factor(Country)Switzerland        -3.622e+00  2.110e-01 -17.166  < 2e-16 ***
  factor(Country)Thailand           -3.796e+00  2.421e-01 -15.681  < 2e-16 ***
  factor(Country)Uruguay            -3.136e+00  1.895e-01 -16.547  < 2e-16 ***
  factor(year_month)2020-04-01       4.925e+00  2.636e+00   1.868  0.06197 .  
factor(year_month)2020-05-01       2.161e+00  1.171e+00   1.845  0.06527 .  
factor(year_month)2020-06-01       1.722e+00  7.585e-01   2.270  0.02343 *  
  factor(year_month)2020-07-01       6.612e+00  2.914e+00   2.269  0.02347 *  
  factor(year_month)2020-08-01       6.937e+00  3.066e+00   2.263  0.02386 *  
  factor(year_month)2020-09-01      -1.513e+00  5.800e-01  -2.609  0.00921 ** 
  factor(year_month)2020-10-01      -3.510e-01  1.828e-01  -1.920  0.05509 .  
factor(year_month)2020-11-01      -1.196e+00  4.261e-01  -2.807  0.00509 ** 
  factor(year_month)2020-12-01       4.326e-01  2.788e-01   1.551  0.12108    
factor(year_month)2021-01-01       1.363e+00  6.323e-01   2.155  0.03135 *  
  factor(year_month)2021-02-01      -2.168e+00  8.846e-01  -2.451  0.01441 *  
  factor(year_month)2021-03-01      -3.745e+00  1.595e+00  -2.348  0.01908 *  
  factor(year_month)2021-04-01       3.156e+00  1.389e+00   2.272  0.02328 *  
  factor(year_month)2021-05-01       5.529e+00  2.437e+00   2.268  0.02350 *  
  factor(year_month)2021-06-01      -3.172e-01  1.663e-01  -1.907  0.05673 .  
factor(year_month)2021-07-01      -8.185e-01  2.662e-01  -3.074  0.00216 ** 
  factor(year_month)2021-08-01      -5.428e-01  2.063e-01  -2.631  0.00863 ** 
  factor(year_month)2021-09-01       5.076e-01  2.876e-01   1.765  0.07788 .  
factor(year_month)2021-10-01       1.002e+00  4.100e-01   2.444  0.01466 *  
  factor(year_month)2021-11-01       3.051e+00  1.357e+00   2.248  0.02476 *  
  factor(year_month)2021-12-01      -1.114e+00  3.835e-01  -2.904  0.00376 ** 
  factor(year_month)2022-01-01       2.527e+00  1.061e+00   2.383  0.01735 *  
  factor(year_month)2022-02-01       3.212e+00  1.253e+00   2.563  0.01051 *  
  factor(year_month)2022-03-01       6.093e+00  2.339e+00   2.605  0.00931 ** 
  factor(year_month)2022-04-01       8.548e-01  1.903e-01   4.491 7.86e-06 ***
  factor(year_month)2022-05-01      -2.820e+00  1.368e+00  -2.062  0.03943 *  
  factor(year_month)2022-06-01       7.733e-01  1.782e-01   4.340 1.56e-05 ***
  factor(year_month)2022-07-01      -4.034e+00  1.714e+00  -2.353  0.01879 *  
  factor(year_month)2022-08-01       2.476e+00  1.070e+00   2.315  0.02083 *  
  factor(year_month)2022-09-01      -3.235e+00  1.467e+00  -2.205  0.02764 *  
  factor(year_month)2022-10-01              NA         NA      NA       NA    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.714 on 1076 degrees of freedom
Multiple R-squared:  0.5592,	Adjusted R-squared:  0.5273 
F-statistic:  17.5 on 78 and 1076 DF,  p-value: < 2.2e-16


# ====================================================
# additional revision

#여러 개월 시차를 한꺼번에 넣기

#코로나 충격이 물가에 반영되려면 2~3개월 정도 걸릴 가능성 존재
# 2기, 3기 시차까지 합쳐서 어느 기간의 확진률이 CPI에 유의한 영향을 주는가

tester11 <- tester11 %>%
  group_by(Country) %>%
  mutate(
    lag1_NewCases = lag(NewCases_change_rate, 1),
    lag2_NewCases = lag(NewCases_change_rate, 2),
    lag3_NewCases = lag(NewCases_change_rate, 3)
  ) %>%
  filter(!is.na(lag1_NewCases) & !is.na(lag2_NewCases) & !is.na(lag3_NewCases)) %>%
  ungroup()
model_multi_lag <- lm(
  cpi_pop_alltime ~ lag1_NewCases + lag2_NewCases + lag3_NewCases +
    emp_change_p + exr_change_p + rt_rc_mob + groc_pha_mob +
    parks_mob + transit_mob + work_mob + resid_mob + gold_usd_changerate +
    factor(Country) + factor(year_month),
  data = tester11
)
summary(model_multi_lag)


Call:
  lm(formula = cpi_pop_alltime ~ lag1_NewCases + lag2_NewCases + 
       lag3_NewCases + emp_change_p + exr_change_p + rt_rc_mob + 
       groc_pha_mob + parks_mob + transit_mob + work_mob + resid_mob + 
       gold_usd_changerate + factor(Country) + factor(year_month), 
     data = tester11)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.5275 -0.3127  0.0018  0.2717  7.6261 

Coefficients: (1 not defined because of singularities)
Estimate Std. Error t value Pr(>|t|)    
(Intercept)                        3.960e+00  2.877e-01  13.764  < 2e-16 ***
  lag1_NewCases                     -2.646e-05  3.074e-05  -0.861  0.38959    
lag2_NewCases                     -4.315e-05  3.068e-05  -1.406  0.15993    
lag3_NewCases                      5.218e-07  1.244e-06   0.419  0.67510    
emp_change_p                       2.028e-03  4.577e-03   0.443  0.65780    
exr_change_p                       9.752e-02  1.249e-02   7.809 1.51e-14 ***
  rt_rc_mob                          4.629e-03  4.894e-03   0.946  0.34454    
groc_pha_mob                       5.462e-03  4.133e-03   1.322  0.18660    
parks_mob                         -1.172e-03  1.217e-03  -0.963  0.33573    
transit_mob                       -1.144e-03  2.292e-03  -0.499  0.61798    
work_mob                          -4.983e-03  3.185e-03  -1.565  0.11802    
resid_mob                          1.821e-02  9.814e-03   1.856  0.06377 .  
gold_usd_changerate               -5.225e-02  2.092e-01  -0.250  0.80283    
factor(Country)Austria            -3.548e+00  2.180e-01 -16.275  < 2e-16 ***
  factor(Country)Belgium            -3.584e+00  2.246e-01 -15.955  < 2e-16 ***
  factor(Country)Brazil             -3.457e+00  2.002e-01 -17.268  < 2e-16 ***
  factor(Country)Bulgaria           -3.233e+00  2.246e-01 -14.396  < 2e-16 ***
  factor(Country)Canada             -3.670e+00  2.359e-01 -15.561  < 2e-16 ***
  factor(Country)Chile              -3.421e+00  2.135e-01 -16.020  < 2e-16 ***
  factor(Country)Colombia           -3.603e+00  1.985e-01 -18.152  < 2e-16 ***
  factor(Country)Denmark            -3.639e+00  2.598e-01 -14.008  < 2e-16 ***
  factor(Country)Dominican Republic -3.539e+00  2.295e-01 -15.419  < 2e-16 ***
  factor(Country)Ecuador            -4.188e+00  2.449e-01 -17.099  < 2e-16 ***
  factor(Country)Finland            -3.676e+00  2.404e-01 -15.290  < 2e-16 ***
  factor(Country)France             -3.796e+00  2.279e-01 -16.660  < 2e-16 ***
  factor(Country)Germany            -3.594e+00  2.285e-01 -15.733  < 2e-16 ***
  factor(Country)Greece             -3.606e+00  2.626e-01 -13.730  < 2e-16 ***
  factor(Country)Hungary            -3.290e+00  2.279e-01 -14.435  < 2e-16 ***
  factor(Country)Ireland            -3.736e+00  2.215e-01 -16.866  < 2e-16 ***
  factor(Country)Israel             -3.814e+00  2.157e-01 -17.679  < 2e-16 ***
  factor(Country)Italy              -3.565e+00  2.197e-01 -16.230  < 2e-16 ***
  factor(Country)Japan              -3.984e+00  2.116e-01 -18.823  < 2e-16 ***
  factor(Country)Luxembourg         -3.686e+00  2.243e-01 -16.436  < 2e-16 ***
  factor(Country)Malta              -3.761e+00  2.198e-01 -17.113  < 2e-16 ***
  factor(Country)Mexico             -3.505e+00  2.021e-01 -17.345  < 2e-16 ***
  factor(Country)Morocco            -3.983e+00  2.370e-01 -16.807  < 2e-16 ***
  factor(Country)Norway             -3.694e+00  2.379e-01 -15.528  < 2e-16 ***
  factor(Country)Pakistan           -3.026e+00  3.186e-01  -9.497  < 2e-16 ***
  factor(Country)Peru               -3.705e+00  2.299e-01 -16.118  < 2e-16 ***
  factor(Country)Philippines        -3.984e+00  2.431e-01 -16.385  < 2e-16 ***
  factor(Country)Portugal           -3.873e+00  2.183e-01 -17.743  < 2e-16 ***
  factor(Country)Romania            -3.268e+00  2.118e-01 -15.431  < 2e-16 ***
  factor(Country)Saudi Arabia       -4.070e+00  2.167e-01 -18.781  < 2e-16 ***
  factor(Country)Singapore          -3.854e+00  2.297e-01 -16.782  < 2e-16 ***
  factor(Country)South Africa       -3.802e+00  2.367e-01 -16.066  < 2e-16 ***
  factor(Country)Spain              -3.593e+00  2.139e-01 -16.796  < 2e-16 ***
  factor(Country)Sri Lanka          -2.303e+00  2.414e-01  -9.542  < 2e-16 ***
  factor(Country)Sweden             -3.670e+00  2.397e-01 -15.312  < 2e-16 ***
  factor(Country)Switzerland        -3.877e+00  2.197e-01 -17.647  < 2e-16 ***
  factor(Country)Thailand           -4.019e+00  2.604e-01 -15.434  < 2e-16 ***
  factor(Country)Uruguay            -3.319e+00  1.961e-01 -16.925  < 2e-16 ***
  factor(year_month)2020-07-01      -1.066e-01  1.541e+00  -0.069  0.94485    
factor(year_month)2020-08-01       1.228e-01  1.614e+00   0.076  0.93936    
factor(year_month)2020-09-01      -4.579e-01  3.546e-01  -1.292  0.19683    
factor(year_month)2020-10-01      -3.320e-01  1.804e-01  -1.840  0.06608 .  
factor(year_month)2020-11-01      -4.957e-01  2.833e-01  -1.750  0.08048 .  
factor(year_month)2020-12-01      -1.578e-01  2.061e-01  -0.766  0.44395    
factor(year_month)2021-01-01      -1.315e-01  3.575e-01  -0.368  0.71304    
factor(year_month)2021-02-01      -4.317e-01  5.050e-01  -0.855  0.39285    
factor(year_month)2021-03-01      -4.377e-01  8.708e-01  -0.503  0.61535    
factor(year_month)2021-04-01       2.160e-02  7.361e-01   0.029  0.97660    
factor(year_month)2021-05-01       1.504e-01  1.284e+00   0.117  0.90681    
factor(year_month)2021-06-01      -4.054e-01  1.681e-01  -2.412  0.01606 *  
  factor(year_month)2021-07-01      -5.009e-01  2.102e-01  -2.383  0.01739 *  
  factor(year_month)2021-08-01      -3.962e-01  1.864e-01  -2.125  0.03380 *  
  factor(year_month)2021-09-01      -9.634e-02  1.996e-01  -0.483  0.62945    
factor(year_month)2021-10-01       8.627e-02  2.512e-01   0.343  0.73138    
factor(year_month)2021-11-01       1.654e-02  7.247e-01   0.023  0.98179    
factor(year_month)2021-12-01      -4.672e-01  2.605e-01  -1.794  0.07316 .  
factor(year_month)2022-01-01       1.428e-01  5.679e-01   0.251  0.80151    
factor(year_month)2022-02-01       4.704e-01  6.720e-01   0.700  0.48409    
factor(year_month)2022-03-01       9.791e-01  1.238e+00   0.791  0.42905    
factor(year_month)2022-04-01       5.052e-01  1.649e-01   3.063  0.00225 ** 
  factor(year_month)2022-05-01       1.879e-02  7.440e-01   0.025  0.97986    
factor(year_month)2022-06-01       5.044e-01  1.624e-01   3.106  0.00195 ** 
  factor(year_month)2022-07-01      -4.531e-01  9.292e-01  -0.488  0.62590    
factor(year_month)2022-08-01       9.264e-02  5.696e-01   0.163  0.87084    
factor(year_month)2022-09-01      -1.577e-01  7.960e-01  -0.198  0.84302    
factor(year_month)2022-10-01              NA         NA      NA       NA    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7 on 960 degrees of freedom
Multiple R-squared:  0.5895,	Adjusted R-squared:  0.5565 
F-statistic:  17.9 on 77 and 960 DF,  p-value: < 2.2e-16 


