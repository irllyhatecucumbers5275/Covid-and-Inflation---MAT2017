
# filter


f99_2020_clean <- f99_2020 %>%
  +     filter(
    +         if_all(5:10, ~ !is.na(.) & is.finite(.))
    +     )
> f99_2023_clean <- f99_2023 %>%
  +     filter(
    +         if_all(5:10, ~ !is.na(.) & is.finite(.))
    +     )
# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 2020 데이터 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

DATA IS : f99_2020_clean

SCATTERPLOT
new_cases
ggplot(f99_2020_clean, aes(new_cases, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and new covid cases ") + xlab("new monthly covid cases") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


SCATTERPLOT
new_death
ggplot(f99_2020_clean, aes(new_death, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and new death cases") + xlab("new monthly Covid death cases") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label =Country), vjust = + 1.5)


SCATTERPLOT
emp
ggplot(f99_2020_clean, aes(emp, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and unemployment ") + xlab("unemployment percentage of population") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


SCATTERPLOT
exr
ggplot(f99_2020_clean, aes(exr, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and exchange rate of local unit per usd") + xlab("exchange rate of lcu per usd") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)

SCATTERPLOT
gold V CPI
ggplot(f99_2020_clean, aes(gold, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  +     geom_point() + ggtitle("Cpi, and gold") + xlab("goldPRICE -USD") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


> names(f99_2020_clean)
[1] "iso2c"           "Country"         "WHO_region"     
[4] "year_month"      "new_cases"       "new_death"      
[7] "cpi_pop_alltime" "emp"             "exr"            
[10] "gold"  

write_csv(f99_2020_clean,"FINAL_2020_dt.csv")
write_csv(f99_2023_clean,"FINAL_2023_dt.csv")

ggplot, stargazer 설치치

MODEL
model2020<-lm(cpi_pop_alltime ~ new_cases + new_death+emp+exr+gold, data =f99_2020_clean)

stargazer(model2020_00,
  type = "text",
  title = "Effect of Covid19 on CPI",
  dep.var.labels = "CPI,POP (percentage of previous period)",
  covariate.labels = c("New Covid 19 cases(monthly)", "New Covid 19 death Cases (monthly)", "Unemployment Rate (Percentage of total population, monthly", "Exchange rate of local Currency unit per usd (monthly)", "Monthly Gold Price (USD)" ))


2020모델 수정해야함함



corrocrororo

num_vars <- f99_2020_clean[, sapply(f99_2020_clean, is.numeric)]

# 상관행렬 계산
cor_matrix <- cor(num_vars, use = "complete.obs")

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex=0.8)





# ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 2023 데이터 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

DATA IS : f99_2023_clean

SCATTERPLOT
new_cases
ggplot(f99_2023_clean, aes(new_cases, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and new covid cases (2023) ") + xlab("new monthly covid cases") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


SCATTERPLOT
new_death
ggplot(f99_2023_clean, aes(new_death, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and new death cases (2023)") + xlab("new monthly Covid death cases") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label =Country), vjust = + 1.5)


SCATTERPLOT
emp
ggplot(f99_2023_clean, aes(emp, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and unemployment (2023) ") + xlab("unemployment percentage of population") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


SCATTERPLOT
exr
ggplot(f99_2023_clean, aes(exr, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  geom_point() + ggtitle("Cpi, and exchange rate of local unit per usd (2023)") + xlab("exchange rate of lcu per usd") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)

SCATTERPLOT
gold V CPI
ggplot(f99_2023_clean, aes(gold, cpi_pop_alltime, label = Country)) + # ggplot2 plot with labels
  +     geom_point() + ggtitle("Cpi, and gold price (2023)") + xlab("goldPRICE -USD") + ylab("CPI, POP") + geom_smooth(method=lm) + geom_text(aes(label = Country), vjust = + 1.5)


> names(f99_2023_clean)
[1] "iso2c"           "Country"         "WHO_region"     
[4] "year_month"      "new_cases"       "new_death"      
[7] "cpi_pop_alltime" "emp"             "exr"            
[10] "gold"  

MODEL
model2023<-lm(cpi_pop_alltime ~ new_cases + new_death+emp+exr+gold, data =f99_2023_clean)

stargazer(model2023,
          type = "text",
          title = "Effect of Covid19 on CPI (2023 monthly cases)",
          dep.var.labels = "CPI,POP (percentage of previous period)",
          covariate.labels = c("New Covid 19 cases(monthly)", "New Covid 19 death Cases (monthly)", "Unemployment Rate (Percentage of total population, monthly", "Exchange rate of local Currency unit per usd (monthly)", "Monthly Gold Price (USD)" ))


