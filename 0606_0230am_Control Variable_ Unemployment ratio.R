# 0606새벽

# 할 계산/전처리
# 실업률 rate, 인구비율.

empl_raw.csv

emp<-emp%>%rename(period=...1)
emp


  exr02_cl<-exr02%>%
  +     mutate(year_month_str=str_replace(period,"M","-"),date=as.Date(paste0(year_month_str,"-01")))%>%filter(date>=as.Date("2020-01-01"))%>%select(date,Country,exr)
  
  # 아래는 증감률
exr99_00<-exr99%>%mutate(prev_month = year_month %m-% months(1))%>%
  +     left_join(
    +         exr99%>% 
      +             select(year_month,Country, exr) %>%
      +             rename(prev_month = year_month, prev_exr =exr),
    +         by = c("Country","prev_month")) %>%mutate(exr_change_p = (exr-prev_exr)/prev_exr*100)
head(exr99_00);
tail(exr99_00)



# PIVOT LONGER
exr02<-exr01%>%pivot_longer(cols = -c(period),names_to=c("Country"),values_to="exr")
head(exr02);
tail(exr02)

# pseudocode - skeleton goes...
emp01<-emp%>%mutate(year_month_str=str_replace(period,"M","-"),date=as.Date(paste0(year_month_str,"-01")))%>%filter(date>=as.Date("2018-12-01"))%>%select(date,Country,???)



# real code
emp<-emp%>%rename(period=...1)
emp01<-emp%>%pivot_longer(cols=-c(period),names_to = c("Country"),values_to="emp")
emp02<-emp01%>%mutate(year_month_str=str_replace(period,"M","-"),date=as.Date(paste0(year_month_str,"-01")))%>%filter(date>=as.Date("2018-12-01"))%>%select(date,Country,emp)
emp02
# A tibble: 5,082 × 3
date       Country       emp
<date>     <chr>       <dbl>
  1 2018-12-01 Argentina   10.0 
2 2018-12-01 Australia    5.07
3 2018-12-01 Austria      7.65

emp06<-emp04%>%filter(year_month>=as.Date("2020-01-01"))%>%select(year_month,Country,emp_change_p)
emp07<-emp06%>%filter(year_month<=as.Date("2025-01-01"))%>%select(year_month,Country,emp_change_p)
emp07


# final  // Save Unemployment Rate data

write_csv(emp07,"emp_changerate_bmd0606_00.csv")

