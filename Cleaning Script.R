library(tidyverse)
library(haven)
library(knitr)

load("~/Desktop/data analysis/codebook/WVS_TimeSeries_4_0.rdata")
EVS = read_dta("ZA7503_v3-0-0.dta/ZA7503_v3-0-0.dta")
WVS = data1

WVS2.0 =
  WVS |>
  mutate(
    c_code = as.numeric(S003),
    c_name = S003,
    r_id = S007,
    r_fulldate = as_date(ymd(S012)),
    r_year = year(r_fulldate),
    r_month = month(r_fulldate),
    r_swt = S017,
    r_female = X001,
    p_interest = E150,
    p_trustA = G007_37,
    p_trustR = G007_39,
    Dness = E236,
    PAR = E116,
    DPS = E117,
    DPBB = E123,
    .keep = 'none'
  )
EVS2.0 =
  EVS |>
  mutate(
    c_code = as.numeric(S003),
    c_name = S003,
    r_id = S007_01,
    r_fulldate = as_date(ymd(S012)),
    r_year = year(r_fulldate),
    r_month = month(r_fulldate),
    r_swt = S017,
    r_female = X001,
    p_ideology = E181C,
    p_interest = E150,
    Dness = E236,
    PAR = E116,
    DPS = E117,
    DPBB = E123,
    .keep = 'none'
  )

EWM = bind_rows(list(EVS = EVS2.0, WVS = WVS2.0), .id = 'source')
EWM = filter(EWM, c_code %in% cd)
cd = read_csv("bernccodes.csv") |>
  pull(ccode)

EWM =
  EWM |>
  relocate(
    starts_with('source'),
    .before = r_id
  )
EWM =
  EWM |>
  mutate(
    r_female = na_if(r_female, -5),
    r_female = na_if(r_female, -4),
    r_female = na_if(r_female, -2),
    r_female = na_if(r_female, -1),
    p_ideology = na_if(p_ideology, -5),
    p_ideology = na_if(p_ideology, -4),
    p_ideology = na_if(p_ideology, -3),
    p_ideology = na_if(p_ideology, -2),
    p_ideology = na_if(p_ideology, -1),
    p_interest = na_if(p_interest, -4),
    p_interest = na_if(p_interest, -2),
    p_interest = na_if(p_interest, -1),
    Dness = na_if(Dness, -5),
    Dness = na_if(Dness, -4),
    Dness = na_if(Dness, -2),
    Dness = na_if(Dness, -1),
    PAR = na_if(PAR, -5),
    PAR = na_if(PAR, -4),
    PAR = na_if(PAR, -2),
    PAR = na_if(PAR, -1),
    DPS = na_if(DPS, -5),
    DPS = na_if(DPS, -4),
    DPS = na_if(DPS, -2),
    DPS = na_if(DPS, -1),
    DPBB = na_if(DPBB, -4),
    DPBB = na_if(DPBB, -2),
    DPBB = na_if(DPBB, -1),
    p_trustA = na_if(p_trustA, -4),
    p_trustA = na_if(p_trustA, -1),
    p_trustR = na_if(p_trustR, -4),
    p_trustR = na_if(p_trustR, -1),
  )

EMW =
  EMW |>
  mutate(across(where(is.labelled), as_factor))

Practice = EWM

save(EWM, file = 'EWM.RData')
#factor-------------------
cbfactor = function(.data, x){
  x = enquo(x)
  count(.data, !!x) |>
    mutate(
      values = row_number(!!x),
      labels = as_factor(!!x),
      freq = n,
      perc = n / sum(n) * 100,
      .keep = 'unused'
    ) |>
    knitr::kable(format = 'pipe', digits = 1L)
}
count(Practice, r_month)

EWM = 
  EWM |>
  mutate(
    r_month = month.name[r_month]
  )

cbfactor(Practice, r_month)


