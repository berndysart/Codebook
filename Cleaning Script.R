library(tidyverse)
library(haven)
library(knitr)

load("~/Desktop/data analysis/codebook/WVS_TimeSeries_4_0.rdata")
EVS = read_dta("ZA7503_v3-0-0.dta/ZA7503_v3-0-0.dta")
WVS = data1

WVS2.0 = #rewrote the names to making rowbinding easier across the two data sets
  WVS |>
  mutate(
    c_code = as.numeric(S003), #needed numeric country id
    c_name = S003, #rename variable to get the character name
    r_id = S007, #id variable name
    r_fulldate = as_date(ymd(S012)), #transform into proper date variable
    r_year = year(r_fulldate), #year of interview
    r_month = month(r_fulldate), #month of interview
    r_swt = S017, #survey weight
    r_female = X001, #allows for distinction between male and female respondents. Male = 1, Female = 2
    p_interest = E150, #responses to survey question: How often does the individual follow politics in the news
    p_trustA = G007_37, #feeling responses to level of trust of Americans
    p_trustR = G007_39, #feeling responses to level of trust of Russians
    Dness = E236, #feeling responses to the survey statement: Democraticness in own country
    PAR = E116, #feeling responses to the survey statement: Having the Army Rule
    DPS = E117, #feeling responses to the survey statement: Having a democratic political system
    DPBB = E123, #feeling responses to the survey statement: Democracy may have problems but is better
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
    p_ideology = E181C, #responses to survey question: Which political Party would you vote for/appeals to you, left/right scale
    p_interest = E150,
    Dness = E236,
    PAR = E116,
    DPS = E117,
    DPBB = E123,
    .keep = 'none'
  )

EWM = bind_rows(list(EVS = EVS2.0, WVS = WVS2.0), .id = 'source') #bind rows with new variable iding from which survey they originated from
EWM = filter(EWM, c_code %in% cd)
cd = read_csv("bernccodes.csv") |> #replacing the actual country codes with names
  pull(ccode)

EWM = #makes the source variable the first variable in the data frame
  EWM |>
  relocate(
    starts_with('source'),
    .before = r_id
  )
EWM = #recodes negative numbers as NA's that prevented any statistical analysis from being done
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

EMW = #uses the factor variable name instead of the numeric codes
  EMW |>
  mutate(across(where(is.labelled), as_factor))

Practice = EWM

save(EWM, file = 'EWM.RData') #cleaned data file
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
} #function that allowed for quick creation of data tables for codebook
count(Practice, r_month)

EWM = #transforming the month variable into one that displayed the names instead of number
  EWM |>
  mutate(
    r_month = month.name[r_month]
  )

cbfactor(Practice, r_month)