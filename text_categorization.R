library("tidyverse")
library("quanteda")

setwd("C:/Users/John/Dropbox/GitHub/Illinois-COVID")
spdg <- read_tsv("IL-covid spdg 0820 TAB DELIM.txt", col_types = cols(.default = "c"))


# run some initial analysis based on Agency Name, Fund Clean, Vendor Name.
# also note that some TEXT values are 'Agency marked as confidential', pull those out


# variable creation

spdg$Warrant_Amt_Numeric <- as.numeric(spdg$"Warrant Amt")
spdg$Agency_Name_Chr <- as.character(spdg$"Agency Name")
spdg$Fund_Clean_Chr <- as.character(spdg$"Fund Clean")
spdg$Vendor_Name_Chr <- as.character(spdg$"Vendor Name")

# evaluate

sum(spdg$Warrant_Amt_Numeric) # 1.18 billion

Amt_ByAgency <- spdg %>% 
  group_by(Agency_Name_Chr) %>%
  summarise(SumWarrant_Amt_Numeric = sum(Warrant_Amt_Numeric),
            Count = n()
  ) %>% 
  arrange(-SumWarrant_Amt_Numeric)

Amt_ByFund <- spdg %>% 
  group_by(Fund_Clean_Chr) %>%
  summarise(SumWarrant_Amt_Numeric = sum(Warrant_Amt_Numeric),
            Count = n()
  ) %>% 
  arrange(-SumWarrant_Amt_Numeric)

Amt_ByVendor <- spdg %>% 
  group_by(Vendor_Name_Chr) %>%
  summarise(SumWarrant_Amt_Numeric = sum(Warrant_Amt_Numeric),
            Count = n()
  ) %>% 
  arrange(-SumWarrant_Amt_Numeric)

Amt_ByAll <- spdg %>%
  group_by(Agency_Name_Chr,Fund_Clean_Chr,Vendor_Name_Chr) %>%
  summarise(SumWarrant_Amt_Numeric = sum(Warrant_Amt_Numeric),
            Count = n()
  ) %>% 
  arrange(-SumWarrant_Amt_Numeric)

Amt_ByConfidential <- spdg %>% filter(TEXT == "Agency marked as confidential") %>%
  group_by(Agency_Name_Chr,Fund_Clean_Chr,Vendor_Name_Chr) %>%
  summarise(SumWarrant_Amt_Numeric = sum(Warrant_Amt_Numeric),
            Count = n()
  ) %>% 
  arrange(-SumWarrant_Amt_Numeric)

sum(Amt_ByConfidential$SumWarrant_Amt_Numeric) # $47 million


knitr::kable(head(Amt_ByAgency, n = 10), digits = 0, format.args = list(big.mark = ",", scientific = FALSE))
knitr::kable(head(Amt_ByFund, n = 10), digits = 0, format.args = list(big.mark = ",", scientific = FALSE))
knitr::kable(head(Amt_ByVendor, n = 10), digits = 0, format.args = list(big.mark = ",", scientific = FALSE))
knitr::kable(head(Amt_ByAll, n = 10), digits = 0, format.args = list(big.mark = ",", scientific = FALSE))
knitr::kable(head(Amt_ByConfidential, n = 10), digits = 0, format.args = list(big.mark = ",", scientific = FALSE))


# categorization of text

dic <- dictionary(list(other_medical = c("ventilator","surgical","consumable", "medical", "supply", "supplies","refrigerated","trailer","hospital","beds","laundry","bleach","disinfectant","sanitizing","wipe"), 
                       testing = c("testing", "test", "site", "kit", "transport", "lab", "laboratory", "reagent"), 
                       ppe = c("ppe", "face", "mask", "nitrile", "glove", "n95", "kn95", "respirator", "infrared", "thermometer", "shield"), 
                       travel = c("travel", "attend", "meeting", "mileage", "lodging", "specs", "specifications", "invest", "under","expectations","expected","expecting","expect"),
                       food = c("catering", "breakfast", "lunch", "dinner", "food", "meal"),
                       provider = c("provider", "breakfast","clinic")
                       ))

TEXT_dfm <- dfm(spdg$TEXT, tolower = TRUE, remove = stopwords("english"))

res <- dfm_lookup(TEXT_dfm, dic, valuetype = "glob", nomatch = "UNMATCHED")

spdg_res <- cbind(convert(res, to = "data.frame"),spdg) 
