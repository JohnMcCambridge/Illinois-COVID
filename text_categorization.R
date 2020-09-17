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

cat_words = read.csv("category_words.csv", strip.white = TRUE, na.strings=c("","NA"))
cat_words_list <- lapply((as.list(cat_words)), function(x) x[!is.na(x)])

dic <- dictionary(cat_words_list)


TEXT_dfm <- dfm(spdg$TEXT, tolower = TRUE, remove = stopwords("english"))

res <- dfm_lookup(TEXT_dfm, dic, valuetype = "glob", nomatch = "UNMATCHED")

spdg_res <- cbind(convert(res, to = "data.frame"),spdg) 

spdg_res$TokenTotal <- ntoken(res)

spdg_res$MatchStatus <- FALSE
spdg_res$MatchStatus[spdg_res$TokenTotal > spdg_res$UNMATCHED] <- TRUE

#match rate
table(spdg_res$MatchStatus)

sum(spdg_res$Warrant_Amt_Numeric[spdg_res$travel > 0])
sum(spdg_res$Warrant_Amt_Numeric[spdg_res$ppe > 0])
