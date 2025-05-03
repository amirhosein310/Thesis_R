setwd('H:/Confilience/Orbis_001')


# Load necessary libraries
library(readr)
library(readxl)
library(data.table)
library(dplyr)

# Read CSV files
df1 <- read_csv("uqqz76erbdjknpg1.csv")
df2 <- read_csv("cebtf8h91bac2xos.csv")
df3 <- read_csv("gll09dufv0tnldoy.csv")
df4 <- read_csv("lgllysd7jgeb8yfc.csv")
df5 <- read_csv("mn2hjisv1lvewgji.csv")

# Combine all into one dataframe
combined_df2 <- bind_rows(df1, df2, df3, df5)

# View the first few rows
summary(combined_df2$CLOSDATE_year)


#merging with PLZ mapping

# merge with kreis data to get match the zipcode to kreis code
plz_mapping <- read_excel("georef-germany-postleitzahl.xlsx")
# Drop the Geometry column
plz_mapping$Geometry <- NULL
# Rename and clean up Kreis code (district code) column
plz_mapping$ao_kreis <- plz_mapping$`Kreis code`
plz_mapping$`Kreis code` <- NULL
plz_mapping$ao_kreis <- sub("^0+", "", plz_mapping$ao_kreis)  # Remove leading zeros
plz_mapping$ao_kreis <- as.numeric(plz_mapping$ao_kreis)      # Convert to numeric
# Rename postal code column
plz_mapping$plz <- plz_mapping$`Postleitzahl / Post code`
plz_mapping$`Postleitzahl / Post code` <- NULL
# add former east Germany state dummy (1 for former east German county 0 for west)
plz_mapping <- plz_mapping %>%
  mutate(former_east = ifelse(`Land code` %in% c(12, 13, 14, 15, 16), 1, 0))

#merge the mapping with the dataset to add the geolocation and also the kreis code and name
plz_mapping_unique <- plz_mapping %>%
  distinct(plz, .keep_all = TRUE)  # Keeps the first occurrence and removes duplicates

# Then merge again
firms_merged <- merge(combined_df2, plz_mapping_unique, by.x = "POSTCODE", by.y = "plz", all.x = TRUE, all.y = FALSE)

#keep only GAAP data
table(firms_merged$ACCPRACTICE)

firms_merged <- firms_merged %>%
  filter(ACCPRACTICE == "Local GAAP")


#sanity checks
sum(duplicated(firms_merged[, c("bvdid", "CLOSDATE_year")])) # 51686 difference between CONSCODE / Source(Annual report or registry filing / Audit status)
sum(is.na(firms_merged$bvdid)) # 0
colSums(is.na(firms_merged))
nrow(firms_merged) - nrow(distinct(firms_merged)) # 180
unique(firms_merged$COUNTRY) # Germany

# keep only the local registry filings
table(firms_merged$FILING_TYPE) # Annual report = 7779 | Local registry filing = 7628077

firms_merged <- firms_merged %>%
  filter(FILING_TYPE == "Local registry filing")

sum(duplicated(firms_merged[, c("bvdid", "CLOSDATE_year", "CONSCODE")])) # 46529 
sum(duplicated(firms_merged[, c("bvdid", "CLOSDATE_year", "CONSCODE", "AUDSTATUS")])) # 9857
table(firms_merged$NR_MONTHS) # 12: 7536716
table(firms_merged$AUDSTATUS) #


duplicates <- firms_merged %>%
       group_by(bvdid, CLOSDATE_year, CONSCODE) %>%
       filter(n() > 1) %>%
       arrange(bvdid, CLOSDATE_year, CONSCODE)

firms_merged <- firms_merged %>%
  filter(NR_MONTHS == 12)

firms_merged <- firms_merged %>%
  filter(AUDSTATUS == "Audit n.a.")

sum(duplicated(firms_merged[, c("bvdid", "CLOSDATE_year")])) #36497

table(firms_merged$ORIG_CURRENCY) #  EUR 7475310


missing_postcode_frims <- combined_df2[is.na(combined_df2$POSTCODE), ]
unique_missing_postcodes_fimrs <- unique(missing_postcode_frims[, c("bvdid", "NAME_INTERNAT")])

#write the data
#write.csv(firms_merged, "firms_data.CSV")
 
fwrite(firms_merged, "firms_merged.csv", quote = TRUE, row.names = FALSE)


# ----- bank data

bank_data <-  read_csv("bank_fin_9261030.csv")

#sanity checks
table(bank_data$LEGALFRM)
# drop any other accounting practices beside local GAAP
bank_data <- bank_data[bank_data$ACCPRACTICE == "Local GAAP", ]
bank_data <- bank_data[bank_data$`_40025` == "Bank", ]

#check for duplicates
bank_data <- bank_data[!duplicated(bank_data[, c("bvdid", "CLOSDATE")]), ] 

bank_data <- merge(bank_data, plz_mapping_unique, by.x="POSTCODE", by.y="plz", all.x = TRUE, all.y = FALSE)

#missing postcodes
missing_postcode_bank <- bank_data[is.na(bank_data$POSTCODE), ]
unique_missing_postcodes_banks <- unique(missing_postcode_bank[, c("bvdid", "NAME_INTERNAT")])



fwrite(bank_data, "banks_merged.csv", quote = TRUE, row.names = FALSE)
