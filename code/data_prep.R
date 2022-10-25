library(dplyr)

{
  LNRs_sequenzierung <- readRDS("results/Proben_Sequenzierung.RDS")  
  dat0 <- openxlsx::read.xlsx("data/2021-08-04 Mikrobiom Dataset anonymisiert.xlsx", startRow = 2)
  dat0$LNR <- as.character(dat0$LNR)
  dat0 <- dat0[dat0$LNR %in% LNRs_sequenzierung, ]
  dat0$volunteer <- FALSE
  
  dat_v <- openxlsx::read.xlsx("data/2021-05-19 Volunteers Validationsstudie.xlsx")[1:5*3, ]
  dat_v$LNR <- substr(dat_v$LNR, 1,4)
  dat_v$R_STÜCK <- as.character(dat_v$LNR)
  dat_v$DIABETES.DAT. <- as.character(dat_v$DIABETES.DAT.)
  dat_v$DIABETES.THER. <- as.character(dat_v$DIABETES.THER.)
  dat_v$DIAB..TH..NAME <- as.character(dat_v$DIAB..TH..NAME)
  dat_v$volunteer <- TRUE
  
  dat <- bind_rows(dat0, dat_v)
  
  dat$Histo.Befund <- forcats::fct_explicit_na(factor(dat$Histo.Befund, levels = c(4, 2, 3, 1), labels = c("Ctrl", "low-risk", "high-risk", "Karzinom"), ordered = TRUE))
  dat$Früherer.Histo.Befund <- forcats::fct_explicit_na(factor(dat$Früherer.Histo.Befund, levels = c(4, 2, 3, 1), labels = c("Ctrl", "low-risk", "high-risk", "Karzinom"), ordered = TRUE))       
  dat$severste.Histo <- forcats::fct_explicit_na(factor(dat$severste.Histo, levels = c(4, 2, 3, 1), labels = c("Ctrl", "low-risk", "high-risk", "Karzinom"), ordered = TRUE))
  dat$ANTIBIOTIKA <- forcats::fct_explicit_na(factor(dat$ANTIBIOTIKA, levels = c(1:4), labels = c("ja", "in letzten 6 Monaten", "länger als 6 Monate her", "nie")))
  dat$SCHULE <- forcats::fct_explicit_na(factor(dat$SCHULE, levels = c(1:4), labels = c("Elementary school", "Secondary school", "Higher school", "University")))
  dat$TABAK  <- forcats::fct_explicit_na(factor(dat$TABAK, levels = c(1:3), labels = c("Smoker", "Former smoker", "Non-smoker")))
  dat$ALKOHOL  <- forcats::fct_explicit_na(factor(dat$ALKOHOL, levels = c(3:1), labels = c("Consumer", "Former consumer", "Non-drinker")))
  
   
  dat$Stuhl.Datum[dat$Stuhl.Datum == -5] <- NA
  
  dat <- dat %>% 
    mutate_at(vars(contains('dat'), -DIABETES.DAT.), .funs = list(~ as.Date(., origin = "1899-12-30")))
  
  
  dat$severste.Histo <- paste0(dat$severste.Histo, ifelse(dat$Histo.Befund < dat$severste.Histo, " anam.", ""))
  
  
  dat$Alter <- as.numeric(dat$Unters.Dat..NEU - dat$`GB-Datum`) / 365.25
  dat$Alter[is.na(dat$Alter)] <- (as.numeric(dat$Stuhl.Datum - dat$`GB-Datum`) / 365.25)[is.na(dat$Alter)]
  dat$BMI <- dat$GEWICHT / (dat$GRÖSSE / 100)^2 
  dat$BMI[dat$GEWICHT == -5 | dat$GRÖSSE == -5] <- NA
  dat$LNR <- as.character(dat$LNR)
  
}

saveRDS(dat, "data/2022-01-18 metadata.RDS")
