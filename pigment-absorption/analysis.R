# Pigment Absorption Spectra Visualization

library(ggplot2)
library(gsheet)
library(dplyr)

# Load spectrophotometer data
url <- "https://docs.google.com/spreadsheets/d/19HP8s_8Ncx_MncCuduugs4CMQ8z50pHBGJ59oYensbs/edit?gid=965327642#gid=965327642"
dat <- gsheet2tbl(url)

colnames(dat) <- c("Sample", "Wavelength", "Absorbance")

# Line graph of absorption spectra
ggplot(dat, aes(Wavelength, Absorbance, color = Sample)) +
  geom_line(size = 1) +
  labs(title = "Absorption Spectra", x = "Wavelength (nm)", y = "Absorbance (A.U.)")

# Peak summary
dat %>%
  group_by(Sample) %>%
  summarise(
    max_abs = max(Absorbance),
    max_nm = Wavelength[which.max(Absorbance)],
    min_abs = min(Absorbance),
    min_nm = Wavelength[which.min(Absorbance)]
  )

