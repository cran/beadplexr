## -----------------------------------------------------------------------------
library(beadplexr)
library(dplyr)
library(ggplot2)
data(simplex)

## ---- fig.show='hold'---------------------------------------------------------
mplex_data <- simplex[["mplex"]]

mplex_data %>% 
  facs_plot(.x = "FSC", .y = "SSC", .type = "hex")

mplex_data %>% 
  facs_plot(.x = "FITC", .y = "PE", .type = "hex")

## ---- fig.show='hold'---------------------------------------------------------
cba_data <- simplex[["cba"]]

cba_data %>% 
  facs_plot(.x = "FSC", .y = "SSC", .type = "hex")

cba_data %>% 
  facs_plot(.x = "APC", .y = "APC-Cy7", .type = "hex")

## ---- fig.width=7-------------------------------------------------------------
mplex_analyte <- mplex_data %>% 
  identify_analyte(.parameter = c("FITC", "PE"), .analyte_id = as.character(c(1:10)))

mplex_analyte %>% 
  facs_plot(.x = "FITC", .y = "PE", .beads = "analyte")

mplex_analyte <- mplex_data %>% 
  identify_analyte(.parameter = c("FITC", "PE"), .analyte_id = as.character(c(1:10)), .trim = 0.1)

mplex_analyte %>% 
  facs_plot(.x = "FITC", .y = "PE", .beads = "analyte")

## ---- fig.width=7-------------------------------------------------------------

mplex_analyte %>% 
  filter(!is.na(analyte)) %>% 
  mutate(analyte = factor(analyte, levels = c(1:10))) %>% 
  ggplot() +
  aes(x = APC, fill = analyte) +
  geom_density()+
  facet_wrap(~ analyte)+
  theme(legend.position = "none")

## ---- fig.width=7-------------------------------------------------------------
mplex_analyte %>% 
  # filter(!is.na(analyte)) %>% 
  group_by(analyte) %>% 
  trim_population(.parameter = "APC", .column_name = "analyte", .trim = 0.05) %>% 
  ungroup() %>% 
  mutate(analyte = factor(analyte, levels = c(1:10))) %>%
  ggplot() +
  aes(x = APC, fill = analyte) +
  geom_density() +
  facet_wrap(~ analyte) +
  theme(legend.position = "none")

## ----fig.width=7--------------------------------------------------------------
cba_analyte <- cba_data %>% 
  identify_analyte(.parameter = c("APC", "APC-Cy7"), .analyte_id = as.character(c(1:30)), .trim = 0.1)

cba_analyte %>% 
  facs_plot(.x = "APC", .y = "APC-Cy7", .beads = "analyte")

## -----------------------------------------------------------------------------
mplex_analyte %>% 
  filter(!is.na(analyte)) %>% 
  group_by(analyte) %>% 
  summarise(`FITC mean` = mean(FITC), `PE mean` = mean(PE)) %>% 
  arrange(`FITC mean`, `PE mean`) %>% 
  knitr::kable()

## ---- warning=FALSE, fig.width=7----------------------------------------------
cba_analyte <- cba_data %>% 
  despeckle(.parameter = c("APC", "APC-Cy7"), .neighbours = 2) %>% 
  identify_analyte(.parameter = c("APC", "APC-Cy7"), .analyte_id = as.character(c(1:30)), .trim = 0.01)

cba_analyte %>% 
  facs_plot(.x = "APC", .y = "APC-Cy7", .beads = "analyte")

## ----results='markup',echo=FALSE----------------------------------------------
sessionInfo()

