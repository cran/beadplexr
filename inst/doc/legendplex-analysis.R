## ----read-fcs-defaults--------------------------------------------------------
library(beadplexr)

.file_name <- system.file("extdata", "K2-C07-A7.fcs", package = "beadplexr")

# Load the fcs file with all defaults
.data <- read_fcs(.file_name = .file_name)


## ----raw-events, fig.show='hold'----------------------------------------------
facs_plot(.data)
facs_plot(.data, .x = "FL2-H", .y = "FL6-H")

## ----read-fcs-no-filter, fig.show='hold', eval=FALSE--------------------------
#  .data <- read_fcs(.file_name = .file_name, .filter = NULL)
#  
#  facs_plot(.data)
#  facs_plot(.data, .x = "FL2-H", .y = "FL6-H")

## ----fcs-h-filter, fig.show='hold'--------------------------------------------
.data <- read_fcs(.file_name = .file_name, .fsc_ssc = c("FSC-H", "SSC-H"), 
                  .filter = list("FSC-H" = c(3.75e5L, 5.5e5L),
                                 "SSC-H" = c(4e5, 1e6),
                                 "FL6-H" = c(7L, Inf)))

facs_plot(.data, .x = "FSC-H", .y = "SSC-H")
facs_plot(.data, .x = "FL2-H", .y = "FL6-H")

## ----read-fcs-fl2-filter, fig.show='hold', eval=FALSE-------------------------
#  .data <- read_fcs(.file_name = .file_name, .fsc_ssc = c("FSC-H", "SSC-H"),
#                    .filter = list("FSC-H" = c(3.75e5L, 5.5e5L),
#                                   "SSC-H" = c(4e5, 1e6),
#                                   "FL6-H" = c(7L, Inf),
#                                   "FL2-H" = c(8, 10)))
#  
#  facs_plot(.data, .x = "FSC-H", .y = "SSC-H")
#  facs_plot(.data, .x = "FL2-H", .y = "FL6-H")

## ----pseudo-color, fig.show='hold'--------------------------------------------
.data <- read_fcs(.file_name = .file_name, .fsc_ssc = c("FSC-A", "SSC-A"),
                  .filter = list("FSC-A" = c(4e5, 6e5L),
                                 "SSC-A" = c(4e5, 1e6),
                                 "FL6-H" = c(7L, Inf)))

# We have relatively few events (around 2500), so decreasing the number of bins
# make the scale more visible
facs_plot(.data, .type = "hexbin", .bins = 50)
facs_plot(.data, .x = "FL2-H", .y = "FL6-H", .type = "hexbin")

## ----load-panel-name----------------------------------------------------------
panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")
panel_info$panel_name

# Is equavalent to
panel_info <- load_panel(.panel_pattern = ".*rowth.*panel")
panel_info$panel_name

## ----content-panel-example, echo=FALSE----------------------------------------
res <- readLines(system.file("resources/legendplex_human_growth_factor_panel_13-plex.yml", package = "beadplexr"))
cat(paste(res, collapse = "\n"))

## ---- eval=FALSE--------------------------------------------------------------
#  system.file(package = "beadplexr")

## ----load-lplex-data, message=FALSE, warning=FALSE----------------------------
library(beadplexr)
library(dplyr)
library(purrr)

data(lplex)

## ----fsc-ssc-ident-1, fig.width=6---------------------------------------------
plex_sub_sample <- lplex[[1]]
plex_sub_sample <- identify_analyte(plex_sub_sample, .parameter = c("FSC-A", "SSC-A"), 
                               .analyte_id = c("A", "B"),
                               .column_name = "Bead group")
facs_plot(plex_sub_sample, .x = "FSC-A", .y = "SSC-A", .beads = "Bead group")


## ----fsc-ssc-ident-2, fig.width=6---------------------------------------------
plex_sub_sample$`Bead group` <- NULL

plex_sub_sample <- identify_analyte(plex_sub_sample, .parameter = c("FSC-A", "SSC-A"), 
                               .analyte_id = c("A", "B"),
                               .column_name = "Bead group", 
                               .trim = 0.03)
facs_plot(plex_sub_sample, .x = "FSC-A", .y = "SSC-A", .beads = "Bead group")

## ----bead-ident, fig.show='hold'----------------------------------------------
library(ggplot2)

panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")

bead_a <- plex_sub_sample %>% filter(`Bead group` == "A")
bead_b <- plex_sub_sample %>% filter(`Bead group` == "B")

bead_a <- identify_analyte(bead_a, .parameter = c("FL6-H"), 
                           .analyte_id = names(panel_info$analytes$A), 
                           .column_name = "Analyte ID")

bead_b <- identify_analyte(bead_b, .parameter = c("FL6-H"), 
                           .analyte_id = names(panel_info$analytes$B), 
                           .column_name = "Analyte ID")

# Factors are added for nicer plotting
bead_a[["Analyte ID"]] <- factor(bead_a[["Analyte ID"]], levels = names(panel_info$analytes$A))
bead_b[["Analyte ID"]] <- factor(bead_b[["Analyte ID"]], levels = names(panel_info$analytes$B))

# Since the plot functions return a ggplot, we can easily add to these
facs_density1d(bead_a, .x = "FL6-H", .beads = "Analyte ID") + ggtitle("Group A")
facs_scatter(bead_a, .x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") + ggtitle("Group A")

facs_density1d(bead_b, .x = "FL6-H", .beads = "Analyte ID") + ggtitle("Group B")
facs_scatter(bead_b, .x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") + ggtitle("Group B")

## ----trim-analyte, fig.show='hold', eval=FALSE--------------------------------
#  bead_a %>% split(list(.$`Analyte ID`)) %>%
#    map_df(trim_population, .parameter = c("FL6-H", "FL2-H"),
#           .column_name = "Analyte ID",
#           .trim = 0.1) %>%
#    # The trim_population removes all factors
#    mutate(`Analyte ID` = factor(`Analyte ID`, levels = names(panel_info$analytes$A))) %>%
#    facs_scatter(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") + ggtitle("Group A")
#  
#  bead_b %>% split(.$`Analyte ID`) %>%
#    map_df(trim_population, .parameter = c("FL6-H", "FL2-H"),
#           .column_name = "Analyte ID",
#           .trim = 0.1) %>%
#    # The trim_population removes all factors
#    mutate(`Analyte ID` = factor(`Analyte ID`, levels = names(panel_info$analytes$B))) %>%
#    facs_scatter(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") + ggtitle("Group B")

## ---- fig.show='hold', cache=TRUE---------------------------------------------
panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")

args_ident_analyte <- list(fs = list(.parameter = c("FSC-A", "SSC-A"),
                                     .column_name = "Bead group",
                                     .method = "mclust",
                                     .trim = 0.03),
                           analytes = list(.parameter = "FL6-H",
                                           .column_name = "Analyte ID"))

analytes_identified <- identify_legendplex_analyte(.data = lplex[[1]],
                                                .analytes = panel_info$analytes,
                                                .method_args = args_ident_analyte) %>%
  group_by(`Analyte ID`) %>% 
  do(trim_population(., .parameter = c("FL6-H", "FL2-H"), 
                     .column_name = "Analyte ID", 
                     .trim = 0.1)) 

analytes_identified %>% facs_plot(.x = "FSC-A", .y = "SSC-A", .beads = "Bead group")

analytes_identified %>%
  filter(`Bead group` == "A") %>% 
  facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID")

analytes_identified %>%
  filter(`Bead group` == "B") %>% 
  facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID")


## ----find-analytes------------------------------------------------------------

find_and_trim <- function(.data){
  identify_legendplex_analyte(.data, .analytes = panel_info$analytes,
                 .method_args = args_ident_analyte) %>% 
    group_by(`Analyte ID`) %>% 
    do(trim_population(., .parameter = c("FL6-H", "FL2-H"), 
                       .column_name = "Analyte ID", 
                       .trim = 0.1))
}

analytes_identified <- lplex %>% lapply(find_and_trim) 

## ----visualize-analytes, message=FALSE, fig.width=7---------------------------
library(gridExtra)

plot_side_by_side <- function(.data, .cur_sample){
  
  plot_all_beads <- .data %>% 
    facs_plot(.x = "FSC-A", .y = "SSC-A", .beads = "Bead group") +
    ggtitle("All events")
  
  plot_a_beads <- .data %>%
    filter(`Bead group` == "A") %>% 
    facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") +
    ggtitle("Bead group A")
  
  plot_b_beads <- .data %>%
    filter(`Bead group` == "B") %>% 
    facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID") +
    ggtitle("Bead group B")
  
  arrangeGrob(plot_all_beads, plot_a_beads, plot_b_beads, 
              nrow = 1, ncol = 3, top = .cur_sample)
}

analytes_identified[1] %>% 
  map2(names(.), plot_side_by_side) %>% 
  walk(grid.arrange)

## ----save-plots, eval=FALSE---------------------------------------------------
#  
#  all_plots <- analytes_identified %>%
#    map2(names(.), plot_side_by_side) %>%
#    marrangeGrob(ncol = 1, nrow = 4, top = NA)
#  
#  ggsave(filename = "dot_plot.pdf", plot = all_plots, width = 8.27, height = 11.69)
#  

## ----calculate-mfi------------------------------------------------------------
analyte_mfi <- analytes_identified %>% 
  map_df(calc_analyte_mfi, 
         .parameter = "FL2-H", 
         .column_name = "Analyte ID", 
         .mean_fun = "geometric", 
         .id = "Sample") %>% 
  mutate(`FL2-H` = log10(`FL2-H`)) %>% 
  filter(!is.na(`Analyte ID`))

## ----split-data---------------------------------------------------------------
library(stringr)
# All standard samples have the pattern C[number]
standard_data <- analyte_mfi %>% 
  filter(str_detect(Sample, "C[0-9]"))

# All non standards are samples... we could also filter on S[number]
sample_data <- analyte_mfi %>% 
  filter(!str_detect(Sample, "C[0-9]")) 

## ----calc-std-conc------------------------------------------------------------
# Helper function to extract the sample number
as_numeric_standard_id <- function(.s){
  .s %>% 
    str_extract("C[0-9]") %>% 
    str_sub(start = -1L) %>% 
    as.numeric()
}

standard_data <- standard_data %>%
  mutate(`Sample number` = as_numeric_standard_id(Sample)) %>%
  left_join(as_data_frame_analyte(panel_info$analytes), by = "Analyte ID") %>%
  group_by(`Analyte ID`) %>%
  mutate(
    Concentration = calc_std_conc(
      `Sample number`,
      concentration,
      .dilution_factor = panel_info$std_dilution
    )
  ) %>% 
  mutate(Concentration = log10(Concentration)) %>% 
  select(-concentration, -`Bead group`)


## ----combine-data-------------------------------------------------------------
library(tidyr)
# It seems that tidyr::nest has problems with non-standard names, so the names 
# must all be concerted to syntactically valid column names.
standard_data <- standard_data %>% 
  ungroup() %>% 
  setNames(make.names(names(.))) %>%
  nest(-`Analyte.ID`, -name, .key = "Standard data")

sample_data <- sample_data %>% 
  ungroup() %>% 
  setNames(make.names(names(.))) %>% 
  nest(-`Analyte.ID`, .key = "Sample data")

plex_data <- 
  inner_join(standard_data, sample_data, by = "Analyte.ID")


## ----fit-standard-curve-------------------------------------------------------
library(purrr)

# When clustering is performed with mclust, the package mclust is loaded in the
# background (an unfortunate necessity). The mclust package also has a function
# called `map`, so an unlucky side effect of clustering with mclust, is that we
# need to be specify which map function we use
plex_data <- plex_data %>% 
  group_by(Analyte.ID) %>% 
  mutate(`Model fit` = purrr::map(`Standard data`, fit_standard_curve))

## ----example-std-curve, echo=FALSE--------------------------------------------
plex_data <- plex_data %>% 
  mutate(`Std curve` = purrr::map2(`Standard data`, `Model fit`, plot_std_curve))

plex_data[2, "Std curve"][[1]][[1]]

## ----calc-conc----------------------------------------------------------------
plex_data <- plex_data %>% 
  group_by(Analyte.ID) %>% 
  mutate(`Standard data` = 
           purrr::map2(`Standard data`, `Model fit`, 
                       calculate_concentration)) %>% 
  mutate(`Sample data` = 
           purrr::map2(`Sample data`, `Model fit`, 
                       calculate_concentration)) %>% 
  mutate(`Std conc` = 
           purrr::map(`Standard data`, 
                      plot_target_est_conc)) %>%
  mutate(`Est curve` = 
           purrr::pmap(list(`Sample data`, `Standard data`, `Model fit`, name), 
                       plot_estimate))


## ----example-comb-plots, eval=TRUE--------------------------------------------
comb_plots <- function(..., .title, .ncol, .nrow = 1){
  .grobs <- list(...)
  
  if(missing(.ncol)){
    .ncol <- length(.grobs)
  }
  gridExtra::marrangeGrob(grobs = .grobs, ncol = .ncol, nrow = .nrow, top = .title)
}

plex_data <- plex_data %>% 
   mutate(`Std plots` = pmap(list(`Std curve`, `Std conc`, .title = name), comb_plots))


## ----example-std-plots, echo=FALSE, fig.show='hold'---------------------------
plex_data[2, "Std plots"][[1]][[1]]
plex_data[2, "Est curve"][[1]][[1]]

## ----example-save-std-plot, eval=FALSE----------------------------------------
#  plots_to_save <- gridExtra::marrangeGrob(plex_data$`Std plots` %>% flatten(),
#                                           ncol = 1, nrow = 6)
#  
#  ggsave("std_plots.pdf", plot = plots_to_save,
#         path = "./", width = 210, height = 297, units = "mm",
#         title = "Standard plots")

## ----example-save-est-plot, eval=FALSE----------------------------------------
#  plots_to_save <- gridExtra::marrangeGrob(plex_data$`Est curve`,
#                                           ncol = 1, nrow = 6)
#  
#  ggsave("estimation_curve.pdf", plot = plots_to_save,
#         path = "./", width = 210, height = 297, units = "mm",
#         title = "Samples on std curve")

## ----example-extract-conc, eval=FALSE-----------------------------------------
#  plex_data %>%
#    unnest(`Sample data`) %>%
#    mutate(Calc.conc = 10^Calc.conc, `Calc.conc error` = 10^`Calc.conc error`)

## ----results='markup', echo=FALSE---------------------------------------------
sessionInfo()

