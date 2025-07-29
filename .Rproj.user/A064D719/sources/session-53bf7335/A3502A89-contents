library(here)
library(dplyr)
library(lme4)
library(sjPlot)
library(lmtest)
library(faux)
library(emmeans)
library(interactions)
library(forcats)
library(sjPlot)
library(ggplot2)
library(ggridges)
library(grid)

# define functions to drop gridlines from plots ----
drop_gridlines <- function(x = TRUE, y = TRUE, minor.only = FALSE) {
  plot <- ggplot2::theme()
  
  if (y) {
    plot <- plot + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    if (!minor.only) {
      plot <- plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
  }
  
  if (x) {
    plot <- plot + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if (!minor.only) {
      plot <- plot + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }
  }
  
  return(plot)
}

# run my_theme.R to set theme for plots ----

source(here("my_theme.R"))

# load study 1 data

Study1 <- read.csv("CDILEXexp1_encrypted.csv")

# scale age ----

## rename Age 

Study1 <- Study1 %>% 
  rename(UnscaledAge = Age)

Study1 <- Study1 %>%
  group_by(EncryptedID) %>%
  slice(1) %>% # choose just one row for each participant
  ungroup() %>% # ungroup so the mean and sd are calculated across all participants
  mutate(Age = (UnscaledAge - mean(UnscaledAge)) / sd(UnscaledAge)) %>% # normalized Age per unique ID
  select(EncryptedID, Age) %>%  # keep only these columns
  right_join(Study1, by = "EncryptedID")  # join back to full data, copying Age to all rows

# select only the rows where inCDI1 is "yes" ----

Study1 <- filter(Study1, inCDI1 == "yes")

# scale ND values

Study1 <- Study1 %>% 
  group_by(Sign) %>%
  slice(1) %>% # choose just one row for each sign
  ungroup() %>% # ungroup so the mean and sd are calculated across all participants
  mutate(PhonologicalNeighborhoodDensity = 
           (NeighborhoodDensityCDI1 - mean(NeighborhoodDensityCDI1)) / sd(NeighborhoodDensityCDI1)) %>% # normalized ND per unique Sign
  select(Sign, PhonologicalNeighborhoodDensity) %>%  # keep only these columns
  right_join(Study1, by = "Sign")  # join back to full data, copying Age to all rows

# keep only "TRUE" and "FALSE" for "comprehends and produces"

Study1 <- filter(Study1, !is.na(ComprehendsAndProduces))

# remove reports with fewer than 10 observations

Study1 <- Study1 %>%
  group_by(EncryptedID) %>%
  filter(n() >= 10)

# convert "Produces" to a categorical variable

Study1$ComprehendsAndProduces <- as.factor(Study1$ComprehendsAndProduces)

control_settings <- glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 400000))

Study1Model <- glmer(
  ComprehendsAndProduces ~ IconicityCDI1 * Age +
    Age * PhonologicalNeighborhoodDensity +
    Age * FrequencyCDI1 + 
    (1 | Sign) +
    (1 + IconicityCDI1 + PhonologicalNeighborhoodDensity + FrequencyCDI1 || EncryptedID),
  data = Study1,
  family = binomial, nAGQ = 0,
  control = control_settings)

tab_model(Study1Model, transform = NULL)

# for log-likelihood comparisons ----

Study1Model_noIconicity <- glmer(
  ComprehendsAndProduces ~ 
    Age * PhonologicalNeighborhoodDensity +
    Age * FrequencyCDI1 +     
    (1 | Sign) +
    (1 + PhonologicalNeighborhoodDensity + FrequencyCDI1 || EncryptedID),
  data = Study1,
  family = binomial, nAGQ = 0, 
  control = glmerControl(optimizer = "nloptwrap"))

Study1Model_noND <- glmer(
  ComprehendsAndProduces ~ Age * IconicityCDI1 +
    Age * FrequencyCDI1 + 
    (1 | Sign) +
    (1 + IconicityCDI1 + FrequencyCDI1 || EncryptedID),
  data = Study1,
  family = binomial, nAGQ = 0, 
  control = glmerControl(optimizer = "nloptwrap"))

Study1Model_noFrequency <- glmer(
  ComprehendsAndProduces ~ Age * IconicityCDI1 +
    Age * PhonologicalNeighborhoodDensity + 
    (1 | Sign) +
    (1 + IconicityCDI1 + PhonologicalNeighborhoodDensity || EncryptedID),
  data = Study1,
  family = binomial, nAGQ = 0, 
  control = glmerControl(optimizer = "nloptwrap"))

iconicity_logtest <- lrtest(Study1Model, Study1Model_noIconicity)

freq_logtest <- lrtest(Study1Model, Study1Model_noFrequency)

nd_logtest <- lrtest(Study1Model, Study1Model_noND)

# run model with unscaled age for plotting ----

Study1Model_unscaledAge <- glmer(
  ComprehendsAndProduces ~ IconicityCDI1 * UnscaledAge +
    UnscaledAge * PhonologicalNeighborhoodDensity +
    UnscaledAge * FrequencyCDI1 + 
    (1 | Sign) +
    (1 + IconicityCDI1 + PhonologicalNeighborhoodDensity + FrequencyCDI1 || EncryptedID),
  data = Study1,
  family = binomial, nAGQ = 0,
  control = control_settings)

# age x frequency plot

Study1_FreqByAge <- 
  plot_model(Study1Model_unscaledAge, type = "int", terms = "UnscaledAge [all]")[[3]] +
  my_theme(remove.y.gridlines = TRUE,
           remove.x.gridlines = TRUE) +
  labs(y = "Likelihood of Acquisition",
       x = "Age (months)",
       title = NULL) +
  aes(linetype = group_col,
          color = group_col)  + # tells R to apply linetype differences by group, 
  # which is min and max freq in the model
  guides(color = "none", # suppresses extra legend
         linetype = guide_legend( element_text("Sign Frequency"),
                                  title.position = "top", 
                                  title.hjust = 0.5,
                                  override.aes = list(linetype = c("solid", "dotted"), # makes lines in legend solid/dotted
                                                      color = c("red", "red"), # makes lines in legend red
                                                      byrow = TRUE, # allows legend.key.size to work
                                                      fill = NA
                                  ),
                                  reverse = TRUE
         )) + # gets rid of gray behind lines in legend
  scale_linetype_manual(values = c("dotted", "solid"),
                        labels = c(
                          expression("Minimum (-1.30)"),
                          expression("Maximum (1.72)")))  +  
  scale_color_discrete(type = c("red", "red")) +
  scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous( # makes tight layout for x-axis 
    limits = c(0, 60), # example limits for Age (months)
    breaks = seq(0, 60, by = 10), # example breaks every 10 months
    expand = c(0, 0) # to remove extra space on x-axis as done for y-axis
  ) +
  theme(plot.margin = margin(.5, 3, .2, .2, "cm"), # increases right margin around plot
        legend.title = element_text(size = 28),
        legend.position = c(.9, 0.045)) 

Study1_FreqByAge$layers[[2]]$show.legend <- FALSE 

Study1_FreqByAge$layers[[2]]$aes_params$fill <- "red"

print(Study1_FreqByAge)

ggsave("ManuscriptImages/Study1_FreqByAge.png", Study1_FreqByAge, width = 10, height = 8, dpi = 300)

# PND plot ----

Study1_PND <- 
  plot_model(Study1Model, type = "pred", terms = "PhonologicalNeighborhoodDensity [all]") +
  my_theme() +
  labs(y = "Likelihood of Acquisition",
       x = "Phonological Neighborhood Density \n (z score)",
       title = NULL) +
   scale_y_continuous( # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  theme(plot.margin = margin(1, 3, .2, .2, "cm"), # increases top margin around plot
        legend.position = c(.8, 0.045))

print(Study1_PND)

ggsave("ManuscriptImages/Study1_PND.png", Study1_PND, width = 10, height = 8, dpi = 300)

# study 2 ----

Study2 <- read.csv("CDILEXexp2_encrypted.csv")

# remove all reports with fewer than 10 rows ----

Study2 <- Study2 %>%
  group_by(EncryptedID) %>%
  filter(n() >= 10)

# scale age ----

Study2 <- Study2 %>% 
  rename(UnscaledAge = Age)

Study2 <- Study2 %>%
  group_by(EncryptedID) %>%
  slice(1) %>% # choose just one row for each participant
  ungroup() %>% # ungroup so the mean and sd are calculated across all participants
  mutate(Age = (UnscaledAge - mean(UnscaledAge)) / sd(UnscaledAge)) %>% # normalized Age per unique ID
  select(EncryptedID, Age) %>%  # keep only these columns
  right_join(Study2, by = "EncryptedID")  # join back to full data, copying Age to all rows


# scale NeighborhoodDensity values ----

Study2 <- Study2 %>% 
  group_by(Sign) %>%
  slice(1) %>% # choose just one row for each sign
  ungroup() %>% # ungroup so the mean and sd are calculated across all Signs
  mutate(PhonologicalNeighborhoodDensity = 
           (NeighborhoodDensity - mean(NeighborhoodDensity, na.rm = TRUE)) / sd(NeighborhoodDensity, na.rm = TRUE)) %>% # normalized ND per unique Sign
  select(Sign, PhonologicalNeighborhoodDensity) %>%  # keep only these columns
  right_join(Study2, by = "Sign")  # join back to full data, copying Age to all rows

# keep only "TRUE" and "FALSE" for "comprehends and produces" ----

Study2 <- filter(Study2, !is.na(ComprehendsAndProduces))

# convert "Produces" to a categorical variable ----

Study2$ComprehendsAndProduces <- as.factor(Study2$ComprehendsAndProduces)

# sum code CaregiverHearingStatus ----

Study2$CaregiverHearingStatus <- contr_code_sum(Study2$CaregiverHearingStatus)

# run Study2 model ----

# set iterations to 400,000 to avoid convergence warnings

control_settings <- glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 400000))

Study2Model <- glmer(
  ComprehendsAndProduces ~ Iconicity * CaregiverHearingStatus * Age +
    PhonologicalNeighborhoodDensity * CaregiverHearingStatus * Age  +
    Frequency * CaregiverHearingStatus * Age +
    (1 | Sign) +
    (1 | EncryptedID) +
    (0 + Iconicity | EncryptedID) + 
    (0 + PhonologicalNeighborhoodDensity | EncryptedID) + 
    (0 + Frequency | EncryptedID),
  data = Study2,
  family = binomial, nAGQ = 0, 
  control = control_settings)

tab_model(Study2Model, transform = NULL)

# sim_slopes gives us the same result as emtrends but the emtrends output
# can be fed into pairs()

emtrends <-
  emtrends(
    Study2Model,
    "CaregiverHearingStatus",
    var = "PhonologicalNeighborhoodDensity",
  )

pairs(emtrends)

nd_slopes <- interactions::sim_slopes(Study2Model,
                                    pred = PhonologicalNeighborhoodDensity,
                                    modx = CaregiverHearingStatus,
                                    data = CDILEx,
                                    r.squared = FALSE,
                                    johnson_neyman = FALSE)

print(nd_slopes)

# age x caregiver plot ----

Study2Model_forCaregiverAgePlot <- glmer(
  ComprehendsAndProduces ~ UnscaledAge * CaregiverHearingStatus * Iconicity +
    PhonologicalNeighborhoodDensity * CaregiverHearingStatus * UnscaledAge  +
    Frequency * CaregiverHearingStatus * UnscaledAge + 
    (1 | Sign) +
    (1 | EncryptedID) +
    (0 + Iconicity | EncryptedID) + 
    (0 + PhonologicalNeighborhoodDensity | EncryptedID) + 
    (0 + Frequency | EncryptedID),
  data = Study2,
  family = binomial, nAGQ = 0, 
  control = control_settings)

Study2_AgeByCaregiver <- 
  plot_model(Study2Model_forCaregiverAgePlot, type = "int") [[1]] +
  my_theme() +
  theme(plot.margin = margin(t = 26, r = 15, b = 6, l = 6, unit = "pt"), # make bigger top margin because making a two line title
        legend.position = c(.85, .05),
        # legend.justification = c(.50, .3),
        # legend.text.align = 0,
        # legend.margin = margin(0,0,0,0),
        legend.key.width = grid::unit(1.5, "cm"), # specifies width of the lines in the legend key
        legend.key.height = unit(1, "cm")) +
  # legend.direction = "horizontal") # specifies that the two items in the legend should be next to each 
  labs(y = "Likelihood of Acquisition",
       x = "Age (months)",
       title = NULL) +
  aes(color = group_col, 
      linetype = group_col) + # tells R to apply linetype differences by group, 
  # which is min and max freq in the model
  #  par(mar=c(2, 0,0,0)) + ## trying to figure out how to make the top margin bigger so we can see both lines of the title
  labs(color = "Caregiver Hearing Status",
       linetype = "Caregiver Hearing Status") + # tells R to apply linetype differences by group, 
  scale_color_discrete(type = c("red", "blue"),
                       labels = c("Deaf", "Hearing")) + # changes legend labels
  scale_linetype_manual(values = c("solid", "dashed")) + 
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) + # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
  scale_x_continuous(expand = c(0, 0)) + # gets rid of extra space on x axis
  guides(
    color = guide_legend(title.position = "top", title.hjust = 0.5,
                         element_text("Caregiver Hearing\n Status"),
                         override.aes = list(linetype = c("solid", "dashed"), 
                                             fill = "transparent")),
    # linetype = guide_legend(
    #                       title.position = "bottom", 
    #                         title.hjust = 0.5),
    linetype = FALSE  # suppresses extra linetype legend
  ) +
  theme(legend.title = element_text(size = 28))

Study2_AgeByCaregiver$layers[[2]]$show.legend <- FALSE 

print(Study2_AgeByCaregiver)

ggsave("ManuscriptImages/Study2_AgeByCaregiver.png", Study2_AgeByCaregiver, width = 11, height = 8, dpi = 300)

# frequency by age and caregiver plot ----

# Frequency x Caregiver ----

Study2Model_forCaregiverFreqAgePlot <- glmer(
  ComprehendsAndProduces ~ UnscaledAge * CaregiverHearingStatus * Frequency +
    UnscaledAge * CaregiverHearingStatus * Iconicity +
    UnscaledAge * CaregiverHearingStatus * PhonologicalNeighborhoodDensity + 
    (1 | Sign) +
    (1 | EncryptedID) +
    (0 + Iconicity | EncryptedID) + 
    (0 + PhonologicalNeighborhoodDensity | EncryptedID) + 
    (0 + Frequency | EncryptedID),
  data = Study2,
  family = binomial, nAGQ = 0, 
  control = glmerControl(optimizer = "nloptwrap"))

Study2Model_CaregiverFreqAgePlot <- 
  plot_model(Study2Model_forCaregiverFreqAgePlot, type = "int")[[8]] +
  my_theme() +
  theme(plot.margin = margin(t = 26, r = 14, b = 6, l = 6, unit = "pt"), # make bigger top margin because making a two line title
        legend.position = c(.9, .25),
        legend.key.width = grid::unit(1.5, "cm"), # specifies width of the lines in the legend key
        legend.key.height = unit(1, "cm")) +
  labs(y = "Likelihood of Acquisition",
       x = "Age (months)",
       title = NULL) +
  aes(color = group_col, 
      linetype = group_col) + # tells R to apply linetype differences by group, 
  # which is min and max freq in the model
  labs(color = "Caregiver Hearing Status",
       linetype = "Caregiver Hearing Status") + # tells R to apply linetype differences by group, 
  scale_color_discrete(type = c("red", "blue"),
                       labels = c("Deaf", "Hearing")) + # changes legend labels
  scale_linetype_manual(values = c("solid", "dashed")) + 
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),
    expand = c(0, 0)
  ) + # makes tight layout for y-axis (i.e. gets rid of extra space below 0%)
  scale_x_continuous(expand = c(0, 0)) + # gets rid of extra space on x axis
  guides(
    color = guide_legend(title.position = "top", 
                         element_text("Caregiver Hearing\n Status"),
                         title.hjust = 0.5,
                         override.aes = list(linetype = c("solid", "dashed"), 
                                             fill = "transparent")),
    linetype = FALSE  # suppresses extra linetype legend
  ) +
  theme(legend.title = element_text(size = 28)) +
  theme(panel.spacing = unit(4, "lines")) # increase space between facets

Study2Model_CaregiverFreqAgePlot$data$facet <- recode(Study2Model_CaregiverFreqAgePlot$data$facet,
                                                          "Frequency = -1.74" = "Minimum Frequency\n (-1.74, z score)",
                                                          "Frequency = 1.72" = "Maximum Frequency\n (1.72, z score)")

Study2Model_CaregiverFreqAgePlot$layers[[2]]$show.legend <- FALSE # gets rid of boxes around lines in legend

print(Study2Model_CaregiverFreqAgePlot)

ggsave("ManuscriptImages/Study2_CaregiverFreqAgePlot.png", 
       Study2Model_CaregiverFreqAgePlot, width = 18, height = 8, dpi = 300)

# Study 3

# load study 3 data

Study3 <- read.csv("CDILEXexp3_encrypted.csv")

# rename lexical class categories ----

Study3$MorphosyntacticCategory <- 
  dplyr::recode (
    Study3$MorphosyntacticCategory,
    Verb = "Predicate", # collapse 'Verb' and 'Adj' as 'Predicate'
    Adjective = "Predicate", # collapse 'Verb' and 'Adj' as 'Predicate'
    Adverb = "Predicate",
    Name = "Noun",
    Minor = "Function Word"
  )


# remove classes except nouns, predicates, and function words for ASLCDIexp3 ----

MorphosyntacticCats <-
  Study3 %>% 
  filter(MorphosyntacticCategory %in% 
                                   c("Noun", "Predicate", "Function Word"))

get_category_proportions <- function(df,
                                     grouping_vars = NULL,
                                     category_col,
                                     known_col,
                                     known_total_col = "known_prop_total") {
  # For character vector of grouping vars, convert to symbols
  group_syms <- if (!is.null(grouping_vars)) rlang::syms(grouping_vars) else NULL
  cat_sym <- rlang::sym(category_col)
  known_sym <- rlang::sym(known_col)
  
  # Get all unique category labels (non-missing)
  category_levels <- df %>%
    dplyr::filter(!is.na(!!cat_sym)) %>%
    dplyr::distinct(!!cat_sym) %>%
    dplyr::pull(!!cat_sym)
  
  # Summarize
  summary_df <- df %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::summarise(
      total_tokens = dplyr::n(),
      num_known = sum(!!known_sym == "TRUE", na.rm = TRUE),
      !!known_total_col := num_known / total_tokens,
      !!!purrr::map(category_levels, function(label) {
        label_expr <- rlang::expr(!!cat_sym == !!label)
        known_label_expr <- rlang::expr((!!known_sym == "TRUE") & (!!cat_sym == !!label))
        
        label_clean <- tolower(gsub("\\s+", "_", as.character(label)))
        num_col <- paste0("num_", label_clean)
        num_known_col <- paste0("num_known_", label_clean)
        prop_col <- as.character(label)
        
        rlang::quos(
          !!rlang::sym(num_col) := sum(!!label_expr, na.rm = TRUE),
          !!rlang::sym(num_known_col) := sum(!!known_label_expr, na.rm = TRUE),
          !!rlang::sym(prop_col) := .data[[num_known_col]] / .data[[num_col]]
        )
      }) %>% purrr::flatten(),
      .groups = "drop"
    )
  
  # Pivot to long format using known category names
  summary_df_long <- summary_df %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(as.character(category_levels)),
      names_to = "CategoryCompared",
      values_to = "CategoryProportion"
    ) %>%
    dplyr::mutate(
      BiasScore = CategoryProportion - !!rlang::sym(known_total_col)
    )
  
  return(summary_df_long)
}

# order CaregiverHearingStatus so that Deaf Caregivers are first

MorphosyntacticCats$CaregiverHearingStatus <-
  factor(MorphosyntacticCats$CaregiverHearingStatus,
         levels = c("Deaf Caregivers", "Hearing Caregivers"))

# run the function to get proportions and difference scores for morphosyntactic categories

morphosyn_category_proportions <- get_category_proportions(
  df = MorphosyntacticCats,
  grouping_vars = c("EncryptedID", "CaregiverHearingStatus"),
  category_col = "MorphosyntacticCategory",
  known_col = "ComprehendsAndProduces"
)

# order categories so that Predicate is first

morphosyn_category_proportions$CategoryCompared <-
  factor(morphosyn_category_proportions$CategoryCompared,
         levels = c("Function Word", "Noun", "Predicate"))

build_category_results_table <- function(data, 
                                         category_col,
                                         group_col = "CaregiverHearingStatus",
                                         score_col = "BiasScore",
                                         category_levels = NULL) {
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(gt)
  library(rlang)
  
  # Convert group_col, category_col, and score_col from strings to symbols 
  # so they can be used in tidyverse expressions 
  group_sym <- sym(group_col) 
  class_sym <- sym(category_col)
  score_sym <- sym(score_col)
  
  # Clean category and group values: trim whitespace and convert category_col back to factor with appropriate levels
  data <- data %>%
    mutate(
      !!category_col := trimws(as.character(.data[[category_col]])),
      !!group_col := trimws(as.character(.data[[group_col]]))
    )
  
  # Determine proper factor levels for category_col
  # Use supplied category_levels if provided, else try to get original levels in data (before trimming),
  # else use unique categories in order of appearance
  levels_to_use <- NULL
  
  if (!is.null(category_levels)) {
    levels_to_use <- category_levels
  } else {
    # Try to get original factor levels before trimming, if possible
    # If data[[category_col]] was originally a factor, try to extract levels from original data column before trimming
    # Otherwise fallback to unique categories found after trimming
    orig_class <- class(data[[category_col]])
    if ("factor" %in% orig_class) {
      levels_to_use <- levels(data[[category_col]])
      if (is.null(levels_to_use) || length(levels_to_use) == 0) {
        levels_to_use <- unique(data[[category_col]])
      }
    } else {
      levels_to_use <- unique(data[[category_col]])
    }
  }
  
  # Re-factor the category_col with the selected levels to enforce order
  data <- data %>%
    mutate(
      !!category_col := factor(.data[[category_col]], levels = levels_to_use)
    )
  
  groups <- unique(data[[group_col]]) # Get unique group names
  if (length(groups) != 2) stop("Function expects exactly 2 groups.")
  group1 <- "Deaf Caregivers"
  group2 <- "Hearing Caregivers"
  categories <- levels(data[[category_col]])
  
  # Get means and SDs for each group and category
  summary_stats <- data %>%
    group_by(!!group_sym, !!class_sym) %>%
    summarise(
      Mean = round(mean(!!score_sym, na.rm = TRUE), 3),
      SD = round(sd(!!score_sym, na.rm = TRUE), 3),
      .groups = "drop"
    )
  
  # Wilcoxon signed-rank test function
  safe_wilcox_within <- function(x) {
    x <- na.omit(x)
    if (length(x) < 2) return(list(statistic = NA_real_, p.value = NA_real_))
    wilcox.test(x, mu = 0)
  }
  
  # Wilcoxon tests within each group and category
  wilcox_by_group <- expand.grid(
    group = groups,
    category = categories,
    stringsAsFactors = FALSE
  ) %>%
    rowwise() %>%
    mutate(
      test = list(
        safe_wilcox_within(
          data %>%
            filter(
              .data[[group_col]] == group,
              .data[[category_col]] == category
            ) %>%
            pull(!!score_sym)
        )
      ),
      Vstat = test$statistic,
      p = test$p.value,
      p_fmt = case_when(
        is.na(p) ~ "NA",
        p < 0.001 ~ "< 0.001*",
        p < 0.0083 ~ paste0(round(p, 3), "*"),
        TRUE ~ as.character(round(p, 3))
      )
    ) %>%
    ungroup()
  
  # Wilcoxon between groups for each category
  between_group_wilcox <- map_dfr(categories, function(cat) {
    dat <- data %>% filter(.data[[category_col]] == cat)
    x <- dat %>% filter(.data[[group_col]] == group1) %>% pull(!!score_sym) %>% na.omit()
    y <- dat %>% filter(.data[[group_col]] == group2) %>% pull(!!score_sym) %>% na.omit()
    
    if (length(x) < 2 || length(y) < 2) {
      tibble(category = cat, W_stat = NA_real_, W_p = NA_real_, W_p_fmt = "NA")
    } else {
      test <- wilcox.test(x, y, paired = FALSE)
      tibble(
        category = cat,
        W_stat = round(test$statistic, 0),
        W_p = test$p.value,
        W_p_fmt = case_when(
          test$p.value < 0.001 ~ "< 0.001*",
          test$p.value < 0.05 ~ paste0(round(test$p.value, 3), "*"),
          TRUE ~ as.character(round(test$p.value, 3))
        )
      )
    }
  })
  
  # Summary table for group1
  group1_tbl <- summary_stats %>%
    filter(.data[[group_col]] == group1) %>%
    left_join(
      wilcox_by_group %>% filter(group == group1),
      by = setNames("category", category_col)
    ) %>%
    select(all_of(category_col), Mean, SD, Vstat, p_fmt) %>%
    rename(
      `Bias Score` = Mean,
      `SD` = SD,
      `V statistic` = Vstat,
      `p value` = p_fmt
    )
  
  # Summary table for group2
  group2_tbl <- summary_stats %>%
    filter(.data[[group_col]] == group2) %>%
    left_join(
      wilcox_by_group %>% filter(group == group2),
      by = setNames("category", category_col)
    ) %>%
    select(all_of(category_col), Mean, SD, Vstat, p_fmt) %>%
    rename(
      `Bias Score ` = Mean,
      `SD ` = SD,
      `V statistic ` = Vstat,
      `p value ` = p_fmt
    )
  
  # Final combined table
  final_df <- group1_tbl %>%
    left_join(group2_tbl, by = category_col) %>%
    left_join(between_group_wilcox, by = setNames("category", category_col)) %>%
    select(-W_p) %>%
    rename(
      Category = !!category_col,
      `W statistic` = W_stat,
      `p value.1` = W_p_fmt
    )
  
  # Build gt table
  final_df %>%
    gt() %>%
    tab_spanner(
      label = md(paste0("***", group1, "***")),
      columns = c("Bias Score", "SD", "V statistic", "p value")
    ) %>%
    tab_spanner(
      label = md(paste0("***", group2, "***")),
      columns = c("Bias Score ", "SD ", "V statistic ", "p value ")
    ) %>%
    tab_spanner(
      label = md("***Difference***"),
      columns = c("W statistic", "p value.1")
    ) %>%
    cols_align(
      "center",
      columns = c(
        "Bias Score", "SD", "V statistic",
        "Bias Score ", "SD ", "V statistic ", "W statistic"
      )
    ) %>%
    cols_align(
      "right",
      columns = c("p value", "p value ", "p value.1")
    ) %>%
    cols_label(
      Category = md("**Category**"),
      `Bias Score` = md("**Bias Score**"),
      `SD` = md("**SD**"),
      `V statistic` = md("**V statistic**"),
      `p value` = md("**p value**"),
      `Bias Score ` = md("**Bias Score**"),
      `SD ` = md("**SD**"),
      `V statistic ` = md("**V statistic**"),
      `p value ` = md("**p value**"),
      `W statistic` = md("**W statistic**"),
      `p value.1` = md("**p value**")
    )
}

build_category_results_table(morphosyn_category_proportions, 
                             category_col = "CategoryCompared",
                             category_levels = c("Function Word", "Noun", "Predicate"))


morphosyn_category_proportions$CaregiverHearingStatus <- factor(
  morphosyn_category_proportions$CaregiverHearingStatus,
  levels = c("Deaf Caregivers", "Hearing Caregivers"),
  labels = c("Deaf", "Hearing")
)

MorphosyntacticPlot <- #----
ggplot(
  morphosyn_category_proportions,
  aes(x = BiasScore, y = CategoryCompared,
      fill = CaregiverHearingStatus)) +
  my_theme() +
  theme(
    plot.margin = margin(t = 45, r = 6, b = 6, l = 6, unit = "pt"),
    legend.position = c(.2, .87),  # Moves legend inside the plot (top left)
    legend.background = element_blank(),  # Removes legend background
  #   legend.title = element_text(size = 28, hjust = 0),
  #   legend.text = element_text(size = 28, hjust = 0),
  #   axis.text = element_text(size = 28),
  #   axis.title.x = element_text(size = 28),
  #   axis.title.y = element_text(size = 28),
  #   strip.text.x = element_text(size = 28),
  #   strip.text.y = element_text(size = 28)
  
  ) +  
  geom_density_ridges(jittered_points = TRUE, alpha = 0.7, scale = 0.4,
                      point_size = 4,
                      position = position_raincloud(height = .3, ygap = 0.04),
                      aes(point_fill = CaregiverHearingStatus,
                          point_color = CaregiverHearingStatus,
                          point_shape = CaregiverHearingStatus
                      )) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(17, 20)) +
  guides(
    fill =  guide_legend(
      override.aes = list(
        color = "white",  # Removes outline around legend symbols
        alpha = 1, # Makes legend symbols opaque
        fill = NA # makes box behind shapes in key white
      ))) +
  scale_y_discrete(expand = c(0.1, 0.2)) +
  geom_vline(xintercept = 0) +
  labs(y = "Morphosyntactic Category",
       x = "Bias Score",
       fill = "Caregiver Hearing Status",
       point_fill = "Caregiver Hearing Status",
       point_color = "Caregiver Hearing Status",
       point_shape = "Caregiver Hearing Status"
  )

# print and save plot ----
print (MorphosyntacticPlot)

ggsave("ManuscriptImages/MorphosyntacticPlot.png", MorphosyntacticPlot, width = 16, height = 8, dpi = 300)

# filter rows in Study3 df where CDISemanticCategory is NA
SemanticCats <- Study3 %>%
  filter(!is.na(CDISemanticCategory))

# save order the semantic categories by the number of unique signs in each category

SemCatOrder <- SemanticCats %>%
  group_by(CDISemanticCategory) %>%
  summarise(UniqueSignCount = n_distinct(Sign)) %>%
  arrange(desc(UniqueSignCount)) %>%
  pull(CDISemanticCategory)

# reverse the order of the categories so that the category with the most unique signs is first

# SemCatOrder <- rev(SemCatOrder)

# order CaregiverHearingStatus so that Deaf Caregivers are first

# SemanticCats$CaregiverHearingStatus <-
#   factor(SemanticCats$CaregiverHearingStatus,
#          levels = c("Deaf Caregivers", "Hearing Caregivers"))
# 

semantic_category_proportions <- get_category_proportions(
  df = SemanticCats,
  grouping_vars = c("EncryptedID", "CaregiverHearingStatus"),
  category_col = "CDISemanticCategory",
  known_col = "ComprehendsAndProduces"
)

# order Semantic Categories by the number of unique signs in each category ----

semantic_category_proportions$CategoryCompared <- factor(
  semantic_category_proportions$CategoryCompared,
  levels = SemCatOrder
)

# semantic_category_proportions$CaregiverHearingStatus <- factor(
#   semantic_category_proportions$CaregiverHearingStatus,
#   levels = c("Deaf Caregivers", "Hearing Caregivers"),
#   labels = c("Deaf", "Hearing")
# )

semantic_category_proportions$CategoryCompared <- factor(
  semantic_category_proportions$CategoryCompared)

build_category_results_table(semantic_category_proportions, 
                             category_col = "CategoryCompared",
                             category_levels = SemCatOrder)

# rename semantic_category_proportions$CaregiverHearingStatus to Deaf and Hearing

semantic_category_proportions$CaregiverHearingStatus <- factor(
  semantic_category_proportions$CaregiverHearingStatus,
  levels = c("Deaf Caregivers", "Hearing Caregivers"),
  labels = c("Deaf", "Hearing")
)
semantic_category_proportions$CategoryCompared <- factor(
  semantic_category_proportions$CategoryCompared,
  levels = rev(SemCatOrder) # reverse the order of the categories
)

SemanticPlot <- #----
ggplot(
  semantic_category_proportions,
  aes(x = BiasScore, y = CategoryCompared,
      fill = CaregiverHearingStatus)) +
  my_theme() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical", # stack title above items
    legend.box.just = "center",      # center the legend contents
    legend.background = element_blank(),  # Removes legend background
    # legend.title = element_text(size = 28, hjust = 0),
    # legend.text = element_text(size = 28, hjust = 0),
    # axis.text = element_text(size = 28),
    # axis.title.x = element_text(size = 28),
    # axis.title.y = element_text(size = 28),
    # strip.text.x = element_text(size = 28),
    # strip.text.y = element_text(size = 28)
  ) +  
  geom_density_ridges(jittered_points = TRUE, alpha = 0.7, scale = 0.4,
                      point_size = 1.5,
                      position = position_raincloud(height = .4, ygap = 0.004),
                      aes(point_fill = CaregiverHearingStatus,
                          point_color = CaregiverHearingStatus,
                          point_shape = CaregiverHearingStatus
                      )) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(17, 20)) +
  guides(
    fill =  guide_legend(
      override.aes = list(
        color = "white",  # Removes outline around legend symbols
        alpha = 1, # Makes legend symbols opaque
        fill = NA, # makes box behind shapes in key white
        point_size = 3 # makes legend symbols larger
      )
      )
    ) +
  #scale_y_discrete(expand = c(0.1, 0.2)) + # expands y-axis to make room for labels
  geom_vline(xintercept = 0) +
  labs(y = "Semantic Category",
       x = "Bias Score",
       fill = "Caregiver Hearing Status",
       point_fill = "Caregiver Hearing Status",
       point_color = "Caregiver Hearing Status",
       point_shape = "Caregiver Hearing Status"
  ) +
  theme(legend.box = "horizontal")


print(SemanticPlot)

ggsave("ManuscriptImages/SemanticPlot.png", SemanticPlot, width = 16, height = 16, dpi = 300)


