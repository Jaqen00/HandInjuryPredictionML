library(tableone)
library(flextable)
library(dplyr)
library(magrittr)
setwd(getwd())
out_col = 'LOS'
train_data$type <- 'train_data'
test_data$type <- 'test_data'
my_data <- rbind(train_data, test_data)
####################

dput(names(data))
my_vars <- data %>% colnames %>% setdiff(c(out_col, 'type'))
cat_vars <-
  my_vars %>% sapply(function(x) {
    is.character(levels(data[[x]]))
  }) %>% .[which(.)] %>% names
my_tab <-
  CreateTableOne(
    vars = my_vars,
    strata = out_col,
    data = data,
    factorVars = cat_vars,
    addOverall = T
  )
tabMat <-
  print(
    my_tab,
    quote = FALSE,
    noSpaces = TRUE,
    printToggle = T,
    showAllLevels = TRUE
  )
tabMat %>% rownames() %>% lapply(function(x) {
  gsub('\\.', ' ', x)
}) %>% as.character() %>% cbind(tabMat) %>% as.data.frame() %>% subset(select =
                                                                         -c(test)) %>%
  flextable() %>%
  theme_zebra() %>%
  bold(j = '.', bold = TRUE, part = "body") %>%
  bold(
    j = 'p',
    i = ~ p < 0.05,
    bold = TRUE,
    part = "body"
  ) %>%
  set_header_labels('.' = '', 'level' = 'Level', 'p' = 'P-value') %>%
  autofit() %>%
  save_as_docx(path = paste(out_col, 'Tables', 'baseline_group_by_los.docx', sep = '/'))
####################

my_data$type <-
  factor(my_data$type, levels = c('train_data', 'test_data'))
my_vars <- colnames(my_data) %>% setdiff(out_col)
cat_vars <-
  my_vars %>% sapply(function(x) {
    is.character(levels(data[[x]]))
  }) %>% .[which(.)] %>% names

my_tab <-
  CreateTableOne(
    vars = my_vars,
    strata = "type",
    data = my_data,
    factorVars = cat_vars,
    addOverall = T
  )
tabMat <-
  print(
    my_tab,
    quote = FALSE,
    noSpaces = TRUE,
    printToggle = T,
    showAllLevels = T
  )
tabMat %>% rownames() %>% lapply(function(x) {
  gsub('\\.', ' ', x)
}) %>% as.character() %>% cbind(tabMat) %>% as.data.frame() %>% subset(select =
                                                                         -c(test)) %>%
  flextable() %>%
  theme_zebra() %>%
  bold(j = '.', bold = TRUE, part = "body") %>%
  bold(
    j = 'p',
    i = ~ p < 0.05,
    bold = TRUE,
    part = "body"
  ) %>%
  set_header_labels(
    '.' = '',
    'level' = 'Level',
    'train_data' = 'Train data',
    'test_data' = 'Test data',
    'p' = 'P-value'
  ) %>%
  autofit() %>%
  save_as_docx(path = paste(out_col, 'Tables', 'baseline_group_by_type.docx', sep = '/'))
