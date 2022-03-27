library(gtsummary)
library(caret)
library(pROC)
library(flextable)
library(randomForest)
set.seed(4232)
# 1. Get data from data file####
source('get_data.R')
data <- get_data() %>% data.frame()
# minmax_data <- data
# minmax <- function(x){(x-min(x))/(max(x)-min(x))}
# minmax_data$Age %<>% minmax
# minmax_data$Injuried.time %<>% minmax
# minmax_data$Surgery.duration %<>% minmax
out_col = 'LOS'
include_cols = colnames(data)
# 2. Create output dirs####
dirs <- c('data', 'Figures', 'Tables')
if (!file.exists(out_col)) {dir.create(out_col)}
for (dir in dirs) {
  path_ <- paste(out_col, dir, sep = '/')
  if (!file.exists(path_)) {
    dir.create(path_)
  }
}

# 3. Feature Selecting
# 3.1 Feature Selecting -- Correction plot ------------------------------------
library(corrplot)
corr_data <- data
corr_data <- as.data.frame(lapply(corr_data, as.numeric), check.names = F)
colnames(corr_data) <- lapply(colnames(corr_data), function(x) {
    gsub('\\.', ' ', x)
  })
corMatMy <- cor(corr_data %>% .[, setdiff(names(.), out_col)])
col <- colorRampPalette(c("darkorange", "white", "steelblue"))(20)
corrplot(corMatMy, type = "upper", order = "hclust", col = col, tl.col = "black", tl.cex = 0.8, tl.srt = 70)
tiff( paste(out_col, 'Figures', 'correation plot.tiff', sep = '/'), width = 2000, height = 2000, res = 300)
corrplot( corMatMy, type = "upper", order = "hclust", col = col, tl.col = "black", tl.cex = 0.8, tl.srt = 70
dev.off()
include_cols %<>% setdiff(c('RBC', 'Muscle.injury', 'NE.'))

# 3.2 Feature Selecting -- Random forest with 5 time 10 fold cross-validation -----
library(randomForest)
result <- data %>% {
  replicate(5, rfcv(.[setdiff(include_cols, out_col)], .[[out_col]], cv.fold = 10, step = 1.2), simplify =
              FALSE)
}
error.cv <- sapply(result, "[[", "error.cv")
matplot( result[[1]]$n.var, cbind(rowMeans(error.cv), error.cv), type = "l",  lwd = c(2, rep(1, ncol(error.cv))), col = 1, lty = 1, log = "x", xlab = "Number of variables", ylab = "CV Error")
otu_train.cv <- result
otu_train.cv.df <- data.frame(sapply(otu_train.cv, '[[', 'error.cv'))
colnames(otu_train.cv.df) <- c("err1", "err2", "err3", "err4", "err5")
otu_train.cv.df %<>%  mutate(errmean = rowMeans(.)) %>% mutate(num = as.numeric(rownames(.)))

optimal = otu_train.cv.df %>% {
  .[.$errmean == min(.$errmean), 'num']
}
optimal = 8
ggplot(otu_train.cv.df, aes(x = num)) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[1])), colour = 'Error at each step'), lwd = 1.0) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[2])), colour = 'Error at each step'), lwd = 1.0) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[3])), colour = 'Error at each step'), lwd = 1.0) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[4])), colour = 'Error at each step'), lwd = 1.0) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[5])), colour = 'Error at each step'), lwd = 1.0) +
  geom_line(aes_(y = as.name(names(otu_train.cv.df[6])), colour = 'Mean error'), lwd = 1.2) +
  scale_colour_manual("", breaks = c('Error at each step', 'Mean error'), values = c('#02B1e6', '#e64602')) +
  geom_vline( xintercept = 8, colour = 'gray', lwd = 0.5, linetype = "dashed") +
  labs(x = "Number of variables", y = "Error rates of cross-validations") +
  coord_trans(x = "reverse") +
  scale_x_continuous(breaks = seq(1, 17, 2)) +
  annotate("text", x = 11, y = max(otu_train.cv.df$errmean) * 0.95, label = "Optimal = 8", size = 3, hjust = 0) +
  theme(
    text = element_text(family = "Arial", size = 10),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect( fill = NA,colour = "grey70", size = rel(1)),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text(colour = "white", size = rel(0.8), margin =  ggplot2::margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)),
    complete = TRUE,
    legend.position = c(0.2, 0.5),
    legend.key = element_rect(colour = "transparent", fill = NA)
  )

ggsave( paste(out_col, 'Figures', 'rfcv.png', sep = '/'), height = 3, width = 5, dpi = 600, bg = "transparent")
model_rf <- randomForest(formula(paste(out_col, '~.')), data = data[include_cols], ntree = 100)
imp <- importance(model_rf, type = 2, scale = F)
row.names(imp) <- lapply(row.names(imp), function(x) {
    gsub('\\.', ' ', x)
  })
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[, 1])
featureImportance %<>% .[order(.$Importance, decreasing = T), ] %>% .[0:optimal, ]
ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#02B1e6", width = 0.65) +
  coord_flip() +
  xlab('Features') +
  theme( 
    text = element_text(family = "Arial", size = 10), panel.background = element_rect(fill = "transparent", colour = NA),panel.border = element_rect(fill = NA,colour = "grey70",size = rel(1)),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text( colour = "white", size = rel(0.8),margin =  ggplot2::margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)),
    complete = TRUE
  )

ggsave(paste(out_col, 'Figures', 'featureImp.tiff', sep = '/'), height = 3, width = 5, dpi = 300, bg = "transparent")
rf_include_cols <- importance(model_rf, type = 2, scale = F) %>% {
    .[order(., decreasing = T), ]
  } %>% names %>% .[0:optimal] %>% append(out_col)

# 3.3 Feature Selecting -- Lasso with 10 fold cross-validation -----
library(glmnet)
data_lasso <- data[include_cols]
x <- data_lasso %>% .[, setdiff(names(.), out_col)] %>% data.matrix()
y <- data_lasso %>% .[, out_col] %>% data.matrix()
fit_cv <- cv.glmnet(x,y, alpha = 1,family = 'binomial',nfolds = 10, trace.it = 1)
df = with(fit_cv, data.frame( lambda = lambda, MSE = cvm, MSEhi = cvup, MSElow = cvlo, NZero = nzero))
ggplot(df, aes(x = log(lambda), y = MSE)) +
  geom_point(col = "#e64602") +
  scale_x_continuous(breaks = seq(-7, -1)) +
  xlab('log(λ)') +
  ylab(fit_cv$name) +
  geom_errorbar(aes(ymin = MSElow, ymax = MSEhi), col = "#02B1e6") +
  geom_vline( xintercept = c(log(fit_cv$lambda.1se), log(fit_cv$lambda.min)), linetype = "dashed", color = 'grey') +
  annotate("text", x = log(1.1 * fit_cv$lambda.1se), y = max(fit_cv$cvup) * 0.94,label = expression(log('λ'['1se'])),size = 3, hjust = 0) +
  annotate( "text", x = log(1.1 * fit_cv$lambda.min), y = max(fit_cv$cvup) * 0.94,label = expression(log("λ"['min'])), size = 3, hjust = 0
  ) +
  annotate( "text", x = log(1.1 * fit_cv$lambda.1se), y = max(fit_cv$cvup) * 0.92,label = paste0(df[df$lambda == fit_cv$lambda.1se, 'NZero'], ' coeff.'), size = 3, hjust = 0) +
  annotate( "text", x = log(1.1 * fit_cv$lambda.min), y = max(fit_cv$cvup) * 0.92, label = paste0(df[df$lambda == fit_cv$lambda.min, 'NZero'], ' coeff.'), size = 3, hjust = 0) +
  theme(
    text = element_text(size = 10),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect( fill = NA, colour = "grey70", size = rel(1)),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text(colour = "white", size = rel(0.8), margin =  ggplot2::margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)),
    complete = TRUE
  )

ggsave( paste(out_col, 'Figures', 'lasso.tiff', sep = '/'), height = 3, width = 5, dpi = 300, device = 'tiff', bg = "transparent")
#Plot LASSO coef ------------------------------------
lasso.coef <- coef(fit_cv$glmnet.fit, s = fit_cv$lambda.1se, exact = F)
imp.df <- data.frame(features = lasso.coef@Dimnames[[1]][c(2:length(lasso.coef@Dimnames[[1]]))][lasso.coef@i],coef = lasso.coef@x[c(2:length(lasso.coef@x))])
ggplot(imp.df, aes(x = reorder(features %>% gsub('\\.', ' ', .), coef), y = coef)) +
  geom_bar(stat = "identity", fill = "#02B1e6", width = 0.65) +
  coord_flip() +
  xlab('Features') +
  theme(
    text = element_text(size = 10),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect( fill = NA, colour = "grey70", size = rel(1)),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text( colour = "white", size = rel(0.8), margin =  ggplot2::margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)),
    complete = TRUE
  )
ggsave( paste(out_col, 'Figures', 'lasso coef.tiff', sep = '/'), height = 3, width = 5, dpi = 300, device = 'tiff', bg = "transparent")
 
# 3.4 Feature Selecting -- Finnal features included ------------------------------------
include_cols <- imp.df[['features']] %>% append(rf_include_cols) %>% unique
         
# 4. Model development
# 4.1 Model development -- Basic setting ------------------------------------
predictor <- levels(data[[out_col]])[2]
fml <- formula(paste(out_col, '~ ', paste0( setdiff(include_cols, out_col), collapse = ' + ')))
# 4.2 Model development -- Split dataset ------------------------------------
indxTrain <- createDataPartition(data[[out_col]], p = 0.7, list = F)
train_data <- data[include_cols][indxTrain, ]
test_data <- data[include_cols][-indxTrain, ]
write.csv(train_data,paste(out_col, 'data', 'train_data.csv', sep = '/'), row.names = F)
write.csv(test_data, paste(out_col, 'data', 'test_data.csv', sep = '/'), row.names = F)
# 4.3 Model development -- Training and tuning ------------------------------------
custom_predict <- function(model) {
  train_probs <- predict(model, newdata = rbind(train_data, test_data), type = 'prob') %>% data.frame()
  test_probs <- predict(model, newdata = test_data, type = 'prob') %>% data.frame()
  train_raws <- predict(model, newdata = rbind(train_data, test_data), type = 'raw') %>% factor(levels = levels(train_data[[out_col]]))
  test_raws <- predict(model, newdata = test_data, type = 'raw') %>%  factor(levels = levels(test_data[[out_col]]))
  train_roc <- roc(rbind(train_data, test_data)[[out_col]], train_probs[[predictor]])
  test_roc <- roc(test_data[[out_col]], test_probs[[predictor]])
  train_auc <- round(ci.auc(train_roc), 4)
  test_auc <- round(ci.auc(test_roc), 4)
  train_matrix <- confusionMatrix(train_raws, rbind(train_data, test_data)[[out_col]], positive = predictor)
  test_matrix <- confusionMatrix(test_raws, test_data[[out_col]], positive = predictor)
  list(
    train = list(
      probs = train_probs,
      raws = train_raws,
      roc = train_roc,
      auc = train_auc,
      matrix = train_matrix
    ),
    test = list(
      probs = test_probs,
      raws = test_raws,
      roc = test_roc,
      auc = test_auc,
      matrix = test_matrix
    )
  )
}
fitControl <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    search = "grid",
    sampling = "down"
  )
# 4.4 Random Forest
rf_grid = expand.grid(mtry = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
RF_model <- train( fml, data = train_data, method = "rf", trControl = fitControl, metric = "ROC", tuneGrid = rf_grid)
RF_model$details <- custom_predict(RF_model)
# 4.5 Naive Bayes
nb_grid = expand.grid(
  laplace = seq(0, 5, 0.5),
  usekernel = c(TRUE, FALSE), 
  adjust = seq(0, 5, 0.5)
)
NB_model <- train( fml, data = train_data, method = "naive_bayes", trControl = fitControl, metric = "ROC", tuneGrid = nb_grid)
NB_model$details <- custom_predict(NB_model)
# 4.6 XGboost
XGboost_grid = expand.grid(
  nrounds = seq(100, 500, 50),
  max_depth = seq(1, 10, 2),
  eta = seq(0.1, 0.5, 0.1),
  gamma = seq(0.1, 0.5, 0.1),
  subsample = seq(0.1, 1, 0.2),
  colsample_bytree = seq(0.5, 1, 0.2),
  rate_drop = 0,
  skip_drop = 0,
  min_child_weight = seq(0.6, 1, 0.2),
)

XGboost_model <- train(fml,data =  train_data, method = "xgbDART", trControl = fitControl, metric = "ROC", tuneGrid = XGboost_grid)
XGboost_model$details <- custom_predict(XGboost_model)
# 4.7 Support Vector Machines
svm_grid = expand.grid(
  degree = seq(1, 5, 1),
  scale = 1,
  C = seq(0, 1, 0.2)
)
SVM_model <- train( fml, data = train_data, method = "svmPoly", trControl = fitControl, metric = "ROC" tuneGrid = svm_grid)
SVM_model$details <- custom_predict(SVM_model)
# 4.8 Generalized Linear Model
LR_model <- train(fml,data = train_data, method = "glm", trControl = fitControl, metric = "ROC")
LR_model$details <- custom_predict(LR_model)
library(gtsummary)
LR_model$finalModel %>%
  tbl_regression(exponentiate = TRUE, pvalue_fun = ~ style_pvalue(.x, digits = 2),) %>%
  add_global_p() %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels() %>%
  as_flex_table() %>%
  theme_zebra() %>%
  autofit() %>%
  flextable::save_as_docx(path = paste(out_col, 'Tables/LR.docx', sep = '/'))
# 5.0 Extract metrics from confusion matrixs and save to local file
performance_matrix <- data.frame(Model = c('LR', 'RF', 'NB', 'SVM', 'XGboost'))
for (dataset in c('train', 'test')) {
  for (name in performance_matrix$Model) {
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "_Accuracy'] <- ", name, "_model$detail$", dataset,  "$matrix$overall['Accuracy']" )))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "AUC-ROC'] <- ", name, "_model$detail$", dataset, "$auc[2]")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "AUC-Lower'] <- ", name, "_model$detail$", dataset, "$auc[1]")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "AUC-Upper'] <- ", name, "_model$detail$", dataset, "$auc[3]")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "Sensitivity'] <- ", name,"_model$details$", dataset,"$matrix$byClass['Sensitivity']")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name,"','",dataset,"Specificity'] <- ", name, "_model$details$", dataset, "$matrix$byClass['Specificity']")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset, "Precision'] <- ", name, "_model$details$", dataset, "$matrix$byClass['Precision']")))
    eval(parse(text = paste0("performance_matrix[performance_matrix$Model=='", name, "','", dataset,"Recall'] <- ", name,"_model$details$",dataset,"$matrix$byClass['Recall']")))
    eval(parse( text = paste0( "performance_matrix[performance_matrix$Model=='", name, "','", dataset, "F1'] <- ",  name, "_model$details$",  dataset, "$matrix$byClass['F1']")))
  }
  
}
which.row <- sapply(as.list(c(2:ncol(performance_matrix))), function(col) { which.max(performance_matrix[, col])})
performance_matrix[, c(2:ncol(performance_matrix))] <- round(performance_matrix[, c(2:ncol(performance_matrix))], 4)
performance_matrix %>% t %>% as.data.frame() %>% setNames(., .[1, ]) %>% .[-1, ] %>% tibble::rownames_to_column('-') %>% flextable() %>%
  theme_zebra() %>%
  bold(bold = TRUE, part = "header") %>%
  autofit() %>%
  save_as_docx(path = paste(out_col, 'Tables/performance of 5 models.docx', sep = '/'))

# 6.0 Plot ROC in test data #######################################
library(plotROC)
library(pROC)
roc_data <- rbind(
  data.frame(m = LR_model$details$test$probs[[predictor]], name = 'LR'),
  data.frame(m = RF_model$details$test$probs[[predictor]], name = 'RF'),
  data.frame(m = NB_model$details$test$probs[[predictor]], name = 'NB'),
  data.frame(m = SVM_model$details$test$probs[[predictor]], name = 'SVM'),
  data.frame(m = XGboost_model$details$test$probs[[predictor]], name = 'XGboost')
) %>% mutate(d = rep(as.numeric(test_data[[out_col]]) - 1, length(unique(.$name))))
legend_labels <- sapply(c('LR', 'NB', 'RF', 'SVM', 'XGboost'), function(x) {paste0(x, " (AUC:", format(round(auc( roc(roc_data[roc_data$name == x, 'd'], roc_data[roc_data$name == x, 'm'])), 4), nsmall = 4), ")")})
ggplot(roc_data, aes(m = m, d = d, color = name)) + 
  geom_roc(n.cuts = 0, size = 0.8, max.num.points = 30) + 
  xlab('1 - Specificity') +
  ylab('Sensitivity') +
  labs(color = 'Models') +
  ggtitle('ROC Curve') +
  scale_color_hue(labels = legend_labels) +
  geom_abline(intercept = 0,slope = 1, col = "grey87",lwd = 0.5, linetype = "dashed") +
  theme(
    text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, margin = ggplot2::margin(0, 0, 10, 0)),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect( fill = NA, colour = "grey87",size = rel(2)),
    axis.ticks = element_line(colour = 'grey87'),
    complete = TRUE,
    legend.position = c(0.75, 0.25),
    legend.key = element_rect(colour = "transparent", fill = NA)
  )
ggsave(paste(out_col, 'Figures/roc_test.tiff', sep = '/'), height = 5, width = 5,dpi = 300, device = 'tiff', bg = "transparent")
# 7.1 Plot calibration curve  #######################################
testProbs <- data.frame(
    obs = factor(test_data[[out_col]], levels = rev(levels(train_data[[out_col]]))),
    LR = LR_model$details$test$probs[[predictor]],
    NB = NB_model$details$test$probs[[predictor]],
    SVM = SVM_model$details$test$probs[[predictor]],
    RF = LR_model$details$test$probs[[predictor]],
    XGboost = XGboost_model$details$test$probs[[predictor]]
  )

calibration(obs ~ LR + NB + SVM + RF + XGboost, data = testProbs)

calPlotData <-
  calibration(obs ~ LR + NB + RF + SVM + XGboost, data = testProbs, cuts = 5)
calPlotData %>%
  ggplot(aes(x = midpoint, y = Percent, color = calibModelVar)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    col = "grey",
    lwd = 1,
    linetype = "dashed"
  ) +
  geom_line(lwd = 1.0) +
  facet_wrap("~ calibModelVar") +
  geom_errorbar(
    aes(ymin = Lower, ymax = Upper, color = calibModelVar),
    width = 3,
    size = 0.5
  ) +
  xlab('Predicted Probability') +
  ylab('Observed Probability') +
  labs(color = 'Models') +
  theme(
    text = element_text(size = 10),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.border = element_rect(
      fill = NA,
      colour = "grey87",
      size = rel(2)
    ),
    panel.grid = element_line(colour = "grey87", ),
    axis.ticks = element_line(colour = "grey70", size = rel(0.5)),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text(
      colour = "white",
      size = rel(0.8),
      margin =  ggplot2::margin(0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)
    ),
    complete = TRUE,
    legend.position = c(0.8, 0.25),
    legend.key = element_rect(colour = "transparent", fill = NA)
  )
ggsave(
  paste(out_col, 'Figures/calibration curves.tiff', sep = '/'),
  height = 5,
  width = 10,
  dpi = 300,
  device = 'tiff',
  bg = "transparent"
)
