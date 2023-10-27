library(mlr3verse)
########
# install.packages("modeldata")
data("credit_data", package = "modeldata") 
df = tibble::as_tibble(credit_data)
df
task = as_task_classif(df, target = "Status")
task

split = partition(task, ratio = 0.8)  # 默认stratify=TRUE
learner = lrn("classif.ranger", predict_type = "prob")
learner
# 构建预处理管道
preproc = po("imputemedian") %>>%
  po("imputesample") %>>%
  po("collapsefactors", no_collapse_above_prevalence = 0.03) %>>%
  po("fixfactors") %>>%
  po("encode", method = "one-hot")

graph = preproc %>>% learner     # 接上学习器
graph$plot(html = FALSE)         # 可视化
glrn = as_learner(graph)        # 转化为图学习器

# 搜索空间
search_space = ps(
  classif.ranger.mtry = p_int(lower = 1, upper = 10),
  classif.ranger.num.trees = p_int(lower = 500, upper = 1000),
  classif.ranger.min.node.size = p_int(lower = 2, upper = 10))

# 定义自动调参器
at = auto_tuner(
  learner = glrn,
  resampling = rsmp("cv", folds = 4),
  measure = msr("classif.auc"),
  search_space = search_space,
  tuner = tnr("random_search"),
  term_evals = 5)

future::plan("multicore")   # 启动并行化
at$train(task)               # 启动调参过程
at$tuning_result             # 查看最优超参数

# 更新图学习器超参数
glrn$param_set$values = at$tuning_result$learner_param_vals[[1]]
glrn$param_set
glrn$train(task, row_ids = split$train)   # 训练集上训练模型

# 在测试集上做预测
predictions = glrn$predict(task, row_ids = split$test)
predictions

predictions$confusion                    # 混淆矩阵
predictions$score(msr("classif.acc"))   # 预测准确率
autoplot(predictions, type = "roc")     # ROC 曲线
predictions$score(msr("classif.auc"))      # AUC 值

#############################################################################
library(naniar)
str(df)
vis_miss(df)
miss_var_summary(df)  #缺失
gg_miss_var(df)
#单变量的异常值
univ_outliers = function(x, method = "boxplot", k = NULL,
                         coef = NULL, lp = NULL, up = NULL){
  switch(method,
         "sd" = {
           if(is.null(k)) k = 3
           mu = mean(x, na.rm = TRUE)
           sd = sd(x, na.rm = TRUE)
           LL = mu - k * sd
           UL = mu + k * sd},
         "boxplot" = {
           if(is.null(coef)) coef = 1.5
           Q1 = quantile(x, 0.25, na.rm = TRUE)
           Q3 = quantile(x, 0.75, na.rm = TRUE)
           iqr = Q3 - Q1
           LL = Q1 - coef * iqr
           UL = Q3 + coef * iqr},
         "percentiles" = {
           if(is.null(lp)) lp = 0.025
           if(is.null(up)) up = 0.975
           LL = quantile(x, lp)
           UL = quantile(x, up)
         })
  idx = which(x < LL | x > UL)
  n = length(idx)
  list(outliers = x[idx], outlier_idx = idx, outlier_num = n)
}
x = mpg$hwy
univ_outliers(x,method = "boxplot")

mod = lm(mpg ~ wt, mtcars)
car::outlierTest(mod)

############################################################
task = as_task_classif(na.omit(df), target = "Status")
task
split = partition(task, ratio = 0.7)    # 默认stratify = TRUE, 按目标变量分层
lrn()
learner = lrn("classif.ranger", predict_type = "prob")
learner
learner$train(task, row_ids = split$train)
learner$model
prediction = learner$predict(task, row_ids = split$test)
prediction
prediction$score(msr("classif.acc"))      # 准确率
autoplot(prediction, type = "roc")        # 绘制ROC曲线, 需要precrec包
prediction$score(msr("classif.auc"))      # AUC面积

####################################
tasks = tsk("sonar")     # 可以是多个任务
learners = lrns(c("classif.rpart","classif.ranger", "classif.svm"), 
                predict_type = "prob")
design = benchmark_grid(tasks, learners, rsmps("cv", folds = 5))
design
bmr = benchmark(design)            # 执行基准测试 
bmr$aggregate(list(msr("classif.acc"), msr("classif.auc")))
autoplot(bmr, type = "roc") 
autoplot(bmr, measure = msr("classif.auc")) 

#实际数据
dat <-import("datas/all.data.xlsx",sheet="historical.data")
str(dat)
tasks=list(task1= as_task_regr(dat[,2:9], target = "ton.dmm", id="y1"), 
           task2= as_task_regr(dat[,c(2:8,10)], target = "ton.mf", id="y2"))
tasks
lrns()
learners = lrns(c("regr.lm", "regr.ranger","regr.rpart"),
                predict_sets = c("train", "test"))
resamplings = rsmps("cv", folds = 3)
bmr = benchmark(benchmark_grid(tasks, learners, resamplings))
bmr
autoplot(bmr)
msr()
measures = list(msr("regr.rmse"), msr("regr.rsq"))
tab = bmr$aggregate(measures)
tab


##################### randomForest
airquality <- airquality %>% na.omit()
airquality
train <- sample(nrow(airquality), nrow(airquality)*0.7)
ozo_train <- airquality[train, ]
ozo_test <- airquality[-train, ]
library(randomForest)
# Random forest calculation（default 500 tress），please see ?randomForest
ozo_train.forest <- randomForest::randomForest(Ozone~., data = ozo_train, importance = TRUE)
ozo_train.forest
ozo_train.forest$importance
varImpPlot(ozo_train.forest, main = "variable importance")
library(ggExtra)
library(ggpmisc)
test_pred=modelr::add_predictions(ozo_test, ozo_train.forest)
g <- ggplot(test_pred, aes(Ozone, pred)) + 
  geom_point() + 
  geom_smooth(method="lm",formula = 'y ~ x', se=F) +
  geom_abline(slope = 1,intercept = 0,lty="dashed") +
  stat_poly_eq(
    aes(label =after_stat(adj.rr.label)),
    formula = y ~ x,  parse = TRUE,
    family="serif",
    size = 6.4,
    color="black",
    label.x = 0.1,  #0-1之间的比例确定位置
    label.y = 1)
g
ggMarginal(g, type = "histogram", fill="transparent")
