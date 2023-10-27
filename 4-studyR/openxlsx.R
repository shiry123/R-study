library(openxlsx) # 加载R包
wb <- createWorkbook() # 创建Excel表
saveWorkbook(wb, file='test.xlsx') # 保存Excel表
addWorksheet(wb, "iris") # 在Excel表中增加iris工作表
iris_head <-head(iris, 20) # 查看iris的前20行
writeData(wb, # 前面创建的Excel表
          "iris", # 指定在哪个工作表写入数据
          iris_head, # 要写入工作表的数据
          startCol = 1, # 从哪一列开始写入数据
          startRow = 1, # 从哪一行开始写入数据
          rowNames = TRUE) # 写入数据框的行名
saveWorkbook(wb, 
             file='test.xlsx',
             overwrite = T) # 为TRUE，则覆盖已经存在的文件
data=read.xlsx('test.xlsx',# 要读取的xlsx文件
               detectDates = TRUE, # 识别日期数据并进行转换
               fillMergedCells = TRUE) # 如果为 TRUE，
                         # 则合并单元格中的值将提供给合并中的所有单元格。
head(data)

wb <- createWorkbook() # 创建Excel表
addWorksheet(wb, "car")
writeData(wb, 
          "car", 
          mtcars, 
          startCol = 1, 
          startRow = 1, 
          rowNames = TRUE)
addFilter(wb, # 指定Excel表
          1, # 指定具体工作表进行筛选，这里通过数字来指定
          row = 1, # 行数
          cols = 1:ncol(mtcars)) # 列数
saveWorkbook(wb, 
             file='mtcars.xlsx',
             overwrite = T)
style <- createStyle(fgFill = "#008B8B",
                     fontColour = "#FFFFFF")
addStyle(wb, 
         sheet = 'car', 
         style=style, 
         rows = 1, 
         cols = 1:(ncol(mtcars)+1), 
         gridExpand = TRUE)
saveWorkbook(wb, 
             file='mtcars.xlsx',
             overwrite = T)

read.xlsx("rawdata/GW012-sam-acutator.xlsx",sheet="apsd",
          colNames = FALSE,
          rowNames = TRUE)

write.xlsx(iris, file = "writeXLSX1.xlsx", colNames = TRUE, borders = "columns")