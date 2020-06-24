rm(list=ls())
gc()

library(circlize)

# 构造一个邻接矩阵
set.seed(999)
mat <- matrix(sample(18, 18), 3, 6) # 3行6列的矩阵
rownames(mat) <- paste0("S", 1:3) # 生成行名
colnames(mat) <- paste0("E", 1:6) # 生成列名

# 构造一个邻接列表
df <- data.frame(from = rep(rownames(mat), times = ncol(mat)), # 第1列对象
                 to = rep(colnames(mat), each = nrow(mat)), # 第2列对象
                 value = as.vector(mat),  # 第3列前2列对象相互作用强度
                 stringsAsFactors = FALSE)

# 使用邻接矩阵
chordDiagram(mat) 
circos.clear() # 结束绘图，否则会继续叠加图层

# 使用邻接列表
chordDiagram(df)
circos.clear() 

# 使用邻接矩阵时
circos.par(gap.after = c(rep(5, nrow(mat)-1),  # 2个5，表示3个行名之间的间隙分别为5个单位
                         15,                   # 表示行名与列名之间的间隙，为15个单位
                         rep(5, ncol(mat)-1),  # 5个5，表示6个列名之间的间隙分别为5个单位
                         15))                  # 表示列名与行名之间的间隙，为15个单位
chordDiagram(mat) 
circos.clear() # 返回默认设置

# 使用邻接列表时
circos.par(gap.after = c(rep(5, length(unique(df[[1]]))-1), # 表示第1列元素之间的间隙为5个单位
                         15,                                # 表示第1列与第2列之间的间隙为15个单位
                         rep(5, length(unique(df[[2]]))-1), # 表示第2列元素之间的间隙为5个单位
                         15))                               # 表示第2列与第1列之间的间隙为15个单位  
chordDiagram(df)
circos.clear()

circos.par(start.degree = 90, clock.wise = FALSE) # 逆时针旋转，起点位置在逆时针90度方向，即12点针方向  
chordDiagram(mat)
circos.clear()

chordDiagram(mat, order = c("S1", "E1", "E2", "S2", "E3", "E4", "S3", "E5", "E6"))  # 使用order参数调整顺序，默认3点钟顺时针方向  
circos.clear()

grid_col <-  c(S1 = "red", S2 = "green", S3 = "blue",
    E1 = "grey", E2 = "grey", E3 = "grey", E4 = "grey", E5 = "grey", E6 = "grey") # 构建颜色向量，指定名称属性，则按名称匹配
chordDiagram(mat, grid.col = grid_col, transparency = 0.7) # 调整外围sector颜色，增加透明度
chordDiagram(t(mat), grid.col = grid_col) # 按名称匹配，则link颜色与mat矩阵的列名一致，全变为灰色
circos.clear()

# 数据是邻接矩阵
col_mat <- rand_color(length(mat), transparency = 0.7) # 产生随机颜色矩阵，并指定透明度
dim(col_mat) <- dim(mat) # 以确保col_mat是一个矩阵
chordDiagram(mat, grid.col = grid_col, col = col_mat) # 设置link颜色，
circos.clear()

# 数据是邻接列表
cols <- rand_color(nrow(df), transparency = 0.7) 
chordDiagram(df, grid.col = grid_col, col = cols)
circos.clear()

# link为连续变量
col_fun <- colorRamp2(range(mat), c("#FFEEEE", "#FF0000"), transparency = 0.5) # 产生连续色块并指定透明度
chordDiagram(mat, grid.col = grid_col, col = col_fun)
circos.clear()

# 用数字指定link颜色
chordDiagram(mat, grid.col = grid_col, row.col = 1:3, transparency = 0.7) # 用数字向量指定颜色，向量长度与连接矩阵的行数相同
chordDiagram(mat, grid.col = grid_col, column.col = 1:6, transparency = 0.7) # 用数字向量指定颜色，向量长度与连接矩阵的列数相同
circos.clear()

# 用长度的1的向量指定
chordDiagram(mat, grid.col = grid_col, link.lwd = 2, link.lty = 2, link.border = "red") # 指定link边线宽度，边线线型，边线颜色
circos.clear()

# 用矩阵指定
lwd_mat <- matrix(1, nrow = nrow(mat), ncol = ncol(mat)) # 元素为1的矩阵，其维度与源数据mat一致
lwd_mat[mat > 12] <- 2 # relation > 12,则加宽link边线
border_mat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat)) # 元素为NA的矩阵，其维度与源数据mat一致
border_mat[mat > 12] <- "red" # relation > 2，则为红色边缘线
chordDiagram(mat, grid.col = grid_col, link.lwd = lwd_mat, link.border = border_mat) # 指定link边缘线宽度，边缘线颜色
circos.clear() 

# 参数矩阵维度与数据源不一致,则改变部分颜色,必须按名称属性匹配
border_mat2 <- matrix("black", nrow = 1, ncol = ncol(mat)) # 生成1行的矩阵，其宽与数据源mat一致
rownames(border_mat2) <- rownames(mat)[2] # 将mat第2个行名赋值给border_mat2，则只会改变第mat第2行的边缘线颜色
colnames(border_mat2) <- colnames(mat) # 赋值列名，与数据源mat一致
chordDiagram(mat, grid.col = grid_col, link.lwd = 2, link.border = border_mat2) #
circos.clear()

# 参数矩阵还可以设置为特殊的3列格式，前2列分别对应数据源的行名称和列名称，第3列为参数列，相当于邻接列表格式的参数矩阵
lty_df <- data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E6"), c(1, 2, 3)) # link边缘线分别为1, 2, 3
lwd_df <- data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(2, 2, 2)) # link边线线宽为2
border_df <- data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), c(1, 1, 1)) # link边缘线颜色为1
chordDiagram(mat, grid.col = grid_col, link.lty = lty_df, link.lwd = lwd_df, link.border = border_df) 
circos.clear()

# 当数据源是邻接列表时，只需要指定跟源数据一样行数的向量，特别方便
chordDiagram(df, grid.col = grid_col, 
             link.lty = sample(1:3,nrow(df), replace = TRUE),
             link.lwd = runif(nrow(df)) * 2, 
             link.border = sample(0:1, nrow(df), replace = TRUE))
circos.clear()

# 通过指定不同的颜色进行高亮
chordDiagram(mat, grid.col = grid_col, row.col = c("#FF000080", "#00FF0010", "#0000FF10"))
circos.clear()

# 通过指定透明色给某些在范围之外的relation 
col_mat[mat < 12] <- "#00000000" # relation < 12则变为透明色
chordDiagram(mat, grid.col = grid_col, col = col_mat) # 
circos.clear()

# 通过函数同时指定透明色和高亮色，对邻接列表数据源也适用
col_fun <- function(x) {ifelse(x < 12, "#00000000", "#FF000080") }# relation小于12则为透明色，反之为#FF000080石榴红
chordDiagram(mat, grid.col = grid_col, col = col_fun, transparency = 0.7)
circos.clear()

# 事实上，所有颜色矩阵或颜色生成函数中色彩都是绘制在图形中的，只是程序内部将其透明度设置为了1，
# 通过3列特殊数据框指定的颜色，其缺失的颜色的relation将不会画出
col_df <- data.frame(c("S1","S2", "S3"), c("E5", "E6", "E4"), 
                     c("#FF000080", "#00FF0080", "#0000FF80"))
chordDiagram(mat, grid.col = grid_col, col = col_df) 
circos.clear()

# 对于邻接列表数据源，高亮弦调整更简单，只需要设置要高亮的颜色，其它为透明色就行了
cols <- rand_color(nrow(df))
cols[df[[3]] < 10] <- "#00000000" # 将df中第3列，即relation列，列值小于10的都更新为透明色HEX色值
chordDiagram(df, grid.col = grid_col, col = cols)

# 通过link.visible参数调整
cols <- rand_color(nrow(df))
chordDiagram(df, grid.col = grid_col, link.visible = df[[3]] >= 10) # 只显示df第3列大于10的弦
circos.clear()

chordDiagram(mat, grid.col = grid_col, link.sort = TRUE, link.decreasing = TRUE) #按弦宽度下降排列,则弦宽沿顺时针方向逐渐下降
title("link.sort = TRUE, link.decreasing = TRUE",cex = 0.8) # 添加标题
circos.clear()

chordDiagram(mat, grid.col = grid_col, link.sort = TRUE, link.decreasing = FALSE) # 弦宽沿顺时针方向逐渐增大
title("link.sort = TRUE, link.decreasing = FALSE", cex = 0.8)
circos.clear()

# 邻接矩阵数据源，求矩阵的秩，然后指定给link.rank参数
chordDiagram(mat, grid.col = grid_col, transparency = 0, link.rank = )# 设置透明度为0，方便观察
chordDiagram(mat, grid.col = grid_col, transparency = 0, link.rank = rank(mat)) # 用mat中的秩进行排序，秩最大先添加
circos.clear()

# 邻接列表数据源，对relation列求秩，然后指定给link.rank参数
chordDiagram(df, grid.col = grid_col, transparency = 0, link.rank = rank(df[[3]])) # 第3列为relation列，求秩
circos.clear()

df2 <- data.frame(start = c("a", "b", "c", "a"), end = c("a", "a", "b", "c"))
chordDiagram(df2, grid.col = 1:3, self.link = 1) # 
chordDiagram(df2, grid.col = 1:3, self.link = 2)
circos.clear()

mat3 <- matrix(rnorm(25), 5) # 生成25个均匀分布的随机数, 5行排列
colnames(mat3) <- letters[1:5] 
cor_mat <- cor(mat3) # 求相关系数,则变为对称矩阵

col_fun <- colorRamp2(c(-1, 0, 1), c("green", "white", "red"))
chordDiagram(cor_mat, grid.col = 1:5, symmetric = TRUE, col = col_fun)
title("symmetric = TRUE") # 增加标题
circos.clear()

chordDiagram(cor_mat, grid.col = 1:5, col = col_fun)
title("symmetric = FALSE")
circos.clear()

par(mfrow = c(1, 3)) # 设置绘图环境，多图布局，1行3列布局

chordDiagram(mat, grid.col = grid_col, directional = 1) # 结束端要短一些
chordDiagram(mat, grid.col = grid_col, directional = 1, diffHeight = uh(5, "mm")) # 设定缩短量为5mm， uh表示传递单位  
chordDiagram(mat, grid.col = grid_col, directional = -1) # 反转方向，这行名对应的端要短一些
circos.clear()
par()
dev.off()

mat2 <- matrix(sample(100, 35), nrow = 5)
rownames(mat2) <- letters[1:5]
colnames(mat2) <- letters[1:7]
mat2
chordDiagram(mat2, grid.col = 1:7, directional = 1, row.col = 1:5)
circos.clear()

mat3 <- mat2 
for (cn in intersect(rownames(mat3), colnames(mat3))) { 
  mat3[cn, cn] <- 0 # 将行名和列名相同的值更改为0
  
}
mat3 

chordDiagram(mat3, grid.col = 1:7, directional = 1, row.col = 1:5) # 设置弦方向为从行名到列名，设置弦颜色
circos.clear()

arr_col <- data.frame(c("S1", "S2", "S3"), c("E5", "E6", "E4"), 
                      c("black", "black", "black")) # 生成箭头的颜色3列特征数据框
chordDiagram(mat, grid.col = grid_col, directional = 1,
             link.arr.col = arr_col, direction.type = "arrows", link.arr.length = 0.2) 
circos.clear()

# 同时设置箭头和弦高diffHeight
chordDiagram(mat, grid.col = grid_col, directional = 1, 
    direction.type = c("diffHeight", "arrows"), # 同时设置箭头和弦高
    link.arr.col = arr_col, link.arr.length = 0.2)
circos.clear()

par(mfrow = c(1, 2))
# 指定箭头类型为大箭头
matx <-  matrix(rnorm(64), 8)

chordDiagram(matx, directional = 1, direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow") # 大箭头，箭头和箭杆合二为一
circos.clear()

# 大箭头加调整弦高diffHeight
chordDiagram(matx, directional = 1, direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow", diffHeight = -uh(2, "mm")) # 设置弦高为-2mm
circos.clear()

# 默认移除小比例值
mat <- matrix(rnorm(36), 6, 6)
rownames(mat) <-  paste0("R", 1:6)
colnames(mat) <- paste0("C", 1:6)
mat[2, ] <- 1e-10 # 将第2行所有值改成很小的值
mat[, 3] <- 1e-10 # 将第3列所有值改成很小的值

chordDiagram(mat)
circos.info() # 显示绘图的对象，不包含第2行的行名(R2)和第3列的列名(C3)，则表示被移除了
circos.clear()

# reduce参数调整
mat[2, ] <- 1e-2
chordDiagram(mat, reduce = 1e-3) # 控制reduce参数比C2小，则C2行不会被移除
circos.info()
circos.clear()

par(mfrow = c(1, 3)) # 多图布局，分3列排版
chordDiagram(mat, grid.col = grid_col, annotationTrack = "grid") # 只显示网格，不显示刻度线和标签轨道
chordDiagram(mat, grid.col = grid_col, annotationTrack = c("name", "grid"), # 指定显示标签和网格轨道
    annotationTrackHeight = c(0.03, 0.01)) # 指定标签轨道和网格轨道的环高  

chordDiagram(mat, grid.col = grid_col, annotationTrack = NULL) # 移除所有轨道
circos.clear()


