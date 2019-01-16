# 票房预测的相关函数定义文件啊


# 文件读取
read_dataset <- function(file){
    # 读取文件
    col_class <- c("numeric", "character", "character", "numeric", "character", "numeric", "character", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric", "numeric","numeric")
    dataset <- read.table(file, header=T, sep=",", fileEncoding="GBK", colClasses=col_class)
    
    # 
#     box <- dataset$box
#     length <- dataset$length
#     before_want <- dataset$before_want
#     before_trans <- dataset$before_trans
#     before_comment <- dataset$before_comment
#     before_zan <- dataset$before_zan
#     fir_want <- dataset$fir_want
#     fir_trans <- dataset$fir_trans
#     fir_comment <- dataset$fir_comment
#     fir_zan <- dataset$fir_zan
#     score <- dataset$score
#     actor <- dataset$actor
#     director <- dataset$director
#     releasetime <- dataset$releasetime
#     type <- dataset$type
       
    
    return(dataset)
}




# 绘图函数及主题
library(ggplot2)
# 轴主题
axis_theme<-theme(
  axis.title=element_text(
    #family=NULL,
    face = "bold", #字体("plain", "italic", "bold", "bold.italic")
    colour = "red", #字体颜色
    size = 25,#字体大小
    hjust = .5, #调整轴标题1：纵轴靠上，横轴靠右；0.5居中；0：纵轴靠下，横轴靠左
    vjust = .5, #1：靠图边框；0靠近轴线；.5居中
    angle = 0 #为什么只对横轴标题有作用？
 ),
  axis.title.x=element_text(colour="blue"),#x轴标题设置，优先级高于axis.title
  axis.title.y=element_text(colour="red"),#同上
  axis.text=element_text(colour="red"),#设置坐标轴刻度数字
  axis.text.x=element_text(colour="blue"),#优先级高于aixis.text
  axis.text.y=element_text(colour="red"),#同上
  axis.ticks=element_line(#坐标轴刻度线的设置
    colour="red",
    size=.5,
    linetype=1,
    lineend=1),
  axis.ticks.x=element_line(colour="blue"),#优先级高于axis.ticks
  axis.ticks.y=element_line(colour="red"),#同上
  axis.ticks.length=unit(.4,"lines"),#设置刻度线的高度
  axis.ticks.margin=unit(.4,"cm"),#设置刻度数字与刻度线的距离
  axis.line=element_line(#设置轴线
    colour="red"),
  axis.line.x=element_line(colour="blue"),#设置x轴线，优先于axis.line
  axis.line.y=element_line(colour="red"))#类似axis.line.x



# 图片区主题
block_theme <- theme(plot.background = element_rect(colour = "black", size = 3, linetype = 4, 
    fill = "lightblue"), plot.title = element_text(colour = "black", face = "bold", 
    size = 25, vjust = 1), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"))

# theme
labs_theme <- theme(plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(hjust = 0.5, face = "bold"))

img_themes <- axis_theme + block_theme + labs_theme




library(ggplot2)
library(gridExtra)
# options(repr.plot.width=4, repr.plot.height=4)
# 直方图绘制函数,单图绘制
plot_hist <- function(data, feature_name){
    a <- ggplot(data = data, aes(data,..density..)) + 
         geom_histogram(color='white',fill='gray70',binwidth = 3) + 
         geom_line(stat='density', size=1) +
         xlab(label = feature_name) +
         ylab(label = "density")
    # 控制图片大小
#     options(repr.plot.width=4, repr.plot.height=4)
#     grid.arrange(a)
#     options(repr.plot.width=8, repr.plot.height=8)
    return(a)
}


# 散点图
plot_Scatter <- function(data, x, y){
    a <- ggplot(data, aes(x=x,y=y)) + geom_point() + stat_smooth(method = 'lm')
    
    return(a)    
}


# QQ
plot_QQ <-function(old_data, bc_data, old_name, bc_name){
    
    options(repr.plot.width=7, repr.plot.height=7)
    par(mfrow = c(2,2))
    hist(old_data)
#     title("原始数据分布直方图")
    hist(bc_data, title= "histogram of bc_data")

    qqnorm(old_data, ylab = old_name)
    qqline(old_data)
    qqnorm(bc_data, ylab = bc_name)
    qqline(bc_data)

}


# 散点对比
# 旧、新
plot_scatter_compare <- function(old_data, bc_data, old_box_data, bc_box_data, old_name, bc_name, old_box_name, new_box_name){
    
    a1<-plot_hist(old_data, old_name)
    a2<-plot_hist(bc_data, bc_name)
    a3<-ggplot(dataset, aes(x=old_data,y=old_box_data)) + geom_point() + stat_smooth(method = 'lm') + labs(title="scatter of old_data",x=old_name, y=old_box_name) + labs_theme
    a4<-ggplot(dataset, aes(x=bc_data,y=bc_box_data)) + geom_point() + stat_smooth(method = 'lm') + labs(title="scatter of bc_data",x=bc_name,y= new_box_name) + labs_theme
    options(repr.plot.width=7, repr.plot.height=7)
    grid.arrange(a1,a2, a3,a4)
}


# 绘制箱线图
plot_box <- function(dataset, data_x, data_y, title, x_text, y_text){
    img <- ggplot(dataset, aes(x=data_x, y=data_y)) + geom_boxplot() + 
                  labs(title=title, x=x_text, y=y_text)  + img_themes 
    grid.arrange(img)
}

# 绘制柱形图
plot_bar <- function(dataset, data_x, data_y, title, x_text, y_text){
    img <- ggplot(dataset,aes(x=data_x, y=data_y)) + geom_bar(stat="identity") + 
                  labs(title=title, x=x_text, y=y_text) + img_themes
    grid.arrange(img)
}


# boxcox
library(MASS)
my_independent_boxcox <- function(y_data, feature_name){
#     options(repr.plot.width=4, repr.plot.height=4)
#     par(mfrow = c(1,3))
#     options(repr.plot.width=8, repr.plot.height=3)
#     X <- y_data  
#     x <- data.frame(feature_name=X)
    x <- y_data
    class(x)    
    result = boxcox(x~1)
#     plot(result)
    mylambda = result$x[which.max(result$y)]
    print(mylambda)
    x2 = (x^mylambda-1)/mylambda    
    
#     hist(x)
#     hist(x2)    
    return(list(x2, mylambda))
}


# timestamp
# 时间戳转化
timestamp <- function(dataset){
    timeformat <- "%Y/%m/%d" 
    timestamp_data <- as.Date(dataset, format=timeformat)
    return(timestamp_data)
}


# 对不符合正态分布的数据进行boxcox变换，
# 绘制变换前后直方图和qq图
data_boxcox_trans <- function(dataset, data, data_name, bc_name){
#     options(repr.plot.width=4, repr.plot.height=4)
    # 零值处理
    if (summary(data)[1] == 0){data <- data + 1}
    # 数据缩小
    small_data <- data * 0.0001
    # boxcox
    result <- my_independent_boxcox(data, data_name)
    bc_data <- result[[1]]
    lambda <- result[[2]]
    
#     bc_col <- as.data.frame(x = bc_data, col.names = bc_name)
    
#     dataset<- cbind(dataset, bc_col)
#     str(dataset)
    summary(bc_data)
    
    plot_QQ(small_data, bc_data, data_name, bc_name)
    return(result)
}


# 异常值检测
abnormal_detection <- function(fit3, dataset){
    # 离群点
    options(repr.plot.width=8, repr.plot.height=8)
    par(mfrow = c(1,3))
    library(car)
    outlierTest(fit3)

    # 高杠杆
    hat.plot <- function(fit) {
     p <- length(coefficients(fit))
     n <- length(fitted(fit))
     plot(hatvalues(fit), main="Index Plot of Hat Values")
     abline(h=c(2,3)*p/n, col="red", lty=2)
     identify(1:n, hatvalues(fit), names(hatvalues(fit)))
     }
    hat.plot(fit3) 

    # 强影响点
    cutoff <- 4/(nrow(dataset)-length(fit3$coefficients)-2)

    plot(fit3, which=4, cook.levels=cutoff)
    abline(h=cutoff, lty=2, col="red") 
    influencePlot(fit3, id.method="identify", main="Influence Plot",
     sub="Circle size is proportional to Cook's distance") 
}


# 残差与拟合效果分析
res_analysis <- function(fit, train_dataset, target_data){
    options(repr.plot.width=8, repr.plot.height=8)
    
    # 拟合值
    train_dataset$fitted <- fitted(fit)
    # 拟合值与预测值散点图
    b <- ggplot(train_dataset, aes(x=target_data, y=train_dataset$fitted)) + geom_point() + 
                  labs(title="scatter of real_val and fit_val(train)",x="real_val", y="fit_val")  + img_themes +
                  geom_abline(intercept = 0, slope = 1, size=1.5, color="red") 
#     grid.arrange(b)   
    # 正态分布检验
    shapiro.test(train_dataset$fitted)
    
    
    # 残差
    train_dataset$res<- residuals(fit)
    # 正态分布检验
    shapiro.test(train_dataset$res)
    # 残差散点图

    a <- ggplot(data.frame(train_dataset$res), aes(x=seq(1,dim(train_dataset)[1],1),y=train_dataset$res)) + geom_point() + stat_smooth(method = 'lm')+ 
                      labs(title="scatter of res(train)",x="index of movie in trainset", y="res")  + img_themes 
    
    
#     hist(sub_bc_train$pc_bc_box.res, title="残差分布直方图")


    c <- ggplot(data = train_dataset, aes(res,..density..)) + 
         geom_histogram(color='white',fill='gray60') + 
         geom_line(stat='density', size=1) +
         xlab(label = "res") +
         ylab(label = "density") +
         labs(title="histogram of res") + img_themes
    grid.arrange(a,b,c)
}


# 计算模型系数
# 映前预测总票房或首周票房
get_model_coef_1 <- function(fit3, pc_5, time_pc){
    # 计算模型系数
    # 映前预测总票房或首周票房
    
    coef_name_list <- c("length","b_want","b_trans","b_comment","b_zan","actor","director","国庆档","贺岁档","暑期档","五一档")
    coef_list <- c()
    for (i in seq(1,5,1)){
        coef <- pc_5$weights[i,1] * fit3$coefficients[1+1] + pc_5$weights[i,2] * fit3$coefficients[1+2] 
        names(coef) <- coef_name_list[i]
        coef_list<- append(coef_list, coef)
    }
    # coef_list

    # 演员导演
    coef <- fit3$coefficients[4]
    names(coef)<- "actor"
    coef_list <- append(coef_list, coef)

    coef <- fit3$coefficients[5]
    names(coef)<- "director"
    coef_list <- append(coef_list, coef)

    # 档期
    for (i in seq(1,4,1)){
             coef <- time_pc$weights[i,1]*fit3$coefficients[1+5] + time_pc$weights[i,2]*fit3$coefficients[1+6] +
                     time_pc$weights[i,3]*fit3$coefficients[1+7]
             names(coef)<- coef_name_list[i+7]
             coef_list<- append(coef_list, coef)
        }

    # 类型
    for (i in seq(1,15,1)){
        coef_list<-append(coef_list, fit3$coefficients[i+8])
    }

    # 偏置
    coef_list<-append(coef_list, fit3$coefficients[1])

    coef_list<- na.omit(coef_list)
    print(coef_list)
    
    
    # 可视化
    dt <- data.frame(obj=names(coef_list), val=coef_list)
    dt$obj <- factor(dt$obj, levels=names(coef_list))
    p <- ggplot(dt, aes(x=obj, y=val, fill=obj, group=factor(1))) + geom_bar(stat="identity") + 
    #     geom_text(aes(label=obj, color=obj),hjust=3, position = position_dodge(0.9)) +
    geom_col(aes(fill = obj), position = "dodge") +
    labs(title="bar graph of model feature coef",x="feture", y="coef") + img_themes + coord_flip()
    grid.arrange(p)
  
}


# 计算模型系数
## 映后预测票房
get_model_coef_2 <- function(fit3, pc_5, time_pc){
    
        coef_name_list <- c("length","b_want","f_want","b_trans","f_trans","b_comment","f_comment","b_zan","f_zan","score","actor","director","国庆档","贺岁档","暑期档","五一档")
    coef_list <- c()
    for (i in seq(1,10,1)){
        coef <- pc_5$weights[i,1] * fit3$coefficients[1+1] + pc_5$weights[i,2] * fit3$coefficients[1+2] + pc_5$weights[i,3] * fit3$coefficients[1+3] 
        names(coef) <- coef_name_list[i]
        coef_list<- append(coef_list, coef)
    }
    # coef_list

    # 演员导演
    coef <- fit3$coefficients[5]
    names(coef)<- "actor"
    coef_list <- append(coef_list, coef)

    coef <- fit3$coefficients[6]
    names(coef)<- "director"
    coef_list <- append(coef_list, coef)

    # 档期
    for (i in seq(1,4,1)){
             coef <- time_pc$weights[i,1]*fit3$coefficients[1+6] + time_pc$weights[i,2]*fit3$coefficients[1+7] +
                     time_pc$weights[i,3]*fit3$coefficients[1+8]
             names(coef)<- coef_name_list[i+12]
             coef_list<- append(coef_list, coef)
        }

    # 类型
    for (i in seq(1,15,1)){
        coef_list<-append(coef_list, fit3$coefficients[i+9])
    }

    # 偏置
    coef_list<-append(coef_list, fit3$coefficients[1])

    coef_list<- na.omit(coef_list)
    print(coef_list)


    # 可视化
    dt <- data.frame(obj=names(coef_list), val=coef_list)
    dt$obj <- factor(dt$obj, levels=names(coef_list))
    p <- ggplot(dt, aes(x=obj, y=val, fill=obj, group=factor(1))) + geom_bar(stat="identity") + 
    #     geom_text(aes(label=obj, color=obj),hjust=3, position = position_dodge(0.9)) +
    geom_col(aes(fill = obj), position = "dodge") +
    labs(title="bar graph of model feature coef",x="feture", y="coef") + img_themes + coord_flip()
    grid.arrange(p)

}





# 映前预测
predict_test_1 <- function(fit3, test_data, target_data){
    sub_bc_test_x <- test_data[2:26]

    test_data$pred <- predict(fit3, sub_bc_test_x)
    c <- ggplot(test_data, aes(x=target_data, y=test_data$pred)) + geom_point() + 
            geom_abline(intercept = 0, slope = 1, size=1.5, color="red") +
            labs(title="真实值与预测散点图（测试效果）",x="真实值", y="预测值")  + img_themes 
    grid.arrange(c)
}

# 映后预测
predict_test_2 <- function(fit3, test_data, target_data){
    sub_bc_test_x <- test_data[2:27]

    test_data$pred <- predict(fit3, sub_bc_test_x)
    c <- ggplot(test_data, aes(x=target_data, y=test_data$pred)) + geom_point() + 
            geom_abline(intercept = 0, slope = 1, size=1.5, color="red") +
            labs(title="真实值与预测散点图（测试效果）",x="真实值", y="预测值")  + img_themes 
    grid.arrange(c)
}



# box_cox lambda
box_cox <- function(data, lambda){
    newdata <- (data^lambda -1) / lambda
    return(newdata)
}