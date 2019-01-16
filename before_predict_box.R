####
##   上映前票房预测实例封装
##   导入模型训练数据，预测数据，设定预测目标
####



# 导入相关函数文件
source("function_.R", encoding = 'UTF-8')

before_predict_allbox_model <- function(dataset_file, pred_file, target){
    
# 数据预处理
    ## 读取建模数据
    dataset1 <- read_dataset(dataset_file)
    dataset2 <- read_dataset(pred_file)
    len1 <- dim(dataset1)[1]
    len2 <- dim(dataset2)[1]
    len  <- len1 + len2
    dataset <- rbind(dataset1, dataset2)

  ## 大盘数据扩增
    # 时间戳
    dataset$releasetime <- timestamp(dataset$releasetime)

    # 根据时间扩增到2018
    library(lubridate)
    dataset$newbox <- dataset$box
    dataset$newfirbox <- dataset$firbox
    dataset <- within(dataset, {

        if (target == "box"){                    
                    newbox[year(releasetime) == 2015]  <- box[year(releasetime) == 2015] * 1.2^3
                    newbox[year(releasetime) == 2016]  <- box[year(releasetime) == 2016] * 1.2^2
                    newbox[year(releasetime) == 2017]  <- box[year(releasetime) == 2017] * 1.2^1
                    newbox[year(releasetime) == 2018]  <- box[year(releasetime) == 2018]}
        else{
                    newbox[year(releasetime) == 2015]  <- firbox[year(releasetime) == 2015] * 1.2^3
                    newbox[year(releasetime) == 2016]  <- firbox[year(releasetime) == 2016] * 1.2^2
                    newbox[year(releasetime) == 2017]  <- firbox[year(releasetime) == 2017] * 1.2^1
                    newbox[year(releasetime) == 2018]  <- firbox[year(releasetime) == 2018]
        }


    })
    # summary(dataset$box)
    # summary(dataset$newbox)

  ## 票房单位转换
    # summary(dataset$newbox)
    dataset$small_newbox <- dataset$newbox * 0.0001
    # summary(dataset$small_newbox)

    # # summary(dataset$newfirbox)
    # dataset$small_newfirbox <- dataset$newfirbox * 0.0001
    # summary(dataset$small_newfirbox)

  ##电影档期处理
    # 创建档期
    schedule_hesui <- c(11,12,1,2)
    schedule_wuyi <- c(3,4,5)
    schedule_shuqi <- c(6,7,8)
    schedule_guoqin <- c(9,10)

    # 从时间戳提取月份
    dataset$Rtime <- month(dataset$releasetime)
    dataset$Rtime <- factor(dataset$Rtime, levels = c(seq(1,12,1)), labels = c(seq(1,12,1)))

    # 生成档期
    dataset$schedule <- as.numeric(dataset$Rtime)
    dataset <- within(dataset, {
        schedule[is.element(Rtime, schedule_hesui)] <- "贺岁档"
        schedule[is.element(Rtime, schedule_wuyi)] <- "五一档"
        schedule[is.element(Rtime, schedule_shuqi)] <- "暑期档"
        schedule[is.element(Rtime, schedule_guoqin)] <- "国庆档"
        
        schedule <- factor(schedule)
        
    })
    # head(dataset[c("Rtime", "schedule")])

    # one_hot编码
    dataset$schedule_onehot <- model.matrix(~schedule-1, dataset) 
    # head(dataset$schedule_onehot)

    # 
    title = "按月份的电影票房箱线图（原始数据）"
    x_text = "月份"
    y_text = "电影票房"
    plot_box(dataset, dataset$Rtime, dataset$small_newbox, title, x_text, y_text)

  ## 电影类型压缩
    dataset$type_one <- dataset$type

    dataset <- within(dataset,{
        for (i in seq(1,len,1)){
            str_type<- unlist(strsplit(type_one[i], ","))
            
            if(length(str_type) > 1 & grepl(pattern = "剧情", x = str_type)[1]==TRUE){
                type_one[i][1] <- str_type[2]
            }
            else{
                type_one[i][1] <- str_type[1]
            }
        }
        str(type_one)
        # 相似类型电影合并
        type_one_dup <- type_one
        type_one_dup[type_one_dup == "悬疑"] <- "恐怖"
        type_one_dup[type_one_dup == "惊悚"] <- "恐怖"
        type_one_dup[type_one_dup == "戏曲"] <- "音乐"
        type_one_dup[type_one_dup == "传记"] <- "历史"
        type_one_dup[type_one_dup == "悬疑"] <- "恐怖"
        type_one_dup[type_one_dup == "武侠"] <- "古装"
        type_one_dup[type_one_dup == "冒险"] <- "奇幻"
        
        type_one_dup <- factor(type_one_dup)
        str(type_one_dup)
        type_one <- factor(type_one)
        
    })
    head(dataset[c("type","type_one")])
    str(dataset$type_one)
    type_set <- levels(dataset$type_one)

    # # one-hot编码
    # dataset$type_onehot <- model.matrix(~type_one-1, dataset) 
    # head(dataset$type_onehot)
    # # str(dataset)

    # one-hot编码
    dataset$bigtype_onehot <- model.matrix(~type_one_dup-1, dataset) 
    # head(dataset$bigtype_onehot) 

    # 不同类型电影的数量统计柱形图
    type_sum <- c()
    for (i in type_set){
        a<-data.frame(dataset$type_one[dataset$type_one == i])
        type_sum<-append(type_sum, dim(a)[1])
    } 

    typedataframe <- data.frame(type=type_set, count=type_sum)

    #
    title = "不同类型电影的数量统计"
    x_text = "电影类型"
    y_text = "数量"
    plot_bar(typedataframe, typedataframe$type, typedataframe$count, title, x_text, y_text)


# 数据正态变换
    # options(repr.plot.width=8, repr.plot.height=3)

    # length
    length_result <- data_boxcox_trans(dataset, dataset$length, "length", "bc_length")
    dataset$bc_length <- length_result[[1]]

    # before_want
    before_want_result <- data_boxcox_trans(dataset, dataset$before_want, "before_want", "bc_before_want")
    dataset$bc_before_want <- before_want_result[[1]]

    # fir_want
    fir_want_result <- data_boxcox_trans(dataset, dataset$fir_want, "fir_want", "bc_fir_want")
    dataset$bc_fir_want <- fir_want_result[[1]]

    # before_trans
    before_trans_result <- data_boxcox_trans(dataset, dataset$before_trans, "before_trans", "bc_before_trans")
    dataset$bc_before_trans <- before_trans_result[[1]]

    # fir_trans
    fir_trans_result <- data_boxcox_trans(dataset, dataset$fir_trans, "fir_trans", "bc_fir_trans")
    dataset$bc_fir_trans <- fir_trans_result[[1]]

    # before_comment
    before_comment_result <- data_boxcox_trans(dataset, dataset$before_comment, "before_comment", "bc_before_comment")
    dataset$bc_before_comment <- before_comment_result[[1]]

    # fir_comment
    fir_comment_result <- data_boxcox_trans(dataset, dataset$fir_comment, "fir_comment", "bc_fir_comment")
    dataset$bc_fir_comment <- fir_comment_result[[1]]

    # before_zan
    before_zan_result <- data_boxcox_trans(dataset, dataset$before_zan, "before_zan", "bc_before_zan")
    dataset$bc_before_zan <- before_zan_result[[1]]

    # fir_comment
    fir_zan_result <- data_boxcox_trans(dataset, dataset$fir_zan, "fir_zan", "bc_fir_zan")
    dataset$bc_fir_zan <- fir_zan_result[[1]]

    # score
    score_result <- data_boxcox_trans(dataset, dataset$score, "score", "bc_score")
    dataset$bc_score <- score_result[[1]]

    # actor
    actor_result <- data_boxcox_trans(dataset, dataset$actor, "actor", "bc_actor")
    dataset$bc_actor <- actor_result[[1]]

    # director
    director_result <- data_boxcox_trans(dataset, dataset$director, "director", "bc_director")
    dataset$bc_director <- director_result[[1]]

    # newbox
    newbox_result <- data_boxcox_trans(dataset, dataset$newbox, "newbox", "bc_newbox")
    dataset$bc_newbox <- newbox_result[[1]]

    # # newfirbox
    # newfirbox_result <- data_boxcox_trans(dataset, dataset$newfirbox, "newfirbox", "bc_newfirbox")
    # dataset$bc_newfirbox <- newfirbox_result[[1]]

  ## 导演演员二值化
    # 其中actor、director两项二值化
    dataset$famous_actor <- dataset$bc_actor
    summ <- as.numeric(summary(dataset$bc_actor))
    actor_threshold <- (summ[6] + summ[1]) / 2
    # actor_threshold
    dataset<- within(dataset, {
        famous_actor[bc_actor <= actor_threshold] <- 0
        famous_actor[bc_actor > actor_threshold] <- 1
    })

    #
    dataset$famous_director <- dataset$director
    summ <- as.numeric(summary(dataset$bc_director))
    director_threshold <- (summ[6] + summ[1]) / 2
    # director_threshold
    dataset <- within(dataset,{
        famous_director[bc_director <= director_threshold] <- 0
        famous_director[bc_director > director_threshold] <-  1
    })

    # str(dataset$famous_actor)
    # str(dataset$famous_director)
    # head(dataset[c("famous_actor", "famous_director")])


# 数据降维
  ## 数值特征降维
    # 部分数据
    some_dataset <- dataset[c("length", "before_want","before_trans", "before_comment", "before_zan")]
    # str(some_dataset)
    # 碎石图
    library(psych)
    fa.parallel(some_dataset, fa="pc", n.iter=100,
    show.legend=FALSE, main="Scree plot with parallel analysis") 

    pc_5 <- principal(some_dataset, nfactors = 2, scores = TRUE)
    print(pc_5)
    head(pc_5$scores)
    print(pc_5$weights)

    # 主成分需正态变换
    min_rc1 <- summary(pc_5$scores[,1])[1]
    min_rc2 <- summary(pc_5$scores[,2])[1]

    # min_rc1
    # min_rc2

    positive_rc1 <- pc_5$scores[,1] - min_rc1 + 1
    positive_rc2 <- pc_5$scores[,2] - min_rc2 + 1
    # positive_rc2

    bc_RC1_result <- my_independent_boxcox(positive_rc1, "RC")
    bc_RC2_result <- my_independent_boxcox(positive_rc2, "RC")
    dataset$bc_RC1 <- bc_RC1_result[[1]]
    dataset$bc_RC2 <- bc_RC2_result[[1]]


    # 数据转存与重读取
    # 转存数据
    write.csv(dataset, "bc_data_1.csv",row.names = FALSE)
    # 重新读取
    re_load_dataset_1 <- read.table("bc_data_1.csv",header=T, sep=",", fileEncoding="GBK")
    # head(re_load_dataset)
    # str(re_load_dataset_1)

    # # 分成两部分数据
    # re_load_dataset_1 <- re_load_dataset[1:len1,]
    # predict_data <- re_load_dataset[len1+1:len1+len2,] 

    # 数据集分割
    # 用于回归的正态变换数据
    sub_bc_col_1 <- c( "famous_actor", "famous_director")

    # sub_bc_dataset_1<- subset(re_load_dataset_1, select=bc_col_1)
    sub_bc_dataset_1 <- re_load_dataset_1[sub_bc_col_1]
    # str(sub_bc_dataset_1)

    sub_bc_dataset_2<- re_load_dataset_1[45:60]   # 类型
    # str(sub_bc_dataset_2)
    sub_bc_dataset_3<- re_load_dataset_1[37:40]  # 档期
    # str(sub_bc_dataset_3)

    sub_bc_dataset <- cbind(dataset["bc_newbox"], sub_bc_dataset_1, dataset["bc_RC1"], dataset["bc_RC2"], sub_bc_dataset_2, sub_bc_dataset_3)
    # str(sub_bc_dataset)


    ###### 修改列名防混淆
    # 修改类型列名
    type_name_list <- c("爱情","动画","动作","犯罪","古装","纪录片","家庭","剧情","科幻","恐怖","历史","奇幻","喜剧","音乐","运动","战争")
    for (i in seq(1, length(type_name_list))){
        names(sub_bc_dataset)[i+5] <- type_name_list[i]
    }

    # 修改档期列名
    schedule_name_list <- c("国庆档","贺岁档","暑期档","五一档")
    for (i in seq(1, length(schedule_name_list))){
        names(sub_bc_dataset)[i+21] <- schedule_name_list[i]
    }

    # str(sub_bc_dataset)

    # 档期降维
    time_pc <- principal(sub_bc_dataset[22:25], nfactors = 3, scores = TRUE)
    print(time_pc$weights)
    # head(time_pc$scores)

    sub_bc_dataset$schedule_pc <- time_pc$scores
    # head(sub_bc_dataset$schedule_pc)

# 数据划分
    sub_bc_dataset_dup <- sub_bc_dataset

    # 数据分开成两部分
    # 第一部分为训练模型的数据
    sub_bc_dataset_copy <- sub_bc_dataset_dup[1:len1,]
    # 第二部分为预测的数据
    pred_dataset <- sub_bc_dataset_dup[(len1+1):len-1,]

    # 去异常值
    # sub_bc_dataset <- sub_bc_dataset[-c(21,23,79,128,196,299,300,317,359,425,442,466,506,508,667),]
    sub_bc_dataset_copy <- sub_bc_dataset_copy[-c(18,21,23,79,123,128,196,300,317,354,359,364,393,397,425,429,440,466,491,499,661,667,671),]



    # 数据集分割
    library(MASS)
    proportion <- 0.2
    sub_test_bc_dataset <- sample(nrow(sub_bc_dataset_copy), len * proportion)
    # sub_test_bc_dataset
    sub_bc_test <- sub_bc_dataset_copy[sub_test_bc_dataset,]               # 测试集
    sub_bc_train <- sub_bc_dataset_copy[-sub_test_bc_dataset,]             # 训练集
    # sub_bc_test

# 回归分析
    fit3 <- lm(bc_newbox~bc_RC1 + bc_RC2 + famous_actor + famous_director + 
          schedule_pc +
          爱情 + 动画 + 动作 + 犯罪 + 古装 +
          纪录片 + 家庭 + 剧情 + 科幻 + 恐怖 +
          历史 + 奇幻 + 喜剧 + 音乐 + 运动, data = sub_bc_train)
    summary(fit3)

    # 
    par(mfrow = c(2,2))
    options(repr.plot.width=8, repr.plot.height=8)
    plot(fit3)


    # 异常检测
    abnormal_detection(fit3, sub_bc_train)

    # 残差和拟合效果分析
    res_analysis(fit3, sub_bc_train, sub_bc_train$bc_newbox)    

    # 测试集测试
    predict_test_1(fit3, sub_bc_test, sub_bc_test$bc_newbox)


    # 计算模型系数
    get_model_coef_1(fit3, pc_5, time_pc)

    # lambde_list <- list(length_result[[2]], before_want_result[[2]], before_trans_result[[2]], before_comment_result[[2]], before_zan_result[[2]], actor)


# 预测
    perd_x <- pred_dataset[,2:26]

    pred_box <- predict(fit3, perd_x)
    box_lambda <- newbox_result[[2]]
    real_box <- (pred_box * box_lambda + 1)^(1 / box_lambda)
    # print(real_box)
    
    report <- cbind(dataset2["name"], as.data.frame(real_box))
    # print(report)
    
    return(list(fit3, report))
}