###{{{ Pre

library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(maptools)
library(broom)
library(extrafont)
loadfonts()

datadir = "./"
picdir = file.path(datadir, 'pic')
setwd(datadir)

files = list(
    market = file.path(datadir, 'market.csv'),
    bird   = file.path(datadir, 'bird.csv'),
    count  = file.path(datadir, 'marketcount.csv')
)

data = list()

for (x in names(files)) {
    data[[x]] = read.csv(
        file = files[[x]],
        header = TRUE,
        stringsAsFactors = FALSE
    )
}

rownames(data[['bird']]) = data[['bird']][['birdid']]

data[['count_i']] = data[['count']][
    substr(data[['count']]$birdid, 1, 1) == 'i',
    ]



tmp1 <- inner_join(
    data[['count_i']],
    data[['bird']],
    by = c('birdid' = 'birdid')
)
tmp2 <- inner_join(
    tmp1,
    data[['market']],
    by = c('recordid' = 'recordid')
)
data[['all']] <- tmp2

data[['区域']] <- data_frame(
    province=c(
        '北京市', '天津市', '河北省', '山西省', '内蒙古自治区',
        '辽宁省', '吉林省', '黑龙江省',
        '上海市', '江苏省', '浙江省', '安徽省', '福建省', '江西省', '山东省',
        '河南省', '湖北省', '湖南省',
        '广东省', '海南省', '广西壮族自治区',
        '重庆市', '四川省', '贵州省', '云南省', '西藏自治区',
        '陕西省', '甘肃省', '青海省', '宁夏回族自治区', '新疆维吾尔族自治区'
    ),
    region=c(
        '华北地区', '华北地区', '华北地区', '华北地区', '华北地区',
        '东北地区', '东北地区', '东北地区',
        '华东地区', '华东地区', '华东地区', '华东地区', '华东地区',
        '华东地区', '华东地区',
        '华中地区', '华中地区', '华中地区',
        '华南地区', '华南地区', '华南地区',
        '西南地区', '西南地区', '西南地区', '西南地区', '西南地区',
        '西北地区', '西北地区', '西北地区', '西北地区', '西北地区'
    )
)

data[['北方市场']] <- list(
    province=c('北京市', '河北省', '辽宁省', '吉林省', '黑龙江省',
               '内蒙古自治区', '宁夏回族自治区', '陕西省', '山西省',
               '河南省', '甘肃省', '山东省'),
    city=c('徐州', '连云港', '淮北', '蚌埠', '固镇')
)

data[['northmarket']] <- rbind(
    data[['market']][
        is.element(
            data[['market']]$province,
            data[['北方市场']]$province
        ),
        ],
    data[['market']][
        grepl(
            paste0(
                '(',
                paste0(data[['北方市场']]$city, collapse='|'),
                ')'
            ),
            data[['market']]$city
        ),
        ]
)

data[['南方市场']] <- list(
    province=c('湖北省', '湖南省', '江西省', '浙江省', '上海市',
               '福建省', '广东省', '广西壮族自治区', '海南省', '贵州省',
               '云南省', '四川省', '安徽省', '江苏省', '重庆市'),
    nocity=c('徐州', '连云港', '淮北', '蚌埠', '固镇', '凯里', '潞西市')
)

data[['sourthmarket']] <- data[['market']][
        is.element(
            data[['market']]$province,
            data[['南方市场']]$province
        ) & (!grepl(
                  paste0(
                      '(',
                      paste0(data[['南方市场']]$nocity, collapse='|'),
                      ')'
                  ),
                  data[['market']]$city
              )
        ),
    ]

## -----------------

result <- list()

result[['总鸟种数']] <- length(
    grep("x", unique(data[['count_i']][,'birdid']), invert = TRUE)
)
## -----------------
by_birdid <- group_by(data[['count_i']], birdid)

birdid_count <- summarize(by_birdid,  sum = sum(number))

topbird <- birdid_count[
    order(birdid_count[['sum']], decreasing = TRUE),
]

result[['鸟类数量排序']] <- inner_join(
    topbird,
    data[['bird']],
    by = c('birdid' ='birdid')
)
result[['鸟类市场数量']] <- inner_join(
    summarize(by_birdid, count = length(unique(recordid))),
    data[['bird']],
    by = c('birdid' = 'birdid')
)
## -----------------
by_recordid_sum <- group_by(data[['count_i']], recordid)
recordid_sum <- summarize(by_recordid_sum,  totalnumber = sum(number))

topmarket <- recordid_sum[
    order(recordid_sum[['totalnumber']], decreasing = TRUE),
    ]

result[['市场鸟类数量排序']] <- inner_join(
    topmarket,
    data[['market']],
    by = c('recordid' ='recordid')
)
## -----------------
by_city_sum <- group_by(result[['市场鸟类数量排序']], city)

city_sum <- summarize(
    by_city_sum,
    totalnumber = sum(totalnumber)
)

topcity <- city_sum[
    order(city_sum[['totalnumber']], decreasing = TRUE),
    ]

result[['城市鸟类数量排序']] <- inner_join(
    topcity,
    unique(data[['market']][,c('province', 'city')]),
    by = c('city', 'city')
)
## -----------------
by_province_sum <- group_by(result[['城市鸟类数量排序']], province)

province_sum <- summarize(
    by_province_sum,
    totalnumber = sum(totalnumber)
)

topprovince <- province_sum[
    order(province_sum[['totalnumber']], decreasing = TRUE),
    ]

result[['省份鸟类数量排序']] <- topprovince
names(result[['省份鸟类数量排序']]) <- c('province', 'total')
## -----------------
recordid_count <- summarize(
    by_recordid_sum,
    species = length(unique(birdid))
)
topmarket_count <- recordid_count[
    order(recordid_count[['species']], decreasing = TRUE),
    ]

result[['市场鸟类种数排序']] <- inner_join(
    topmarket_count,
    data[['market']],
    by = c('recordid' ='recordid')
)
## -----------------
record <- inner_join(
    data[['count_i']],
    data[['market']],
    by = c('recordid' = 'recordid')
)

by_city_count <- group_by(record, city)

city_count <- summarize(
    by_city_count,
    species=length(unique(birdid))
)

topcity_count <- city_count[
    order(city_count[['species']], decreasing=TRUE),
    ]

result[['城市鸟类种数排序']] <- inner_join(
    topcity_count,
    unique(data[['market']][,c('province', 'city')]),
    by = c('city', 'city')
)

## -----------------
## 北方市场

northcount <- data[['count']][
    is.element(data[['count']]$recordid, data[['northmarket']]$recordid),
    ]

by_birdid <- group_by(northcount, birdid)

sum_birdid <- summarize(by_birdid, total=sum(number))

result[['北方市场鸟类数量']] <- left_join(
    sum_birdid,
    data[['bird']],
    by=c('birdid'='birdid')
)

result[['北方市场鸟类数量']] <- result[['北方市场鸟类数量']][
    order(result[['北方市场鸟类数量']]$total, decreasing=TRUE),
    ]

write.csv(result[['北方市场鸟类数量']], '北方市场鸟类数量.csv',quote=FALSE)
write.csv(data[['northmarket']], '北方市场.csv',quote=FALSE)
write.csv(sourthcount, '北方市场鸟类记录.csv',quote=FALSE)


plotdata <- head(result[['北方市场鸟类数量']],10)
plotdata$chinese <- factor(
    head(result[['北方市场鸟类数量']]$chinese,10),
    levels=head(result[['北方市场鸟类数量']]$chinese,10)
)
plotdata$color <- ifelse(
    substr(plotdata$birdid, 1, 1) == 'i',
    brewer.pal(9, 'Reds')[6],
    brewer.pal(9, 'Blues')[6]
)
plotdata$来源 <-  ifelse(
    substr(plotdata$birdid, 1, 1) == 'i',
    '野生',
    '驯化'
)

p <- ggplot(
    plotdata,
    aes(x=chinese, y=total)
) + geom_bar(
        stat='identity',
        color=I(plotdata$color),
        fill=I(plotdata$color)
    ) + theme(
            axis.text.x=element_text(angle=45, hjust=1)
    ) + xlab('鸟种') + ylab('总数量')

ggsave('北方市场鸟类数量.pdf', p, family='GB1')

plotdata <- head(
    result[['北方市场鸟类数量']][
        substr(result[['北方市场鸟类数量']]$birdid, 1, 1) == 'i',
    ],
   15
)

plotdata$chinese <- factor(
    head(
        result[['北方市场鸟类数量']][
            substr(result[['北方市场鸟类数量']]$birdid, 1, 1) == 'i',
        ]$chinese,
        15
    ),
    levels=head(
        result[['北方市场鸟类数量']][
            substr(result[['北方市场鸟类数量']]$birdid, 1, 1) == 'i',
        ]$chinese,
        15
    )
)


p <- ggplot(
    plotdata,
    aes(x=chinese, y=total)
) + geom_bar(
        stat='identity',
        color=I(brewer.pal(9, 'Reds')[6]),
        fill=I(brewer.pal(9, 'Reds')[6])
    ) + theme(
            axis.text.x=element_text(angle=45, hjust=1)
    ) + xlab('鸟种') + ylab('总数量')


ggsave('北方市场野生鸟类数量.pdf', p, family='GB1')

## -----------------
## 南方市场

sourthcount <- data[['count']][
    is.element(data[['count']]$recordid, data[['sourthmarket']]$recordid),
    ]

by_birdid <- group_by(sourthcount, birdid)

sum_birdid <- summarize(by_birdid, total=sum(number))

result[['南方市场鸟类数量']] <- left_join(
    sum_birdid,
    data[['bird']],
    by=c('birdid'='birdid')
)

result[['南方市场鸟类数量']] <- result[['南方市场鸟类数量']][
    order(result[['南方市场鸟类数量']]$total, decreasing=TRUE),
    ]

write.csv(result[['南方市场鸟类数量']], '南方市场鸟类数量.csv',quote=FALSE)
write.csv(data[['sourthmarket']], '南方市场.csv',quote=FALSE)
write.csv(sourthcount, '南方市场鸟类记录.csv',quote=FALSE)


plotdata <- head(result[['南方市场鸟类数量']],10)
plotdata$chinese <- factor(
    head(result[['南方市场鸟类数量']]$chinese,10),
    levels=head(result[['南方市场鸟类数量']]$chinese,10)
)
plotdata$color <- ifelse(
    substr(plotdata$birdid, 1, 1) == 'i',
    brewer.pal(9, 'Reds')[6],
    brewer.pal(9, 'Blues')[6]
)
plotdata$来源 <-  ifelse(
    substr(plotdata$birdid, 1, 1) == 'i',
    '野生',
    '驯化'
)

p <- ggplot(
    plotdata,
    aes(x=chinese, y=total)
) + geom_bar(
        stat='identity',
        color=I(plotdata$color),
        fill=I(plotdata$color)
    ) + theme(
            axis.text.x=element_text(angle=45, hjust=1)
    ) + xlab('鸟种') + ylab('总数量')

ggsave('南方市场鸟类数量.pdf', p, family='GB1')

plotdata <- head(
    result[['南方市场鸟类数量']][
        substr(result[['南方市场鸟类数量']]$birdid, 1, 1) == 'i',
    ],
   15
)

plotdata$chinese <- factor(
    head(
        result[['南方市场鸟类数量']][
            substr(result[['南方市场鸟类数量']]$birdid, 1, 1) == 'i',
        ]$chinese,
        15
    ),
    levels=head(
        result[['南方市场鸟类数量']][
            substr(result[['南方市场鸟类数量']]$birdid, 1, 1) == 'i',
        ]$chinese,
        15
    )
)


p <- ggplot(
    plotdata,
    aes(x=chinese, y=total)
) + geom_bar(
        stat='identity',
        color=I(brewer.pal(9, 'Reds')[6]),
        fill=I(brewer.pal(9, 'Reds')[6])
    ) + theme(
            axis.text.x=element_text(angle=45, hjust=1)
    ) + xlab('鸟种') + ylab('总数量')


ggsave('南方市场野生鸟类数量.pdf', p, family='GB1')



## -----------------
# 鸟类在区域中的信息

birdregion <- list()

for (i in unique(data[['区域']][['region']])) {
    birdregion[[i]] <- list()
    rpro <- unname(unlist(data[['区域']][
        data[['区域']][['region']] == i,
        'province'
    ]))
    birdregion[[i]][['all']] <- data[['all']][
        unlist(
            lapply(
                data[['all']][['province']],
                function(x) {
                    return(is.element(x, rpro))
                }
            )
        ),
        ]
    ## write.csv(
    ##     birdregion[[i]][['all']],
    ##     file=file.path(datadir, paste0(i, '_all', '.csv')),
    ##     quote=FALSE
    ## )
    rby_bird <- group_by(
        birdregion[[i]][['all']],
        birdid
    )
    rby_bird_sum <- summarize(
        rby_bird,
        count=sum(number)
    )
    birdregion[[i]][['bird']] <- inner_join(
        rby_bird_sum[
            order(rby_bird_sum[['count']], decreasing=TRUE),
            ],
        data[['bird']],
        by='birdid'
    )
    write.csv(
        birdregion[[i]][['bird']],
        file=file.path(datadir, paste0(i, '_bird', '.csv')),
        quote=FALSE
    )
}



###}}}

###{{{ Map data

## -----------------
## Reading map data
## -----------------
mapdir <- '/home/zhiheng/Documents/Project/littletools/ChinaMap/data'
mapfile <- list()
for (x in list.files(mapdir)) {
    mapfile[[x]] <- file.path(
        file.path(mapdir, x),
        list.files(file.path(mapdir, x))
    )
    names(mapfile[[x]]) <- basename(mapfile[[x]])
}


country          <- list()
country[['shp']] <- readShapePoly(
    mapfile[['国界']]['bou1_4p.shp']
)

province          <- list()
province[['shp']] <- readShapePoly(
    mapfile[['国界与省界']]['bou2_4p.shp']
)
province[['shp']]$NAME <- iconv(
    province[['shp']]$NAME,
    from = 'GBK'
)
province[['mapping']] <- data_frame(
    name=as.character(province[['shp']]$NAME),
    id=as.character(
        seq(length(province[['shp']]$NAME)) - 1
    )
)
province[['shp']] <- province[['shp']][
    !is.na(province[['shp']]$NAME),
    ]
province[['mapping']] <- province[['mapping']][
    !is.na(province[['mapping']]$name),
    ]
names(province[['mapping']]) <- c('province', 'ids')

city <- list()
city[['shp']] <- readShapePoly(
    mapfile[['县级行政界线']]['BOUNT_poly.shp']
)
city[['shp']]$NAME99 <- iconv(
    city[['shp']]$NAME99,
    from='GBK'
)

city[['mapping']] <- data_frame(
    name=as.character(city[['shp']]$NAME99),
    ids=as.character(
        seq(length(city[['shp']]$NAME99)) - 1
    )
)
city[['shp']] <- city[['shp']][
    !is.na(city[['shp']]$NAME99),
    ]
city[['mapping']] <- city[['mapping']][
    !is.na(city[['mapping']]$name),]

city[['center']] <- data_frame(
    name=city[['shp']]$NAME99,
    x=city[['shp']]$CENTROID_X,
    y=city[['shp']]$CENTROID_Y
)


###}}}

###{{{ Plot

## -----------------
## Plot data on map
## -----------------
figures <- list()

market <- list()
market[['province_count']] <- merge(
    result[['省份鸟类数量排序']],
    province[['mapping']],
    by.x = 'province',
    by.y = 'province'
)

figures[['province_count']] = ggplot(
    tidy(province[['shp']])
) + geom_polygon(
        aes(x = long, y = lat, group = id),
        color = 'lavender'
    ) + geom_map(
            aes(map_id= ids, fill = total),
            data = market[['province_count']],
            color = "white",
            map = tidy(province[['shp']])
        ) + scale_fill_gradient(
                high = 'darkcyan',
                low = 'lightblue'
            ) + expand_limits(
                    province[['shp']]
                ) + coord_map() + theme_void()

market[['province_bar']] <- data.frame(
    province = factor(
        result[['省份鸟类数量排序']]$province,
        levels = as.character(result[['省份鸟类数量排序']]$province)
    ),
    total   = as.numeric(result[['省份鸟类数量排序']]$total)
)

figures[['province_bar']] <- ggplot(
    market[['province_bar']],
    aes(x = province, y = total)
) + geom_bar(
        stat = 'identity',
        fill = 'royalblue'
    ) + theme_classic(
        ) + theme(
                axis.text.x  = element_text(
                    ## family = 'Microsoft YaHei',
                    angle = 45,
                    vjust = 0.9,
                    hjust = 0.9,
                    size  = 13
                ),
                axis.title.x = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                axis.title.y = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                title = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15,
                    hjust = 0.5
                )
            ) +
    xlab('省级行政单位') + ylab('数量') + ggtitle('')


market[['bird_count']] <- data.frame(
    chinese = factor(
        result[['鸟类数量排序']]$chinese,
        levels = as.character(result[['鸟类数量排序']]$chinese)
    ),
    total   = as.numeric(result[['鸟类数量排序']]$sum),
    birdid  = as.character(result[['鸟类数量排序']]$birdid)
)

figures[['bird_count_all']] <- ggplot(
    market[['bird_count']][1:15, ],
    aes(x = chinese, y = total)
) + geom_bar(
        stat = 'identity',
        fill = 'royalblue'
    ) + theme_classic(
        ) + theme(
                axis.text.x  = element_text(
                    ## family = 'Microsoft YaHei',
                    angle = 45,
                    vjust = 0.9,
                    hjust = 0.9,
                    size  = 13
                ),
                axis.title.x = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                axis.title.y = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                title = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15,
                    hjust = 0.5
                )
            ) +
    xlab('中文名') + ylab('数量') + ggtitle('总数最多的15种鸟')


figures[['bird_count_china']] <- ggplot(
    market[['bird_count']][
        substr(market[['bird_count']]$birdid, 1, 1) == 'i',
        ][1:15, ],
    aes(x = chinese, y = total)
) + geom_bar(
        stat = 'identity',
        fill = 'royalblue'
    ) + theme_classic(
        ) + theme(
                axis.text.x  = element_text(
                    ## family = 'Microsoft YaHei',
                    angle = 45,
                    vjust = 0.9,
                    hjust = 0.9,
                    size  = 13
                ),
                axis.title.x = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                axis.title.y = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                title = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15,
                    hjust = 0.5
                )
            ) +
    xlab('中文名') + ylab('数量') + ggtitle('总数最多的15种国内鸟种')

figures[['bird_count_out']] <- ggplot(
    market[['bird_count']][
        substr(market[['bird_count']]$birdid, 1, 1) == 'o',
        ][1:15, ],
    aes(x = chinese, y = total)
) + geom_bar(
        stat = 'identity',
        fill = 'royalblue'
    ) + theme_classic(
        ) + theme(
                axis.text.x  = element_text(
                    ## family = 'Microsoft YaHei',
                    angle = 45,
                    vjust = 0.9,
                    hjust = 0.9,
                    size  = 13
                ),
                axis.title.x = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                axis.title.y = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15
                ),
                title = element_text(
                    ## family = 'Microsoft YaHei',
                    size = 15,
                    hjust = 0.5
                )
            ) +
    xlab('中文名') + ylab('数量') + ggtitle('总数最多的15种境外鸟种')



market[['city_species']] <- merge(
    result[['城市鸟类种数排序']],
    city[['mapping']],
    by.x = 'city',
    by.y = 'name'
)

## -----------------
## plot the bird species count in the city level
figures[['city_species']] = ggplot(
    tidy(province[['shp']])
) + geom_polygon(
        aes(x = long, y = lat, group = id),
        color = 'gray95',
        fill  = 'gray85'
    ) + geom_map(
            aes(map_id= ids, fill = species),
            data = market[['city_species']],
            map = tidy(city[['shp']])
        ) + scale_fill_gradient(
                low  = 'darksalmon',
                high = 'red3'
            ) + expand_limits(
                    city[['shp']]
                ) + ggtitle(
                        '全国城市鸟市野鸟种数调查'
                    ) + coord_map() + theme_void()

## -----------------
## plot the bird number by species in city level
allbird <- unique(data[['count_i']]$chinese)
printedbird <- c()
for (bird in c("远东山雀")){#unique(data[['count_i']]$chinese)) {
    birddata <- inner_join(
        data[['count_i']][
            data[['count_i']]$chinese == bird,
            ],
        data[['market']],
        by = c('recordid' = 'recordid')
    )
    birdby_city <- group_by(birddata, city)
    birdsum <- summarize(birdby_city, total = sum(number))
    birdmarket <- merge(
        birdsum,
        city[['mapping']],
        by.x = 'city',
        by.y = 'name'
    )
    birdfigure <- ggplot(
        tidy(province[['shp']])
    ) + geom_polygon(
            aes(x = long, y=lat, group=id),
            color = 'gray95',
            fill  = 'gray85'
        ) + geom_map(
                aes(map_id=ids, fill=total),
                data = birdmarket,
                map = tidy(city[['shp']])
            ) + scale_fill_gradient(
                    low  = 'darksalmon',
                    high = 'red3'
                ) + expand_limits(
                        city[['shp']]
                    ) + ggtitle(
                            paste0(bird, '在全国鸟市中的调查结果')
                        ) + coord_map() + theme_void()
    ggsave(
        file.path(picdir, paste0('city_', bird, '.pdf')),
        plot = birdfigure,
        family = 'GB1'
    )
#    printedbird <- c(printedbird, bird)
}


ggsave(
    file.path(picdir, 'city_species.pdf'),
    plot=figures[['city_species']],
    family='GB1'
)


## -----------------
## market numbers in city
## -----------------

northmarketnodup <- data[['northmarket']][
    !duplicated(data[['northmarket']]$market),
    ]

marketnumby_city <- group_by(northmarketnodup, city)
marketnum <- as_data_frame(
    merge(
        summarize(marketnumby_city, count=n()),
        as_data_frame(city[['center']]),
        by.x='city',
        by.y='name'
    )
)

marketnum <- marketnum[!duplicated(marketnum$city),]

p <- ggplot(
    tidy(province[['shp']])
) + geom_polygon(
        aes(x=long, y=lat, group=id),
        color='gray95',
        fill='gray85'
    ) + geom_point(
            aes(x=x, y=y, size=count, color=city, fill=city),
            data=marketnum
            ## color=brewer.pal(9, 'Reds')[6],
            ## fill=brewer.pal(9,'Reds')[6]
        ) + expand_limits(
                    city[['shp']]
            ) + coord_map() + theme_void()

ggsave(
    file.path(picdir, 'city_北方市场.pdf'),
    plot=p,
    family='GB1'
)

## -----------------

sourthmarketnodup <- data[['sourthmarket']][
    !duplicated(data[['sourthmarket']]$market),
    ]

marketnumby_city <- group_by(sourthmarketnodup, city)
marketnum <- as_data_frame(
    merge(
        summarize(marketnumby_city, count=n()),
        as_data_frame(city[['center']]),
        by.x='city',
        by.y='name'
    )
)

marketnum <- marketnum[!duplicated(marketnum$city),]


p <- ggplot(
    tidy(province[['shp']])
) + geom_polygon(
        aes(x=long, y=lat, group=id),
        color='gray95',
        fill='gray85'
    ) + geom_point(
            aes(x=x, y=y, size=count, color=city, fill=city),
            data=marketnum
            ## color=brewer.pal(9, 'Reds')[6],
            ## fill=brewer.pal(9,'Reds')[6]
        ) + expand_limits(
                    city[['shp']]
            ) + coord_map() + theme_void()

ggsave(
    file.path(picdir, 'city_南方市场.pdf'),
    plot=p,
    family='GB1'
)



###}}}
