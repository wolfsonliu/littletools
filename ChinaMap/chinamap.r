## -----------------
## Library
## -----------------
library(dplyr)
library(ggplot2)
library(maptools)
library(broom)
library(extrafont)
loadfonts()
## -----------------
## Data
## -----------------
mapdir <- './data'                      # data dir
mapfile <- list()                       # map data list

for (x in list.files(mapdir)) {
    ## read map data
    mapfile[[x]] <- file.path(
        file.path(mapdir, x),
        list.files(file.path(mapdir, x))
    )
    names(mapfile[[x]]) <- basename(mapfile[[x]])
}

## country map data
country          <- list()
country[['shp']] <- readShapePoly(
    mapfile[['国界']]['bou1_4p.shp']
)
## province map data
province          <- list()
province[['shp']] <- readShapePoly(
    mapfile[['国界与省界']]['bou2_4p.shp']
)
province[['shp']]$NAME <- iconv(
    province[['shp']]$NAME,
    from = 'GBK'
)
province[['mapping']] <- data_frame(
    name = as.character(province[['shp']]$NAME),
    id   = as.character(
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
## city map data
city <- list()
city[['shp']] <- readShapePoly(
    mapfile[['县级行政界线']]['BOUNT_poly.shp']
)
city[['shp']]$NAME99 <- iconv(
    city[['shp']]$NAME99,
    from = 'GBK'
)

city[['mapping']] <- data_frame(
    name = as.character(city[['shp']]$NAME99),
    ids  = as.character(
        seq(length(city[['shp']]$NAME99)) - 1
    )
)
city[['shp']] <- city[['shp']][
    !is.na(city[['shp']]$NAME99),
    ]
city[['mapping']] <- city[['mapping']][
    !is.na(city[['mapping']]$name),]

## -----------------
## Example
## -----------------
## example data
province_count <- data_frame(
    province=c(
        '四川省','江苏省', '浙江省', '北京市', '山东省',
        '山西省', '广西壮族自治区', '贵州省', '河北省','河南省'
    ),
    count=c(
        10144, 7895, 6624, 5843, 4330, 3874, 3834, 3665, 3309, 3307
    )
)

## merge data to make data could be used to plot
market <- merge(
    province_count,
    province[['mapping']],
    by.x = 'province',
    by.y = 'province'
)

figure <- ggplot(
    tidy(province[['shp']])
) + geom_polygon(
        aes(x = long, y = lat, group = id),
        color = 'gray95',
        fill  = 'gray85'
    ) + geom_map(
            aes(map_id= ids, fill = species),
            data = market,
            map = tidy(city[['shp']])
        ) + scale_fill_gradient(
                low  = 'darksalmon',
                high = 'red3'
            ) + expand_limits(
                    city[['shp']]
                ) + coord_map() + theme_void()

ggsave('chinamapexample.pdf', plot=figure, family='GB1')

####################
