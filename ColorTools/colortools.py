# -*-coding:utf-8-*-
# import matplotlib as mpl
# mpl.use('Gtk3Agg')
import matplotlib.pyplot as plt
import matplotlib.colors as colors

cmaps = {
    'Perceptually Uniform Sequential': [
        'viridis', 'plasma', 'inferno', 'magma'
    ],
    'Sequential': [
        'Greys', 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds',
        'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu', 'BuPu',
        'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn', 'YlGn'
    ],
    'Sequential (2)': [
        'binary', 'gist_yarg', 'gist_gray', 'gray', 'bone', 'pink',
        'spring', 'summer', 'autumn', 'winter', 'cool', 'Wistia',
        'hot', 'afmhot', 'gist_heat', 'copper'
    ],
    'Diverging': [
        'PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu',
        'RdYlBu', 'RdYlGn', 'Spectral', 'coolwarm', 'bwr', 'seismic'
    ],
    'Qualitative': [
        'Pastel1', 'Pastel2', 'Paired', 'Accent',
        'Dark2', 'Set1', 'Set2', 'Set3',
        'tab10', 'tab20', 'tab20b', 'tab20c'
    ],
    'Miscellaneous': [
        'flag', 'prism', 'ocean', 'gist_earth', 'terrain', 'gist_stern',
        'gnuplot', 'gnuplot2', 'CMRmap', 'cubehelix', 'brg', 'hsv',
        'gist_rainbow', 'rainbow', 'jet', 'nipy_spectral', 'gist_ncar'
    ]
}

def easycolor(colormap, n):
    def func(scale):
        return colors.rgb2hex(
            plt.get_cmap(colormap, n)(scale)
        )
    return func

colorbrewer = dict(
    zip(
        cmaps['Sequential'],
        [ easycolor(x, 256) for x in cmaps['Sequential'] ]
    )
)

def colorseries(colormap, n):
    return [
        colors.rgb2hex(x) for x in plt.get_cmap(colormap, n)(range(n))
    ]

####################
