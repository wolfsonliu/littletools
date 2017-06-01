def easycolor(x, n):
    def func(scale):
        return colors.rgb2hex(
            plt.get_cmap(x, n)(scale)
        )
    return func

colorbrewer = {
    'Blues':   easycolor('Blues', 10),
    'BuGn':    easycolor('BuGn', 10),
    'BuPu':    easycolor('BuPu', 10),
    'GnBu':    easycolor('GnBu', 10),
    'Greens':  easycolor('Greens', 10),
    'Greys':   easycolor('Greys', 10),
    'Oranges': easycolor('Oranges', 10),
    'OrRd':    easycolor('OrRd', 10),
    'PuBu':    easycolor('PuBu', 10),
    'PuBuGn':  easycolor('PuBuGn', 10),
    'PuRd':    easycolor('PuRd', 10),
    'Purples': easycolor('Purples', 10),
    'RdPu':    easycolor('RdPu', 10),
    'Reds':    easycolor('Reds', 10),
    'YlGn':    easycolor('YlGn', 10),
    'YlGnBu':  easycolor('YlGnBu', 10),
    'YlOrBr':  easycolor('YlOrBr', 10),
    'YlOrRd':  easycolor('YlOrRd', 10)
}
