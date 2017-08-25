# -*-coding:utf-8-*-

def p_adjust(p, method='BH'):
    from scipy.stats.rand import rankdata
    if method == 'BH':
        prank = rankdata(p, method='average')
        plen = len(p)
    else:
        pass
    padjlist = [x * plen / xr for x,xr in zip(p, prank)]
    pajd = [min(x, 1) for x in padjlist]
    return padj

####################
