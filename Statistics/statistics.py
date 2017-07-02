import scipy.stats as ss

def p_adjust(p, method='BH'):
    if method == 'BH':
        prank = ss.rankdata(p, method='average')
        plen = len(p)
    else:
        pass
    padj = [x * plen / xr for x,xr in zip(p, prank)]
    return padj
