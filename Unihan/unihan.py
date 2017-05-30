# ------------------
# Libraries
# ------------------

import pandas as pd
import numpy as np
import os

# ------------------
# Variables
# ------------------


__all__ = [
    'chinese2pinyin',
    'pinyinshengmu'
]


initial = {
    'single' : [
        'b', 'p', 'm', 'f', 'd', 't', 'n', 'l',
        'g', 'k', 'h', 'j', 'q', 'x', 'r', 'z', 'c', 's'
    ],
    'double' : [
       'zh', 'ch', 'sh'
    ]
}



# ------------------
# Functions
# ------------------


def unicode2str(uni):
    uninum = uni.replace('U+', '')
    uninum = '0' * (8 - len(uni) + 2) + uninum
    return (b'\U' + uninum.encode('ascii')).decode('unicode_escape')


def chinese2pinyin(char):
    return unihan['read'].loc[
        np.logical_and(
            unihan['read']['Source'] == 'kMandarin',
            unihan['read']['Character'] == char
        ),
        'Read'
    ].values[0].lower()


def pinyinshengmu(pinyin):
    if sum(pinyin.find(x) == 0 for x in initial['double']):
        return pinyin[0:2]
    elif sum(pinyin.find(x) == 0 for x in initial['single']):
        return pinyin[0]
    else:
        return pinyin

# ------------------

unihan = dict()

unihan['read'] = pd.read_table(
    os.path.join(os.path.dirname(__file__), 'Unihan_Readings.txt'),
    header=None,
    comment='#'
)
unihan['read'].columns = ['Unicode', 'Source', 'Read']
unihan['read']['Character'] = unihan['read']['Unicode'].map(
    lambda x: unicode2str(x)
)
