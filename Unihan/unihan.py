# ------------------
# Libraries
# ------------------


import pandas as pd
import numpy as np
import os
import re


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
    '''
    Convert unicode to string. U+0061 to "a".
    '''
    uninum = uni.replace('U+', '')
    uninum = '0' * (8 - len(uni) + 2) + uninum
    return (b'\U' + uninum.encode('ascii')).decode('unicode_escape')


def chinese2pinyin(char):
    '''
    汉字转为拼音.
    '''
    if char in [x for x in unihan['read']['Character']]:
        return unihan['read'].loc[
            np.logical_and(
                unihan['read']['Source'] == 'kMandarin',
                unihan['read']['Character'] == char
            ),
            'Read'
        ].values[0].lower()
    else:
        return char


def pinyinshengmu(pinyin):
    '''
    返回拼音的声母.
    '''
    if sum(pinyin.find(x) == 0 for x in initial['double']):
        return pinyin[0:2]
    elif sum(pinyin.find(x) == 0 for x in initial['single']):
        return pinyin[0]
    else:
        return pinyin


def simplified_traditional(char):
    '''
    简体中文繁體中文转换.
    '''
    if not char in unihan['variant']['Character'].loc[
        np.logical_or(
            unihan['variant']['Type'] == 'kTraditionalVariant',
            unihan['variant']['Type'] == 'kSimplifiedVariant'
        )
    ].unique():
        return [char]
    else:
        return [
            x[0] for x in unihan['variant']['VariantChar'].loc[
                np.logical_and(
                    unihan['variant']['Character'] == char,
                    np.logical_or(
                        unihan['variant']['Type'] == 'kTraditionalVariant',
                        unihan['variant']['Type'] == 'kSimplifiedVariant'
                    )
                )
            ].tolist()
        ]

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

unihan['variant'] = pd.read_table(
    os.path.join(os.path.dirname(__file__), 'Unihan_Variants.txt'),
    header=None,
    comment='#'
)

unihan['variant'].columns = ['Unicode', 'Type', 'Variant']

unihan['variant']['Character'] = unihan['variant']['Unicode'].map(
    lambda x: unicode2str(x)
)
unihan['variant']['VariantChar'] = unihan['variant']['Variant'].map(
    lambda x: re.split('[< ,]', x)
).map(
    lambda x: [y for y in x if y != '' and y[0] == 'U']
).map(
    lambda x: [unicode2str(y) for y in x]
)


####################
