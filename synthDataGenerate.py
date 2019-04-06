#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr  6 07:57:06 2019

@author: lubos
"""

from sklearn.datasets import make_regression


# D0
ar = make_regression(n_samples = 500, n_features = 3, n_informative = 1, bias = 10)

import pandas as pd

data = pd.DataFrame.from_records(data = ar[0], columns = ('x1', 'x2', 'x3'))

data['y'] = ar[1]

pd.DataFrame.to_csv(data, 'SynthDataD0.csv')

# D1

ar = make_regression(n_samples = 500, n_features = 3, n_informative = 1, bias = 100)

import pandas as pd

data = pd.DataFrame.from_records(data = ar[0], columns = ('x1', 'x2', 'x3'))

data['y'] = ar[1]

pd.DataFrame.to_csv(data, 'SynthDataD1.csv')
