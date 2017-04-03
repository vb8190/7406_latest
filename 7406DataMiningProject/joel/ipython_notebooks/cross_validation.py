#I got this far writing this script before I realized that
#scikit already has a CV function

import pandas as pd
import random as rd
import math

def cv_picks(N_size, K_Folds = 10):
    row_nums = list(range(N_size))
    rd.shuffle(row_nums)
    test_size = N_size/K_Folds
    fold_size = math.floor(test_size)
    remainder = 0
    test_picks = []
    for i in range(K_Folds):
        CV_begin = (i * fold_size) + math.floor(remainder)
        remainder = remainder + test_size - fold_size
        if math.isclose(remainder,math.ceil(remainder)):
            remainder = math.ceil(remainder)
        CV_end = ((i + 1) * fold_size) + math.floor(remainder)
        test_picks += [row_nums[CV_begin:CV_end]]
    return test_picks

def cv_set(data, test_picks):
    if type(test_picks) is not list:
        print("***test_picks must be a list or a list of lists***")
        return
    else:
        if type(test_picks[0]) is list:
            pick_list = test_picks.pop(0)
        else:
            pick_list = test_picks
        test_data = data.iloc[pick_list,]
        train_data = data.drop(data.index[pick_list])
        return train_data, test_data, test_picks

def cv_sets(data, K_Folds = 10):
    training_sets = []
    test_sets = []
    test_picks = cv_picks(data.shape[0], K_Folds)
    while len(test_picks) > 0:
        sets = cv_set(data, test_picks)
        training_sets.append(sets[0])
        test_sets.append(sets[1])
        test_picks = sets[2]
    return training_sets, test_sets
