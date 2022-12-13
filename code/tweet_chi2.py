import os
import pickle 

import numpy as np 
import pandas as pd 

from sklearn.feature_extraction.text import CountVectorizer


# if pickle
with open("mse231/new_tweets_df.pkl", "rb") as f:
  df = pickle.load(f)

# if pandas df: 
df = pd.read_csv("mse231/new_tweets_df.csv")
df['text_proc'] = df['text'].str.lower()
df = df.loc[~df.text_proc.isna()]

vectorizer = CountVectorizer(analyzer='word', ngram_range=(2, 2), stop_words='english', max_features = 20000)

tok_text = vectorizer.fit_transform(df['text_proc'].values)

grams = vectorizer.get_feature_names_out()

total_counts = np.array(tok_text.sum(0)).flatten()
R_counts = np.array(tok_text[df["ideology_label"] == "conservative", :].sum(0)).flatten()
D_counts = np.array(tok_text[df["ideology_label"] == "liberal", :].sum(0)).flatten()

#D_counts = np.array(tok_text[df["quantile"].isin([1, 2]), :].sum(0)).flatten()
#R_counts = np.array(tok_text[df["quantile"].isin([6, 7]), :].sum(0)).flatten()

cnts = pd.DataFrame({
    'grams':grams, 
    'R_counts': R_counts, 
    'D_counts': D_counts
})


# calculate constants for chi-2
total_dem = D_counts.sum()
total_gop = R_counts.sum()
# chi-2 test
def chi2(big):
    fr = big['R_counts']
    fd = big['D_counts']
    fnr = total_gop - fr
    fnd = total_dem - fd
    num = (fr*fnd - fd*fnr) ** 2
    denom = (fr+fd) * (fr+fnr) * (fd+fnd) * (fnr+fnd)
    chi2 = num / denom
    return chi2

# apply chi2 for each bigram
cnts['chi2'] = cnts.apply(chi2, axis=1)
# sort values
cnts = cnts.sort_values('chi2', ascending=False)

cnts.to_csv("mse231/new_tweets_cnts_chi2.csv")
