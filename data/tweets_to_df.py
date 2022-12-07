#%%
import json
import pandas as pd
import os
import pickle
import numpy as np

#%%
#set up auxiliary files
with open('data/score_mappings.json', 'r') as f:
    score_mappings = json.load(f)

with open('data/accounts.pkl', 'rb') as f:
    accounts = pickle.load(f)

for account in accounts:
    if len(account) < 3:
        account = (*account, "did not pull") 
        # original accounts lists did not have name field, 
        # so we only observe name field when it was pulled 
        # in a follower list (unfollowed politicians don't 
        # have a name field)

accounts_dict = {}
for account in accounts:
    accounts_dict[account[1]] = account

#%%
# convert user level tweets to user dfs
tweet_samples = ["new_tweets", "old_tweets"]
for sample in tweet_samples:
    path = "data/" + sample
    files = os.listdir(path)
    for file in files[3000:]: # SUBSET INDEXES HERE
        account_id = file.removesuffix("_tweets.pkl")

        completed = os.listdir("data/" + sample + "_dfs")
        if np.any([str(account_id) in f for f in completed]):
            continue # we have already pulled this user's tweets

        if account_id not in score_mappings.keys():
            continue # skip accounts without ideology score

        df = pd.DataFrame(columns = ['tweet_id', 
        'account_id', 'username', 'name', 'score', 'quantile', 'ideology_label', 
        'text', 'retweet_count', 'reply_count', 'like_count', 
        'quote_count', 'lang', 'created_at'])

        score = score_mappings[account_id]["score"]
        quantile = score_mappings[account_id]["quantile"]
        ideology = "conservative" if quantile > 4 else "liberal" if quantile < 4 else "neutral" 
        

        with open(path + "/" + file, "rb") as f:
            tweets_list = pickle.load(f)
        
        for tweet in tweets_list:
            row = pd.DataFrame({'tweet_id' : tweet["id"], 
            'account_id' : account_id, 
            'username' : accounts_dict[account_id][0], 
            'name' : accounts_dict[account_id][2],
            'score' : score, 
            'quantile' : quantile,
            'ideology_label' : ideology, 
            'text' : tweet["text"], 
            'retweet_count' : tweet["public_metrics"]["retweet_count"], 
            'reply_count' : tweet["public_metrics"]["reply_count"], 
            'like_count' : tweet["public_metrics"]["like_count"], 
            'quote_count' : tweet["public_metrics"]["quote_count"], 
            'lang' : tweet["lang"], 
            'created_at' : tweet["created_at"]}, index=[0])
            print(row)
            df = pd.concat([df, row]).reset_index(drop=True)
            print(df)
        
        with open("data/" + sample + "_dfs/" + account_id + "_df.pkl", 'wb') as f:
            pickle.dump(df, f)

#%%
# concatenate user dfs
for sample in tweet_samples:
    path = "data/" + sample + "_dfs"
    files = os.listdir(path)
    df = pd.DataFrame(columns = ['tweet_id', 
        'account_id', 'username', 'name', 'score', 'quantile', 'ideology_label', 
        'text', 'retweet_count', 'reply_count', 'like_count', 
        'quote_count', 'lang', 'created_at'])
    for file in files:
        with open(path + "/" + file, "rb") as f:
            new = pickle.load(f)
        df = pd.concat([df, row]).reset_index(drop=True)
        
    with open("data/" + sample + "_df.pkl", 'wb') as f:
        pickle.dump(df, f)
