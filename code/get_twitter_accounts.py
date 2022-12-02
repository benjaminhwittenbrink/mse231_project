##################################
### AUTHOR: 
###     BENJAMIN WITTENBRINK
###     JOHN KOHLER 
###     EVA BATELAAN
###     MICHELLE LAHRKAMP
###     THOMAS BRINK
###     MS&E 231 FINAL PROJECT
##################################

#%%
import tweepy
import argparse
import pandas as pd 
import csv
import os
import time
import pickle
from datetime import datetime
import numpy as np


#%%
def load_politician_accounts(house, senate, prez):
    politicians_set = set()
    for filename in [house, senate, prez]:
        path = os.path.abspath(filename)
        with open(path) as f:
            account_list = csv.reader(f)
            for row in account_list:
                politicians_set.add(tuple(row))

    return politicians_set

#%%
def get_following_lists(client, users):
    for user in users: 
        id = user[1]

        completed_files = os.listdir("data/following_lists")
        if np.any([str(id) in f for f in completed_files]):
            continue # we have already pulled this user's following list

        user = client.get_user(id = id, user_fields=["public_metrics"])
        if user.data.public_metrics["following_count"] > 5000:
            continue # we will ignore this user's following

        paginator = tweepy.Paginator(
		    client.get_users_following,
		    id = id, 
		    user_fields=['username'], 
            max_results=1000
        )

        following = set()
        for rsp in paginator:
            time.sleep(60)
            if rsp.data is not None:
                for followee in rsp.data:
                    following.add((followee["username"],str(followee["id"]), followee["name"])) 
    
        filename = "data/following_lists/" + id + "_following.pkl"
        with open(filename, "wb") as f:
            pickle.dump(following, f)
    
    return

#%%
def create_full_accounts_list(politicians):
    follower_lists_dir = os.listdir("data/following_lists")
    follower_lists = []
    for filename in follower_lists_dir:
        path = "data/following_lists/" + filename
        with open(path, "rb") as f:
            list = pickle.load(f)
        follower_lists.append(list)
    
    follower_counts = {}
    for list in follower_lists:
        for user in list:
            id = user[1]
            if id not in follower_counts.keys():
                follower_counts[id] = 1
            else:
                follower_counts[id] += 1

    politician_ids = set()
    for politician in politicians:
        id = politician[1]
        politician_ids.add(id)

    accounts_dict = {}
    for list in follower_lists:
        for user in list:
            id = user[1]
            if follower_counts[id] >= 15 or id in politician_ids:
                if id not in accounts_dict.keys():
                    accounts_dict[id] = user

    for politician in politicians:
        id = politician[1]
        if id not in accounts_dict.keys():
            print("lonely politician:" + politician[0])
            accounts_dict[id] = politician
    
    accounts_list = []
    for id in accounts_dict.keys():
        accounts_list.append(accounts_dict[id])

    filename = "data/accounts.pkl"
    with open(filename, "wb") as f:
        pickle.dump(accounts_list, f)

    


#%%
## MAIN 
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
		description="Get Following Matrix from Politician Twitter Accounts"
	)
    parser.add_argument("--keyfile", help="file with user credentials", required=True)
    
    flags = parser.parse_args()
    creds = {}
    for line in open(flags.keyfile, "r"):
        row = line.strip()
        if row:
            key, value = row.split()
            creds[key] = value

    politicians = load_politician_accounts("data/115th-Congress-House-seeds.csv", "data/115th-Congress-Senate-seeds.csv", "data/2016-presidential-candidates-seeds.csv")

    client = tweepy.Client(bearer_token=creds["bearer_token"])

    get_following_lists(client, politicians)

    create_full_accounts_list(politicians)
    with open("data/accounts.pkl", "rb") as f:
        full_accounts_list = pickle.load(f)
    
    get_following_lists(client, full_accounts_list)

# %%
