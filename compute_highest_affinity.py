import itertools
from itertools import repeat


def highest_affinity(site_list, user_list, time_list):
    # Create a unique list of user_list and build a dictionary using this user_list1 as key. All values are empty lists.
    user_list1 = set(user_list)
    d = {k: [] for k in user_list1}

    # Loop through the dictionary, add the site each user visits into the value of dictionary.
    for k, v in d.items():
        num = 0
        for j in user_list:
            if k == j:
                v.append(site_list[num])
            num = num + 1

    # Create a unique list of site_list and find all the possible and distinct pairs within it.
    # Then loop through the list with pairs. Sort all the pairs and create a new list to store.
    site_list1 = set(site_list)
    total_pair1 = list(itertools.combinations(site_list1, 2))
    total_pair = []
    for q in total_pair1:
        q = list(q)
        q = sorted(q)
        q = tuple(q)
        total_pair.append(q)

    # Create a list called pair_index. This list started with all 0 value and with the same length of total_pair.
    pair_index = list(repeat(0, len(total_pair)))

    # Loop through values of the dictionary.
    # Find pairs in each value and store all these pairs in my_list1
    my_list1 = []
    for v in d.values():
        v_list = list(v)
        v_pair = list(itertools.combinations(v_list, 2))
        for p in v_pair:
            my_list1.append(p)

    # Loop through my_list1 and sort each pair in the list. Store the values in my_list
    my_list = []
    for m in my_list1:
        m = list(m)
        m = sorted(m)
        m = tuple(m)
        my_list.append(m)

    # Compare my_list and total_pair.
    # Find the index of the value in my_list in total_pair.
    index_num = []
    for a in my_list:
        for b in total_pair:
            if a == b:
                index_num.append(total_pair.index(b))

    # Count the number by add 1 in that index in pair_index list.
    for j in index_num:
        pair_index[j] = pair_index[j] + 1

    # Find the maximum in pair_index
    # Find the index of that maximum value
    # Use that index in total_pair to get the highest affinity pair
    max_value = 0
    max_index = 0
    for k in pair_index:
        if k > max_value:
            max_value = k
            max_index = pair_index.index(k)

    return tuple(sorted((total_pair[max_index])))
