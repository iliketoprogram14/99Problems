__author__ = "Randy Miller"
__date__ = "09/05/11 11:06 pm"

import random

#Problem 1: Find the last element of a list
def last(l):
    return None if len(l) == 0 else l[len(l)-1]

#Problem 2: Find the last but one element of a list
def sec_to_last(l):
    return None if len(l) <= 1 else l[len(l)-2]

#Problem 3: Find the kth element of a list (assuming 0 <= k < len(l))
def kth(l, k):
    return None if k >= len(l) else l[k]

#Problem 4: Find the number of elements of a list
def count(l):
    return len(l)

#Problem 5: Reverse a list
def rev(l):
    return l.reverse()

def rev2(l):
    return reduce(lambda x, y: [y]+x, l, [])

#Problem 6: Find out whether a list is a palindrome
def palindrome(l):
    leftOver = 0 if len(l)%2 == 0 else l[(len(l)-1)/2]
    return leftOver == reduce(lambda x,y: x ^ y, l)

#Problem 7: Flatten a list
def flat(l, accum=[]):
    if len(l) == 0: return accum
    if not isinstance(l[0], list):
        return flat(l[1:], accum+[l[0]])
    return flat(l[1:], flat(l[0], accum))

#Problem 8: Eliminate consecutive duplicates in a list
def elim_dup(l, last=None, accum=[]):
    if len(l) == 0: return accum
    if l[0] == last: return elim_dup(l[1:], last, accum)
    return elim_dup(l[1:], l[0], accum+[l[0]])

#Problem 9: Pack consecutive duplicates of list elements into sublists
def pack(l, last=[], accum=[]):
    if len(l) == 0: return accum+[last]
    if len(last) == 0: return pack(l[1:], [l[0]], accum)
    if l[0] == last[0]: return pack(l[1:], last+[l[0]], accum)
    else: return pack(l[1:], [l[0]], accum+[last])

#Problem 10: Run-length encoding of a list
def encode_runl(l):
    return map(lambda x: [len(x), x[0]], pack(l))

#Problem 11: Modified run-length encoding of a list
def encode_mod_runl(l):
    return map(lambda x: x[0] if len(x)==1 else [len(x),x[0]], pack(l))

#Problem 12: Decode a modified run-length encoded list
def expand(num, v, accum=[]):
    if num == 0: return accum
    return expand(num-1, v, accum+[v])

def decode_runl(l):
    return flat(map(lambda x: x if not isinstance(x, list) else expand(x[0],x[1]), encode_mod_runl(l)))


#Problem 13: Run-length encoding of a list (direct solution)
def encode_dir_runl(l, last=[], accum=[]):
    if len(l) == 0:     return accum+[last[1]] if last[0] == 1 else accum+[last]
    if len(last) == 0:  return encode_dir_runl(l[1:], [1, l[0]], accum)
    if l[0] == last[1]: return encode_dir_runl(l[1:], [last[0]+1,l[0]], accum)
    if last[0] == 1:    return encode_dir_runl(l[1:], [1, l[0]], accum+[last[1]])
    return encode_dir_runl(l[1:], [1, l[0]], accum+[last])

#Problem 14: Duplicate the elements of a list
def dup_once(l):
    return dup(l, 2)

#Problem 15: Duplicate the elements of a list a given number of times
def dup(l, n):
    return flat(map(lambda x: expand(n, x), l))

#Problem 16: Drop every Nth element from a list (starting at 1)
def drop_nth(l, n, curr=1, accum=[]):
    if len(l) == 0: return accum
    if curr==n: return drop_nth(l[1:], n, 1, accum)
    return drop_nth(l[1:], n, curr+1, accum+[l[0]])

#Problem 17: Split a list into two parts; the length of the first part is given
def split(l, n):
    return (l, None) if len(l) <= n else (l[:n], l[n:])

#Problem 18: Extract a slice from a list
def get_slice(l, s, e):
    return l[s-1:e]

#Problem 19: Rotate a list N places to the left
def rot(l, n, cur=0, accum=[]):
    if cur == -1: return accum+[l[cur+n]]
    if n+cur >= len(l): return rot(l, n, -n, accum)
    return rot(l, n, cur+1, accum+[l[cur+n]])

#Problem 20: Remove the kth element from a list (starting at 0)
def rm(l, k):
    return (None, l) if k >= len(l) else (l[k],l[:k]+l[k+1:])

#Problem 21: Insert an element at a given position into a list (starting at 0)
def insert(l, n, pos):
    return l+[n] if pos > len(l) else l[:pos-1]+ [n] + l[pos-1:]

#Problem 22: Create a list containing all integers within a given range
def create_range(low, hi, accum=[]):
    if low == hi: return accum+[low]
    return create_range(low+1, hi, accum+[low])

#Problem 23: Extract a given number of randomly selected elements from a list
def rnd_select(l, n, accum=[]):
    if n == 0 or len(l) == 0: return accum
    k = random.randrange(0, len(l))
    numRemoved, newLst = rm(l, k)
    print str(l)+' '+str(numRemoved) +' '+str(newLst)
    return rnd_select(newLst, n-1, accum+[numRemoved])
    
#Problem 24: Lotto: Draw N different random numbers from the set 1-M
def lotto(lo, hi, n):
    return rnd_select(create_range(lo,hi), n)

#Problem 25: Generate a random permutation of the elements of a list
def rnd_perm(l):
    return rnd_select(l, len(l))

#Problem 26: Generate the combinations of k distinct objects chosen from the N elements of a list
def combination(k, l, curr=[]):
    n = len(l)
    count = 0
    if k==1:
        for j in range(k-1, n):
            print curr+[l[j]]
        return j+1
    for i in range(0,n-k+1):
        count += combination(k-1, l[i+1:], curr+[l[i]])
    return count
            
    
#Problem 27: Group the elements of a set into disjoint subsets
def group(l, sizes):
    pass #don't feel like implementing

#Problem 28: Sorting a list of lists according to length of sublists
def my_decode(tup):
    length, lst = tup
    return lst

#run length encode, sort, and decode
def lsort(l):
    sortedEncodedLst = sorted(map(lambda x: (len(x), x), l))
    return map(lambda x: my_decode(x), sortedEncodedLst)
