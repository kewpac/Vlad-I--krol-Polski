#!/usr/bin/env python
# coding: utf-8

# In[13]:


def avg(a):
    b=0
    for i in range(0,len(a)):
        b+=a[i]
    return(b/len(a))
avg([9,11.2,2321,12])

print(avg([2,2,2,2,2,2]) - 2 < 0.0000001)

print(avg([4, 6, 55, 18, 17, 12]) - 18.666666666666668 < 0.0000001)

print(avg([86, 89, 24, 45, 62, 17, 61, 63, 30, 13]) - 49 < 0.0000001)

