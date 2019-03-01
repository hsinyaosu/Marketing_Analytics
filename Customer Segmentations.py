
# coding: utf-8

# In[163]:


get_ipython().run_line_magic('matplotlib', 'inline')
get_ipython().run_line_magic('config', 'IPCompleter.greedy = True')
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"

import matplotlib.pyplot as plt
from sklearn import tree
from sklearn.tree import DecisionTreeClassifier, export_graphviz
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans


import graphviz 

import pandas as pd
import numpy as np


# In[3]:


clean = pd.read_csv("clean data.csv")
clean.head()


# ## Data Preparation

# In[10]:


clean = clean.drop(['Unnamed: 0'],axis=1)


# In[14]:


# Find baby related product categories and filter on those
np.sort(clean.category_desc_eng.unique())


# In[84]:


baby = ['BABY BATH PRODUCTS', 'BABY FOOD', 'BABY HYGIENE','BABY SHAMPOO AND CONDITIONER', 'OUT. BABY HYGIENE ITEMS',
        'DIAPERS']

market = clean.loc[clean['category_desc_eng'].isin(baby)]
market.shape


# In[85]:


market.category_desc_eng.value_counts()
market.brand_desc.value_counts()


# In[86]:


huggies = market[market.brand_desc == 'HUGGIES']
competitors = market[market.brand_desc != 'HUGGIES']


# ### Customer Segmentation

# In[46]:


# Using attributes from previous project to observe segments of market, huggies & competitors

customers = pd.read_csv("Customers_input.csv")
customers = customers.iloc[:,:13]
customers.shape
customers.head()


# In[180]:


# this is strangely not working!
#market_customers = market.merge(customers, "inner", on = "cust_id")
#market_customers = pd.merge(market, customers, on='cust_id', how='inner')

market_customers = customers.loc[customers['cust_id'].isin(market.cust_id)]
huggies_customers = market_customers.loc[market_customers['cust_id'].isin(huggies.cust_id)]
competitors_customers = market_customers.loc[~(market_customers['cust_id'].isin(huggies.cust_id))]


# In[98]:


# Making sure we got the correct number of customers for each and they add up 
market_customers.cust_id.nunique()
huggies_customers.cust_id.nunique()
competitors_customers.cust_id.nunique()


# ### Exploring Characteristics of Customers 

# ##### Market : Huggies vs Not Huggies

# In[99]:


market_customers['buyHuggies'] = np.where(market_customers['cust_id'].isin(huggies_customers.cust_id), 1, 0)
market_customers.buyHuggies.value_counts()


# In[141]:


# Skew too large for non huggies customers hence we downsampled competitors 
n = market_customers[market_customers.buyHuggies == 0]
n = ds.iloc[:1500,:]
y = market_customers[market_customers.buyHuggies == 1]
ny = pd.concat([n,y])


# In[148]:


# Observing splits and characteristics of these two groups of customers with a Decision Tree

features = ny.iloc[:,1:12]
target = ny.loc[:,'buyHuggies']

## Fit decision tree
dt_buyHuggies = tree.DecisionTreeClassifier(criterion = 'entropy', max_depth = 5, random_state=99,
                                            min_samples_split= 0.1)

dt_buyHuggies = dt_buyHuggies.fit(features, target)

visual = tree.export_graphviz(dt_buyHuggies, out_file=None, 
                         feature_names=list(features),   
                         filled=True, rounded=True,
                         special_characters=True)


# Plotting the tree
graph = graphviz.Source(visual) 
#graph.render('buyHuggies')
#graph


# ##### Huggies Customers Only

# In[172]:


# Clustering to find distinct segments
huggies_customers = huggies_customers.reset_index().set_index('cust_id')

# Standardize
hc = huggies_customers.copy(deep=True)
cols = hc.columns
hc[cols] = hc[cols].astype('float64') 
hc[cols] = StandardScaler().fit_transform(hc[cols])


# In[164]:


# Find optimal k with elbow plot
wcss = []
for i in range(1,15):
    km = KMeans(n_clusters=i,init='k-means++', max_iter=300, n_init=20, random_state=550)
    km.fit(hc)
    wcss.append(km.inertia_)
plt.plot(range(1,15),wcss)
plt.title('Elbow Method')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()


# In[173]:


# Using optimal k fit model
km = KMeans(n_clusters=3,init='k-means++', max_iter=300, n_init=30, random_state=99)
km.fit(hc)


# In[174]:


# Appending clusters back to data

huggies_customers['cluster'] = km.labels_
huggies_customers = huggies_customers.reset_index()
huggies_customers.cluster.value_counts()
huggies_customers = huggies_customers.drop(['index'],axis=1)
huggies_customers.head()


# In[177]:


# Subsetting for tree model
huggies_customers = huggies_customers.drop(['index'],axis=1)
features = huggies_customers.iloc[:,1:-1]
target = huggies_customers.loc[:,'cluster']


# In[176]:


# Decision Tree to analyze characteristics
features = huggies_customers.iloc[:,1:-1]
target = huggies_customers.loc[:,'cluster']

## Fit decision tree
dt_Huggies = tree.DecisionTreeClassifier(criterion = 'entropy', max_depth = None, random_state=99,
                                            min_samples_split= 0.1)

dt_Huggies = dt_Huggies.fit(features, target)

visual = tree.export_graphviz(dt_Huggies, out_file=None, 
                         feature_names=list(features),   
                         filled=True, rounded=True,
                         special_characters=True)


# Plotting the tree
graph = graphviz.Source(visual) 
graph.render('Huggies')
graph


# ##### Competitors Only

# In[181]:


# Clustering to find distinct segments
competitors_customers = competitors_customers.reset_index().set_index('cust_id')

# Standardize
cc = competitors_customers.copy(deep=True)
cols = cc.columns
cc[cols] = cc[cols].astype('float64') 
cc[cols] = StandardScaler().fit_transform(cc[cols])


# In[182]:


# Find optimal k with elbow plot
wcss = []
for i in range(1,15):
    km = KMeans(n_clusters=i,init='k-means++', max_iter=300, n_init=20, random_state=550)
    km.fit(cc)
    wcss.append(km.inertia_)
plt.plot(range(1,15),wcss)
plt.title('Elbow Method')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()


# In[183]:


# Using optimal k fit model
km = KMeans(n_clusters=3,init='k-means++', max_iter=300, n_init=30, random_state=99)
km.fit(cc)


# Appending clusters back to data
competitors_customers['cluster'] = km.labels_
competitors_customers = competitors_customers.reset_index()
competitors_customers.cluster.value_counts()
competitors_customers = competitors_customers.drop(['index'],axis=1)
competitors_customers.head()


# In[185]:


# Subsetting for tree model
#competitors_customers = competitors_customers.drop(['index'],axis=1)
features = competitors_customers.iloc[:,1:-1]
target = competitors_customers.loc[:,'cluster']


# Decision Tree to analyze characteristics
features = competitors_customers.iloc[:,1:-1]
target = competitors_customers.loc[:,'cluster']

## Fit decision tree
dt_Competitors = tree.DecisionTreeClassifier(criterion = 'entropy', max_depth = None, random_state=99,
                                            min_samples_split= 0.1)

dt_Competitors = dt_Competitors.fit(features, target)

visual = tree.export_graphviz(dt_Competitors, out_file=None, 
                         feature_names=list(features),   
                         filled=True, rounded=True,
                         special_characters=True)


# Plotting the tree
graph = graphviz.Source(visual) 
graph.render('Competitors')
graph


# ##### Market 

# In[186]:


# Clustering to find distinct segments
market_customers = market_customers.reset_index().set_index('cust_id')

# Standardize
m = market_customers.copy(deep=True)
cols = m.columns
m[cols] = m[cols].astype('float64') 
m[cols] = StandardScaler().fit_transform(m[cols])


# In[187]:


# Find optimal k with elbow plot
wcss = []
for i in range(1,15):
    km = KMeans(n_clusters=i,init='k-means++', max_iter=300, n_init=20, random_state=550)
    km.fit(m)
    wcss.append(km.inertia_)
plt.plot(range(1,15),wcss)
plt.title('Elbow Method')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()


# In[189]:


# Using optimal k fit model
km = KMeans(n_clusters=6,init='k-means++', max_iter=300, n_init=30, random_state=99)
km.fit(m)


# Appending clusters back to data
market_customers['cluster'] = km.labels_
market_customers = market_customers.reset_index()
market_customers.cluster.value_counts()
market_customers = market_customers.drop(['index'],axis=1)
market_customers.head()


# In[190]:


# Subsetting for tree model
#market_customers = market_customers.drop(['index'],axis=1)

# Decision Tree to analyze characteristics
features = market_customers.iloc[:,1:-1]
target = market_customers.loc[:,'cluster']


## Fit decision tree
dt_Market = tree.DecisionTreeClassifier(criterion = 'entropy', max_depth = None, random_state=99,
                                            min_samples_split= 0.1)

dt_Market = dt_Market.fit(features, target)

visual = tree.export_graphviz(dt_Market, out_file=None, 
                         feature_names=list(features),   
                         filled=True, rounded=True,
                         special_characters=True)


# Plotting the tree
graph = graphviz.Source(visual) 
graph.render('Market')
graph

