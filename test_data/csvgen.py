import numpy as np
import pandas as pd
records = 1000
X = 100 * np.random.rand(records, 1)
y = 4 + 3*X + 50*np.random.randn(records,1)
pd.DataFrame(np.concatenate((X,y),axis=1), columns=["x","y"]) \
    .to_csv("test.csv",index=False)
