import numpy as np
import pandas as pd
records = 1000
X = 100 * np.random.rand(records, 1)
y = 1 + 2*X + X**2 + 10*np.random.randn(records,1)
pd.DataFrame(np.concatenate((X,y),axis=1), columns=[" X "," Y "]) \
    .to_json("test.json", orient="records", lines=False)
