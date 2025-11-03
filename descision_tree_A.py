import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn.tree import DecisionTreeRegressor 
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn import tree

#----------------------------
#--- Predict A2p directly ---
#----------------------------
# Load data
data = pd.read_csv("./Python_csv/A2p.csv")

# Split into target - features
y = data["A2p"]
# Let the model use the single rivers how it wants
X = data.drop(columns=["A2p", "north", "west"])

# Test on last two years
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X, y, test_size=0.08, shuffle = False)

# Scale the input data
scaler = StandardScaler()
# Only train scaler on training data
scaler.fit(X_train_raw)
X_train = scaler.transform(X_train_raw)
X_test = scaler.transform(X_test_raw)

# Initialize the Random forest model
regr = RandomForestRegressor(n_estimators=100)
# Fit the model
m1 = regr.fit(X_train, y_train)

# Predict on the test set
y_pred = m1.predict(X_test)
R_score = m1.score(X_test, y_test)
MSE = np.mean((y_pred-y_test)**2)
print("R score: ", R_score)
print("MSE: ", MSE)

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow percentage")
plt.title("Random forest: Predictions vs. True values for A2p (A2p predicted directly)")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

#------------------------------
#--- Predict A1 and A2 sep. ---
#------------------------------
data2 = pd.read_csv("./Python_csv/A2.csv")
data1 = pd.read_csv("./Python_csv/A1.csv")

# Split into target - features
y2 = data2["A2"]
y1 = data1["A1"]
# Use our interpretable features
X1 = data1[["north", "wind_speed", "wind_dir"]]
X2 = data2[["north", "west", "wind_speed", "wind_dir"]]

# Test on last two years
X2_train, X2_test, y2_train, y2_test = train_test_split(X2, y2, test_size=0.08, shuffle = False)
X1_train, X1_test, y1_train, y1_test = train_test_split(X1, y1, test_size=0.08, shuffle = False)

# We don't scale the data here as we want to see the real values in the decisions

# Initialize the decision tree models
# Only 4 layers deep to see the decisions easily
regr1 = DecisionTreeRegressor(max_depth = 4)
regr2 = DecisionTreeRegressor(max_depth = 4)

# Fit and predict
mA1 = regr1.fit(X1_train, y1_train)
mA2 = regr2.fit(X2_train, y2_train)

y1_pred = mA1.predict(X1_test)
R_score1 = mA1.score(X1_test, y1_test)
MSE1 = np.mean((y1_pred-y1_test)**2)
print("R score for A1: ", R_score1)
print("MSE for A1: ", MSE)

y2_pred = mA2.predict(X2_test)
R_score2 = mA2.score(X2_test, y2_test)
MSE2 = np.mean((y2_pred-y2_test)**2)
print("R score for A2: ", R_score2)
print("MSE for A2: ", MSE2)

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y1_test.index, y1_test, label="True values", linewidth=3)
plt.plot(y1_test.index, y1_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow m3/s")
plt.title("Tree with depth 4: Predictions vs. True values for A1")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y2_test.index, y2_test, label="True values", linewidth=3)
plt.plot(y2_test.index, y2_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow m3/s")
plt.title("Tree with depth 4: Predictions vs. True values for A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

# Plot prediction trees
fig = plt.figure(figsize=(40,5))
_ = tree.plot_tree(mA1, feature_names=X1_train.columns, filled=True)
plt.tight_layout()
plt.show()

fig = plt.figure(figsize=(40,5))
_ = tree.plot_tree(mA2, feature_names=X2_train.columns, filled=True)
plt.tight_layout()
plt.show()

# Plot predicted vs. true percentage
p1 = [pred if pred >= 0 else 0 for pred in y1_pred]
p2 = [pred if pred >= 0 else 0 for pred in y2_pred]
y_pred_p = [p2[i]/(p1[i]+p2[i]) if (p1[i]+p2[i]) != 0 else 0 for i in range(len(p1))]
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred_p, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow percentage")
plt.title("Tree with depth 4: Predictions vs. True values for percentage A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()


