# Author: Henrik Jonasson
# We tried both decision tree with low depth and random forest regression, however linear regression performed better and was chosen for the final deliverable
# Script includes predictions for basin A, trying both predicting interfaces seperately and the percentage flow directly, with and without river inflow data

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn.tree import DecisionTreeRegressor 
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV, train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn import tree
from sklearn.metrics import r2_score

# load data
data2p = pd.read_csv("./Python_csv/A2p.csv")
data2 = pd.read_csv("./Python_csv/A2.csv")
data1 = pd.read_csv("./Python_csv/A1.csv")


#----------------------------
#--- Predict A2p directly ---
#----------------------------

#--- Decision trees with depth 4 ---#

# Load data
# Split into target - features
y = data2p["A2p"]
# First only use interpretable features with low-depth tree
# Including rivers
X_rivers = data2p[["north", "west", "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]

# Test on last 20% data
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X_rivers, y, test_size=0.2, shuffle = False)

# Only 4 layers deep to see the decisions easily
regr_A2p_rivers = DecisionTreeRegressor(max_depth = 4)

# Fit and predict
mA2p_rivers = regr_A2p_rivers.fit(X_train_raw, y_train)

y_pred = mA2p_rivers.predict(X_test_raw)
R_score1 = mA2p_rivers.score(X_test_raw, y_test)
print("R score for A2p directly with rivers and decision tree: ", R_score1)
print(r2_score(y_true = y_test, y_pred = y_pred))

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Percentage")
plt.title("Tree with depth 4: Predictions vs. True values for A2p")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

# Plot prediction trees
fig = plt.figure(figsize=(40,5))
_ = tree.plot_tree(mA2p_rivers, feature_names=X_train_raw.columns, filled=True)
plt.tight_layout()
plt.show()

X_no_rivers = data2p[["wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]

# Test on last 20% data
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X_no_rivers, y, test_size=0.2, shuffle = False)

# Only 4 layers deep to see the decisions easily
regr_A2p_rivers = DecisionTreeRegressor(max_depth = 4)

# Fit and predict
mA2p_rivers = regr_A2p_rivers.fit(X_train_raw, y_train)

y_pred = mA2p_rivers.predict(X_test_raw)
R_score1 = mA2p_rivers.score(X_test_raw, y_test)
print("R score for A2p directly with no rivers and decision tree: ", R_score1)
print(r2_score(y_true = y_test, y_pred = y_pred))

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Percentage")
plt.title("Tree with depth 4: Predictions vs. True values for A2p")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

# Plot prediction trees
fig = plt.figure(figsize=(40,5))
_ = tree.plot_tree(mA2p_rivers, feature_names=X_train_raw.columns, filled=True)
plt.tight_layout()
plt.show()

#--- Random forest ---#
# Using rivers
# Let the model use the single rivers how it wants
X = data2p.drop(columns=["A2p", "north", "west", "date"])

# Test on last 20% data
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X, y, test_size=0.2, shuffle = False)

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
print("R score for A2p directly with rivers and random forest: ", R_score)

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

# Using no rivers
X = data2p[["wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]
# Test on last 20% data
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X, y, test_size=0.2, shuffle = False)

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
print("R score for A2p directly with no rivers and random forest: ", R_score)

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

#--- Decision trees ---#
# With river info
# Split into target - features
y2 = data2["A2"]
y1 = data1["A1"]
# Use our interpretable features
X1 = data1[["north", "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]
X2 = data2[["north", "west", "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]

# Test on last 20%
X2_train, X2_test, y2_train, y2_test = train_test_split(X2, y2, test_size=0.2, shuffle = False)
X1_train, X1_test, y1_train, y1_test = train_test_split(X1, y1, test_size=0.2, shuffle = False)

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
print("R score for A1 using decision tree and rivers: ", R_score1)

y2_pred = mA2.predict(X2_test)
R_score2 = mA2.score(X2_test, y2_test)
MSE2 = np.mean((y2_pred-y2_test)**2)
print("R score for A2 using decision tree and rivers: ", R_score2)

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
print(" R2 score for A2p using A1 A2 sep, decision tree and rivers: ",
      r2_score(y_true = y_test, y_pred= y_pred_p))

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

# With no rivers
# Split into target - features
y2 = data2["A2"]
y1 = data1["A1"]
# Use our interpretable features
X1 = data1[[ "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]
X2 = data2[[ "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]

# Test on last 20%
X2_train, X2_test, y2_train, y2_test = train_test_split(X2, y2, test_size=0.2, shuffle = False)
X1_train, X1_test, y1_train, y1_test = train_test_split(X1, y1, test_size=0.2, shuffle = False)

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
print("R score for A1 using decision tree and no rivers: ", R_score1)

y2_pred = mA2.predict(X2_test)
R_score2 = mA2.score(X2_test, y2_test)
MSE2 = np.mean((y2_pred-y2_test)**2)
print("R score for A2 using decision tree and no rivers: ", R_score2)

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
print(" R2 score for A2p using A1 A2 sep, decision tree and no rivers: ",
      r2_score(y_true = y_test, y_pred= y_pred_p))

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

#--- Random forest ---#
# With river info
# Split into target - features
y2 = data2["A2"]
y1 = data1["A1"]
# Let it use the rivers how it wants
X1 = data1.drop(columns=["A1", "date"])
X2 = data2.drop(columns=["A2", "date"])

# Test on last 20%
X2_train_raw, X2_test_raw, y2_train, y2_test = train_test_split(X2, y2, test_size=0.2, shuffle = False)
X1_train_raw, X1_test_raw, y1_train, y1_test = train_test_split(X1, y1, test_size=0.2, shuffle = False)

# Scale the input data
scaler1 = StandardScaler()
scaler2 = StandardScaler()
# Only train scaler on training data
scaler1.fit(X1_train_raw)
X1_train = scaler1.transform(X1_train_raw)
X1_test = scaler1.transform(X1_test_raw)

scaler2.fit(X2_train_raw)
X2_train = scaler2.transform(X2_train_raw)
X2_test = scaler2.transform(X2_test_raw)

# Initialize the Random forest model
regr1 = RandomForestRegressor(n_estimators=100)
regr2 = RandomForestRegressor(n_estimators=100)

# Fit and predict
mA1 = regr1.fit(X1_train, y1_train)
mA2 = regr2.fit(X2_train, y2_train)

y1_pred = mA1.predict(X1_test)
R_score1 = mA1.score(X1_test, y1_test)
print("R score for A1 using random forest and rivers: ", R_score1)

y2_pred = mA2.predict(X2_test)
R_score2 = mA2.score(X2_test, y2_test)
MSE2 = np.mean((y2_pred-y2_test)**2)
print("R score for A2 using random forest and rivers: ", R_score2)

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y1_test.index, y1_test, label="True values", linewidth=3)
plt.plot(y1_test.index, y1_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow m3/s")
plt.title("Random forest: Predictions vs. True values for A1")
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
plt.title("Random forest: Predictions vs. True values for A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()


# Plot predicted vs. true percentage
p1 = [pred if pred >= 0 else 0 for pred in y1_pred]
p2 = [pred if pred >= 0 else 0 for pred in y2_pred]
y_pred_p = [p2[i]/(p1[i]+p2[i]) if (p1[i]+p2[i]) != 0 else 0 for i in range(len(p1))]
print(" R2 score for A2p using A1 A2 sep, random forest and rivers: ",
      r2_score(y_true = y_test, y_pred= y_pred_p))
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred_p, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow percentage")
plt.title("Random forest: Predictions vs. True values for percentage A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()

# With no rivers
# Split into target - features
y2 = data2["A2"]
y1 = data1["A1"]
# Use our interpretable features
X1 = data1[[ "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]
X2 = data2[[ "wind_speed", "wind_dir", "temp", "precip",  "precip_lag", "month"]]

# Test on last 20%
X2_train_raw, X2_test_raw, y2_train, y2_test = train_test_split(X2, y2, test_size=0.2, shuffle = False)
X1_train_raw, X1_test_raw, y1_train, y1_test = train_test_split(X1, y1, test_size=0.2, shuffle = False)

# Scale the input data
scaler1 = StandardScaler()
scaler2 = StandardScaler()
# Only train scaler on training data
scaler1.fit(X1_train_raw)
X1_train = scaler.transform(X1_train_raw)
X1_test = scaler.transform(X1_test_raw)

scaler2.fit(X2_train_raw)
X2_train = scaler.transform(X2_train_raw)
X2_test = scaler.transform(X2_test_raw)

# Initialize the Random forest model
regr1 = RandomForestRegressor(n_estimators=100)
regr2 = RandomForestRegressor(n_estimators=100)

# Fit and predict
mA1 = regr1.fit(X1_train, y1_train)
mA2 = regr2.fit(X2_train, y2_train)

y1_pred = mA1.predict(X1_test)
R_score1 = mA1.score(X1_test, y1_test)
print("R score for A1 using random forest and  norivers: ", R_score1)

y2_pred = mA2.predict(X2_test)
R_score2 = mA2.score(X2_test, y2_test)
print("R score for A2 using random forest and no rivers: ", R_score2)

# Plot predictions with true values
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y1_test.index, y1_test, label="True values", linewidth=3)
plt.plot(y1_test.index, y1_pred, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow m3/s")
plt.title("Random forest: Predictions vs. True values for A1")
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
plt.title("Random forest: Predictions vs. True values for A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()


# Plot predicted vs. true percentage
p1 = [pred if pred >= 0 else 0 for pred in y1_pred]
p2 = [pred if pred >= 0 else 0 for pred in y2_pred]
y_pred_p = [p2[i]/(p1[i]+p2[i]) if (p1[i]+p2[i]) != 0 else 0 for i in range(len(p1))]
print(" R2 score for A2p using A1 A2 sep, random forest and no rivers: ",
      r2_score(y_true = y_test, y_pred= y_pred_p))
plt.figure(figsize=(14, 12))
plt.rcParams.update({'font.size': 22})
plt.plot(y_test.index, y_test, label="True values", linewidth=3)
plt.plot(y_test.index, y_pred_p, label="Predicted values", linewidth=3)

ymin, ymax = plt.ylim()
plt.ylim(ymin, ymax*1.2)

plt.xlabel("Time")
plt.ylabel("Flow percentage")
plt.title("Random forest: Predictions vs. True values for percentage A2")
plt.legend(facecolor='white', framealpha=1, loc="upper left")
plt.show()