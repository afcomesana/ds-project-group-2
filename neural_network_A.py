import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import torch
import torch.nn as nn
import torch.optim as optim


data = pd.read_csv("./Python_csv/A2p.csv")
A2 = pd.read_csv("./Discharge/discharge_A2.csv")
A1 = pd.read_csv("./Discharge/discharge_A1.csv")

# Minus on A1 as negative direction from north = into basin
data.insert(0, 'A2', A2['flow'])
data.insert(0, 'A1', -A1['flow'])

# Split into target - features
y = data[["A1", "A2"]]
#y = data[[ "A2p"]]

# Use our interpretable features
X = data[["north", "west", "wind_speed", "wind_dir", "precip"]]

# Use meteo data and every river seperately
#X = data.drop(columns=["A2p", "north", "west", "A1", "A2"])

# Predict on last two years to match our time series models
X_train_raw, X_test_raw, y_train, y_test = train_test_split(X, y, test_size=0.08, shuffle = False)

# Scale the input data
scaler = StandardScaler()
# Only fit scaler to training data (no data leakage)
scaler.fit(X_train_raw)
X_train = scaler.transform(X_train_raw)
X_test = scaler.transform(X_test_raw)

# Convert to PyTorch tensors
# Use GPU if available
device = torch.device('cuda:0' if torch.cuda.is_available() else 'cpu')
X_train = torch.tensor(X_train, dtype=torch.float32).to(device)
y_train = torch.tensor(y_train.to_numpy(), dtype=torch.float32).to(device)
X_test = torch.tensor(X_test, dtype=torch.float32).to(device)
y_test = torch.tensor(y_test.to_numpy(), dtype=torch.float32).to(device)
# Double check shapes
print(X_train.shape)
print(y_train.shape)


# Define simple fully-connected model
model = nn.Sequential(
    nn.Linear(5, 50),
    nn.ReLU(),
    nn.Linear(50, 40),
    nn.ReLU(),
    nn.Linear(40, 30),
    nn.ReLU(),
    nn.Linear(30, 20),
    nn.ReLU(),
    nn.Linear(20, 2)
)
model.to(device)
 
# Loss and optimizer
loss_function = nn.MSELoss() 
optimizer = optim.Adam(model.parameters(), lr=0.0001)
 
# Epoch and batches
n_epochs = 300   
batch_size = 10  
batch_start = torch.arange(0, len(X_train), batch_size)
 
# Keep track of how error improves with training
training_history = []
 
for epoch in range(n_epochs):
    for start in batch_start:
        # Fetch batch
        X_batch = X_train[start:start+batch_size]
        y_batch = y_train[start:start+batch_size]
        # Forward pass and loss
        y_pred = model(X_batch)
        loss = loss_function(y_pred, y_batch)
        # Backward pass
        optimizer.zero_grad()
        loss.backward()
        # Optimize
        optimizer.step()

    # Check test performance at end of epoch
    y_pred = model(X_test)
    mse = float(loss_function(y_pred, y_test))
    training_history.append(mse)
    print("Epoch: ", epoch, "MSE: ", mse)

 
# Plot test error from training iterations
plt.plot(training_history)
plt.show()
 
# Print final predictions on test data
with torch.no_grad():
    y_pred = model(X_test).cpu().numpy()
    y_test = y_test.cpu()
    
    # Plot predictions with true values
    # Plot A1
    plt.figure(figsize=(14, 12))
    plt.rcParams.update({'font.size': 22})
    plt.plot(y_test[:,0], label="True values", linewidth=3)
    plt.plot(y_pred[:,0], label="Predicted values", linewidth=3)

    ymin, ymax = plt.ylim()
    plt.ylim(ymin, ymax*1.2)

    plt.xlabel("Time")
    plt.ylabel("Flow m3/s")
    plt.title("Neural network: Predictions vs. True values for A1")
    plt.legend(facecolor='white', framealpha=1, loc="upper left")
    plt.show()

    # Plot A2
    plt.figure(figsize=(14, 12))
    plt.rcParams.update({'font.size': 22})
    plt.plot(y_test[:,1], label="True values", linewidth=3)
    plt.plot(y_pred[:,1], label="Predicted values", linewidth=3)

    ymin, ymax = plt.ylim()
    plt.ylim(ymin, ymax*1.2)

    plt.xlabel("Time")
    plt.ylabel("Flow m3/s")
    plt.title("Neural network: Predictions vs. True values for A2")
    plt.legend(facecolor='white', framealpha=1, loc="upper left")
    plt.show()

    # Plot predicted vs. true percentage
    p1 = [pred if pred >= 0 else 0 for pred in y_pred[:,0]]
    p2 = [pred if pred >= 0 else 0 for pred in y_pred[:,1]]
    y_pred_p = [p2[i]/(p1[i]+p2[i]) if (p1[i]+p2[i]) != 0 else 0 for i in range(len(p1))]

    pt1 = [pred if pred >= 0 else 0 for pred in y_test[:,0]]
    pt2 = [pred if pred >= 0 else 0 for pred in y_test[:,1]]
    y_p = [pt2[i]/(pt1[i]+pt2[i]) if (pt1[i]+pt2[i]) != 0 else 0 for i in range(len(pt1))]
    plt.figure(figsize=(14, 12))
    plt.rcParams.update({'font.size': 22})
    plt.plot(y_p, label="True values", linewidth=3)
    plt.plot(y_pred_p, label="Predicted values", linewidth=3)

    ymin, ymax = plt.ylim()
    plt.ylim(ymin, ymax*1.2)

    plt.xlabel("Time")
    plt.ylabel("Flow percentage")
    plt.title("Neural network: Predictions vs. True values for percentage A2")
    plt.legend(facecolor='white', framealpha=1, loc="upper left")
    plt.show()

