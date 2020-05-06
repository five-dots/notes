import numpy as np
import matplotlib.pyplot as plt
import sys
import os
import pickle


def AND_old(x1, x2):
    w1, w2, theta = 0.5, 0.5, 0.7
    tmp = x1 * w1 + x2 * w2
    if tmp <= theta:
        return 0
    elif tmp > theta:
        return 1


def AND(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.7
    tmp = np.sum(x * w) + b
    if tmp <= 0:
        return 0
    else:
        return 1


def NAND(x1, x2):
    x = np.array([x1, x2])
    w = np.array([-0.5, -0.5])
    b = 0.7
    tmp = np.sum(x * w) + b
    if tmp <= 0:
        return 0
    else:
        return 1


def OR(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.2
    tmp = np.sum(x * w) + b
    if tmp <= 0:
        return 0
    else:
        return 1


def XOR(x1, x2):
    s1 = NAND(x1, x2)
    s2 = OR(x1, x2)
    y = AND(s1, s2)
    return(y)


def step(x):
    y = x > 0
    return y.astype(np.int)


def sigmoid(x):
    return 1 / (1 + np.exp(-x))


def relu(x):
    return np.maximum(0, x)


def identity(x):
    return x


# def softmax(a):
#     c = np.max(a)
#     exp_a = np.exp(a - c)
#     sum_exp_a = np.sum(exp_a)
#     y = exp_a / sum_exp_a
#     return y


def img_show(img):
    pil_image = Image.fromarray(np.uint8(img))
    pil_image.show()


def get_data():
    (x_train, t_train), (x_test, t_test) = \
        load_mnist(flatten = True, normalize = True, one_hot_label = False)
    return x_test, t_test


def init_network():
    with open("/home/shun/Dropbox/memo/lang/python/DL-from-Scratch/ch03/sample_weight.pkl", "rb") as f:
        network = pickle.load(f)
    return network

def predict(network, x):
    W1, W2, W3 = network["W1"], network["W2"], network["W3"]
    b1, b2, b3 = network["b1"], network["b2"], network["b3"]
    a1 = np.dot(x, W1) + b1
    z1 = sigmoid(a1)
    a2 = np.dot(z1, W2) + b2
    z2 = sigmoid(a2)
    a3 = np.dot(z2, W3) + b3
    y = softmax(a3)
    return y


def mean_squared_error(y, t):
    return 0.5 * np.sum((y - t)**2)


# def cross_entropy_error(y, t):
#     delta = 1e-7
#     return -np.sum(t * np.log(y + delta))


# def cross_entropy_error(y, t):
#     if y.ndim == 1:
#         t = t.reshape(1, t.size)
#         y = y.reshape(1, y.size)
#     batch_size = y.shape[0]
#     return -np.sum(t * np.log(y[np.arrange(batch_size), t] + 1e-7)) / batch_size


def numerical_diff(f, x):
    h = 1e-4
    return (f(x + h) - f(x - h)) / (2 * h)


def function_1(x):
    return 0.01*x**2 + 0.1*x


def function_2(x):
    return x[0]**2 + x[1]**2


# def numerical_gradient(f, x):
#     h = 1e-4
#     grad = np.zeros_like(x)
#     for idx in range(x.size):
#         tmp_val = x[idx]
#         x[idx] = tmp_val + h
#         fxh1= f(x)
#         x[idx] = tmp_val - h
#         fxh2 = f(x)
#         grad[idx] = (fxh1 - fxh2) / (2*h)
#         x[idx] = tmp_val
#     return grad


def gradient_desent(f, init_x, lr = 0.01, step_num = 100):
    x = init_x
    for i in range(step_num):
        grad = numerical_gradient(f, x)
        x -= lr * grad
    return grad




x = np.arange(-5.0, 5.0, 0.1)
y = step(x)
plt.plot(x, y)
plt.show()
plt.close()

y_sigmoid = sigmoid(x)
plt.plot(x, y_sigmoid)
plt.show()


A = np.array([[5, 3], [9, 2]])
B = np.array([[4, 2], [8, 11]])
np.dot(A, B)


X = np.array([1.0, 0.5])
W1 = np.array([[0.1, 0.3, 0.5], [0.2, 0.4, 0.6]])
B1 = np.array([0.1, 0.2, 0.3])
A1 = np.dot(X, W1) + B1
Z1 = sigmoid(A1)

W2 = np.array([[0.1, 0.4], [0.2, 0.5], [0.3, 0.6]])
B2 = np.array([0.1, 0.2])
A2 = np.dot(Z1, W2) + B2
Z2 = sigmoid(A2)

W3 = np.array([[0.1, 0.3], [0.2, 0.4]])
B3 = np.array([0.1, 0.2])
A3 = np.dot(Z2, W3) + B3
Y = identity(A3)


sys.path.append(os.pardir)

os.chdir("/home/shun/Dropbox/memo/lang/python/DL-from-Scratch/")
from dataset.mnist import load_mnist
from PIL import Image

(x_train, t_train), (x_test, t_test) = \
    load_mnist(flatten = True, normalize = False)

(x_train, t_train), (x_test, t_test) = \
    load_mnist(flatten = False, normalize = False)


x, t = get_data()
network = init_network()
accuracy_count = 0

for i in range(len(x)):
    y = predict(network, x[i])
    p = np.argmax(y)
    if p == t[i]:
        accuracy_count += 1

print("Accuracy: " + str(float(accuracy_count) / len(x)))


x, t = get_data()
network = init_network()
batch_size = 100
accuracy_count = 0

for i in range(0, len(x), batch_size):
    x_batch = x[i:i+batch_size]
    y_batch = predict(network, x_batch)
    p = np.argmax(y_batch, axis = 1)
    accuracy_count += np.sum(p == t[i:i+batch_size])

print("Accuracy: " + str(float(accuracy_count) / len(x)))


y = predict(network, x)
p = np.argmax(y, axis = 1)
accuracy_count = np.sum(p == t)
print("Accuracy: " + str(float(accuracy_count) / len(x)))


(x_train, t_train), (x_test, t_test) = \
    load_mnist(normalize = True, one_hot_label = True)

train_size = x_train.shape[0]
batch_size = 10
batch_mask = np.random.choice(train_size, batch_size)
x_batch = x_train[batch_mask]
t_batch = t_train[batch_mask]


from common.functions import softmax, cross_entropy_error
from common.gradient import numerical_gradient

class simpleNet:
    def __init__(self):
        self.W = np.random.randn(2, 3)

    def predict(self, x):
        return np.dot(x, self.W)

    def loss(self, x, t):
        z = self.predict(x)
        y = softmax(z)
        loss = cross_entropy_error(y, t)
        return loss

net = simpleNet()

def f(W):
    return net.loss(x, t)

dW = numerical_gradient(f, net.W)


class twoLayerNet:
    def __init__(self, input_size, hidden_size, output_size, weight_init_std = 0.01):
        self.params = {}
        self.params["W1"] = weight_init_std * np.random.randn(input_size, hidden_size)
        self.params["B1"] = np.zeros(hidden_size)
        self.params["W2"] = weight_init_std * np.random.randn(hidden_size, output_size)
        self.params["B2"] = np.zeros(output_size)

    def predict(self, x):
        W1, W2 = self.params["W1"], self.params["W2"]
        b1, b2 = self.params["b1"], self.params["b2"]
        a1 = np.dot(W1, x) + b1
        z1 = sigmoid(a1)
        a2 = np.dot(W2, z1) + b2
        y = softmax(a2)
        return y

    def loss(self, x, t):
        y = self.predict(x)
        return cross_entropy_error(y, t)

    def accuracy(self, x, t):
        y = self.predict(x)
        y = np.argmax(y, axis = 1)
        t = np.argmax(t, axis = 1)
        accuracy = np.sum(y == t) / float(x.shape[0])
        return accuracy

    def numerical_gradient(self, x, t):
        loss_W = lambda W: self.loss(x, t)
        grads = {}
        grads["W1"] = numerical_gradient(loss_W, self.params["W1"])
        grads["W2"] = numerical_gradient(loss_W, self.params["W2"])
        grads["b1"] = numerical_gradient(loss_W, self.params["b1"])
        grads["b2"] = numerical_gradient(loss_W, self.params["b2"])
        return grads


(x_train, t_train), (x_test, t_test) = \
    load_mnist(normalize = True, one_hot_label = True)

train_loss_init = []
iters_num = 10000
train_size = x_train.shape[0]
batch_size = 100
learning_rat = 0.1

network = twoLayerNet(784, 50, 10)

for i in range(iters_num):
    batch_mask = np.random.choice(train_size, batch_size)
    x_batch = x_train[batch_mask]
    t_batch = t_train[batch_mask]

    grad = network.numerical_gradient(x, t)

    for key in ("W1", "w2"):

