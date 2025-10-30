# --- 3. 函數 (Question 4): 必須放在檔案最前面 ---
def fact(n):
    if n <= 1:
        return 1
    # 測試乘法和遞迴
    return n * fact(n - 1)

def do_nothing():
    y = 1
# ----------------------------------------------------

# --- 1. 算術與變數 (Question 1 & 3) ---
x = 41
x = x + 1
print(x)                  # 預期輸出: 42

# --- 2. 布林與條件判斷 (Question 2) ---
print(1 < 2)              # 預期輸出: True
b = x < 100 and x > 40
print(b)                  # 預期輸出: True

if False or True:
    print("ok")           # 預期輸出: ok
else:
    print("oups")

# 測試 Truthiness: 空列表為 False
if []:
    print("list not empty")
else:
    print("list empty")   # 預期輸出: list empty

# --- 3. 函數呼叫 (必須在定義之後) ---
print(fact(5))            # 預期輸出: 120
print(do_nothing())       # 預期輸出: None

# --- 4. 字串與列表 (Question 3 & 5) ---
s = "hello" + " world!"
print(s)                  # 預期輸出: hello world!

# 列表創建、內建函數 list(range) 和 len
a = list(range(3))        # [0, 1, 2]
b = [10, 20]
c = a + b                 # 列表拼接 [0, 1, 2, 10, 20]
print(c)                  # 預期輸出: [0, 1, 2, 10, 20]
print(len(c))             # 預期輸出: 5

# 列表元素存取與賦值
c[0] = 99
print(c[0])               # 預期輸出: 99
# c: [99, 1, 2, 10, 20]

# for 迴圈
total = 0
for val in c:
    total = total + val
print(total)              # 預期輸出: 132

# --- 5. 結構化比較 (Question 7 Bonus) ---
# 測試列表的字典序比較
print([1, 2, 3] == [1, 2, 3])  # 預期輸出: True
print([1, 2, 3] < [1, 3, 0])   # 預期輸出: True