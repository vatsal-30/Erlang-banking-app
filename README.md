# Concurrent Banking Simulator — Erlang

## 📌 Description
A concurrent banking simulator in Erlang where customers request loans from banks using message passing, with a master process logging transactions and generating a final report.

---

## ✨ Features
- **Actor-style concurrency**: one process per customer, per bank, and one master.
- **Message passing only**: no shared state; customers and banks never print directly.
- **Randomized customer behavior**:
  - Waits 10–100 ms between loan requests.
  - Requests up to 50 dollars (or remaining needed).
  - Randomly chooses banks.
- **Bank logic**: approves if funds remain ≥ 0, otherwise denies.
- **Master-only logging**: prints all requests, approvals, denials, and final summary.
- **Final report**: validates that total bank loans equal total customer receipts.

---

## ▶️ How to Run

### 1) Compile
```bash
cd src
erlc money.erl customer.erl bank.erl
```

### 2) Run with input files
```bash
erl -noshell -run money start ../sample-data/c1.txt ../sample-data/b1.txt -s 
```
 - First argument → customer file (e.g., c1.txt)
 - Second argument → bank file (e.g., b1.txt)

### 3) Input format (Erlang tuples)
Customers (c1.txt)
```bash
{jill,450}.
{joe,157}.
{bob,100}.
```
Banks (b1.txt)
```bash
{rbc,800}.
{bmo,700}.
{ing,200}.
```
## 📊 Output
During execution, the master prints transaction logs in real time:
```bash
? jill requests a loan of 45 dollar(s) from rbc
$ rbc approves a loan of 45 dollar(s) to jill
? joe requests a loan of 30 dollar(s) from ing
$ ing denies a loan of 30 dollar(s) to joe
? bob requests a loan of 20 dollar(s) from bmo
$ bmo approves a loan of 20 dollar(s) to bob
```
## 📊 Output
At the end, a final report is displayed:
```bash
---- Banking Report ----
Customer Summary
jill: objective=450, received=450
joe: objective=157, received=120
bob: objective=100, received=90
Totals: objective=707, received=660

Bank Summary
rbc: initial=800, remaining=500
bmo: initial=700, remaining=650
ing: initial=200, remaining=190
Totals: initial=1700, remaining=1340
```
