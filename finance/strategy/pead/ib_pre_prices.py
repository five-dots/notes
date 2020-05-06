from ib_insync import *

ib = IB()
ib.connect("127.0.0.1", 7469, clientId=10)


contracts = [Forex(pair) for pair in ("EURUSD", "USDJPY")]
ib.qualifyContracts(*contracts)

eurusd = contracts[0]

for contract in contracts:
    ib.reqMktData(contract, "", False, False)

ticker = ib.ticker(eurusd)
ticker
ticker.marketPrice()
