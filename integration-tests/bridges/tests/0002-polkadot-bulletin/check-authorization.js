async function run(nodeName, networkInfo, args) {
    const {wsUri, userDefinedTypes} = networkInfo.nodesByName[nodeName];
    const api = await zombie.connect(wsUri, userDefinedTypes);

    const isAuthorized = args.isAuthorized;
    const account = args.account;
    const authorization_key = { Account: account };
    while (true) {
        console.log(" Checking for authorization for a key: " + JSON.stringify(authorization_key) + " isAuthorized: " + isAuthorized);
        const authorization = await api.query.transactionStorage.authorizations(authorization_key);

        if (isAuthorized && authorization.isSome) {
            const authorizationValue = authorization.unwrap();
            console.log(" Ok - Found authorization for a key: " + JSON.stringify(authorization_key) + " : " + authorizationValue);
            const expectedTransactions = BigInt(args.transactions);
            const actualTransactions = authorizationValue.extent.transactions.toBigInt();
            if (expectedTransactions === actualTransactions) {
                console.log(" Ok - expected transaction count matched: " + expectedTransactions);
                return true
            } else {
                throw new Error("Invalid authorized transactions count! expectedTransactions: " + expectedTransactions + " actual: " + actualTransactions);
            }
        }

        if (!isAuthorized && authorization.isNone) {
            console.log(" Ok - Authorization not found for a key: " + JSON.stringify(authorization_key));
            return true;
        }

        // else sleep and retry
        await new Promise((resolve) => setTimeout(resolve, 6000));
    }
}

module.exports = { run }
