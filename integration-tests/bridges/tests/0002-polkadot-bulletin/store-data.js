async function run(nodeName, networkInfo, args) {
    const {wsUri, userDefinedTypes} = networkInfo.nodesByName[nodeName];
    const api = await zombie.connect(wsUri, userDefinedTypes);

    await zombie.util.cryptoWaitReady();

    // account to submit tx
    const authorizedAccountSeed = args.authorizedAccountSeed;
    const data = args.data;
    const keyring = new zombie.Keyring({ type: "sr25519" });
    const authorizedAccount = keyring.addFromUri(authorizedAccountSeed);

    await new Promise(async (resolve, reject) => {
        const unsub = await api.tx.transactionStorage.store(data)
            .signAndSend(authorizedAccount, ({ status, isError }) => {
                if (status.isInBlock) {
                    console.log(
                        `Transaction included at blockhash ${status.asInBlock}`,
                    );
                } else if (status.isFinalized) {
                    console.log(
                        `Transaction finalized at blockHash ${status.asFinalized}`,
                    );
                    unsub();
                    return resolve();
                } else if (isError) {
                    console.log(`Transaction error`);
                    reject(`Transaction error`);
                }
            });
    });

    return 0;
}

module.exports = { run };
