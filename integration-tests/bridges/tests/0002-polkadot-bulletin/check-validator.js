async function run(nodeName, networkInfo, args) {
    const {wsUri, userDefinedTypes} = networkInfo.nodesByName[nodeName];
    const api = await zombie.connect(wsUri, userDefinedTypes);

    const isPresent = args.isPresent;
    const validatorAddress = args.validatorAddress;
    while (true) {
        console.log(" Checking for ValidatorSet containing(isPresent=" + isPresent + "): address: " + validatorAddress);
        const validator = await api.query.validatorSet.validators(validatorAddress);
        if (isPresent && validator.isSome) {
            console.log(" Ok - Found validator: " + JSON.stringify(validator));
            return true;
        }
        if (!isPresent && validator.isNone) {
            console.log(" Ok - Validator not found: " + validatorAddress);
            return true;
        }

        // else sleep and retry
        await new Promise((resolve) => setTimeout(resolve, 6000));
    }
}

module.exports = { run }
