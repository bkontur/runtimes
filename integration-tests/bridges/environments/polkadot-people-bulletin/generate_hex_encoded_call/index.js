const fs = require("fs");
const { exit } = require("process");
const { WsProvider, ApiPromise } = require("@polkadot/api");
const util = require("@polkadot/util");

// connect to a substrate chain and return the api object
async function connect(endpoint, types = {}) {
	const provider = new WsProvider(endpoint);
	const api = await ApiPromise.create({
		provider,
		types,
		throwOnConnect: false,
	});
	return api;
}

function writeHexEncodedBytesToOutput(method, outputFile) {
	console.log("Payload (hex): ", method.toHex());
	console.log("Payload (bytes): ", Array.from(method.toU8a()));
	console.log("Payload (plain): ", JSON.stringify(method));
	fs.writeFileSync(outputFile, JSON.stringify(Array.from(method.toU8a())));
}

function addBulletinValidator(endpoint, outputFile, validator_id) {
	console.log(`Generating addBulletinValidator from RPC endpoint: ${endpoint} to outputFile: ${outputFile}, validator_id: ${validator_id}`);
	connect(endpoint)
		.then((api) => {
			const call = api.tx.validatorSet.addValidator(validator_id);
			writeHexEncodedBytesToOutput(call.method, outputFile);
			exit(0);
		})
		.catch((e) => {
			console.error(e);
			exit(1);
		});
}

function peopleXcmSendToBulletin(endpoint, outputFile, bulletin_xcm_call_hex) {
	console.log(`Generating peopleXcmSendToBulletin from RPC endpoint: ${endpoint} to outputFile: ${outputFile}, bulletin_xcm_call_hex: ${bulletin_xcm_call_hex}`);
	connect(endpoint)
		.then((api) => {
			// Destination: Location::new(2, [GlobalConsensus(PolkadotBulletin)])
			const dest = {
				"V5": {
					"parents": 2,
					"interior": {
						"X1": [
							{ "GlobalConsensus": "PolkadotBulletin" }
						]
					}
				}
			};

			// XCM message to Bulletin chain: UnpaidExecution + Transact with ValidatorSet::add
			const xcm_message = {
				"V5": [
					{
						"UnpaidExecution": {
							"weight_limit": "Unlimited"
						}
					},
					{
						"Transact": {
							"origin_kind": "Superuser",
							"call": {
								"encoded": JSON.parse(bulletin_xcm_call_hex)
							}
						}
					}
				]
			};

			const call = api.tx.polkadotXcm.send(dest, xcm_message);
			writeHexEncodedBytesToOutput(call.method, outputFile);
			exit(0);
		})
		.catch((e) => {
			console.error(e);
			exit(1);
		});
}

function bulletinTransactionStorageAuthorizeAccount(endpoint, outputFile, who, transactions, bytes) {
	console.log(`Generating bulletinTransactionStorageAuthorizeAccount from RPC endpoint: ${endpoint} to outputFile: ${outputFile}, who: ${who}, transactions: ${transactions}, bytes: ${bytes}`);
	connect(endpoint)
		.then((api) => {
			const call = api.tx.transactionStorage.authorizeAccount(who, transactions, bytes);
			writeHexEncodedBytesToOutput(call.method, outputFile);
			exit(0);
		})
		.catch((e) => {
			console.error(e);
			exit(1);
		});
}

if (!process.argv[2] || !process.argv[3]) {
	console.log("usage: node ./script/generate_hex_encoded_call <type> <endpoint> <output hex-encoded data file> <input message>");
	exit(1);
}

const type = process.argv[2];
const rpcEndpoint = process.argv[3];
const output = process.argv[4];
const inputArgs = process.argv.slice(5, process.argv.length);
console.log(`Generating hex-encoded call data for:`);
console.log(`	type: ${type}`);
console.log(`	rpcEndpoint: ${rpcEndpoint}`);
console.log(`	output: ${output}`);
console.log(`	inputArgs: ${inputArgs}`);

switch (type) {
	case 'add-bulletin-validator':
		addBulletinValidator(rpcEndpoint, output, inputArgs[0]);
		break;
	case 'people-xcm-send-to-bulletin':
		peopleXcmSendToBulletin(rpcEndpoint, output, inputArgs[0]);
		break;
	case 'bulletin-transaction-storage-authorize-account':
		bulletinTransactionStorageAuthorizeAccount(rpcEndpoint, output, inputArgs[0], inputArgs[1], inputArgs[2]);
		break;
	case 'check':
		console.log(`Checking nodejs installation, if you see this everything is ready!`);
		break;
	default:
		console.log(`Sorry, we are out of ${type} - not yet supported!`);
}
