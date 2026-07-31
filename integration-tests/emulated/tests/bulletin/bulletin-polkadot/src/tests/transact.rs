// Copyright (C) Parity Technologies and the various Polkadot contributors, see Contributions.md
// for a list of specific contributors.
// SPDX-License-Identifier: Apache-2.0

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! XCM `Transact` policy tests: storage-mutating calls must not be reachable via XCM from
//! other chains.

use crate::*;
use emulated_integration_tests_common::{
	impls::{assert_expected_events, bx, Encode},
	macros::{pallet_message_queue, pallet_xcm, Dispatchable},
};
use pallet_bulletin_transaction_storage::AuthorizationExtent;

/// An XCM `Transact` carrying `transaction_storage::store`, sent from Asset Hub over XCMP,
/// must be rejected on Bulletin.
///
/// The message passes the barrier (sibling system parachains get unpaid execution) and fails
/// inside the `Transact` instruction: the `SafeCallFilter`
/// (`EverythingBut<StorageCallInspector>`) rejects the decoded call with
/// `XcmError::NoPermission` before origin conversion and dispatch. The executor emits
/// `PolkadotXcm::ProcessXcmError` and the message queue reports the message as processed
/// unsuccessfully.
#[test]
fn xcm_transact_store_from_asset_hub_is_blocked() {
	let data = vec![42u8; 100];

	// The account `store` would be dispatched as (via `OriginKind::SovereignAccount`) if the
	// filter let the call through.
	let ah_sovereign: AccountId = BulletinPolkadot::sovereign_account_id_of(
		BulletinPolkadot::sibling_location_of(AssetHubPolkadot::para_id()),
	);

	// Authorize the sovereign account so the call filter is the only blocker.
	let granted_extent = BulletinPolkadot::execute_with(|| {
		type BulletinRuntime = <BulletinPolkadot as Chain>::Runtime;
		type TransactionStorage = pallet_bulletin_transaction_storage::Pallet<BulletinRuntime>;

		assert_ok!(TransactionStorage::authorize_account(
			<BulletinPolkadot as Chain>::RuntimeOrigin::root(),
			ah_sovereign.clone(),
			1,
			data.len() as u64,
		));
		let extent = TransactionStorage::account_authorization_extent(ah_sovereign.clone());
		assert_ne!(extent, AuthorizationExtent::default(), "authorization must have been granted");
		extent
	});

	// Encode the `store` call for the Bulletin runtime.
	let store_call = {
		type BulletinRuntime = <BulletinPolkadot as Chain>::Runtime;
		type BulletinRuntimeCall = <BulletinPolkadot as Chain>::RuntimeCall;
		BulletinRuntimeCall::TransactionStorage(pallet_bulletin_transaction_storage::Call::<
			BulletinRuntime,
		>::store {
			data: data.clone(),
		})
		.encode()
	};

	// Have Asset Hub send the XCM to the Bulletin chain. Root sends from `Here`, so the message
	// arrives on Bulletin with the plain sibling-parachain origin and passes the
	// unpaid-execution barrier.
	AssetHubPolkadot::execute_with(|| {
		type Runtime = <AssetHubPolkadot as Chain>::Runtime;
		type RuntimeCall = <AssetHubPolkadot as Chain>::RuntimeCall;
		type RuntimeEvent = <AssetHubPolkadot as Chain>::RuntimeEvent;

		let send_xcm = RuntimeCall::PolkadotXcm(pallet_xcm::Call::<Runtime>::send {
			dest: bx!(VersionedLocation::from(AssetHubPolkadot::sibling_location_of(
				BulletinPolkadot::para_id()
			))),
			message: bx!(VersionedXcm::from(Xcm(vec![
				UnpaidExecution { weight_limit: Unlimited, check_origin: None },
				Transact {
					origin_kind: OriginKind::SovereignAccount,
					fallback_max_weight: None,
					call: store_call.into(),
				},
			]))),
		});

		assert_ok!(send_xcm.dispatch(<AssetHubPolkadot as Chain>::RuntimeOrigin::root()));

		assert_expected_events!(
			AssetHubPolkadot,
			vec![
				RuntimeEvent::PolkadotXcm(pallet_xcm::Event::Sent { .. }) => {},
			]
		);
	});

	BulletinPolkadot::execute_with(|| {
		type RuntimeEvent = <BulletinPolkadot as Chain>::RuntimeEvent;

		assert_expected_events!(
			BulletinPolkadot,
			vec![
				RuntimeEvent::PolkadotXcm(pallet_xcm::Event::ProcessXcmError { error, .. }) => {
					error: *error == XcmError::NoPermission,
				},
				RuntimeEvent::MessageQueue(
					pallet_message_queue::Event::Processed { success: false, .. }
				) => {},
			]
		);

		// Nothing was stored and the authorization was not consumed.
		assert!(
			!BulletinPolkadot::events().iter().any(|event| matches!(
				event,
				RuntimeEvent::TransactionStorage(
					pallet_bulletin_transaction_storage::Event::Stored { .. }
				)
			)),
			"no data must be stored on Bulletin"
		);

		type BulletinRuntime = <BulletinPolkadot as Chain>::Runtime;
		assert_eq!(
			pallet_bulletin_transaction_storage::Pallet::<BulletinRuntime>::account_authorization_extent(
				ah_sovereign
			),
			granted_extent,
		);
	});
}
