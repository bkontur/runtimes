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

use crate::{
	xcm_config::{GovernanceLocation, LocationToAccountId},
	Block, Runtime, RuntimeCall, RuntimeOrigin, WeightToFee,
	bridge_common_config::{
		BridgeGrandpaPolkadotBulletinInstance, BridgeRelayersInstance,
	},
	bridge_to_bulletin_config::WithPolkadotBulletinMessagesInstance,
	bridge_to_bulletin_config::{
		PolkadotBulletinGlobalConsensusNetworkLocation,
		XcmOverPolkadotBulletinInstance,
	},
	xcm_config::{LocationToAccountId, XcmConfig},
	AllPalletsWithoutSystem, ExistentialDeposit, ParachainSystem, PolkadotXcm, Runtime,
	RuntimeEvent, RuntimeOrigin, SessionKeys, SLOT_DURATION,
};
use bridge_hub_test_utils::{
	test_cases::from_grandpa_chain,
	SlotDurations,
};
use bp_messages::LegacyLaneId;
use codec::Decode;
use frame_support::parameter_types;
use frame_support::traits::ConstU8;
use parachains_common::AccountId;
use sp_core::crypto::Ss58Codec;
use sp_core::sr25519;
use system_parachains_constants::polkadot::consensus::RELAY_CHAIN_SLOT_DURATION_MILLIS;
use xcm::latest::prelude::*;
use xcm_runtime_apis::conversions::LocationToAccountHelper;

use frame_support::{assert_err, assert_ok};
use parachains_runtimes_test_utils::GovernanceOrigin;
use sp_runtime::Either;

const ALICE: [u8; 32] = [1u8; 32];

// Para id of sibling chain used in tests.
pub const SIBLING_PARACHAIN_ID: u32 = 1000;

parameter_types! {
	pub SiblingParachainLocation: Location = Location::new(1, [Parachain(SIBLING_PARACHAIN_ID)]);
	pub BridgedUniversalLocation: InteriorLocation = [GlobalConsensus(bp_polkadot_bulletin::PolkadotBulletinGlobalConsensusNetwork::get())].into();
	pub TestNetworkId: NetworkId = NetworkId::Polkadot;
}

fn collator_session_keys() -> bridge_hub_test_utils::CollatorSessionKeys<Runtime> {
	let pubkey = sr25519::Public::from_raw(ALICE);
	bridge_hub_test_utils::CollatorSessionKeys::new(
		AccountId::from(ALICE),
		AccountId::from(ALICE),
		SessionKeys { aura: pubkey.into() },
	)
}

fn slot_durations() -> SlotDurations {
	SlotDurations {
		relay: SlotDuration::from_millis(RELAY_CHAIN_SLOT_DURATION_MILLIS.into()),
		para: SlotDuration::from_millis(SLOT_DURATION),
	}
}

#[test]
fn handle_export_message_from_system_parachain_add_to_outbound_queue_works() {
	bridge_hub_test_utils::test_cases::handle_export_message_from_system_parachain_to_outbound_queue_works::<
		Runtime,
		XcmConfig,
		WithPolkadotBulletinMessagesInstance,
	>(
		collator_session_keys(),
		polkadot_runtime_constants::system_parachain::PEOPLE_ID,
		SIBLING_PARACHAIN_ID,
		Box::new(|runtime_event_encoded: Vec<u8>| {
			match RuntimeEvent::decode(&mut &runtime_event_encoded[..]) {
				Ok(RuntimeEvent::BridgePolkadotBulletinMessages(event)) => Some(event),
				_ => None,
			}
		}),
		|| ExportMessage { network: bp_polkadot_bulletin::PolkadotBulletinGlobalConsensusNetwork::get(), destination: Here.into(), xcm: Xcm(vec![]) },
		Some((Location::parent(), ExistentialDeposit::get()).into()),
		Some((Location::parent(), 1_000_000_000).into()),
		|| {
			PolkadotXcm::force_xcm_version(RuntimeOrigin::root(), Box::new(PolkadotBulletinGlobalConsensusNetworkLocation::get()), XCM_VERSION).expect("version saved!");

			bridge_hub_test_utils::ensure_opened_bridge::<
				Runtime,
				XcmOverPolkadotBulletinInstance,
				LocationToAccountId,
				SiblingParachainLocation,
			>(
				SiblingParachainLocation::get(),
				BridgedUniversalLocation::get(),
				false,
				|locations, _fee| {
					bridge_hub_test_utils::open_bridge_with_storage::<
						Runtime,
						XcmOverPolkadotBulletinInstance
					>(locations, LegacyLaneId([0, 0, 0, 1]))
				}
			).1
		},
	)
}

#[test]
fn message_dispatch_routing_works() {
	bridge_hub_test_utils::test_cases::message_dispatch_routing_works::<
		Runtime,
		AllPalletsWithoutSystem,
		XcmConfig,
		ParachainSystem,
		WithPolkadotBulletinMessagesInstance,
		TestNetworkId,
		bp_polkadot_bulletin::PolkadotBulletinGlobalConsensusNetwork,
		ConstU8<2>,
	>(
		collator_session_keys(),
		slot_durations(),
		polkadot_runtime_constants::system_parachain::PEOPLE_ID,
		SIBLING_PARACHAIN_ID,
		Box::new(|runtime_event_encoded: Vec<u8>| {
			match RuntimeEvent::decode(&mut &runtime_event_encoded[..]) {
				Ok(RuntimeEvent::ParachainSystem(event)) => Some(event),
				_ => None,
			}
		}),
		Box::new(|runtime_event_encoded: Vec<u8>| {
			match RuntimeEvent::decode(&mut &runtime_event_encoded[..]) {
				Ok(RuntimeEvent::XcmpQueue(event)) => Some(event),
				_ => None,
			}
		}),
		|| (),
	)
}

#[test]
fn location_conversion_works() {
	let alice_32 = AccountId32 { network: None, id: AccountId::from(ALICE).into() };
	let bob_20 = AccountKey20 { network: None, key: [123u8; 20] };

	// the purpose of hardcoded values is to catch an unintended location conversion logic change.
	struct TestCase {
		description: &'static str,
		location: Location,
		expected_account_id_str: &'static str,
	}

	let test_cases = vec![
		// DescribeTerminus
		TestCase {
			description: "DescribeTerminus Parent",
			location: Location::new(1, Here),
			expected_account_id_str: "5Dt6dpkWPwLaH4BBCKJwjiWrFVAGyYk3tLUabvyn4v7KtESG",
		},
		TestCase {
			description: "DescribeTerminus Sibling",
			location: Location::new(1, [Parachain(1111)]),
			expected_account_id_str: "5Eg2fnssmmJnF3z1iZ1NouAuzciDaaDQH7qURAy3w15jULDk",
		},
		// DescribePalletTerminal
		TestCase {
			description: "DescribePalletTerminal Parent",
			location: Location::new(1, [PalletInstance(50)]),
			expected_account_id_str: "5CnwemvaAXkWFVwibiCvf2EjqwiqBi29S5cLLydZLEaEw6jZ",
		},
		TestCase {
			description: "DescribePalletTerminal Sibling",
			location: Location::new(1, [Parachain(1111), PalletInstance(50)]),
			expected_account_id_str: "5GFBgPjpEQPdaxEnFirUoa51u5erVx84twYxJVuBRAT2UP2g",
		},
		// DescribeAccountId32Terminal
		TestCase {
			description: "DescribeAccountId32Terminal Parent",
			location: Location::new(1, [alice_32]),
			expected_account_id_str: "5DN5SGsuUG7PAqFL47J9meViwdnk9AdeSWKFkcHC45hEzVz4",
		},
		TestCase {
			description: "DescribeAccountId32Terminal Sibling",
			location: Location::new(1, [Parachain(1111), alice_32]),
			expected_account_id_str: "5DGRXLYwWGce7wvm14vX1Ms4Vf118FSWQbJkyQigY2pfm6bg",
		},
		// DescribeAccountKey20Terminal
		TestCase {
			description: "DescribeAccountKey20Terminal Parent",
			location: Location::new(1, [bob_20]),
			expected_account_id_str: "5CJeW9bdeos6EmaEofTUiNrvyVobMBfWbdQvhTe6UciGjH2n",
		},
		TestCase {
			description: "DescribeAccountKey20Terminal Sibling",
			location: Location::new(1, [Parachain(1111), bob_20]),
			expected_account_id_str: "5CE6V5AKH8H4rg2aq5KMbvaVUDMumHKVPPQEEDMHPy3GmJQp",
		},
		// DescribeTreasuryVoiceTerminal
		TestCase {
			description: "DescribeTreasuryVoiceTerminal Parent",
			location: Location::new(1, [Plurality { id: BodyId::Treasury, part: BodyPart::Voice }]),
			expected_account_id_str: "5CUjnE2vgcUCuhxPwFoQ5r7p1DkhujgvMNDHaF2bLqRp4D5F",
		},
		TestCase {
			description: "DescribeTreasuryVoiceTerminal Sibling",
			location: Location::new(
				1,
				[Parachain(1111), Plurality { id: BodyId::Treasury, part: BodyPart::Voice }],
			),
			expected_account_id_str: "5G6TDwaVgbWmhqRUKjBhRRnH4ry9L9cjRymUEmiRsLbSE4gB",
		},
		// DescribeBodyTerminal
		TestCase {
			description: "DescribeBodyTerminal Parent",
			location: Location::new(1, [Plurality { id: BodyId::Unit, part: BodyPart::Voice }]),
			expected_account_id_str: "5EBRMTBkDisEXsaN283SRbzx9Xf2PXwUxxFCJohSGo4jYe6B",
		},
		TestCase {
			description: "DescribeBodyTerminal Sibling",
			location: Location::new(
				1,
				[Parachain(1111), Plurality { id: BodyId::Unit, part: BodyPart::Voice }],
			),
			expected_account_id_str: "5DBoExvojy8tYnHgLL97phNH975CyT45PWTZEeGoBZfAyRMH",
		},
	];

	for tc in test_cases {
		let expected =
			AccountId::from_string(tc.expected_account_id_str).expect("Invalid AccountId string");

		let got = LocationToAccountHelper::<AccountId, LocationToAccountId>::convert_location(
			tc.location.into(),
		)
		.unwrap();

		assert_eq!(got, expected, "{}", tc.description);
	}
}

#[test]
fn xcm_payment_api_works() {
	parachains_runtimes_test_utils::test_cases::xcm_payment_api_with_native_token_works::<
		Runtime,
		RuntimeCall,
		RuntimeOrigin,
		Block,
		WeightToFee,
	>();
}

#[test]
fn governance_authorize_upgrade_works() {
	use polkadot_runtime_constants::system_parachain::{ASSET_HUB_ID, COLLECTIVES_ID};

	// no - random para
	assert_err!(
		parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
			Runtime,
			RuntimeOrigin,
		>(GovernanceOrigin::Location(Location::new(1, Parachain(12334)))),
		Either::Right(InstructionError { index: 0, error: XcmError::Barrier })
	);
	// no - AssetHub
	assert_err!(
		parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
			Runtime,
			RuntimeOrigin,
		>(GovernanceOrigin::Location(Location::new(1, Parachain(ASSET_HUB_ID)))),
		Either::Right(InstructionError { index: 0, error: XcmError::Barrier })
	);
	// no - Collectives
	assert_err!(
		parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
			Runtime,
			RuntimeOrigin,
		>(GovernanceOrigin::Location(Location::new(1, Parachain(COLLECTIVES_ID)))),
		Either::Right(InstructionError { index: 0, error: XcmError::Barrier })
	);
	// no - Collectives Voice of Fellows plurality
	assert_err!(
		parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
			Runtime,
			RuntimeOrigin,
		>(GovernanceOrigin::LocationAndDescendOrigin(
			Location::new(1, Parachain(COLLECTIVES_ID)),
			Plurality { id: BodyId::Technical, part: BodyPart::Voice }.into()
		)),
		Either::Right(InstructionError { index: 2, error: XcmError::BadOrigin })
	);

	// ok - relaychain
	assert_ok!(parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
		Runtime,
		RuntimeOrigin,
	>(GovernanceOrigin::Location(Location::parent())));
	assert_ok!(parachains_runtimes_test_utils::test_cases::can_governance_authorize_upgrade::<
		Runtime,
		RuntimeOrigin,
	>(GovernanceOrigin::Location(GovernanceLocation::get())));
}

type GrandpaRuntimeTestsAdapter = from_grandpa_chain::WithRemoteGrandpaChainHelperAdapter<
	Runtime,
	AllPalletsWithoutSystem,
	BridgeGrandpaPolkadotBulletinInstance,
	WithPolkadotBulletinMessagesInstance,
	BridgeRelayersInstance,
>;

#[test]
fn relayed_incoming_message_works() {
	from_grandpa_chain::relayed_incoming_message_works::<GrandpaRuntimeTestsAdapter>(
		collator_session_keys(),
		slot_durations(),
		polkadot_runtime_constants::system_parachain::PEOPLE_ID,
		0, // Bulletin relay chain id
		NetworkId::PolkadotBulletin,
		|| {
			bridge_hub_test_utils::ensure_opened_bridge::<
				Runtime,
				XcmOverPolkadotBulletinInstance,
				LocationToAccountId,
				SiblingParachainLocation,
			>(
				SiblingParachainLocation::get(),
				BridgedUniversalLocation::get(),
				false,
				|locations, _fee| {
					bridge_hub_test_utils::open_bridge_with_storage::<
						Runtime,
						XcmOverPolkadotBulletinInstance,
					>(locations, LegacyLaneId([0, 0, 0, 1]))
				},
			)
			.1
		},
		|_relayer_at_target, _call| Ok(()),
		true,
	);
}

#[test]
fn free_relay_extrinsic_works() {
	from_grandpa_chain::free_relay_extrinsic_works::<GrandpaRuntimeTestsAdapter>(
		collator_session_keys(),
		slot_durations(),
		polkadot_runtime_constants::system_parachain::PEOPLE_ID,
		0, // Bulletin relay chain id
		NetworkId::PolkadotBulletin,
		|| {
			// Initialize bridge state for GRANDPA chain tests
			bridge_hub_test_utils::ensure_opened_bridge::<
				Runtime,
				XcmOverPolkadotBulletinInstance,
				LocationToAccountId,
				SiblingParachainLocation,
			>(
				SiblingParachainLocation::get(),
				BridgedUniversalLocation::get(),
				false,
				|locations, _fee| {
					bridge_hub_test_utils::open_bridge_with_storage::<
						Runtime,
						XcmOverPolkadotBulletinInstance,
					>(locations, LegacyLaneId([0, 0, 0, 1]))
				},
			)
			.1
		},
		|_relayer_at_target, _call| Ok(()),
		true,
	);
}
