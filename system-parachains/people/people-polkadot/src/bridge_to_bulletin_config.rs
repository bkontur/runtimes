// Copyright (C) Parity Technologies (UK) Ltd.
// This file is part of Cumulus.
// SPDX-License-Identifier: Apache-2.0

// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 	http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Bridge definitions used on PeopleHubPolkadot for bridging to Polkadot Bulletin.
//!
//! Polkadot Bulletin chain will be the 1:1 copy of the Polkadot Bulletin, so we
//! are reusing Polkadot Bulletin chain primitives everywhere here.

use crate::{
	bridge_common_config::RelayersForLegacyLaneIdsMessagesInstance,
	xcm_config::UniversalLocation, AccountId, Balance, Balances, BridgePolkadotBulletinGrandpa,
	BridgePolkadotBulletinMessages, Runtime, RuntimeEvent, RuntimeHoldReason, XcmOverPolkadotBulletin,
	XcmRouter,
};
use bp_messages::{
	source_chain::FromBridgedChainMessagesDeliveryProof, target_chain::FromBridgedChainMessagesProof, HashedLaneId,
};

use frame_support::{
	parameter_types,
	traits::{Equals, PalletInfoAccess},
};
use frame_system::{EnsureNever, EnsureRoot};
use pallet_bridge_messages::LaneIdOf;
use pallet_bridge_relayers::extension::{
	BridgeRelayersTransactionExtension, WithMessagesExtensionConfig,
};
use pallet_xcm_bridge_hub::XcmAsPlainPayload;
use polkadot_parachain_primitives::primitives::Sibling;
use system_parachains_constants::polkadot::currency::UNITS as ROC;
use xcm::{
	latest::prelude::*,
	prelude::{InteriorLocation, NetworkId},
	AlwaysV5,
};
use xcm_builder::{BridgeBlobDispatcher, ParentIsPreset, SiblingParachainConvertsVia};

parameter_types! {
	/// Interior location (relative to this runtime) of the with-PolkadotBulletin messages pallet.
	pub BridgePolkadotToPolkadotBulletinMessagesPalletInstance: InteriorLocation = [
		PalletInstance(<BridgePolkadotBulletinMessages as PalletInfoAccess>::index() as u8)
	].into();
	/// Polkadot Bulletin Network identifier.
	pub PolkadotBulletinGlobalConsensusNetwork: NetworkId = NetworkId::PolkadotBulletin;
	/// Relative location of the Polkadot Bulletin chain.
	pub PolkadotBulletinGlobalConsensusNetworkLocation: Location = Location::new(
		2,
		[GlobalConsensus(PolkadotBulletinGlobalConsensusNetwork::get())]
	);

	// see the `FEE_BOOST_PER_RELAY_HEADER` constant get the meaning of this value
	pub PriorityBoostPerRelayHeader: u64 = 58_014_163_614_163;

	/// Priority boost that the registered relayer receives for every additional message in the message
	/// delivery transaction.
	///
	/// It is determined semi-automatically - see `FEE_BOOST_PER_MESSAGE` constant to get the
	/// meaning of this value.
	pub PriorityBoostPerMessage: u64 = 364_088_888_888_888;

	/// PeoplePolkadot location
	pub PeoplePolkadotLocation: Location = Location::new(1, [Parachain(polkadot_runtime_constants::system_parachain::PEOPLE_ID)]);

	pub storage BridgeDeposit: Balance = 5 * ROC;
}

/// Proof of messages, coming from Polkadot Bulletin chain.
pub type FromPolkadotBulletinMessagesProof<MI> =
	FromBridgedChainMessagesProof<bp_polkadot_bulletin::Hash, LaneIdOf<Runtime, MI>>;
/// Messages delivery proof for Polkadot Bridge Hub -> Polkadot Bulletin messages.
pub type ToPolkadotBulletinMessagesDeliveryProof<MI> =
	FromBridgedChainMessagesDeliveryProof<bp_polkadot_bulletin::Hash, LaneIdOf<Runtime, MI>>;

/// Dispatches received XCM messages from other bridge.
type FromPolkadotBulletinMessageBlobDispatcher = BridgeBlobDispatcher<
	XcmRouter,
	UniversalLocation,
	BridgePolkadotToPolkadotBulletinMessagesPalletInstance,
>;

/// Transaction extension that refunds relayers that are delivering messages from the Polkadot
/// Bulletin chain.
pub type OnPeopleHubPolkadotRefundPolkadotBulletinMessages = BridgeRelayersTransactionExtension<
	Runtime,
	WithMessagesExtensionConfig<
		StrOnPeopleHubPolkadotRefundPolkadotBulletinMessages,
		Runtime,
		WithPolkadotBulletinMessagesInstance,
		RelayersForLegacyLaneIdsMessagesInstance,
		PriorityBoostPerMessage,
	>,
	bp_messages::LegacyLaneId,
>;
bp_runtime::generate_static_str_provider!(OnPeopleHubPolkadotRefundPolkadotBulletinMessages);

/// Add XCM messages support for PeopleHubPolkadot to support Polkadot->Polkadot Bulletin XCM messages.
pub type WithPolkadotBulletinMessagesInstance = pallet_bridge_messages::Instance1;
impl pallet_bridge_messages::Config<WithPolkadotBulletinMessagesInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type WeightInfo =
		();

	type ThisChain = bp_people_hub_polkadot::PeopleHubPolkadot;
	type BridgedChain = bp_polkadot_bulletin::PolkadotBulletin;
	type BridgedHeaderChain = BridgePolkadotBulletinGrandpa;

	type OutboundPayload = XcmAsPlainPayload;
	type InboundPayload = XcmAsPlainPayload;
	type LaneId = HashedLaneId;

	type DeliveryPayments = ();
	type DeliveryConfirmationPayments = ();

	type MessageDispatch = XcmOverPolkadotBulletin;
	type OnMessagesDelivered = XcmOverPolkadotBulletin;
}

/// Add support for the export and dispatch of XCM programs.
pub type XcmOverPolkadotBulletinInstance = pallet_xcm_bridge_hub::Instance1;
impl pallet_xcm_bridge_hub::Config<XcmOverPolkadotBulletinInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;

	type UniversalLocation = UniversalLocation;
	type BridgedNetwork = PolkadotBulletinGlobalConsensusNetworkLocation;
	type BridgeMessagesPalletInstance = WithPolkadotBulletinMessagesInstance;

	type MessageExportPrice = ();
	type DestinationVersion = AlwaysV5;

	type ForceOrigin = EnsureRoot<AccountId>;
	// We don't want to allow creating bridges for this instance.
	type OpenBridgeOrigin = EnsureNever<Location>;
	// Converter aligned with `OpenBridgeOrigin`.
	type BridgeOriginAccountIdConverter =
		(ParentIsPreset<AccountId>, SiblingParachainConvertsVia<Sibling, AccountId>);

	type BridgeDeposit = BridgeDeposit;
	type Currency = Balances;
	type RuntimeHoldReason = RuntimeHoldReason;
	// Do not require deposit from People parachains.
	type AllowWithoutBridgeDeposit = Equals<PeoplePolkadotLocation>;

	type LocalXcmChannelManager = ();
	type BlobDispatcher = FromPolkadotBulletinMessageBlobDispatcher;
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::bridge_common_config::BridgeGrandpaPolkadotBulletinInstance;
	use bridge_runtime_common::{
		assert_complete_bridge_types, integrity::check_message_lane_weights,
	};
	use parachains_common::Balance;
	use system_parachains_constants::polkadot;

	/// Every additional message in the message delivery transaction boosts its priority.
	/// So the priority of transaction with `N+1` messages is larger than priority of
	/// transaction with `N` messages by the `PriorityBoostPerMessage`.
	///
	/// Economically, it is an equivalent of adding tip to the transaction with `N` messages.
	/// The `FEE_BOOST_PER_MESSAGE` constant is the value of this tip.
	///
	/// We want this tip to be large enough (delivery transactions with more messages = less
	/// operational costs and a faster bridge), so this value should be significant.
	const FEE_BOOST_PER_MESSAGE: Balance = 2 * polkadot::currency::UNITS;

	// see `FEE_BOOST_PER_MESSAGE` comment
	const FEE_BOOST_PER_RELAY_HEADER: Balance = 2 * polkadot::currency::UNITS;

	#[test]
	fn ensure_bridge_hub_polkadot_message_lane_weights_are_correct() {
		check_message_lane_weights::<
			bp_people_hub_polkadot::PeopleHubPolkadot,
			Runtime,
			WithPolkadotBulletinMessagesInstance,
		>(
			bp_polkadot_bulletin::EXTRA_STORAGE_PROOF_SIZE,
			bp_people_hub_polkadot::MAX_UNREWARDED_RELAYERS_IN_CONFIRMATION_TX,
			bp_people_hub_polkadot::MAX_UNCONFIRMED_MESSAGES_IN_CONFIRMATION_TX,
			true,
		);
	}

	#[test]
	fn ensure_bridge_integrity() {
		assert_complete_bridge_types!(
			runtime: Runtime,
			with_bridged_chain_messages_instance: WithPolkadotBulletinMessagesInstance,
			this_chain: bp_people_hub_polkadot::PeopleHubPolkadot,
			bridged_chain: bp_polkadot_bulletin::PolkadotBulletin,
			expected_payload_type: XcmAsPlainPayload,
		);

		// we can't use `assert_complete_bridge_constants` here, because there's a trick with
		// Bulletin chain - it has the same (almost) runtime for Polkadot Bulletin and Polkadot
		// Bulletin, so we have to adhere Polkadot names here

		pallet_bridge_relayers::extension::per_relay_header::ensure_priority_boost_is_sane::<
			Runtime,
			BridgeGrandpaPolkadotBulletinInstance,
			PriorityBoostPerRelayHeader,
		>(FEE_BOOST_PER_RELAY_HEADER);

		pallet_bridge_relayers::extension::per_message::ensure_priority_boost_is_sane::<
			Runtime,
			WithPolkadotBulletinMessagesInstance,
			PriorityBoostPerMessage,
		>(FEE_BOOST_PER_MESSAGE);

		let expected: InteriorLocation = PalletInstance(
			bp_people_hub_polkadot::WITH_PEOPLE_POLKADOT_TO_BULLETIN_MESSAGES_PALLET_INDEX,
		)
		.into();

		assert_eq!(BridgePolkadotToPolkadotBulletinMessagesPalletInstance::get(), expected,);
	}
}

#[cfg(feature = "runtime-benchmarks")]
pub(crate) fn open_bridge_for_benchmarks<R, XBHI, C>(
	with: pallet_xcm_bridge_hub::LaneIdOf<R, XBHI>,
	sibling_para_id: u32,
) -> InteriorLocation
where
	R: pallet_xcm_bridge_hub::Config<XBHI>,
	XBHI: 'static,
	C: xcm_executor::traits::ConvertLocation<
		bp_runtime::AccountIdOf<pallet_xcm_bridge_hub::ThisChainOf<R, XBHI>>,
	>,
{
	use pallet_xcm_bridge_hub::{Bridge, BridgeId, BridgeState};
	use sp_runtime::traits::Zero;
	use xcm::{latest::POLKADOT_GENESIS_HASH, VersionedInteriorLocation};

	// insert bridge metadata
	let lane_id = with;
	let sibling_parachain = Location::new(1, [Parachain(sibling_para_id)]);
	let universal_source =
		[GlobalConsensus(ByGenesis(POLKADOT_GENESIS_HASH)), Parachain(sibling_para_id)].into();
	let universal_destination =
		[GlobalConsensus(PolkadotBulletinGlobalConsensusNetwork::get())].into();
	let bridge_id = BridgeId::new(&universal_source, &universal_destination);

	// insert only bridge metadata, because the benchmarks create lanes
	pallet_xcm_bridge_hub::Bridges::<R, XBHI>::insert(
		bridge_id,
		Bridge {
			bridge_origin_relative_location: alloc::boxed::Box::new(
				sibling_parachain.clone().into(),
			),
			bridge_origin_universal_location: alloc::boxed::Box::new(
				VersionedInteriorLocation::from(universal_source.clone()),
			),
			bridge_destination_universal_location: alloc::boxed::Box::new(
				VersionedInteriorLocation::from(universal_destination),
			),
			state: BridgeState::Opened,
			bridge_owner_account: C::convert_location(&sibling_parachain).expect("valid AccountId"),
			deposit: Zero::zero(),
			lane_id,
		},
	);
	pallet_xcm_bridge_hub::LaneToBridge::<R, XBHI>::insert(lane_id, bridge_id);

	universal_source
}
