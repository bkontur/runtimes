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

//! Bridge definitions used on PeoplePolkadot for bridging to Polkadot Bulletin.
//!
//! Polkadot Bulletin chain will be the 1:1 copy of the Polkadot Bulletin, so we
//! are reusing Polkadot Bulletin chain primitives everywhere here.

use crate::{
	xcm_config::{LocationToAccountId, UniversalLocation},
	AccountId, Balance, Balances, BlockNumber, BridgePolkadotBulletinGrandpa,
	BridgePolkadotBulletinMessages, Runtime, RuntimeEvent, RuntimeHoldReason,
	XcmOverPolkadotBulletin, XcmRouter,
};
use alloc::{boxed::Box, vec::Vec};
use bp_messages::{
	source_chain::FromBridgedChainMessagesDeliveryProof,
	target_chain::FromBridgedChainMessagesProof, LegacyLaneId,
};
use bp_relayers::RewardsAccountParams;
use codec::{Decode, DecodeWithMemTracking, Encode, MaxEncodedLen};
use frame_support::{
	parameter_types,
	traits::{ConstU128, ConstU32, Equals, PalletInfoAccess},
};
use frame_system::{EnsureRoot, EnsureRootWithSuccess};
use pallet_bridge_messages::LaneIdOf;
use pallet_xcm_bridge_hub::XcmAsPlainPayload;
use scale_info::TypeInfo;
use xcm::{
	latest::prelude::*, prelude::InteriorLocation, AlwaysV5, VersionedLocation, VersionedXcm,
};
use xcm_builder::{BridgeBlobDispatcher, InspectMessageQueues, LocalExporter};

parameter_types! {
	/// Interior location (relative to this runtime) of the with-PolkadotBulletin messages pallet.
	pub BridgePolkadotToPolkadotBulletinMessagesPalletInstance: InteriorLocation = [
		PalletInstance(<BridgePolkadotBulletinMessages as PalletInfoAccess>::index() as u8)
	].into();
	/// Relative location of the Polkadot Bulletin chain.
	pub PolkadotBulletinGlobalConsensusNetworkLocation: Location = Location::new(
		2,
		[GlobalConsensus(bp_polkadot_bulletin::PolkadotBulletinGlobalConsensusNetwork::get())]
	);

	// see the `FEE_BOOST_PER_RELAY_HEADER` constant get the meaning of this value
	pub PriorityBoostPerRelayHeader: u64 = 58_014_163_614_1;

	/// Priority boost that the registered relayer receives for every additional message in the message
	/// delivery transaction.
	///
	/// It is determined semi-automatically - see `FEE_BOOST_PER_MESSAGE` constant to get the
	/// meaning of this value.
	pub PriorityBoostPerMessage: u64 = 3_980_971_916_971;

	/// Here location
	pub HereLocation: Location = Here.into_location();

	/// Number of Bulletin headers to keep in the runtime storage.
	pub const RelayChainHeadersToKeep: u32 = 1_200;

	pub storage RequiredStakeForStakeAndSlash: Balance = 1_000_000;
	pub const RelayerStakeLease: u32 = 8;
	pub const RelayerStakeReserveId: [u8; 8] = *b"brdgrlrs";
}

parameter_types! {}

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

/// Add GRANDPA bridge pallet to track Polkadot Bulletin chain.
pub type BridgeGrandpaPolkadotBulletinInstance = pallet_bridge_grandpa::Instance1;
impl pallet_bridge_grandpa::Config<BridgeGrandpaPolkadotBulletinInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type BridgedChain = bp_polkadot_bulletin::PolkadotBulletin;
	type MaxFreeHeadersPerBlock = ConstU32<4>;
	type FreeHeadersInterval = ConstU32<5>;
	type HeadersToKeep = RelayChainHeadersToKeep;
	// TODO: (setup benchmarking correctly)
	type WeightInfo = ();
}

/// Potential rewards.
#[derive(
	Clone,
	Copy,
	Debug,
	Decode,
	DecodeWithMemTracking,
	Encode,
	Eq,
	MaxEncodedLen,
	PartialEq,
	TypeInfo,
)]
pub enum BridgeReward {
	/// Rewards for the Bulletin bridge.
	BulletinBridge(RewardsAccountParams<LegacyLaneId>),
}
impl From<RewardsAccountParams<LegacyLaneId>> for BridgeReward {
	fn from(value: RewardsAccountParams<LegacyLaneId>) -> Self {
		Self::BulletinBridge(value)
	}
}

/// An enum representing the different types of supported beneficiaries.
#[derive(
	Clone, Debug, Decode, DecodeWithMemTracking, Encode, Eq, MaxEncodedLen, PartialEq, TypeInfo,
)]
pub enum BridgeRewardBeneficiaries {
	/// A local chain account.
	LocalAccount(AccountId),
	/// A beneficiary specified by a VersionedLocation.
	AssetHubLocation(Box<VersionedLocation>),
}

impl From<sp_runtime::AccountId32> for BridgeRewardBeneficiaries {
	fn from(value: sp_runtime::AccountId32) -> Self {
		BridgeRewardBeneficiaries::LocalAccount(value)
	}
}

/// Implementation of `bp_relayers::PaymentProcedure` as a pay/claim rewards scheme.
pub struct BridgeRewardPayer;
impl bp_relayers::PaymentProcedure<AccountId, BridgeReward, u128> for BridgeRewardPayer {
	type Error = sp_runtime::DispatchError;
	type Beneficiary = BridgeRewardBeneficiaries;

	fn pay_reward(
		relayer: &AccountId,
		reward_kind: BridgeReward,
		reward: u128,
		beneficiary: BridgeRewardBeneficiaries,
	) -> Result<(), Self::Error> {
		match reward_kind {
			BridgeReward::BulletinBridge(lane_params) => {
				match beneficiary {
					BridgeRewardBeneficiaries::LocalAccount(account) => {
						bp_relayers::PayRewardFromAccount::<
							Balances,
							AccountId,
							LegacyLaneId,
							u128,
						>::pay_reward(
							relayer, lane_params, reward, account,
						)
					},
					BridgeRewardBeneficiaries::AssetHubLocation(_) => Err(Self::Error::Other("`AssetHubLocation` beneficiary is not supported for `BulletinBridge` rewards!")),
				}
			}
		}
	}
}

/// Allows collect and claim rewards for relayers
pub type BridgeRelayersInstance = ();
impl pallet_bridge_relayers::Config<BridgeRelayersInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type RewardBalance = Balance;
	type Reward = BridgeReward;
	type PaymentProcedure = BridgeRewardPayer;
	type StakeAndSlash = pallet_bridge_relayers::StakeAndSlashNamed<
		AccountId,
		BlockNumber,
		Balances,
		RelayerStakeReserveId,
		RequiredStakeForStakeAndSlash,
		RelayerStakeLease,
	>;
	type Balance = Balance;
	// TODO: (setup real weights and benchmarking)
	type WeightInfo = ();
}

/// Add XCM messages support for PeoplePolkadot to support Polkadot->Polkadot Bulletin XCM messages.
pub type WithPolkadotBulletinMessagesInstance = pallet_bridge_messages::Instance1;
impl pallet_bridge_messages::Config<WithPolkadotBulletinMessagesInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type WeightInfo = ();

	type ThisChain = bp_people_polkadot::PeoplePolkadot;
	type BridgedChain = bp_polkadot_bulletin::PolkadotBulletin;
	type BridgedHeaderChain = BridgePolkadotBulletinGrandpa;

	type OutboundPayload = XcmAsPlainPayload;
	type InboundPayload = XcmAsPlainPayload;
	type LaneId = LegacyLaneId;

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
	// TODO: (setup XCM version with PolkadotXcm) -> type DestinationVersion = XcmVersionOfDestAndRemoteBridge<PolkadotXcm, BridgeHubKusamaLocation>;
	type DestinationVersion = AlwaysV5;

	type ForceOrigin = EnsureRoot<AccountId>;
	// We allow creating bridges only for the runtime itself.
	// We want to translate `RuntimeOrigin::root()` to the `Location::here()`, e.g. for
	// governance calls.
	type OpenBridgeOrigin = EnsureRootWithSuccess<AccountId, HereLocation>;
	// Converter aligned with `OpenBridgeOrigin`.
	type BridgeOriginAccountIdConverter = LocationToAccountId;

	type BridgeDeposit = ConstU128<0>;
	type Currency = Balances;
	type RuntimeHoldReason = RuntimeHoldReason;
	// Do not require deposit from the chain itself.
	type AllowWithoutBridgeDeposit = Equals<HereLocation>;

	// TODO: setup back-pressure and congestion
	type LocalXcmChannelManager = ();
	// TODO: probably not needed now
	type BlobDispatcher = FromPolkadotBulletinMessageBlobDispatcher;
}

/// Router for a Polkadot Bulletin chain.
/// We use `LocalExporter` to ensure that `pallet_xcm_bridge_hub` can directly export messages.
pub type ToBulletinXcmRouter =
	LocalBulletinExporter<LocalExporter<XcmOverPolkadotBulletin, UniversalLocation>>;
pub struct LocalBulletinExporter<Inner>(core::marker::PhantomData<Inner>);
impl<Inner: SendXcm> SendXcm for LocalBulletinExporter<Inner> {
	type Ticket = Inner::Ticket;

	fn validate(
		dest: &mut Option<Location>,
		xcm: &mut Option<Xcm<()>>,
	) -> SendResult<Self::Ticket> {
		Inner::validate(dest, xcm)
	}
	fn deliver(ticket: Self::Ticket) -> Result<XcmHash, SendError> {
		Inner::deliver(ticket)
	}
}
impl<Inner> InspectMessageQueues for LocalBulletinExporter<Inner> {
	fn clear_messages() {}
	fn get_messages() -> Vec<(VersionedLocation, Vec<VersionedXcm<()>>)> {
		Vec::new()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
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
			bp_people_polkadot::PeoplePolkadot,
			Runtime,
			WithPolkadotBulletinMessagesInstance,
		>(
			bp_polkadot_bulletin::EXTRA_STORAGE_PROOF_SIZE,
			bp_people_polkadot::MAX_UNREWARDED_RELAYERS_IN_CONFIRMATION_TX,
			bp_people_polkadot::MAX_UNCONFIRMED_MESSAGES_IN_CONFIRMATION_TX,
			true,
		);
	}

	#[test]
	fn ensure_bridge_integrity() {
		assert_complete_bridge_types!(
			runtime: Runtime,
			with_bridged_chain_messages_instance: WithPolkadotBulletinMessagesInstance,
			this_chain: bp_people_polkadot::PeoplePolkadot,
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
			bp_people_polkadot::WITH_PEOPLE_POLKADOT_TO_BULLETIN_MESSAGES_PALLET_INDEX,
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
	use xcm::VersionedInteriorLocation;

	// insert bridge metadata
	let lane_id = with;
	let sibling_parachain = Location::new(1, [Parachain(sibling_para_id)]);
	let universal_source = [GlobalConsensus(Polkadot), Parachain(sibling_para_id)].into();
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
