// Copyright (C) Parity Technologies (UK) Ltd.
// This file is part of Cumulus.

// Cumulus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Cumulus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Cumulus.  If not, see <http://www.gnu.org/licenses/>.

//! Bridge definitions used on BridgeHubKusama for bridging to BridgeHubPolkadot.
use crate::{
	bridge_common_config::{BridgeRelayersInstance, DeliveryRewardInBalance},
	weights, xcm_config,
	xcm_config::UniversalLocation,
	AccountId, AssetHubPolkadotProofRootStore, Balance, Balances, BridgePolkadotMessages,
	MessageQueue, PolkadotXcm, Runtime, RuntimeEvent, RuntimeHoldReason,
	ToPolkadotOverAssetHubPolkadotXcmRouter, XcmOverAssetHubPolkadot,
};
use alloc::{vec, vec::Vec};
use bp_messages::HashedLaneId;
use bp_runtime::HashOf;
use bridge_hub_common::xcm_version::XcmVersionOfDestAndRemoteBridge;
use cumulus_primitives_core::AggregateMessageOrigin;
use pallet_xcm_bridge::XcmAsPlainPayload;

use frame_support::{
	parameter_types,
	traits::{EitherOf, EitherOfDiverse, Equals, PalletInfoAccess},
};
use frame_system::{EnsureRoot, EnsureRootWithSuccess};
use pallet_bridge_relayers::extension::{
	BridgeRelayersTransactionExtension, WithMessagesExtensionConfig,
};
use pallet_xcm::EnsureXcm;
use pallet_xcm_bridge::congestion::{
	BlobDispatcherWithChannelStatus, HereOrLocalConsensusXcmChannelManager,
	UpdateBridgeStatusXcmChannelManager,
};
use parachains_common::xcm_config::{
	AllSiblingSystemParachains, ParentRelayOrSiblingParachains, RelayOrOtherSystemParachains,
};
use polkadot_parachain_primitives::primitives::Sibling;
use sp_runtime::traits::{ConstU32, Convert, MaybeConvert};
use kusama_runtime_constants::currency::UNITS as KSM;
use xcm::{
	latest::{prelude::*},
	prelude::NetworkId,
};
use xcm_builder::{
	BridgeBlobDispatcher, LocalExporter, MessageQueueRouterFor, ParentIsPreset,
	SiblingParachainConvertsVia,
};

parameter_types! {
	pub BridgeKusamaToPolkadotMessagesPalletInstance: InteriorLocation = [PalletInstance(<BridgePolkadotMessages as PalletInfoAccess>::index() as u8)].into();
	pub const HereLocation: Location = Location::here();
	pub PolkadotGlobalConsensusNetwork: NetworkId = NetworkId::Polkadot;
	pub PolkadotGlobalConsensusNetworkLocation: Location = Location::new(
		2,
		[GlobalConsensus(PolkadotGlobalConsensusNetwork::get())]
	);
	// see the `FEE_BOOST_PER_MESSAGE` constant to get the meaning of this value
	pub PriorityBoostPerMessage: u64 = 364_088_888_888_888;

	// The other side of the bridge
	pub AssetHubPolkadotLocation: Location = Location::new(
		2,
		[
			GlobalConsensus(PolkadotGlobalConsensusNetwork::get()),
			Parachain(<bp_asset_hub_polkadot::AssetHubPolkadot as bp_runtime::Parachain>::PARACHAIN_ID)
		]
	);

	pub storage BridgeDeposit: Balance = 5 * KSM;

	// The fee for exporting/delivery.
	pub MessageExportPrice: Assets = (
		xcm_config::bridging::XcmBridgeHubRouterFeeAssetId::get(),
		xcm_config::bridging::ToPolkadotOverAssetHubPolkadotXcmRouterBaseFee::get(),
	).into();
}

/// A converter that accepts only the `Here` location and converts it into `AggregateMessageOrigin`.
pub struct AcceptOnlyHere;
impl MaybeConvert<&Location, AggregateMessageOrigin> for AcceptOnlyHere {
	fn maybe_convert(loc: &Location) -> Option<AggregateMessageOrigin> {
		match loc.unpack() {
			(0, []) => {
				// Let's use `Here` for local message queue dispatch, but we can also change to
				// custom.
				Some(AggregateMessageOrigin::Here)
			},
			_ => None,
		}
	}
}

/// Transaction extension that refunds relayers that are delivering messages from the Polkadot
/// parachain.
pub type OnAssetHubKusamaRefundAssetHubPolkadotMessages = BridgeRelayersTransactionExtension<
	Runtime,
	WithMessagesExtensionConfig<
		StrOnAssetHubKusamaRefundAssetHubPolkadotMessages,
		Runtime,
		WithAssetHubPolkadotMessagesInstance,
		BridgeRelayersInstance,
		PriorityBoostPerMessage,
	>,
>;
bp_runtime::generate_static_str_provider!(OnAssetHubKusamaRefundAssetHubPolkadotMessages);

/// Add XCM messages support for AssetHubKusama to support Kusama->Polkadot XCM messages
pub type WithAssetHubPolkadotMessagesInstance = pallet_bridge_messages::Instance1;
impl pallet_bridge_messages::Config<WithAssetHubPolkadotMessagesInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type WeightInfo = weights::pallet_bridge_messages::WeightInfo<Runtime>;

	type ThisChain = bp_asset_hub_kusama::AssetHubKusama;
	type BridgedChain = bp_asset_hub_polkadot::AssetHubPolkadot;
	type BridgedHeaderChain = AssetHubPolkadotHeaders;

	type OutboundPayload = XcmAsPlainPayload;
	type InboundPayload = XcmAsPlainPayload;
	type LaneId = HashedLaneId;

	type DeliveryPayments = ();
	type DeliveryConfirmationPayments = pallet_bridge_relayers::DeliveryConfirmationPaymentsAdapter<
		Runtime,
		WithAssetHubPolkadotMessagesInstance,
		BridgeRelayersInstance,
		DeliveryRewardInBalance,
	>;

	type MessageDispatch = XcmOverAssetHubPolkadot;
	type OnMessagesDelivered = XcmOverAssetHubPolkadot;
}

/// Add support for storing bridged AssetHubPolkadot state roots.
pub type AssetHubPolkadotProofRootStoreInstance = pallet_bridge_proof_root_store::Instance1;
impl pallet_bridge_proof_root_store::Config<AssetHubPolkadotProofRootStoreInstance> for Runtime {
	// TOOD: FAIL-CI weights
	type WeightInfo = ();
	type SubmitOrigin = EitherOfDiverse<
		// `Root` can do whatever
		EnsureRoot<AccountId>,
		// and only the local BridgeHub can send updates.
		EnsureXcm<Equals<xcm_config::bridging::SiblingBridgeHub>>,
	>;
	// Means `block_hash` of AHW.
	type Key = HashOf<
		pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
	>;
	// Means `state_root` of AHW.
	type Value = HashOf<
		pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
	>;
	// Configured according to the BHR's `ParachainHeadsToKeep`
	type RootsToKeep = ConstU32<64>;
}

/// Adapter `bp_header_chain::HeaderChain` implementation which resolves AssetHubPolkadot
/// `state_root` for `block_hash`.
pub struct AssetHubPolkadotHeaders;
impl
	bp_header_chain::HeaderChain<
		pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
	> for AssetHubPolkadotHeaders
{
	fn finalized_header_state_root(
		header_hash: HashOf<
			pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
		>,
	) -> Option<
		HashOf<
			pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
		>,
	> {
		AssetHubPolkadotProofRootStore::get_root(&header_hash)
	}
}

/// Converts encoded call to the unpaid XCM `Transact`.
pub struct UpdateBridgeStatusXcmProvider;
impl Convert<Vec<u8>, Xcm<()>> for UpdateBridgeStatusXcmProvider {
	fn convert(encoded_call: Vec<u8>) -> Xcm<()> {
		Xcm(vec![
			UnpaidExecution { weight_limit: Unlimited, check_origin: None },
			Transact {
				origin_kind: OriginKind::Xcm,
				call: encoded_call.into(),
				// TODO: FAIL-CI - add some test for this or remove TODO
				fallback_max_weight: Some(Weight::from_parts(200_000_000, 6144)),
			},
			ExpectTransactStatus(MaybeErrorCode::Success),
		])
	}
}

/// Add support for the export and dispatch of XCM programs withing
/// `WithAssetHubPolkadotMessagesInstance`.
pub type XcmOverAssetHubPolkadotInstance = pallet_xcm_bridge::Instance1;
impl pallet_xcm_bridge::Config<XcmOverAssetHubPolkadotInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type WeightInfo = weights::pallet_xcm_bridge::WeightInfo<Runtime>;

	type UniversalLocation = UniversalLocation;
	type BridgedNetwork = PolkadotGlobalConsensusNetworkLocation;
	type BridgeMessagesPalletInstance = WithAssetHubPolkadotMessagesInstance;

	type MessageExportPrice = MessageExportPrice;
	type DestinationVersion = XcmVersionOfDestAndRemoteBridge<PolkadotXcm, AssetHubPolkadotLocation>;

	type ForceOrigin = EnsureRoot<AccountId>;
	// We allow creating bridges for the runtime itself and for other local consensus chains (relay,
	// paras).
	type OpenBridgeOrigin = EitherOf<
		// We want to translate `RuntimeOrigin::root()` to the `Location::here()`, e.g. for
		// governance calls.
		EnsureRootWithSuccess<AccountId, HereLocation>,
		// For relay or sibling chains
		EnsureXcm<ParentRelayOrSiblingParachains>,
	>;
	// Converter aligned with `OpenBridgeOrigin`.
	type BridgeOriginAccountIdConverter =
		(ParentIsPreset<AccountId>, SiblingParachainConvertsVia<Sibling, AccountId>);

	type BridgeDeposit = BridgeDeposit;
	type Currency = Balances;
	type RuntimeHoldReason = RuntimeHoldReason;
	// Do not require deposit from system parachains (including itself) or relay chain
	type AllowWithoutBridgeDeposit =
		(RelayOrOtherSystemParachains<AllSiblingSystemParachains, Runtime>, Equals<HereLocation>);

	// This pallet is deployed on AH, so we expect a remote router with `ExportMessage`. We handle
	// congestion with XCM using `udpate_bridge_status` sent to the sending chain. (congestion with
	// local sending chain)
	type LocalXcmChannelManager = HereOrLocalConsensusXcmChannelManager<
		pallet_xcm_bridge::BridgeId,
		// handles congestion for local chain router for local AH's bridges
		ToPolkadotOverAssetHubPolkadotXcmRouter,
		// handles congestion for other local chains with XCM using `update_bridge_status` sent to
		// the sending chain.
		UpdateBridgeStatusXcmChannelManager<
			Runtime,
			XcmOverAssetHubPolkadotInstance,
			UpdateBridgeStatusXcmProvider,
			xcm_config::LocalXcmRouter,
		>,
	>;
	// Dispatching inbound messages from the bridge and managing congestion with the local
	// receiving/destination chain
	type BlobDispatcher = BlobDispatcherWithChannelStatus<
		// Dispatches received XCM messages from other bridge
		BridgeBlobDispatcher<
			(
				// This router handles parent (UMP) or sibling (HRMP) dispatch.
				xcm_config::LocalXcmRouter,
				// This router enqueues a message for local XCM execution to the message queue.
				// (According to the `DispatchBlob` trait, we should just enqueue a message).
				MessageQueueRouterFor<MessageQueue, AggregateMessageOrigin, AcceptOnlyHere>,
			),
			UniversalLocation,
			// TODO: FAIL-CI wait for https://github.com/paritytech/polkadot-sdk/pull/6002#issuecomment-2469892343
			BridgeKusamaToPolkadotMessagesPalletInstance,
		>,
		// Provides the status of the XCMP queue's outbound queue, indicating whether messages can
		// be dispatched to the sibling.
		cumulus_pallet_xcmp_queue::bridging::OutXcmpChannelStatusProvider<Runtime>,
	>;
	type CongestionLimits = ();
}

/// XCM router instance to the local `pallet_xcm_bridge::<XcmOverAssetHubPolkadotInstance>` with
/// direct bridging capabilities for `Polkadot` global consensus with dynamic fees and back-pressure.
pub type ToPolkadotOverAssetHubPolkadotXcmRouterInstance = pallet_xcm_bridge_router::Instance4;
impl pallet_xcm_bridge_router::Config<ToPolkadotOverAssetHubPolkadotXcmRouterInstance> for Runtime {
	type RuntimeEvent = RuntimeEvent;
	type WeightInfo =
		weights::pallet_xcm_bridge_router_to_polkadot_over_asset_hub_polkadot::WeightInfo<Runtime>;

	type DestinationVersion = PolkadotXcm;

	// We use `LocalExporter` with `ViaLocalBridgeHubExporter` ensures that
	// `pallet_xcm_bridge_router` can trigger directly `pallet_xcm_bridge` as exporter.
	type MessageExporter = pallet_xcm_bridge_router::impls::ViaLocalBridgeExporter<
		Runtime,
		ToPolkadotOverAssetHubPolkadotXcmRouterInstance,
		LocalExporter<XcmOverAssetHubPolkadot, UniversalLocation>,
	>;

	// For congestion - resolves `BridgeId` using the same algorithm as `pallet_xcm_bridge` on
	// the BH.
	type BridgeIdResolver =
		pallet_xcm_bridge_router::impls::EnsureIsRemoteBridgeIdResolver<UniversalLocation>;
	// We don't expect here `update_bridge_status` calls, but let's allow just for root (governance,
	// ...).
	type UpdateBridgeStatusOrigin = EnsureRoot<AccountId>;

	// For adding message size fees
	type ByteFee = xcm_config::bridging::XcmBridgeHubRouterByteFee;
	// For adding message size fees
	type FeeAsset = xcm_config::bridging::XcmBridgeHubRouterFeeAssetId;
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::RuntimeCall;
	use bridge_runtime_common::{
		assert_complete_bridge_types,
		integrity::{
			assert_standalone_messages_bridge_constants, check_message_lane_weights,
			AssertChainConstants, AssertCompleteBridgeConstants,
		},
	};
	use codec::Encode;
	use frame_support::BoundedVec;

	/// Every additional message in the message delivery transaction boosts its priority.
	/// So the priority of transaction with `N+1` messages is larger than priority of
	/// transaction with `N` messages by the `PriorityBoostPerMessage`.
	///
	/// Economically, it is an equivalent of adding tip to the transaction with `N` messages.
	/// The `FEE_BOOST_PER_MESSAGE` constant is the value of this tip.
	///
	/// We want this tip to be large enough (delivery transactions with more messages = less
	/// operational costs and a faster bridge), so this value should be significant.
	const FEE_BOOST_PER_MESSAGE: Balance = 2 * KSM;

	#[test]
	fn ensure_bridge_hub_kusama_message_lane_weights_are_correct() {
		check_message_lane_weights::<
			bp_asset_hub_kusama::AssetHubKusama,
			Runtime,
			WithAssetHubPolkadotMessagesInstance,
		>(
			bp_asset_hub_polkadot::EXTRA_STORAGE_PROOF_SIZE,
			bp_asset_hub_kusama::MAX_UNREWARDED_RELAYERS_IN_CONFIRMATION_TX,
			bp_asset_hub_kusama::MAX_UNCONFIRMED_MESSAGES_IN_CONFIRMATION_TX,
			true,
		);
	}

	#[test]
	fn ensure_bridge_integrity() {
		assert_complete_bridge_types!(
			runtime: Runtime,
			with_bridged_chain_messages_instance: WithAssetHubPolkadotMessagesInstance,
			this_chain: bp_asset_hub_kusama::AssetHubKusama,
			bridged_chain: bp_asset_hub_polkadot::AssetHubPolkadot,
			expected_payload_type: XcmAsPlainPayload,
		);

		assert_standalone_messages_bridge_constants::<Runtime, WithAssetHubPolkadotMessagesInstance>(
			AssertCompleteBridgeConstants {
				this_chain_constants: AssertChainConstants {
					block_length: bp_bridge_hub_kusama::BlockLength::get(),
					block_weights: bp_bridge_hub_kusama::BlockWeightsForAsyncBacking::get(),
				},
			},
		);

		pallet_bridge_relayers::extension::per_message::ensure_priority_boost_is_sane::<
			Runtime,
			WithAssetHubPolkadotMessagesInstance,
			PriorityBoostPerMessage,
		>(FEE_BOOST_PER_MESSAGE);

		let expected: InteriorLocation = [PalletInstance(
			bp_asset_hub_kusama::WITH_BRIDGE_KUSAMA_TO_POLKADOT_MESSAGES_PALLET_INDEX,
		)]
		.into();
		assert_eq!(BridgeKusamaToPolkadotMessagesPalletInstance::get(), expected);
	}

	#[test]
	fn ensure_encoding_compatibility() {
		let hash = HashOf::<
			pallet_bridge_messages::BridgedChainOf<Runtime, WithAssetHubPolkadotMessagesInstance>,
		>::from([1; 32]);
		let roots = vec![(hash, hash), (hash, hash)];

		assert_eq!(
			bp_asset_hub_kusama::Call::AssetHubPolkadotProofRootStore(
				bp_asset_hub_kusama::ProofRootStoreCall::note_new_roots { roots: roots.clone() }
			)
			.encode(),
			RuntimeCall::AssetHubPolkadotProofRootStore(
				pallet_bridge_proof_root_store::Call::note_new_roots {
					roots: BoundedVec::truncate_from(roots)
				}
			)
			.encode()
		);
	}
}