// Copyright (C) Parity Technologies (UK) Ltd.
// This file is part of Polkadot.

// Polkadot is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Polkadot is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Polkadot.  If not, see <http://www.gnu.org/licenses/>.

//! Module with configuration which reflects AssetHubPolkadot runtime setup (AccountId, Headers,
//! Hashes...)

#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::vec::Vec;
pub use bp_bridge_hub_cumulus::*;
use bp_messages::*;
use bp_runtime::{
	decl_bridge_finality_runtime_apis, decl_bridge_messages_runtime_apis, Chain, ChainId, Parachain,
};
pub use bp_xcm_bridge_hub_router::XcmBridgeHubRouterCall;
use codec::{Decode, Encode};
use frame_support::{
	dispatch::DispatchClass,
	sp_runtime::{MultiAddress, MultiSigner, RuntimeDebug, StateVersion},
};
use scale_info::TypeInfo;

pub use bp_proof_root_store::ProofRootStoreCall;
pub use bp_xcm_bridge_router::XcmBridgeHubCall;
use xcm::latest::prelude::*;

use system_parachains_constants::polkadot::currency::*;

/// `AssetHubPolkadot` Runtime `Call` enum.
///
/// The enum represents a subset of possible `Call`s we can send to `AssetHubPolkadot` chain.
/// Ideally this code would be auto-generated from metadata, because we want to
/// avoid depending directly on the ENTIRE runtime just to get the encoding of `Dispatchable`s.
///
/// All entries here (like pretty much in the entire file) must be kept in sync with
/// `AssetHubPolkadot` `construct_runtime`, so that we maintain SCALE-compatibility.
#[allow(clippy::large_enum_variant)]
#[derive(Encode, Decode, Debug, PartialEq, Eq, Clone, TypeInfo)]
pub enum Call {
	/// `ToKusamaXcmRouter` bridge pallet.
	#[codec(index = 34)]
	ToKusamaXcmRouter(XcmBridgeHubCall<sp_core::H256>),
	/// Points to the `pallet_xcm_bridge` pallet instance for `AssetHubKusama`.
	#[codec(index = 62)] // TODO: FAIL-CI - corect index when AssetHubPolkadot
	XcmOverAssetHubKusama(bp_xcm_bridge::XcmBridgeCall),
	/// `AssetHubKusamaProofRootStore` bridge pallet.
	#[codec(index = 66)]
	AssetHubKusamaProofRootStore(ProofRootStoreCall<Hash, Hash>),
}

frame_support::parameter_types! {
	/// Some sane weight to execute `xcm::Transact(pallet-xcm-bridge-hub-router::Call::report_bridge_status)`.
	pub const XcmBridgeHubRouterTransactCallMaxWeight: Weight = Weight::from_parts(200_000_000, 6144);

	/// Should match the `AssetDeposit` of the `ForeignAssets` pallet on Asset Hub.
	pub const CreateForeignAssetDeposit: u128 = system_para_deposit(1, 190);
}

/// Builds an (un)congestion XCM program with the `report_bridge_status` call for
/// `ToKusamaXcmRouter`.
pub fn build_congestion_message<RuntimeCall>(
	bridge_id: sp_core::H256,
	is_congested: bool,
) -> Vec<Instruction<RuntimeCall>> {
	alloc::vec![
		UnpaidExecution { weight_limit: Unlimited, check_origin: None },
		Transact {
			origin_kind: OriginKind::Xcm,
			fallback_max_weight: Some(XcmBridgeHubRouterTransactCallMaxWeight::get()),
			call: Call::ToKusamaXcmRouter(XcmBridgeHubCall::update_bridge_status {
				bridge_id,
				is_congested,
			})
			.encode()
			.into(),
		},
		ExpectTransactStatus(MaybeErrorCode::Success),
	]
}

/// Identifier of AssetHubPolkadot in the Polkadot relay chain.
pub const ASSET_HUB_POLKADOT_PARACHAIN_ID: u32 = 1000;

/// AssetHubPolkadot parachain.
#[derive(RuntimeDebug)]
pub struct AssetHubPolkadot;

impl Chain for AssetHubPolkadot {
	const ID: ChainId = *b"ahpd";

	type BlockNumber = BlockNumber;
	type Hash = Hash;
	type Hasher = Hasher;
	type Header = Header;

	type AccountId = AccountId;
	type Balance = Balance;
	type Nonce = Nonce;
	type Signature = Signature;

	const STATE_VERSION: StateVersion = StateVersion::V1;

	fn max_extrinsic_size() -> u32 {
		*BlockLength::get().max.get(DispatchClass::Normal)
	}

	fn max_extrinsic_weight() -> Weight {
		BlockWeightsForAsyncBacking::get()
			.get(DispatchClass::Normal)
			.max_extrinsic
			.unwrap_or(Weight::MAX)
	}
}

impl Parachain for AssetHubPolkadot {
	const PARACHAIN_ID: u32 = ASSET_HUB_POLKADOT_PARACHAIN_ID;
	const MAX_HEADER_SIZE: u32 = MAX_ASSET_HUB_HEADER_SIZE;
}

/// Describing permissionless lanes instance
impl ChainWithMessages for AssetHubPolkadot {
	const WITH_CHAIN_MESSAGES_PALLET_NAME: &'static str =
		WITH_ASSET_HUB_POLKADOT_MESSAGES_PALLET_NAME;

	const MAX_UNREWARDED_RELAYERS_IN_CONFIRMATION_TX: MessageNonce =
		MAX_UNREWARDED_RELAYERS_IN_CONFIRMATION_TX;
	const MAX_UNCONFIRMED_MESSAGES_IN_CONFIRMATION_TX: MessageNonce =
		MAX_UNCONFIRMED_MESSAGES_IN_CONFIRMATION_TX;
}

/// Public key of the chain account that may be used to verify signatures.
pub type AccountSigner = MultiSigner;

/// The address format for describing accounts.
pub type Address = MultiAddress<AccountId, ()>;

/// Name of the With-AssetHubPolkadot messages pallet instance that is deployed at bridged chains.
pub const WITH_ASSET_HUB_POLKADOT_MESSAGES_PALLET_NAME: &str = "BridgePolkadotMessages";

/// Name of the With-AssetHubPolkadot bridge-relayers pallet instance that is deployed at bridged
/// chains.
pub const WITH_ASSET_HUB_POLKADOT_RELAYERS_PALLET_NAME: &str = "BridgeRelayers";

/// Pallet index of `BridgeKusamaMessages: pallet_bridge_messages::<Instance1>`.
pub const WITH_BRIDGE_POLKADOT_TO_KUSAMA_MESSAGES_PALLET_INDEX: u8 = 63;

decl_bridge_finality_runtime_apis!(asset_hub_polkadot);
decl_bridge_messages_runtime_apis!(asset_hub_polkadot, HashedLaneId);

frame_support::parameter_types! {
	/// TODO: FAIL-CI - probably not needed
	/// The XCM fee that is paid for executing XCM program (with `ExportMessage` instruction) at the Polkadot
	/// AssetHub.
	/// (initially was calculated by test `AssetHubPolkadot::can_calculate_weight_for_paid_export_message_with_reserve_transfer` + `33%`)
	pub const AssetHubPolkadotBaseXcmFeeInDots: u128 = 57_325_000;

	/// Transaction fee that is paid at the Polkadot AssetHub for delivering single inbound message.
	/// (initially was calculated by test `AssetHubPolkadot::can_calculate_fee_for_standalone_message_delivery_transaction` + `33%`)
	pub const AssetHubPolkadotBaseDeliveryFeeInDots: u128 = 297_685_840;

	/// Transaction fee that is paid at the Polkadot AssetHub for delivering single outbound message confirmation.
	/// (initially was calculated by test `AssetHubPolkadot::can_calculate_fee_for_standalone_message_confirmation_transaction` + `33%`)
	pub const AssetHubPolkadotBaseConfirmationFeeInDots: u128 = 56_782_099;
}
