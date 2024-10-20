
//! Autogenerated weights for `pallet_xcm_bridge_hub_router`
//!
//! THIS FILE WAS AUTO-GENERATED USING THE SUBSTRATE BENCHMARK CLI VERSION 4.0.0-dev
//! DATE: 2023-12-05, STEPS: `50`, REPEAT: `20`, LOW RANGE: `[]`, HIGH RANGE: `[]`
//! WORST CASE MAP SIZE: `1000000`
//! HOSTNAME: `svyatonik-benchmarking`, CPU: `Intel(R) Xeon(R) CPU @ 2.80GHz`
//! WASM-EXECUTION: `Compiled`, CHAIN: `Some("ah-kusama-local-raw.json")`, DB CACHE: 1024

// Executed Command:
// ../polkadot-sdk/target/production/polkadot-parachain-benchmarks
// benchmark
// pallet
// --chain
// ah-kusama-local-raw.json
// --pallet
// pallet-xcm-bridge-hub-router
// --extrinsic
// *
// --output=system-parachains/asset-hubs/asset-hub-kusama/src/weights
// --no-median-slopes
// --no-min-squares

#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(missing_docs)]

use frame_support::{traits::Get, weights::Weight};
use core::marker::PhantomData;

/// Weight functions for `pallet_xcm_bridge_hub_router`.
pub struct WeightInfo<T>(PhantomData<T>);
impl<T: frame_system::Config> pallet_xcm_bridge_hub_router::WeightInfo for WeightInfo<T> {
	/// Storage: `XcmpQueue::InboundXcmpStatus` (r:1 w:0)
	/// Proof: `XcmpQueue::InboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `XcmpQueue::OutboundXcmpStatus` (r:1 w:0)
	/// Proof: `XcmpQueue::OutboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `ToPolkadotXcmRouter::Bridge` (r:1 w:1)
	/// Proof: `ToPolkadotXcmRouter::Bridge` (`max_values`: Some(1), `max_size`: Some(17), added: 512, mode: `MaxEncodedLen`)
	fn on_initialize_when_non_congested() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `159`
		//  Estimated: `1644`
		// Minimum execution time: 10_642_000 picoseconds.
		Weight::from_parts(11_071_000, 0)
			.saturating_add(Weight::from_parts(0, 1644))
			.saturating_add(T::DbWeight::get().reads(3))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `XcmpQueue::InboundXcmpStatus` (r:1 w:0)
	/// Proof: `XcmpQueue::InboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `XcmpQueue::OutboundXcmpStatus` (r:1 w:0)
	/// Proof: `XcmpQueue::OutboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	fn on_initialize_when_congested() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `111`
		//  Estimated: `1596`
		// Minimum execution time: 5_037_000 picoseconds.
		Weight::from_parts(5_209_000, 0)
			.saturating_add(Weight::from_parts(0, 1596))
			.saturating_add(T::DbWeight::get().reads(2))
	}
	/// Storage: `ToPolkadotXcmRouter::Bridge` (r:1 w:1)
	/// Proof: `ToPolkadotXcmRouter::Bridge` (`max_values`: Some(1), `max_size`: Some(17), added: 512, mode: `MaxEncodedLen`)
	fn report_bridge_status() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `83`
		//  Estimated: `1502`
		// Minimum execution time: 13_006_000 picoseconds.
		Weight::from_parts(13_656_000, 0)
			.saturating_add(Weight::from_parts(0, 1502))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `ParachainInfo::ParachainId` (r:1 w:0)
	/// Proof: `ParachainInfo::ParachainId` (`max_values`: Some(1), `max_size`: Some(4), added: 499, mode: `MaxEncodedLen`)
	/// Storage: UNKNOWN KEY `0x3302afcb67e838a3f960251b417b9a4f` (r:1 w:0)
	/// Proof: UNKNOWN KEY `0x3302afcb67e838a3f960251b417b9a4f` (r:1 w:0)
	/// Storage: UNKNOWN KEY `0x0973fe64c85043ba1c965cbc38eb63c7` (r:1 w:0)
	/// Proof: UNKNOWN KEY `0x0973fe64c85043ba1c965cbc38eb63c7` (r:1 w:0)
	/// Storage: `ToPolkadotXcmRouter::Bridge` (r:1 w:1)
	/// Proof: `ToPolkadotXcmRouter::Bridge` (`max_values`: Some(1), `max_size`: Some(17), added: 512, mode: `MaxEncodedLen`)
	/// Storage: `XcmpQueue::DeliveryFeeFactor` (r:1 w:0)
	/// Proof: `XcmpQueue::DeliveryFeeFactor` (`max_values`: None, `max_size`: None, mode: `Measured`)
	/// Storage: `PolkadotXcm::SupportedVersion` (r:1 w:0)
	/// Proof: `PolkadotXcm::SupportedVersion` (`max_values`: None, `max_size`: None, mode: `Measured`)
	/// Storage: `PolkadotXcm::VersionDiscoveryQueue` (r:1 w:1)
	/// Proof: `PolkadotXcm::VersionDiscoveryQueue` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `PolkadotXcm::SafeXcmVersion` (r:1 w:0)
	/// Proof: `PolkadotXcm::SafeXcmVersion` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `ParachainSystem::RelevantMessagingState` (r:1 w:0)
	/// Proof: `ParachainSystem::RelevantMessagingState` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `XcmpQueue::OutboundXcmpStatus` (r:1 w:1)
	/// Proof: `XcmpQueue::OutboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `XcmpQueue::InboundXcmpStatus` (r:1 w:0)
	/// Proof: `XcmpQueue::InboundXcmpStatus` (`max_values`: Some(1), `max_size`: None, mode: `Measured`)
	/// Storage: `XcmpQueue::OutboundXcmpMessages` (r:0 w:1)
	/// Proof: `XcmpQueue::OutboundXcmpMessages` (`max_values`: None, `max_size`: None, mode: `Measured`)
	fn send_message() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `392`
		//  Estimated: `3857`
		// Minimum execution time: 63_791_000 picoseconds.
		Weight::from_parts(68_199_000, 0)
			.saturating_add(Weight::from_parts(0, 3857))
			.saturating_add(T::DbWeight::get().reads(11))
			.saturating_add(T::DbWeight::get().writes(4))
	}
}
