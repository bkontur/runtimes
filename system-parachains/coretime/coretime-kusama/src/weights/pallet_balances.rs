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

//! Autogenerated weights for `pallet_balances`
//!
//! THIS FILE WAS AUTO-GENERATED USING THE SUBSTRATE BENCHMARK CLI VERSION 46.0.0
//! DATE: 2025-02-24, STEPS: `50`, REPEAT: `20`, LOW RANGE: `[]`, HIGH RANGE: `[]`
//! WORST CASE MAP SIZE: `1000000`
//! HOSTNAME: `a4ecf6faf58e`, CPU: `AMD EPYC 9354 32-Core Processor`
//! WASM-EXECUTION: `Compiled`, CHAIN: `None`, DB CACHE: 1024

// Executed Command:
// frame-omni-bencher
// v1
// benchmark
// pallet
// --extrinsic=*
// --runtime=target/production/wbuild/coretime-kusama-runtime/coretime_kusama_runtime.wasm
// --pallet=pallet_balances
// --header=/_work/zur1-vm-colgitrun-001/runtimes/runtimes/.github/scripts/cmd/file_header.txt
// --output=./system-parachains/coretime/coretime-kusama/src/weights
// --wasm-execution=compiled
// --steps=50
// --repeat=20
// --heap-pages=4096

#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(missing_docs)]

use frame_support::{traits::Get, weights::Weight};
use core::marker::PhantomData;

/// Weight functions for `pallet_balances`.
pub struct WeightInfo<T>(PhantomData<T>);
impl<T: frame_system::Config> pallet_balances::WeightInfo for WeightInfo<T> {
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn transfer_allow_death() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `52`
		//  Estimated: `3593`
		// Minimum execution time: 55_601_000 picoseconds.
		Weight::from_parts(64_542_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn transfer_keep_alive() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `52`
		//  Estimated: `3593`
		// Minimum execution time: 44_940_000 picoseconds.
		Weight::from_parts(56_870_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn force_set_balance_creating() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `174`
		//  Estimated: `3593`
		// Minimum execution time: 17_190_000 picoseconds.
		Weight::from_parts(19_010_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn force_set_balance_killing() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `174`
		//  Estimated: `3593`
		// Minimum execution time: 24_030_000 picoseconds.
		Weight::from_parts(26_990_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:2 w:2)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn force_transfer() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `155`
		//  Estimated: `6196`
		// Minimum execution time: 56_622_000 picoseconds.
		Weight::from_parts(70_020_000, 0)
			.saturating_add(Weight::from_parts(0, 6196))
			.saturating_add(T::DbWeight::get().reads(2))
			.saturating_add(T::DbWeight::get().writes(2))
	}
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn transfer_all() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `52`
		//  Estimated: `3593`
		// Minimum execution time: 56_600_000 picoseconds.
		Weight::from_parts(70_061_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:1 w:1)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	fn force_unreserve() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `174`
		//  Estimated: `3593`
		// Minimum execution time: 21_050_000 picoseconds.
		Weight::from_parts(32_600_000, 0)
			.saturating_add(Weight::from_parts(0, 3593))
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
	/// Storage: `System::Account` (r:999 w:999)
	/// Proof: `System::Account` (`max_values`: None, `max_size`: Some(128), added: 2603, mode: `MaxEncodedLen`)
	/// The range of component `u` is `[1, 1000]`.
	fn upgrade_accounts(u: u32, ) -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `0 + u * (136 ±0)`
		//  Estimated: `990 + u * (2603 ±0)`
		// Minimum execution time: 18_720_000 picoseconds.
		Weight::from_parts(19_190_000, 0)
			.saturating_add(Weight::from_parts(0, 990))
			// Standard Error: 55_232
			.saturating_add(Weight::from_parts(17_719_015, 0).saturating_mul(u.into()))
			.saturating_add(T::DbWeight::get().reads((1_u64).saturating_mul(u.into())))
			.saturating_add(T::DbWeight::get().writes((1_u64).saturating_mul(u.into())))
			.saturating_add(Weight::from_parts(0, 2603).saturating_mul(u.into()))
	}
	/// Storage: `Balances::InactiveIssuance` (r:1 w:0)
	/// Proof: `Balances::InactiveIssuance` (`max_values`: Some(1), `max_size`: Some(16), added: 511, mode: `MaxEncodedLen`)
	fn force_adjust_total_issuance() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `0`
		//  Estimated: `1501`
		// Minimum execution time: 6_509_000 picoseconds.
		Weight::from_parts(12_459_000, 0)
			.saturating_add(Weight::from_parts(0, 1501))
			.saturating_add(T::DbWeight::get().reads(1))
	}
	fn burn_allow_death() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `0`
		//  Estimated: `0`
		// Minimum execution time: 33_560_000 picoseconds.
		Weight::from_parts(34_939_000, 0)
			.saturating_add(Weight::from_parts(0, 0))
	}
	fn burn_keep_alive() -> Weight {
		// Proof Size summary in bytes:
		//  Measured:  `0`
		//  Estimated: `0`
		// Minimum execution time: 22_770_000 picoseconds.
		Weight::from_parts(25_260_000, 0)
			.saturating_add(Weight::from_parts(0, 0))
	}
}
