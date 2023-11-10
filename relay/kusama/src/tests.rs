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
// along with Polkadot. If not, see <http://www.gnu.org/licenses/>.

//! Tests for the Kusama Runtime Configuration

use crate::*;
use frame_support::{
	assert_ok, dispatch::GetDispatchInfo, traits::WhitelistedStorageKeys,
	weights::WeightToFee as WeightToFeeT,
};
use keyring::Sr25519Keyring::Charlie;
use pallet_transaction_payment::Multiplier;
use parity_scale_codec::Encode;
use runtime_common::MinimumMultiplier;
use runtime_parachains::configuration::HostConfiguration;
use separator::Separatable;
use sp_core::hexdisplay::HexDisplay;
use sp_runtime::FixedPointNumber;
use std::collections::HashSet;
use xcm::latest::prelude::*;
use xcm_executor::{traits::ConvertLocation, XcmExecutor};

#[test]
fn nis_hold_reason_encoding_is_correct() {
	assert_eq!(RuntimeHoldReason::Nis(pallet_nis::HoldReason::NftReceipt).encode(), [38, 0]);
}

#[test]
fn remove_keys_weight_is_sensible() {
	use runtime_common::crowdloan::WeightInfo;
	let max_weight = <Runtime as crowdloan::Config>::WeightInfo::refund(RemoveKeysLimit::get());
	// Max remove keys limit should be no more than half the total block weight.
	assert!((max_weight * 2).all_lt(BlockWeights::get().max_block));
}

#[test]
fn sample_size_is_sensible() {
	use runtime_common::auctions::WeightInfo;
	// Need to clean up all samples at the end of an auction.
	let samples: BlockNumber = EndingPeriod::get() / SampleLength::get();
	let max_weight: Weight = RocksDbWeight::get().reads_writes(samples.into(), samples.into());
	// Max sample cleanup should be no more than half the total block weight.
	assert!((max_weight * 2).all_lt(BlockWeights::get().max_block));
	assert!((<Runtime as auctions::Config>::WeightInfo::on_initialize() * 2)
		.all_lt(BlockWeights::get().max_block));
}

#[test]
fn payout_weight_portion() {
	use pallet_staking::WeightInfo;
	let payout_weight =
		<Runtime as pallet_staking::Config>::WeightInfo::payout_stakers_alive_staked(
			MaxNominatorRewardedPerValidator::get(),
		)
		.ref_time() as f64;
	let block_weight = BlockWeights::get().max_block.ref_time() as f64;

	println!(
		"a full payout takes {:.2} of the block weight [{} / {}]",
		payout_weight / block_weight,
		payout_weight,
		block_weight
	);
	assert!(payout_weight * 2f64 < block_weight);
}

#[test]
#[ignore]
fn block_cost() {
	let max_block_weight = BlockWeights::get().max_block;
	let raw_fee = WeightToFee::weight_to_fee(&max_block_weight);

	println!(
		"Full Block weight == {} // WeightToFee(full_block) == {} plank",
		max_block_weight,
		raw_fee.separated_string(),
	);
}

#[test]
#[ignore]
fn transfer_cost_min_multiplier() {
	let min_multiplier = MinimumMultiplier::get();
	let call = pallet_balances::Call::<Runtime>::transfer_keep_alive {
		dest: Charlie.to_account_id().into(),
		value: Default::default(),
	};
	let info = call.get_dispatch_info();
	// convert to outer call.
	let call = RuntimeCall::Balances(call);
	let len = call.using_encoded(|e| e.len()) as u32;

	let mut ext = sp_io::TestExternalities::new_empty();
	let mut test_with_multiplier = |m| {
		ext.execute_with(|| {
			pallet_transaction_payment::NextFeeMultiplier::<Runtime>::put(m);
			let fee = TransactionPayment::compute_fee(len, &info, 0);
			println!(
				"weight = {:?} // multiplier = {:?} // full transfer fee = {:?}",
				info.weight.ref_time().separated_string(),
				pallet_transaction_payment::NextFeeMultiplier::<Runtime>::get(),
				fee.separated_string(),
			);
		});
	};

	test_with_multiplier(min_multiplier);
	test_with_multiplier(Multiplier::saturating_from_rational(1, 1u128));
	test_with_multiplier(Multiplier::saturating_from_rational(1, 1_000u128));
	test_with_multiplier(Multiplier::saturating_from_rational(1, 1_000_000u128));
	test_with_multiplier(Multiplier::saturating_from_rational(1, 1_000_000_000u128));
}

#[test]
fn nominator_limit() {
	use pallet_election_provider_multi_phase::WeightInfo;
	// starting point of the nominators.
	let all_voters: u32 = 10_000;

	// assuming we want around 5k candidates and 1k active validators.
	let all_targets: u32 = 5_000;
	let desired: u32 = 1_000;
	let weight_with = |active| {
		<Runtime as pallet_election_provider_multi_phase::Config>::WeightInfo::submit_unsigned(
			all_voters.max(active),
			all_targets,
			active,
			desired,
		)
	};

	let mut active = 1;
	while weight_with(active).all_lte(OffchainSolutionWeightLimit::get()) || active == all_voters {
		active += 1;
	}

	println!("can support {} nominators to yield a weight of {}", active, weight_with(active));
}

#[test]
fn call_size() {
	RuntimeCall::assert_size_under(256);
}

#[test]
fn check_whitelist() {
	let whitelist: HashSet<String> = AllPalletsWithSystem::whitelisted_storage_keys()
		.iter()
		.map(|e| HexDisplay::from(&e.key).to_string())
		.collect();

	// Block number
	assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef702a5c1b19ab7a04f536c519aca4983ac"));
	// Total issuance
	assert!(whitelist.contains("c2261276cc9d1f8598ea4b6a74b15c2f57c875e4cff74148e4628f264b974c80"));
	// Execution phase
	assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef7ff553b5a9862a516939d82b3d3d8661a"));
	// Event count
	assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef70a98fdbe9ce6c55837576c60c7af3850"));
	// System events
	assert!(whitelist.contains("26aa394eea5630e07c48ae0c9558cef780d41e5e16056765bc8461851072c9d7"));
	// Configuration ActiveConfig
	assert!(whitelist.contains("06de3d8a54d27e44a9d5ce189618f22db4b49d95320d9021994c850f25b8e385"));
	// XcmPallet VersionDiscoveryQueue
	assert!(whitelist.contains("1405f2411d0af5a7ff397e7c9dc68d194a222ba0333561192e474c59ed8e30e1"));
	// XcmPallet SafeXcmVersion
	assert!(whitelist.contains("1405f2411d0af5a7ff397e7c9dc68d196323ae84c43568be0d1394d5d0d522c4"));
}

// https://kusama.subscan.io/xcm_message/kusama-98082ccbd5ae3e416b17276a0aaaeadd85aecb7a
// https://forum.moonbeam.network/t/proposal-mr38-hotfix-for-kusama-fails-to-convert-xcm-from-v3-v2/1366
#[test]
fn xcm_router_for_xcm_version_2_or_3_as_mangata_kusama_moonriver_test() {
	let manganta = MultiLocation { parents: 0, interior: X1(Parachain(2110)) };
	let manganta_sa = xcm_config::SovereignAccountOf::convert_location(&manganta).unwrap();
	let moonriver = MultiLocation { parents: 0, interior: X1(Parachain(2023)) };
	let moonriver_sa = xcm_config::SovereignAccountOf::convert_location(&moonriver).unwrap();

	let native_asset_amount_to_transfer = 20000473873659;
	let buy_execution_fee_amount_eta1 = 10000236936829;
	let buy_execution_fee_amount_eta2 = 10000236936829;

	// setup ext
	let mut t = frame_system::GenesisConfig::<Runtime>::default().build_storage().unwrap();
	pallet_balances::GenesisConfig::<Runtime> {
		balances: vec![
			(manganta_sa, ExistentialDeposit::get() + native_asset_amount_to_transfer * 2),
			(moonriver_sa, ExistentialDeposit::get()),
		],
	}
	.assimilate_storage(&mut t)
	.unwrap();
	parachains_configuration::GenesisConfig::<Runtime> {
		// for `ChildParachainRouter` to pass
		config: HostConfiguration { max_downward_message_size: 2024, ..Default::default() },
	}
	.assimilate_storage(&mut t)
	.unwrap();

	// test case fn simulating: https://kusama.subscan.io/xcm_message/kusama-98082ccbd5ae3e416b17276a0aaaeadd85aecb7a
	let process_xcm = || -> Outcome {
		// 1. process received teleported assets from relaychain
		let xcm = Xcm(vec![
			WithdrawAsset(MultiAssets::from(vec![MultiAsset {
				id: Concrete(Here.into_location()),
				fun: Fungible(native_asset_amount_to_transfer),
			}])),
			ClearOrigin,
			BuyExecution {
				fees: MultiAsset {
					id: Concrete(Here.into_location()),
					fun: Fungible(buy_execution_fee_amount_eta1),
				},
				weight_limit: Unlimited,
			},
			DepositReserveAsset {
				assets: Wild(AllCounted(1)),
				dest: moonriver,
				xcm: Xcm(vec![
					BuyExecution {
						fees: MultiAsset {
							id: Concrete(Parent.into()),
							fun: Fungible(buy_execution_fee_amount_eta2),
						},
						weight_limit: Unlimited,
					},
					DepositAsset {
						assets: Wild(AllCounted(1)),
						beneficiary: MultiLocation {
							parents: 0,
							interior: X1(AccountKey20 {
								network: None,
								key: hex_literal::hex!("f8b497a64b1216b8caaaadcc710b99d22d4b38d2"),
							}),
						},
					},
				]),
			},
		]);

		let hash = xcm.using_encoded(sp_io::hashing::blake2_256);
		XcmExecutor::<xcm_config::XcmConfig>::execute_xcm(
			manganta, // Mangata
			xcm,
			hash,
			MessageQueueServiceWeight::get(),
		)
	};

	// execute test
	sp_io::TestExternalities::new(t).execute_with(|| {
		// set xcm version for Kusama
		assert_ok!(XcmPallet::force_default_xcm_version(RuntimeOrigin::root(), Some(2),));

		// set xcm version `3` for Moonriver
		assert_ok!(XcmPallet::force_xcm_version(RuntimeOrigin::root(), Box::new(moonriver), 3,));

		// xcm should work
		assert_eq!(process_xcm().ensure_complete(), Ok(()));

		// set xcm version `2` for Moonriver
		assert_ok!(XcmPallet::force_xcm_version(RuntimeOrigin::root(), Box::new(moonriver), 2,));

		// xcm should work
		assert_eq!(process_xcm().ensure_complete(), Ok(()));
	});
}

// https://kusama.subscan.io/xcm_message/kusama-360aabeeab32fb7406725efb3e1c479307c64c00
#[test]
fn mangata_kusama_test() {
	let manganta = MultiLocation { parents: 0, interior: X1(Parachain(2110)) };
	let manganta_sa = xcm_config::SovereignAccountOf::convert_location(&manganta).unwrap();

	let native_asset_amount_to_transfer = 2000000000000;
	let buy_execution_fee_amount_eta1 = 2000000000000;

	// setup ext
	let mut t = frame_system::GenesisConfig::<Runtime>::default().build_storage().unwrap();
	pallet_balances::GenesisConfig::<Runtime> {
		balances: vec![(
			manganta_sa,
			ExistentialDeposit::get() + native_asset_amount_to_transfer * 2,
		)],
	}
	.assimilate_storage(&mut t)
	.unwrap();
	parachains_configuration::GenesisConfig::<Runtime> {
		// for `ChildParachainRouter` to pass
		config: HostConfiguration { max_downward_message_size: 2024, ..Default::default() },
	}
	.assimilate_storage(&mut t)
	.unwrap();

	// test case fn simulating: https://kusama.subscan.io/xcm_message/kusama-360aabeeab32fb7406725efb3e1c479307c64c00
	let process_xcm = |weight_limit| -> Outcome {
		// 1. process received teleported assets from relaychain
		let xcm = Xcm(vec![
			WithdrawAsset(MultiAssets::from(vec![MultiAsset {
				id: Concrete(Here.into_location()),
				fun: Fungible(native_asset_amount_to_transfer),
			}])),
			ClearOrigin,
			BuyExecution {
				fees: MultiAsset {
					id: Concrete(Here.into_location()),
					fun: Fungible(buy_execution_fee_amount_eta1),
				},
				weight_limit,
			},
			DepositAsset {
				assets: Wild(AllCounted(1)),
				beneficiary: MultiLocation {
					parents: 0,
					interior: X1(AccountId32 {
						network: None,
						id: hex_literal::hex!(
							"aee65bf22cdf1f98c91b6c176854d8072f1328e027d2e84d23607b517b1b9429"
						),
					}),
				},
			},
		]);

		let hash = xcm.using_encoded(sp_io::hashing::blake2_256);
		XcmExecutor::<xcm_config::XcmConfig>::execute_xcm(
			manganta,
			xcm,
			hash,
			MessageQueueServiceWeight::get(),
		)
	};

	// execute test
	sp_io::TestExternalities::new(t).execute_with(|| {
		sp_tracing::try_init_simple();

		// set xcm version for Kusama
		assert_ok!(XcmPallet::force_default_xcm_version(RuntimeOrigin::root(), Some(2),));

		// xcm fails on Barrier
		assert_eq!(
			process_xcm(Limited(Weight::from_parts(300000000, 0))).ensure_complete(),
			Err(XcmError::Barrier)
		);

		// xcm works
		assert_eq!(
			process_xcm(Limited(Weight::from_parts(305952000, 7186))).ensure_complete(),
			Ok(())
		);

		// xcm works
		assert_eq!(process_xcm(Unlimited).ensure_complete(), Ok(()));
	});
}
