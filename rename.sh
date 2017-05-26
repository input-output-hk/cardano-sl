#!/usr/bin/env bash

pushd $1

for pre in get delete new update remove add import change restore create does "set" \
		Get Does Set Create Add Remove; do
    mass-replace "${pre}Account" "${pre}WAddress"
    mass-replace "${pre}WalletSet" "${pre}WSet"
    mass-replace "${pre}Wallet" "${pre}Account"
    mass-replace "${pre}WSet" "${pre}Wallet"
done

mass-replace getAccountAccounts getAccountWAddresses
mass-replace GetAccountAccounts GetAccountWAddresses
mass-replace getAccountState getWalletState
mass-replace getAccountWebSockets getWalletWebSockets
mass-replace getAccountWebState getWalletWebState
mass-replace getWalletWalletAddrs getWalletAccountIds

mass-replace getSetAccounts getWalletAccounts
mass-replace GetSetAccounts GetWalletAccounts

popd
