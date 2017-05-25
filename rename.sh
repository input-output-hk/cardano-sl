#!/usr/bin/env bash

pushd $1
mass-replace CAddress CId
mass-replace CAccountAddress CWAddressMeta
mass-replace CAccount CAddress
mass-replace WalletAddress AccountId
mass-replace CWalletMeta CAccountMeta
mass-replace CWalletInit CAccountInit
mass-replace CWalletAddress CAccountId
mass-replace "\bCWallet\b" CAccount
mass-replace CWalletSetMeta CWalletMeta
mass-replace CWalletSetInit CWalletInit
mass-replace CWalletSetAssurance CWalletAssurance
mass-replace CWalletSet CWallet
mass-replace "\bWS\b(?=[^.])" Wal
mass-replace "\bAcc\b" Addr

mass-replace addressToCAddress addressToCId
mass-replace cAddressToAddress cIdToAddress
mass-replace encToCAddress encToCId
mass-replace walletAddrByAccount walletAddrMetaToAccount
mass-replace fromCWalletAddress fromCAccountAddress
mass-replace toCWalletAddress toCAccountAddress

mass-replace waWSId aiWSId
mass-replace waIndex aiIndex

mass-replace caaWSId cwamWSId
mass-replace caaAccountIndex cwamAccountIndex
mass-replace caaAddressIndex cwamAddressIndex
mass-replace caaId cwamId

mass-replace caId cadId
mass-replace caAmount cadAmount

mass-replace cwName caName

mass-replace cwId caId
mass-replace cwMeta caMeta
mass-replace cwAccounts caAccount
mass-replace cwAmount caAmount

mass-replace cwInitMeta caInitMeta
mass-replace cwInitWSetId cwInitWId

mass-replace cwsName cwName
mass-replace cwsAssurance cwAssurance
mass-replace cwsUnit csUnit

mass-replace cwsId cwId
mass-replace cwsWSetMeta wWSetMeta
mass-replace cwsWalletsNumber cwAccountsNumber
mass-replace cwsAmount cwAmount
mass-replace cwsHasPassphrase cwHasPassphrase
mass-replace cwsPassphraseLU cwPassphraseLU

mass-replace cwsInitMeta cwInitMeta
mass-replace cwsBackupPhrase cwBackupPhrase

mass-replace Ides Ids

popd
