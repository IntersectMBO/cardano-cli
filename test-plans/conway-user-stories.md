# CLI Conway user story inventory

Notice:
This collection of user stories and feature designs provides a snapshot of the current state of Conway era features for the cardano-cli. The included commands are designed to fulfill user stories that cover the necessary features for the Conway era. However, it's essential to note that the design and structure of each command listed herein may be subject to change in future releases. Any modifications made to the current design aim to enhance functionality or address evolving requirements while ensuring that users can still achieve the same intended results. Users are advised to stay updated with official documentation and release notes for any changes and to adapt their implementations accordingly.

# PERSONAS
|ID|NAME|DESCRIPTION|
|----|:----|:----|
|HOLDER | ADA Holder | |
|DRep | DRep | |
|CCM | Constitutional Community Member | |
|SPO | Stake Pool Operator | |

# CARDANO-CLI

## User Story ID:  CLI.001
### Title: Obtain constitution hash for verification (HOLDER)
### User Story

- As an Ada holder,<br>
I want to obtain the hash of the off-chain text of a Constitution, <br>
So that I can compare it against the hash registered on-chain to verify its authenticity.<br>

### Acceptance criteria

- [ ] A command is implemented on the cli
- [ ] The command takes as a text file, binary file or text as input  
- [ ] Calculates and returns the corresponding blake2b-256 hash of the file 
- [ ] Supports an option to save the output (the hash) to a file, default is printing to stdout
- [ ] The file is saved on the path given by the user
- [ ] The file contains the hash of the document
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation is provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.002

### Title: Generate hash of the off-chain constitution (HOLDER)
### User Story
- As an Ada holder,<br>
I want to generate the hash of the off-chain text for a proposed Constitution<br>
So that the hash can be utilized in a governance action.

Note: This is a different user story but is covered by the exact same command as CLI.001

### Acceptance criteria

- [ ] A command is implemented on the cli
- [ ] The command takes as a text file, binary file or text as input  
- [ ] Calculates and returns the corresponding blake2b-256 hash of the file 
- [ ] Supports an option to save the output (the hash) to a file, default is printing to stdout
- [ ] The file is saved on the path given by the user
- [ ] The file contains the hash of the document
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.003

### Title: Generate Committee member cold key pair (CCM)
### User Story
- As a potential Constitutional Committee member,<br>
I want to generate COLD key pair, <br>
so that I can be proposed for the Committee in a Governance action

### Acceptance criteria

- [ ] A command is implemented on the cli
- [ ] Running the command with the required parameters generates generates a COLD key pair. 
- [ ] If a parameter or the command format is incorrect an error is raised.
- [ ] The generated keys adhere to text envelope format used for other artifacts, and contains the fields Type, Description and cborHex.
- [ ] The signing key text envelope contains the correct type and description
    - Type: "ConstitutionalCommitteeColdSigningKey_ed25519"
    - Description: "Constitutional Committee Cold Signing Key" 
- [ ] The verification key text envelope has:
    - Type: "ConstitutionalCommitteeColdVerificationKey_ed25519"
    - Description: "Constitutional Committee Cold Verification Key"
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.004

### Title: Generate committee member hot key pair (CCM)
### User Story
- As Constitutional Committee member,<br>
I want to generate HOT key pair,<br>
So that I can authorize the Hot key to sign votes on behalf of the Cold key.

### Acceptance criteria

- [ ] A command is implemented on the cli
- [ ] Running the command with the required parameters generates generates a HOT key pair. 
- [ ] If a parameter or the command format is incorrect an error is raised.
- [ ] The generated keys adhere to text envelope format used for other artifacts, and contains the fields Type, Description and cborHex.
- [ ] The signing key text envelope contains the correct type and description
    - Type: "ConstitutionalCommitteeHotSigningKey_ed25519"
    - Description: "Constitutional Committee Hot Signing Key" 
- [ ] The verification key text envelope has:
    - Type: "ConstitutionalCommitteeHotVerificationKey_ed25519"
    - Description: "Constitutional Committee Hot Verification Key"
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.005

### Title: Authorization certificate (CCM)
### User Story
- As a committee member,<br>
I want to issue an authorization certificate from my cold key to a hot key,<br>
so that I can sign votes using the hot key and keep the cold key in cold storage.

### Acceptance criteria


- [ ] Running the command with accepted input parameters generates a hot key authorization certificate. 
- [ ] If a parameter or the command format is incorrect an error is raised.
- [ ] The command allows passing credentials taking as input:
    - Cold verification key 
    - Cold verification key file 
    - Cold verification key hash  
    - Hot verification key 
    - Hot verification key file
    - Hot verification key hash
- [ ] The hot key authorization certificate follows the text envelope format of other existing certificates, including the type, description, and CBOR hex fields.
- [ ] Generates a authorization certificate compliant with the conway cddl. auth_committee_hot_cert = (14, committee_cold_credential, committee_hot_credential) 
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.006

### Title: Generate committee member key hash (CCM)
### User Story
- As a potential constitutional committee member,<br>
I want to generate the key hashes of my cold verification key,<br>
so that it can be used by third parties* to propose me as a new committee member;<br>
and for identification purposes once Ive been elected as committee member.

*If I'm going to propose myself, It might be more convenient to use the cold verification key or cold verification key file directly.  

### Acceptance criteria


- [ ] Generates the blake2b 256 hash of the verification key.
- [ ] Supports passing the Cold verification key and Cold verification key file as inputs
- [ ] Gives the option to save the key hash to a file, default is printing to stdout
- [ ] The command handles potential errors, such as missing or invalid flags, and provide appropriate error messages indicating the missing or required parameters. Failing to provide the right input results in a clear error message that helps the user to identify the problem
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.007

### Title: Committee member resignation certificate (CCM)
### User Story
- As a constitutional committee member,<br>
I want to be able to generate a resignation certificate,<br>
so that i can submit it to the chain on a transaction to signal to the ada holders that I’m resigning from my duties.

### Acceptance criteria


- [ ] Takes as input any of:
    - Cold verification key 
    - Cold verification key file 
    - Cold verification key hash
    - ScriptHash
    - Script file
- [ ] Allows an optional anchor (url/hash) to express motives for resignation. The CLI does not need to check for the validity of the URL, the contents, or that the documents and declared hash match.
- [ ] Includes a mandatory flag for saving the certificate to a file.
- [ ] The resignation certificate follows the text envelope format of other existing certificates, including the type, description, and CBOR hex fields:
    - Type: "CertificateConway".
    - Description:"Constitutional Committee Cold Key Resignation Certificate."
- [ ] Generates a resignation certificate compliant with the Conway cddl: 
    - resign_committee_cold_cert = (15, committee_cold_credential, anchor / null)
- [ ] The command handles potential errors, such as missing or invalid flags or keys, and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.008

### Title: Generate Drep keys (HOLDER)
### User Story
- As an ada holder,<br>
I want to generate Ed25519 key pair,<br>
so that I can register as a DRep.

### Acceptance criteria


- [ ] Supports an option to specify the file where the verification key will be saved.
- [ ] Supports an option to specify the file where the signing key will be saved.
- [ ] The generated key files must adhere to text envelope format used for other artifacts and contains the fields Type, Description and cborHex:
- Verification key:
    - Type: "DRepVerificationKey_ed25519"
    - Description: "Delegate Representative Verification Key"
- Signing key:
    - Type: "DRepSigningKey_ed25519"
    - Description: "Delegate Representative Signing Key"
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.009

### Title: Generate DRep ID (DRep)
### User Story
- As a DRep,<br>
I want to generate a DRep Id (verification key hash),<br>
so that my voting record can be tracked,<br>
and ada holders can use the DRepId to delegate their votes to me.

### Acceptance criteria


- [ ] Generates the blake2b-224 hash digest of the serialized DRep credential
- [ ] The command accepts supplying the verification key with either:
    - Drep verification key
    - Drep verification key file
    - Drep script file **
- [ ] Supports an optional flag to allow users to save the generated DRep ID to a file. Default is printing to stdout.
- [ ] Provides an option to specify the output format. Accepted output formats are "hex" and "bech32". "bech32" is the default format.
- [ ] The command handles potential errors, such as missing or invalid flags or keys, and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.010

### Title: DRep Registration Certificate Generation (DRep)
### User Story
As a DRep,<br>
I want to generate a DRep registration certificate,<br>
so that I can submit it in a transaction and be eligible for receiving delegation.

### Acceptance criteria


- [ ] Requires the user to provide the DRep key deposit amount.
- [ ] The command allows the user to provide the DRep credential in the following ways:
    - DRep verification key.
    - DRep verification key file
    - DRep script file
    - DRep script hash
    - DRep ID
- [ ] Allows adding an optional anchor (url/hash) to submit any DRep metadata.
- [ ] Supports a mandatory flag to specify the file where the generated DRep registration certificate will be saved.
- [ ] Generates a registration certificate compliant with the Conway CDDL.    
    - reg_drep_cert = (16, drep_credential, coin, anchor / null)
- [ ] The certificate should be in a text envelope format, containing a json object with the fields type, description and cborHex.
    - "type": "CertificateConway",
    - "description": "DRep Registration Certificate",
- [ ] The command handles potential errors, such as missing or invalid flags or keys, and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.011

### Title: DRep Retirement Certificate Generation (DRep)
### User Story
As a DRep,<br>
I want to generate a DRep retirement (unregistration) certificate,<br>
so that I can submit it in a transaction stop acting as a governance actor,<br>
and retrieve my DRep deposit.

### Acceptance criteria


- [ ] Generates a DRep retirement certificate in a text envelope format.
- [ ] Allows supplying the DRep credentials in the following ways:
    - DRep verification key
    - DRep verification key file
    - DRep Id
    - Script file
    - Script hash
- [ ] Supports a mandatory flag to require the user to input the DRep deposited amount that is to be returned.
- [ ] Supports a mandatory flag to specify the file where the generated DRep retirement certificate will be saved.
- [ ] The certificate is compliant with the conway cddl
    - `unreg_drep_cert = (17, drep_credential, coin)`
- [ ] The command handles potential errors, such as missing or invalid flags or keys, and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.012

### Title: DRep Metadata Hash Generation (DRep)
### User Story
 - As a DRep,<br>
 I want to generate the hash of my DRep metadata,<br>
 so that I can supply it when registering as a DRep.

### Acceptance criteria


- [ ] Calculates the blake2b 256 hash of the metadata file supplied by the user.
- [ ] The command allows users to use the optional flag to save the calculated metadata hash to the specified file. If not used, the hash is printed to stdout.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.013

### Title: Create Update Constitution Governance Action 
### User Story
 - As an ADA holder,<br>
 I want to create a governance action that updates the constitution,
 <br>so that it can be submitted to the chain and be voted on by the governance bodies.

### Acceptance criteria


- [ ] Creates a governance action (proposal_procedure) for updating the constitution.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired
- [ ] Allows the user to provide the transaction ID and index of the previously enacted action of this type. 
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the reasoning behind the proposed change.
- [ ] Requires the user to provide an anchor of the new constitution
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the Conway CDDL
    - new_constitution = (5, gov_action_id / null, constitution)
    - constitution = [ anchor, scripthash / null ]
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.014

### Title: Create Update Constitutional Committee Governance Action (HOLDER)
### User Story
As an ADA holder,<br>
I want to create a governance action that update the constitutional committee quorum, and for adding or removing committee members<br>
so that it can be submitted to the chain and be voted on by the governance bodies.

### Acceptance criteria


- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Allows the user to provide the transaction ID and index of the previously enacted action of this type. 
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the reasoning behind the proposed change.
- [ ] Requires the user to provide an anchor of the new constitution
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] Allows options to add new members to the constitutional committee.
- [ ] Allows options to remove members from the constitutional committee.
- [ ] The command allows proposing a new quorum threshold:
    -  When adding members
    -  When removing members
    - As a standalone action (no additions or removals)
- [ ] The generated governance action complies with the Conway CDDL
    - update_committee = (4, gov_action_id / null, set<committee_cold_credential>, { committee_cold_credential => epoch }, unit_interval)
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.015

### Title: Create Treasury Withdrawal Governance Action (HOLDER)
### User Story
As an ADA holder,<br>
I want to create a governance action to withdraw funds from the treasury,<br>
so that it can be submitted to the chain and be voted on by the governance bodies.

### Acceptance criteria



- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Requires the user to provide the stake credential that will receive the funds from the treasury if the action is ratified.
- [ ] Requires the user to provide the amount in lovelace that will be transferred from the treasury to the stake credential if the action is ratified.
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the reasoning behind the proposed change.
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the Conway CDDL
    - treasury_withdrawals_action = (2, { reward_account => coin })
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.016

### Title: Create info governance action (HOLDER)
### User Story
 - As an ada holder<br>I want to create an info governance action<br>So that it can be submitted to the chain and be voted by the governance bodies<br>cardano-cli conway governance action create-info
### Acceptance criteria


- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the matter of the action. 
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the Conway CDDL:
    - info_action = 6
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.017

### Title: Create update protocol parameters governance action
### User Story
 - As an ada holder<br>I want to create a governance action to update protocol parameters<br>So that it can be submitted to the chain and be voted by the governance bodies
### Acceptance criteria



- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the matter of the action. 
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the Conway CDDL:
    - parameter_change_action = (0, gov_action_id / null, protocol_param_update)
- [ ] The command allows the user to provide the transaction id and index of the previously enacted action of this type. 
- [ ] The command includes dedicated flags to reference the protocol parameter that the user is attempting to modify
- [ ] The parameters that can be included in this type of proposal are:<br><br><br>The network group consists of:<br>- maximum block body size (maxBBSize)<br>- maximum transaction size (maxTxSize)<br>- maximum block header size (maxBHSize)<br>- maximum size of a serialized asset value (maxValSize)<br>- maximum script execution units in a single transaction (maxTxExUnits)<br>- maximum script execution units in a single block (maxBlockExUnits)<br>- maximum number of collateral inputs (maxCollateralInputs)<br><br>The economic group consists of:<br>- minimum fee coefficient (minFeeA)<br>- minimum fee constant (minFeeB)<br>- delegation key Lovelace deposit (keyDeposit)<br>- pool registration Lovelace deposit (poolDeposit)<br>- monetary expansion (rho)<br>- treasury expansion (tau)<br>- minimum fixed rewards cut for pools (minPoolCost)<br>- minimum Lovelace deposit per byte of serialized UTxO (coinsPerUTxOByte)<br>- prices of Plutus execution units (prices)<br><br>The technical group consists of:<br>- pool pledge influence (a0)<br>- pool retirement maximum epoch (eMax)<br>- desired number of pools (nOpt)<br><br>Plutus execution cost models (costModels)<br>- proportion of collateral needed for scripts (collateralPercentage)<br><br>The governance group consists of all the new protocol parameters that are introduced in this CIP:<br>- governance voting thresholds<br>- dRepVotingThresholds<br>- dvtCommitteeNoConfidence<br>- dvtCommitteeNormal<br>- dvtHardForkInitiation<br>- dvtMotionNoConfidence<br>- dvtPPEconomicGroup<br>- dvtPPGovGroup<br>- dvtPPNetworkGroup<br>- dvtPPTechnicalGroup<br>- dvtTreasuryWithdrawal<br>- dvtUpdateToConstitution<br>- poolVotingThresholds<br>- pvtCommitteeNoConfidence<br>- pvtCommitteeNormal<br>- pvtHardForkInitiation<br>- pvtMotionNoConfidence<br>- governance action maximum lifetime in epochs (govActionLifetime)<br>- governance action deposit (govActionDeposit)<br>- DRep deposit amount (drepDeposit)<br>- DRep activity period in epochs (drepActivity)<br>- minimal constitutional committee size (ccMinSize)<br>- maximum term length (in epochs) for the constitutional committee members (ccMaxTermLength) 
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.018

### Title: Create no-confidence governance action (HOLDER)
### User Story
 - As an ada holder<br>I want to create a no-confidence governance action<br>So that it can be submitted to the chain and be voted by the governance bodies
### Acceptance criteria


- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the matter of the action. 
- [ ] Allows the user to provide the transaction id and index of the last enacted action updating the committee. 
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the Conway CDDL:
    - no_confidence = (3, gov_action_id / null)` 
 - [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.019

### Title: Create Hard-fork initiation governance action
### User Story
 - As an ada holder<br>I want to create a governance action to initiate a hardfork<br>So that it can be submitted to the chain and be voted by the governance bodies
### Acceptance criteria


- [ ] Creates a governance action (proposal_procedure) for updating the constitutional committee.
    - proposal_procedure = [ deposit : coin, reward_account, gov_action, anchor]
- [ ] The command prompts the user to provide the deposit amount for submitting governance actions.
- [ ] Requires the user to provide the stake credential that will receive the deposit return when the action is enacted/expired.
- [ ] Requires the user to provide a mandatory anchor (URL/hash) of the proposal, a document where the proposer exposes the matter of the action. 
- [ ] Allows the user to provide the transaction id and index of the last enacted action of this type. 
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The generated governance action complies with the conway cddl:
    - hard_fork_initiation_action = (1, gov_action_id / null, [protocol_version])
 - [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.020

### Title: View governance action file
### User Story
 - As an ada holder<br>I want to inspect the contents of a governance action file <br>So that I can verify it is correct before submitting it in a transaction
### Acceptance criteria


- [ ] Decodes the cbor hex contents of the governance action file and prints it in human readable format. 
- [ ] The command has a flag to specify the path where the output file will be saved. If not used, the output printed to stdout. 
- [ ] Gives an option to select the output format (json or yaml).
- [ ] By default if no flag is picked, the JSON format will be applied
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.021

### Title: Create a governance action vote (DRep/SPO/CCM)
### User Story
 - As a Drep, SPO or CC member <br>I want to create a vote for a governance action <br>So that I can include it in a transaction and submit it to the chain <br>
### Acceptance criteria


- [ ] - [ ] The command creates a vote file based on voting procedures:
    - voting_procedures = { + voter => { + gov_action_id => voting_procedure }
    - voting_procedure =[ vote, anchor / null ]
    - voter = [ 0, addr_keyhash // 1, scripthash // 2, addr_keyhash // 3, scripthash // 4, addr_keyhash ]
    - vote = 0 .. 2. // no - 0; yes - 1; abstain - 2 
- [ ] The command provides a way to vote yes, no and abstain
- [ ] Requires to specify the governance action ID and index that the vote is about.
- [ ] Requires the user to provide DRep, SPO or CC credential
- [ ] The command has option to add an anchor (url / hash), for the voter to express the reasonging or any relevant data backing his vote. 
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.022

### Title: View vote file (DRep/SPO/CCM)
### User Story
 - As a DRep, SPO or CC member <br>I want to inspect the contents of a vote file <br>So that I can verify it is correct before submitting it in a transaction
### Acceptance criteria


- [ ] The command takes a vote file as an input and decodes the cbor and shows the information of the vote based on the voting procedures in a human readable format (english).
- [ ] Gives an option to select the output format (json or yaml).
- [ ] The command has a flag to specify the path where the output file will be saved. 
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.023

### Title: Build a transaction to submit proposal
### User Story
 - As an ada holder <br>
 I want to build a transaction that includes a proposal (containing a governance action)<br>
 So that I can later sign and submit to the chain.
 
### Acceptance criteria

- [ ] Modify `transaction build` command to allow including a proposal (governance action) in a transaction body
- [ ] When constructing a transaction body that includes a proposal, the resulting tx body conforms to the conway cddl so that proposal procedures are recorded with the tag 20:
    - transaction_body =<br>
{ 0 : set<transaction_input> ; inputs<br>
, 1 : [* transaction_output]<br>
, 2 : coin ; fee<br>
, ? 3 : uint ; time to live<br>
, ? 4 : certificates<br>
, ? 5 : withdrawals<br>
, ? 7 : auxiliary_data_hash<br>
, ? 8 : uint ; validity interval start<br>
, ? 9 : mint<br>
, ? 11 : script_data_hash<br>
, ? 13 : nonempty_set<transaction_input> ; collateral inputs<br>
, ? 14 : required_signers<br>
, ? 15 : network_id<br>
, ? 16 : transaction_output ; collateral return<br>
, ? 17 : coin ; total collateral<br>
, ? 18 : nonempty_set<transaction_input> ; reference inputs<br>
, ? 19 : voting_procedures ; New; Voting procedures<br>
, ? 20 : proposal_procedures ; New; Proposal procedures<br>
, ? 21 : coin ; New; current treasury value<br>
, ? 22 : positive_coin ; New; donation<br>
}<br>
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.024

### Title: Build transaction for proposal vote (DRep, SPO, CCM)
### User Story
 - As a DRep, SPO or CC member<br>I want to build a transaction that includes my vote on a particular governance action<br>So that I can later sign and submit to the chain<br>
### Acceptance criteria

- [ ] Modify `transaction build` command to allow including a vote in a transaction body
- [ ] When constructing a transaction body that includes a vote, the resulting tx body conforms to the conway cddl so that proposal procedures are recorded with the tag 19:
    - transaction_body =<br>
{ 0 : set<transaction_input> ; inputs<br>
, 1 : [* transaction_output]<br>
, 2 : coin ; fee<br>
, ? 3 : uint ; time to live<br>
, ? 4 : certificates<br>
, ? 5 : withdrawals<br>
, ? 7 : auxiliary_data_hash<br>
, ? 8 : uint ; validity interval start<br>
, ? 9 : mint<br>
, ? 11 : script_data_hash<br>
, ? 13 : nonempty_set<transaction_input> ; collateral inputs<br>
, ? 14 : required_signers<br>
, ? 15 : network_id<br>
, ? 16 : transaction_output ; collateral return<br>
, ? 17 : coin ; total collateral<br>
, ? 18 : nonempty_set<transaction_input> ; reference inputs<br>
, ? 19 : voting_procedures ; New; Voting procedures<br>
, ? 20 : proposal_procedures ; New; Proposal procedures<br>
, ? 21 : coin ; New; current treasury value<br>
, ? 22 : positive_coin ; New; donation<br>
}<br>
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.025

### Title: Build RAW transaction for proposal vote (HOLDER)
### User Story
 - As an ada holder <br>
 I want to build a transaction that includes a proposal (containing a governance action)<br>So that I can later sign and submit to the chain <br>transaction build-raw
### Acceptance criteria

- [ ] Modify `transaction build-raw` command to allow including a proposal (governance action) in a transaction body
- [ ] When constructing a transaction body that includes a proposal, the resulting tx body conforms to the conway cddl so that proposal procedures are recorded with the tag 20:
    - transaction_body =<br>
{ 0 : set<transaction_input> ; inputs<br>
, 1 : [* transaction_output]<br>
, 2 : coin ; fee<br>
, ? 3 : uint ; time to live<br>
, ? 4 : certificates<br>
, ? 5 : withdrawals<br>
, ? 7 : auxiliary_data_hash<br>
, ? 8 : uint ; validity interval start<br>
, ? 9 : mint<br>
, ? 11 : script_data_hash<br>
, ? 13 : nonempty_set<transaction_input> ; collateral inputs<br>
, ? 14 : required_signers<br>
, ? 15 : network_id<br>
, ? 16 : transaction_output ; collateral return<br>
, ? 17 : coin ; total collateral<br>
, ? 18 : nonempty_set<transaction_input> ; reference inputs<br>
, ? 19 : voting_procedures ; New; Voting procedures<br>
, ? 20 : proposal_procedures ; New; Proposal procedures<br>
, ? 21 : coin ; New; current treasury value<br>
, ? 22 : positive_coin ; New; donation<br>
}<br>
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.026

### Title: Build RAW transaction for proposal vote (DRep/SPO/CCM)
### User Story
 - As a DRep, SPO or CC member<br>I want to build a transaction that includes my vote on a particular governance action<br>So that I can later sign and submit to the chain<br><br>`transaction build-raw`<br>
### Acceptance criteria

- [ ] Modify `transaction build-raw` command to allow including a vote in a transaction body
- [ ] When constructing a transaction body that includes a vote, the resulting tx body conforms to the conway cddl so that proposal procedures are recorded with the tag 19:
    - transaction_body =<br>
{ 0 : set<transaction_input> ; inputs<br>
, 1 : [* transaction_output]<br>
, 2 : coin ; fee<br>
, ? 3 : uint ; time to live<br>
, ? 4 : certificates<br>
, ? 5 : withdrawals<br>
, ? 7 : auxiliary_data_hash<br>
, ? 8 : uint ; validity interval start<br>
, ? 9 : mint<br>
, ? 11 : script_data_hash<br>
, ? 13 : nonempty_set<transaction_input> ; collateral inputs<br>
, ? 14 : required_signers<br>
, ? 15 : network_id<br>
, ? 16 : transaction_output ; collateral return<br>
, ? 17 : coin ; total collateral<br>
, ? 18 : nonempty_set<transaction_input> ; reference inputs<br>
, ? 19 : voting_procedures ; New; Voting procedures<br>
, ? 20 : proposal_procedures ; New; Proposal procedures<br>
, ? 21 : coin ; New; current treasury value<br>
, ? 22 : positive_coin ; New; donation<br>
}<br>
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.027

### Title: Create stake registration certificate 
### User Story
 - As an ada holder<br>
I want to create a Conway cddl-compliant stake registration certificate <br>
So that I can take part of the protocol.

### Acceptance criteria

- [ ] Allows the user to provide credentials in any of the following forms:
    - Stake verification key
    - Stake verification key file
    - Stake address
    - Stake script file
- [ ] Requires the user to provide the key deposit amount in lovelace
- [ ] The resulting certificate conforms with the Conway cddl:
    - reg_cert = (7, stake_credential, coin)
- [ ] The command has a flag to specify the path where the certificate will be saved.
- [ ] The certificate is saved on a text envelope format consisting of a json object with type, description and cbor hex fields, where:
    - "type": "CertificateConway",
    - "description": "Stake Address Registration Certificate",
    - "cborHex": ""
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.028

### Title: Create stake deregistration certificate (HOLDER)
### User Story
 - As an ada holder<br>
 I want to create a Conway cddl-compliant stake deregistration certificate<br> 
 So that I can get my deposit back and stop taking part on the protocol<br>
### Acceptance criteria

- [ ] Allows the user to provide credentials in any of the following forms:
    - Stake verification key
    - Stake verification key file
    - Stake address
    - Stake script file
- [ ] Requires the user to provide the key deposit amount (in lovelace) that was paid on registration.
- [ ] The resulting certificate conforms with the Conway cddl:
    - unreg_cert = (8, stake_credential, coin)
- [ ] The command has a flag to specify the path where the certificate will be saved.
- [ ] The certificate is saved on a text envelope format consisting of a json object with type, description and cbor hex fields, where:
    - "type": "CertificateConway",
    - "description": "Stake Address Deregistration Certificate",
    - "cborHex": ""
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.029

### Title: Delegate vote to DRep (HOLDER)
### User Story
 - As an ada holder<br>
 I want to delegate my votes to a DRep (registered or default)<br>
 So that my stake is counted according to the rules when governance actions are voted. 
 
### Acceptance criteria

- [ ] Allows the user to provide credentials in any of the following forms:
    - Stake verification key
    - Stake verification key file
    - Stake address
    - Stake script file
- [ ] When delegating to a registered DRep, the user can provide the target DRep with
    - DRep script hash
    - DRep verification key
    - DRep verification key file
    - DRep key hash (DRep ID)
- [ ] When delegating to a default DRep the user can use a flag to select either always-abstain or always-no-confidence
- [ ] The certificate should be saved on a text envelope format consisting of a json object with type, description and cbor hex fields, where:
    - "type": "CertificateConway"
    - "description": "Vote Delegation Certificate"
    - "cborHex": ""
- [ ] The resulting certificate conforms with the conway cddl, where 
    - vote_deleg_cert = (9, stake_credential, drep)
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.030

### Title: Delegate stake to SPO and votes to DRep with a single certificate (HOLDER)
### User Story
 - As an ada holder<br>
 I want to delegate my stake to a stake pool AND
 my votes to a DRep (registered or default) with a single certificate.

### Acceptance criteria

- [ ] Allows the user to provide credentials in any of the following forms:
    - Stake verification key
    - Stake verification key file
    - Stake address
    - Stake script file
- [ ] When delegating to a registered DRep, the user can provide the target DRep with
    - DRep script hash
    - DRep verification key
    - DRep verification key file
    - DRep key hash (DRep ID)
- [ ] When delegating to a default DRep the user can use a flag to select either always-abstain or always-no-confidence
- [ ] The user can provide the target stake pool with: 
    - Stake pool cold verification key
    - Stake pool cold verification key file
    - Stake pool ID
- [ ] The certificate should be saved on a text envelope format consisting of a json object with type, description and cbor hex fields, where:
    - "type": "CertificateConway"
    - "description": "Stake and Vote Delegation Certificate"
    - "cborHex": ""
- [ ] The resulting certificate conforms with the conway cddl, where 
    - stake_vote_deleg_cert = (10, stake_credential, pool_keyhash, drep)
- [ ] The command has a flag to specify the path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.031

### Title: Query governance state
### User Story
- As any persona<br>
I want to query the nodes for the current Governance state<br>
so that I can inform my decisions.
### Acceptance criteria

- [ ] Returns all of the state related to governance (govState)
- [ ] The output is in a json format so that it can be further processed programmatically.
- [ ] Has a flag to specify the path where the output file will be saved
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] Requires a connection to the node, an exception is raised if there is non.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.


## User Story ID:  CLI.032

### Title: Query committee state (CCM)
### User Story
- As a CC member<br>I want to query the committee state<br>so that I can find my expiration term,<br> and whether my hot key authorization certificate has been recorded on chain correctly.
### Acceptance criteria

- [ ] Returns the state of the constitutional committee. 
- [ ] Supports filtering by 
    - credential
    - status (active/expired)  
- [ ] The command has an option to specify a path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] Requires a connection to the node, an exception is raised if there is non.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.033

### Title: Query DRep state (HOLDER)
### User Story
 - As an ada holder<br>
 I want to query the DRep state <br>
 So that I can find detailed information about registered Dreps<br>

### Acceptance criteria

- [ ] Returns the state of the registered Dreps. 
- [ ] Supports a query for an specific DRep credential. 
- [ ] If no Drep credential is specified it returns all DReps.
- [ ] The output is a JSON showing, the following information:
    - Drep ID
    - Anchor url/hash, null (Drep metadata)
    - Deposit
    - Expiry (from Drep activity)
- [ ] The output is in a json format so that it can be further processed programmatically
- [ ] The command has an option to specify a path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] Requires a connection to the node, an exception is raised if there is non.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.034

### Title: Query DRep stake distribution (HOLDER)
### User Story
 - As an ada holder and DRep<br>
 I want to query the DRep stake distribution<br>
 So that I can find the weight (of the votes) of each DRep<br>

### Acceptance criteria
- [ ] Returns the map of DRep key hashes and the current voting stake delegated to each, including default DReps.
- [ ] Supports a query for an specific DRep credential:
    - DRep verification key
    - DRep verification key file
    - DRep verification key hash (DRep ID)
- [ ] The output is in a json format so that it can be further processed programmatically.
- [ ] The command has an option to specify a path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] Requires a connection to the node, an exception is raised if there is non.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID: CLI.035

### Title: Expand query stake-address-info to show deposits and vote delegation (HOLDER)
### User Story

 - As an ada holder,<br>
 I want to query my stake address information<br> 
 So that I can learn to which Stake Pool and DRep I'm delegating to and the value in Lovelace of my deposits for registering stake address and for submitting governance actions.

### Acceptance criteria

- [ ] Expand the command `query stake-address-info` to return the StakePool and DRep that the stake credential is delegated to and the value of the existing deposits.
- [ ] The command has an option to specify a path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] Requires a connection to the node, an exception is raised if there is non.    
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.036

### Title: query constitution
### User Story
- As any persona I want to query the on-chain constitution so that I can know the url where it is stored and the document hash so that I can verify authenticity. 

### Acceptance criteria

- [ ] Returns the current constitution URL and hash.
- [ ] The command has an option to specify a path where the output file will be saved.
- [ ] The command handles potential errors, such as missing or invalid flags and provide appropriate error messages indicating the missing or required parameters.
- [ ] End user documentation should be provided, including a corresponding CLI usage, describing the feature, its purpose, and how to use it, along with the expected types of inputs and outputs.

## User Story ID:  CLI.037

### Title: Register scripts as DRep 
### User Story
As an ada holder I want to register a native or a Plutus script as a DRep.
### Acceptance criteria


## User Story ID:  CLI.038

### Title: Template
### User Story
### Acceptance criteria
As an ada holder I want to unregister a native or a Plutus script as a DRep.

## User Story ID:  CLI.039

### Title: Template
### User Story
As an script based DRep I want to vote on a governance action
### Acceptance criteria





