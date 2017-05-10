# Polyverse - A Secure Distributed File System

## Introduction

Polyverse is a secured distributed file system created in Erlang. Users of Polyverse are able to upload files into the network securely and retrieve them at a later time. All files uploaded into the network are encrypted first, and only the uploader (or someone with the private key used for encryption) can decrypt the file and view its contents.

Each polyverse node will have an area on disk allocated for storage. This storage area is considered to be part of the distributed file system, and should be considered apart from the local file system of the node. When a user uploads a file to the network, one copy will exist in the local storage allocated by Polyverse, and copies will be made to other nodes. This means that users of Polyverse will host files for other users, but will only be able to view the contents of files they have the corresponding private key for.

This project was created for the Concurrent and Distributed Programming course taught at Reykjavik University.

## Secure Distributed File Systems - The Problem

There are only a few intrinsic properties to a secure distributed file system. As the name implies, security is provided for some definition of security, and the files are distributed for some definition of distribution.

The problems we will focus on solving are the ones outlined in the project description given for this course.

Functional requirements are:

* It should be able to provide the stored files
* It shall use multiple places to store the files
* It shall store the files redundantly
* It shall not use a centralized directory of its files
* At each place, files need not be stored in their entirety
* Files should be encrypted

Security concerns outlined in assignment with regard to an adversary:

* He may supply invalid files
* He may try to remove data
* He may try to intercept traffic
* Hey may try to identify the originator of a file

This document will outline how we realize a toy implementation of a secure distributed file system with regards to the requirements given. The details of our solution will be discussed and what could be done in an ideal environment. The primary reason for the difference between current implemenation and ideal implementation are time constraints.

## Network Structure

### Current Implementation

Polyverse uses an interconnected series of nodes to produce its network. No node serves any role more important than any other node. Each node is tasked with the ability to receive files from a sender, and send files to a requester.

For a node to connect to the network, it must be aware of at least one other node. Nodes in Polyverse are connected using the Erlang node connection system and epmd (Erlang Port Mapper Daemon). When a node A connects to node B, A is automatically connected to all the nodes B was connected to. Topologically this forms a complete graph and is not practical at a large scale. 

### Ideal Implementation

The complete graph works for our toy implementation, but in production a system for determining how many connections are appropriate, and whom to connect to would need to be done such that each node is sufficiently connected to the network (it can find all files, and other nodes can find all files on it).

The Erlang node connection system has the downsides of being unencrypted, utilizing only a single TCP connection for each node, and giving full access to other nodes in the network. Utilizing TCP connections setup manually would allow a node to send and receive data from other nodes simultaneously and prevent timeouts while other work is being done. These TCP connections could then also use SSL or some other secure transport protocol. Secure connections between nodes would help to prevent malicious activity that could be the result of knowing the messages exchanged between the two nodes. Lastly, using these TCP connections would allow nodes to expose a selected interface to other nodes instead of full access to the application.

## Hash Structure

Files in Polyverse are identified by a hash. The hash is built in the following manner:

hash(encrypted_file)

Where hash() is some hashing function. In the case of Polyverse, the hash is SHA256.

## Encryption

### Current Implementation

Files in Polyverse are encrypted using GPG. In our implementation, this allowed us to quickly produce encrypted files for storage on the network. Each node in the network must specify the ID of a public key inside the local GPG key-ring to be used for encryption.

### Ideal Implementation

In an ideal situation, no meta data (like that associated with the files produced by GPG) would be present. Polyverse would implement its own hybrid encryption using public/private key encryption and symmetric key encryption. If done well, this would help to make the data on the disk look indistinguishable from noise.

A node receiving a file would make sure that it is encrypted first. Unfortunately there is no way to determine if a piece of data is actually encrypted. Polyverse makes the assumption that data received is already encrypted. In future, Polyverse would employ some mechanism to store received data with yet another layer of encryption. This would ensure that files on disk are definitely encrypted.

## Uploading a File

### Current Implementation

When a node wants to upload a file to the network, that file is first encrypted, a hash is then produced based on the encrypted version of the file. The encrypted file is first stored in Polyverse's local storage on the uploader's computer, and then transmitted to all nodes connected to the uploading node.

If a node B is connected to node A, when node A uploads a file to the network, it will not attempt to upload the file to B if B is on A's blacklist. Conversely, B would not accept the file if A was on B's blacklist.

Files are sent between nodes using Erlang messages. This has its downsides but is suitable for a toy implementation as sending of large files isn't required.

### Ideal Implementation

When uploading a file the nodes on the network the uploader requests to host its file would be able to either accept or decline the file based on factors such as limited storage space, busy with other activities etc. Additionally, Polyverse would determine some sort of replication factor such that files are still stored redundantly on the network, but need not be store on each connected node.

Transfer of files would happen using TCP sockets and some protocol on top of that. This would allow for the transfer of arbitrarily large files without needing to first store the entire file in memory.

## Requesting a File

### Current Implementation

A file is requested from Polyverse by the hash produced when it is added to the network. When a user requests a file, Polyverse's storage on the local machine will be checked first. If the file does not exist on local storage, then a request is sent out to each connected node sequentially asking for the file. Each node asked replies with the file, a message indicating the node does not have the file, or a message stating the requesting node is blacklisted.

Once a file is received, it is hashed to see if it matches the hash used to request the file. If the hash does not match, the file is deleted from local storage and the sending node is blacklisted on the requesting node. Currently, another request would need to be made to the network for the file if the received file was found to be bogus.

### Ideal Implementation

A node should be able to request a file from the network, and if its connected nodes don't have it, they should be able to help the requesting node locate it. Nodes could achieve this by storing a list of files they know their neighbor has, and/or by relaying the request on behalf of the requesting node.

From a usability standpoint, Polyverse should attempt to find the file again automatically if the received file was bogus.

## Receiving a File (From an Uploader)

### Current Implementation

When a remote node A wants to upload a file to the network, it will broadcast to all its connected nodes that it intends to send the file. If A is on the blacklist of one of the nodes receiving the broadcast, it will notify A that it is blacklisted, otherwise it accepts the file.

### Ideal Implementation

Due to the way Erlang nodes work, if a receiver chooses not to receive the file, the whole file is still transmitted; the receiver just chooses to not write the file to disk. Use of manually setup TCP connections would allow a receiver to not read any data being sent to it from a blacklisted host.

## Lifetime of a File

### Current Implementation

A node on Polyverse will maintain a file uploaded to it indefinitely unless it is manually removed from the underlying file system.

### Ideal Implementation

Nodes should have a removal policy for removing files. When files are allowed to occupy space indefinitely, and can continuously be accepted, a malicious actor can fill up the disk space in a short amount of time. Even legitimate actors will eventually fill up the disk over time.

A removal policy could consist of how long it has been since a file has been accessed vs. how much remaining disk space there is.

# Polyverse Protocol

## An Overview

The number of messages exchanged in Polyverse is currently very few. They all consist of a tuple with an Erlang atom and some accompanying parameters. We will describe them here.

## request_file

This message is sent as a tuple of the form `{request_file, Node, FileName}` where Node is the requesting node, and FileName is the hash associated with the requested file.

The intent of this message is to request a file from the Polyverse network.

Two replies are possible. They are the atoms `transmitting_file` and `file_not_found`. In the event of the former, the request can expect a `receive_file` message to indicate a file is to be sent back.

## receive_file

This message is sent as a tuple of the form `{receive_file, Node, FileName, Binary}` where Node is the sending node, FileName is the hash associated with the sent file, and Binary is the binary form of the sent file.

The intent of this message is to notify the receiver of it that a file is being transmitted to it.

Two replies are possible. They are the atoms `blacklisted` which indicates the sending node is on the receiving nodes blacklist, or `file_transfer_done` which indicates a successful transfer.

## broadcast_file

This message is sent as a tuple of the form `{broadcast_file, FileName}` where FileName is the hash of the associated file in local storage. The message is only intended to be sent from a node to itself.

The intent of this message is to upload a file to all connected nodes of a given node.

The only reply is the atom `broadcast_made`.

## get_file_list

This message is sent as a tuple of the form `{get_file_list}`.

The intent of this message is to obtain a list of files on a node.

The response is an Erlang list of the files available on the receiving node.

## add_local_file

This message is sent as the tuple of the form `{add_local_file, FileName}` where FileName is the path to a file on the local node which is to be added to the Polyverse network. This message is only intended to be sent from a node to itself.

The intent of this message is to notify a node to encrypt a file and add it to local storage.

Three possible replies exist. They are `file_added` to denote the file has been added to local storage, `error_encrypting_file` to denote an error during encryption, and `error_adding_file` to denote an error adding a file to local storage. In the latter two cases, any created files are to be removed.

