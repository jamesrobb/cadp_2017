# Polyverse - A Secure Distributed File System

## Introduction

Polyverse is a secured distributed file system created in Erlang. Users of Polyverse are able to upload files into the network securely and retrieve them at a later time. All files uploaded into the network are encrypted first, and only the uploader (or someone with the private key used for encryption) can decrypt the file and view its contents.

Each polyverse node will have an area on disk allocated for storage. This storage area is considered to be part of the distributed file system, and should be considered apart from the local file system of the node. When a user uploads a file to the network, one copy will exist in the local storage allocated by Polyverse, and copies will be made to other nodes. This means that users of Polyverse will host files for other users, but will only be able to view the contents of files they have the corresponding private key for.

This project was created for the Concurrent and Distributed Programming course taught at Reykjavik University.

## Network Structure

Polyverse uses an interconnected series of nodes to produce its network. No node serves any role more important than any other node. Each node is tasked with the ability to receive files from a sender, and send files to a requester.

For a node to connect to the network, it must be aware of at least one other node. It can then ask that node for the nodes it knows about to achieve a stronger level of connectivity with the network and be able to access a wider selection of files.

Each node stores a list of other nodes it is aware of. Upon request the node will respond with the nodes it knows about. Each node also stores a table of files currently stored on it as a list of hashes. If a requester requests a file, they do so by providing a hash. If a node has a file stored with the corresponding hash, it is sent to the requester.

## Hash Structure

Files in Polyverse are identified by a hash. The hash is built in the following manner:

hash(encrypted_file)

Where hash() is some hashing function. In the case of Polyverse, the hash is SHA512.

## Encryption

Files will be encrypted using an RSA public/private key encryption scheme. Polyverse nodes expect the public and private key to be provided to it. 

## Uploading a File

When a user wants to upload a file, a hash is first produced based on the encrypted version of the file. The encrypted file is first stored in Polyverse's local storage on the uploader's computer, and then transmitted to X (a configurable amount) of other Polyverse nodes.

To transmit files to other nodes, first a request is sent out to all known nodes asking if the nodes will accept the transmission. Nodes willing to accept the file will reply with a confirmation message. The file can then be transmitted to the accepting nodes.

Take note that the original filename is lost and not transmitted. Once the file has been uploaded to Polyverse, its filename is the produced hash.

## Requesting a File

When a user requests a file, Polyverse's storage on the local machine will be checked first. If the file does not exist on local storage, then a request broadcast is sent out to all known nodes. The first to reply with a message denoting that the node has the requested file will then be used. Another message is then sent to the remote node to ask it to begin transmission of the requested file.

Once a file is received, an attempt to decrypt it is made. If the file can not be decrypted successfully, then it is not determined to not be the file originally sent and is therefore bogus.

When a node receives a file determined to be bogus, the node that sent the file is flagged as malicious, and a request is then sent to another node to retrieve the requested file.

## Receiving a File (From an Uploader)

When a remote user attempts to upload a file into the Polyverse network, a message from that user will be received if the user wants your node to host the file. If your node can accommodate the file, a reply is sent back to the requester confirming that your node will accept the file, and that they can begin transmission.

If your node is not able to accommodate the file, no response is to be sent.

## Lifetime of a File

Currently a node on Polyverse will maintain a file uploaded to it indefinitely unless it is manually removed from the underlying file system. In the future, a popularity/available space model could be used to determine when a file should be removed.

# Dealing with Malicious Actors

## Current Security Measures

As all files transmitted are already encrypted, it is not practically possible for a malicious actor to determine the contents of a file. However, attack vectors related to file transmission still do exist. A malicious actor could send a file that was not requested to a requester. To combat this, a requester can always compute the hash of the unencrypted file. If the requester can decrypt the file and produce a hash that matches the one used to request the file, then the requester knows the file received is the correct one.

The nodes in the network can try to "muscle out" malicious actors by removing the malicious actor from their respective node lists. This forces all actors to be good actors at least some of the time, otherwise all nodes will eventually ignore the malicious actor.

Malicious actors could attempt to overwrite a file already on the network. To combat this, if a node already has the file, it does nothing. As the hash is based on the file contents, accepting a file with the same hash should be the same file, and therefore overwriting the existing one is not necessary.

## Future Security Considerations

In the future, a more adaptable security model could be adapted. For example, by only blacklisting a node for a couple hours before allowing it to rejoin the network. Nodes could also notify each other of malicious actors.

Malicious actors could send unencrypted malicious files to users on Polyverse. This could be prevented by checking that the files received are encrypted files.

Malicious actors could poison the network by uploading files with the same hash as another file on the network. When the user that uploaded the legitimate file to the network tries to retrieve it, he/she could try to download the file from a non-malicious actor that is hosting the bogus file. As a result, the original uploader will flag the non-malicious actor as malicious and stop interacting with them.
