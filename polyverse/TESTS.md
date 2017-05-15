# Testing

The testing of Polyverse was not very formal. Most testing would be considered exploratory testing where we tested the system by trying to use it. The test cases we considered are described below.

### Test Case 1

A node is able to start up and read it's local file storage.

### Test Case 2

When a node connects to a network all it's files are propagated to the nodes in the network and all of the connecting nodes files are propagated to the network.

### Test Case 3

A node is able to upload a file to the network and it's propagated to all nodes it is connected to.

### Test Case 4

When node A stores a file on node B that file is confirmed to be correct on node B by re-hashing the file it received and comparing the filename with the respective hash. If the two hashes are not equal the file is deleted of the local file system and node A is put on node B's blacklist.

### Test Case 5

When a node broadcasts a file to it's connected nodes it does not send files to nodes on it's blacklist.

### Test Case 6

When two nodes are already connected but their local file storage isn't synced up (they don't have all of each others files) and a third joins, the connecting node receives all files from both nodes it connected to.

### Summary

These exploratory tests all worked as intended, but we do recognize that this method of testing leaves a lot to be desired.


# On The Subject of Deadlocks

In the application Polyverse we utilized Erlang's application, Erlang's generic server, and Erlang's supervisor models to manage the Erlang nodes. This means that a lot of generic logic has been introduced and one of those is timeouts. The timeouts make it so that when the server is handling a call, that call only has a certain time to be handled, which means that if we encounter a deadlock while handling a call that deadlocked process will be killed off after a small wait. All the logic done in Polyverse happens leveraging timeouts, sequential logic, or a sepearate isolated thread. For these reasons Polyverse's code base does not contain situations in where deadlocks are possible.

# On The Subject of Security

In polyverse the security of files is apparent. No metadata other than the metadata you select yourself to add to your GPG private key. A legitimate user can be sure that his files are stored securely because before adding his files to the network his files will be encrypted.

There are security holes that are introduced because of the projects time constraints. Those security holes are that in Erlangs node connection system a node is given full access to a node it connects to. This could be disastrous in a production environment because any node can theoretically do anything erlang can do on any other node, that can include deleting files, reading files etc.

More detalied descriptions of the security issues in Polyverse, and thoughts on how to address them are in DOCUMENTATION.md.